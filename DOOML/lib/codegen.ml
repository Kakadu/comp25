module Map = Base.Map.Poly

let context = Llvm.global_context ()
let builder = Llvm.builder context
let the_module = Llvm.create_module context "main"
open (val Llvm_wrapper.make context builder the_module)

let failf fmt = Format.kasprintf failwith fmt

type visibility =
    | Internal
    | External

let unbox funcs v =
    let (f, typ, _) = Map.find_exn funcs "unbox" in
    build_call typ f [ v ]

let box_imm funcs imm =
    let (f, typ, _) = Map.find_exn funcs "box_imm" in
    build_call typ f [ imm ]

let define_ibinop ?(box_ret = false) funcs name ret build_f =
    let typ = function_type ret [| i64_type; i64_type |] in
    let func = define_func name (Llvm.return_type typ) (Llvm.param_types typ) in
    let entry = entry_block func in
    position_at_end entry;
    (match params func with
    | [| lhs; rhs |] ->
        let (lhs, rhs) = (unbox funcs lhs, unbox funcs rhs) in
        let binop = build_f lhs rhs in
        let binop = if box_ret then box_imm funcs binop else binop in
        build_ret binop |> ignore;
    | _ -> assert false);
    Llvm_analysis.assert_valid_function func;
    ( name, (func, typ, External) )

let declare_internal name ret params = 
    let f = declare_func name ret params in
    let t = function_type ret params in
    ( name, (f, t, Internal ) )

let declare_external name ret params = 
    let f = declare_func name ret params in
    let t = function_type ret params in
    ( name, (f, t, External ) )

let emit_builtins () = 
    let rt = [ declare_external "print_int" i64_type [| i64_type |];
    declare_internal "create_closure" i64_type [| i64_type; i64_type; i64_type; i64_type |];
    declare_internal "closure_apply" i64_type [| i64_type; i64_type; i64_type |];
    declare_internal "create_tuple" i64_type [| i64_type; i64_type |];
    declare_external "tuple_nth" i64_type [| i64_type; i64_type |];
    declare_internal "unbox" i64_type [| i64_type |];
    declare_internal "box_imm" i64_type [| i64_type |];
    declare_internal "gc_init" void_type [| |];
    declare_internal "sp_init" void_type [| i64_type |];
    declare_internal "llvm.stacksave.p0" ptr_type [| |];
    declare_external "collect" void_type [| |];
    ] |> Map.of_alist_exn in
    let binops =[ define_ibinop ~box_ret:true rt "+" i64_type build_add;
    define_ibinop ~box_ret:true rt "-" i64_type build_sub;
    define_ibinop ~box_ret:true rt "*" i64_type build_mul;
    define_ibinop ~box_ret:true rt "/" i64_type build_sdiv;
    define_ibinop rt "<" i1_type (build_icmp Llvm.Icmp.Slt);
    define_ibinop rt ">" i1_type (build_icmp Llvm.Icmp.Sgt);
    define_ibinop rt "<=" i1_type (build_icmp Llvm.Icmp.Sle);
    define_ibinop rt ">=" i1_type (build_icmp Llvm.Icmp.Sge);
    define_ibinop rt "=" i1_type (build_icmp Llvm.Icmp.Eq) ] |> Map.of_alist_exn in
    Map.merge_disjoint_exn rt binops

let emit_create_closure funcs func args = 
     let arity = params func |> Array.length in
    let argc = List.length args in
     let (create_closure, typ, _) = Map.find_exn funcs "create_closure" in
     let func = build_pointercast func i64_type ~name:"func_toi64_cast" in 
     let argc_lv = const_int i64_type argc in
     let argv_lv = build_array_alloca ~name:"create_closure_argv" i64_type argc_lv in
     args |> List.iteri
     (fun i a ->
         let el_ptr = build_gep argv_lv [| (const_int i64_type i) |] in
         build_store a el_ptr |> ignore);
     let argv_lv = build_pointercast argv_lv i64_type ~name:"args_arr_toi64_cast" in 
     let arity_lv = const_int i64_type arity in
     build_call typ create_closure [ func; arity_lv; argc_lv; argv_lv ]

let emit_create_tuple funcs init =
    let size = List.length init in
     let size_lv = const_int i64_type size in
     let init_lv = build_array_alloca ~name:"create_tuple_init" i64_type size_lv in
     init |> List.iteri
     (fun i a ->
         let el_ptr = build_gep init_lv [| (const_int i64_type i) |] in
         build_store a el_ptr |> ignore);
     let init_lv = build_pointercast init_lv i64_type ~name:"init_arr_toi64_cast" in 
     let (create_tuple, typ, _) = Map.find_exn funcs "create_tuple" in
     build_call typ create_tuple [ size_lv; init_lv ]

let rec emit_immexpr binds funcs = 
    function
  | Anf.ImmNum n -> const_int i64_type n |> box_imm funcs
  | Anf.ImmId s ->
    (match Map.find binds s with
     | Some lv -> lv
     | None -> (match Map.find funcs s with
        | Some (f, _, External) -> emit_create_closure funcs f []
        | Some _
        | None -> failf "Unbound variable %s" s))
  | Anf.ImmTuple immexprs -> 
        let init = List.map (fun immexpr -> emit_immexpr binds funcs immexpr) immexprs in
        emit_create_tuple funcs init

let emit_capp binds funcs name args = 
    let app_type = match Map.find funcs name with
    (** binops are defined inside llvm ir and processed as regular functions
        they will be inlined **)
    | Some (func, typ, External) -> `Fun (func, typ, params func |> Array.length)
    | Some _
    | None -> (match Map.find binds name with
        | Some closure -> `Closure closure
        | None -> failf "Unbound application %s" name)
    in
    let argc = List.length args
    in 
    match app_type with
    | `Fun (func, typ, arity) when argc == arity ->
        let args_lv = args |> List.map
         (fun a ->
             emit_immexpr binds funcs a)
         in
         build_call typ func args_lv
    | `Fun (func, _, arity) when argc < arity ->
        let args = args |> List.map
        (fun a -> emit_immexpr binds funcs a) in
        emit_create_closure funcs func args
    | `Fun (_, _, arity) ->
            failf
           "Too many arguments (%d) are passed for the function %s, expected %d"
           argc
           name
           arity
    | `Closure closure ->
        let args_lv = args |> List.map
         (fun a ->
             emit_immexpr binds funcs a)
         in
         let (closure_apply, typ, _) = Map.find_exn funcs "closure_apply" in
         let argc_lv = const_int i64_type argc in
         let argv_lv = build_array_alloca ~name:"closure_apply_argv" i64_type argc_lv in
         args_lv |> List.iteri
         (fun i a ->
             let el_ptr = build_gep argv_lv [| (const_int i64_type i) |] in
             build_store a el_ptr |> ignore);
         let argv_lv = build_pointercast argv_lv i64_type ~name:"args_arr_toi64_cast" in 
         let apply_args = [ closure; argc_lv; argv_lv ] in
         build_call typ closure_apply apply_args

let rec emit_cexpr binds funcs =
    function
    | Anf.CImm imm -> emit_immexpr binds funcs imm
    | Anf.CIte (cond_, then_, else_) ->
        let cond_lv = emit_immexpr binds funcs cond_ in
        let zero = const_int i1_type 0 in
        build_icmp Llvm.Icmp.Ne cond_lv zero |> ignore;

        let start_bb = insertion_block () in
        let the_function = block_parent start_bb in

        let then_bb = append_block ~name:"then" the_function in
        position_at_end then_bb;
        let then_lv = emit_aexpr binds funcs then_ in
        let new_then_bb = insertion_block () in

        let else_bb = append_block ~name:"else" the_function in
        position_at_end else_bb;
        let else_lv = emit_aexpr binds funcs else_ in
        let new_else_bb = insertion_block () in

        let merge_bb = append_block ~name:"merge" the_function in
        position_at_end merge_bb;

        let phi_setup = [(then_lv, new_then_bb); (else_lv, new_else_bb)] in
        let phi = build_phi ~name:"ifphi" phi_setup in

        position_at_end start_bb;
        build_cond_br cond_lv then_bb else_bb |> ignore;
        position_at_end new_then_bb;
        build_br merge_bb |> ignore;
        position_at_end new_else_bb;
        build_br merge_bb |> ignore;
        position_at_end merge_bb;

        phi
    | Anf.CApp (name, args) ->
        emit_capp binds funcs name args

and emit_aexpr binds funcs = function
    | Anf.AExpr expr -> emit_cexpr binds funcs expr
    | Anf.ALet (pattern, bind, body) -> 
        let bind_lv = emit_cexpr binds funcs bind in
        let binds = Map.update binds pattern ~f:(fun _ -> bind_lv) in
        emit_aexpr binds funcs body

let emit_decl funcs (decl: Anf.decl) = 
    match decl with
    | Anf.Decl (rec_flag, name, par, body) ->
        (if Map.find funcs name != None then failf "Function redefinition %s" name);
        let (_, funcs_entry) = List.map (fun _ -> i64_type) par |> Array.of_list |> declare_external name i64_type in
        let (f, _, _) = funcs_entry in
        let funcs = (match rec_flag with
        | Ast.Rec -> funcs |> Map.add_exn ~key:name ~data:funcs_entry
        | Ast.NonRec -> funcs) in
        let par_binds = par |>
            List.mapi (fun i a -> (i, a)) |> 
            List.fold_left (fun acc (i, a) -> 
                (a, (params f).(i)) :: acc) [ ] |> Map.of_alist in
        let par_binds = match par_binds with
        | `Duplicate_key k -> failf "Multiple parameters %s in fun %s" k name
        | `Ok m ->  m in
        let entry_bb = append_block ~name:"entry" f in
        position_at_end entry_bb;
        (if name = "main" then 
            let (gc_init, gc_init_t, _) = Map.find_exn funcs "gc_init" in
            build_call gc_init_t gc_init [ ] |> ignore;
            let (sp_init, sp_init_t, _) = Map.find_exn funcs "sp_init" in
            let (ss, ss_t, _) = Map.find_exn funcs "llvm.stacksave.p0" in
            let sp = build_call ss_t ss [ ] in
            let sp = build_pointercast sp i64_type in
            build_call sp_init_t sp_init [ sp ] |> ignore);
        let body = emit_aexpr par_binds funcs body in
        let body = if name = "main" then unbox funcs body else body in
        build_ret body |> ignore;
        let funcs = (match rec_flag with
        | Ast.Rec -> funcs
        | Ast.NonRec -> funcs |> Map.add_exn ~key:name ~data:funcs_entry) in
        Llvm_analysis.assert_valid_function f;
        funcs
;;

let emit_ir ?(triple = "x86_64-pc-linux-gnu") program  =
    assert (Llvm_executionengine.initialize ());
    Llvm.set_target_triple triple the_module;
    let funcs = emit_builtins () in
    List.fold_left (fun funcs d -> emit_decl funcs d) funcs program |> ignore;
    Llvm_all_backends.initialize ();
    the_module

let optimize_ir ?(triple = "x86_64-pc-linux-gnu") module_ =
    let target = Llvm_target.Target.by_triple triple in
    let machine =
      Llvm_target.TargetMachine.create
        ~triple:triple target in
    let options = Llvm_passbuilder.create_passbuilder_options () in
    Llvm_passbuilder.passbuilder_options_set_verify_each options true;
    Llvm_passbuilder.passbuilder_options_set_slp_vectorization options true;
    Llvm_passbuilder.passbuilder_options_set_merge_functions options true;
    Llvm_passbuilder.passbuilder_options_set_inliner_threshold options 2;
      (match Llvm_passbuilder.run_passes module_ "default<O2>" machine options with
      | Error e -> failf "Optimization error %s" e
      | Ok () -> ());
    Llvm_passbuilder.dispose_passbuilder_options options

let emit_binary ?(triple = "x86_64-pc-linux-gnu") ?(features = "") module_ file = 
    let target = Llvm_target.Target.by_triple triple in
    let machine =
      Llvm_target.TargetMachine.create
        ~triple:triple ~features:features target in
    Llvm_target.TargetMachine.emit_to_file module_ Llvm_target.CodeGenFileType.ObjectFile file machine

let pp_module ppf module_=
    Format.fprintf ppf "%s" (Llvm.string_of_llmodule module_)

let%expect_test "basic" =
  let ast =
    Fe.parse
      {|
    let rec f = fun n ->
      if n = 1 then 1
      else (f (n - 1)) * n
    ;;
  |}
    |> Result.map_error (fun err -> Format.printf "Error %s" err)
    |> Result.get_ok
  in
  Format.printf
    "%a"
    pp_module
    (Cc.cc ast
     |> fun asts ->
     Format.printf
       "CC: %a\n\n"
       (Format.pp_print_list ~pp_sep:Format.pp_print_newline Ast.pp_top_level)
       asts;
     asts
     |> Ll.ll
     |> fun asts ->
     Format.printf
       "LL: %a\n\n"
       (Format.pp_print_list ~pp_sep:Format.pp_print_newline Ast.pp_top_level)
       asts;
     asts
     |> Anf.anf
     |> fun asts ->
     Format.printf
       "ANF %a\n\n"
       (Format.pp_print_list ~pp_sep:Format.pp_print_newline Anf.pp_decl)
       asts;
     asts |> emit_ir);
  [%expect
    {|
    CC: let rec f = fun n -> if (=) n 1 then 1 else (*) (f ((-) n 1)) n;;


    LL: let rec f = fun n -> if (=) n 1 then 1 else (*) (f ((-) n 1)) n;;


    ANF let rec f n =
            let sup1 =
              (=) n 1
            in
            let ite7 =
              if sup1 then
                1
              else
                let sup4 =
                  (-) n 1
                in
                let sup5 =
                  (f) sup4
                in
                let sup6 =
                  (*) sup5 n
                in
                sup6
            in
            ite7
          ;;


    ; ModuleID = 'main'
    source_filename = "main"
    target triple = "x86_64-pc-linux-gnu"

    define i1 @"="(i64 %0, i64 %1) {
    entry:
      %2 = icmp eq i64 %0, %1
      ret i1 %2
    }

    define i1 @">="(i64 %0, i64 %1) {
    entry:
      %2 = icmp sge i64 %0, %1
      ret i1 %2
    }

    define i1 @"<="(i64 %0, i64 %1) {
    entry:
      %2 = icmp sle i64 %0, %1
      ret i1 %2
    }

    define i1 @">"(i64 %0, i64 %1) {
    entry:
      %2 = icmp sgt i64 %0, %1
      ret i1 %2
    }

    define i1 @"<"(i64 %0, i64 %1) {
    entry:
      %2 = icmp slt i64 %0, %1
      ret i1 %2
    }

    define i64 @"/"(i64 %0, i64 %1) {
    entry:
      %2 = sdiv i64 %0, %1
      ret i64 %2
    }

    define i64 @"*"(i64 %0, i64 %1) {
    entry:
      %2 = mul i64 %0, %1
      ret i64 %2
    }

    define i64 @-(i64 %0, i64 %1) {
    entry:
      %2 = sub i64 %0, %1
      ret i64 %2
    }

    define i64 @"+"(i64 %0, i64 %1) {
    entry:
      %2 = add i64 %0, %1
      ret i64 %2
    }

    declare i64 @tuple_nth(i64, i64)

    declare i64 @create_tuple(i64, i64)

    declare i64 @closure_apply(i64, i64, i64)

    declare i64 @create_closure(i64, i64, i64, i64)

    declare void @print_int(i64)

    define i64 @f(i64 %0) {
    entry:
      %1 = call i1 @"="(i64 %0, i64 1)
      %2 = icmp ne i1 %1, false
      br i1 %1, label %then, label %else

    then:                                             ; preds = %entry
      br label %merge

    else:                                             ; preds = %entry
      %3 = call i64 @-(i64 %0, i64 1)
      %4 = call i64 @f(i64 %3)
      %5 = call i64 @"*"(i64 %4, i64 %0)
      br label %merge

    merge:                                            ; preds = %else, %then
      %ifphi = phi i64 [ 1, %then ], [ %5, %else ]
      ret i64 %ifphi
    }
    |}]
;;
