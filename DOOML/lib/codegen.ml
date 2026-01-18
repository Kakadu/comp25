module Map = Base.Map.Poly

let context = Llvm.global_context ()
let builder = Llvm.builder context
let the_module = Llvm.create_module context "main"
open (val Llvm_wrapper.make context builder the_module)

let failf fmt = Format.kasprintf failwith fmt

let define_ibinop name ret build_f = 
    let typ = function_type ret [| i64_type; i64_type |] in
    let func = define_func name (Llvm.return_type typ) (Llvm.param_types typ) in
    let entry = entry_block func in
    position_at_end entry;
    (match params func with
    | [| lhs; rhs |] ->
        let binop = build_f lhs rhs in
        build_ret binop |> ignore;
    | _ -> assert false);
    Llvm_analysis.assert_valid_function func

let emit_builtins () = 
    declare_func "print_int" void_type [| i64_type |] |> ignore;
    declare_func "create_closure" i64_type [| i64_type; i64_type; i64_type; i64_type |] |> ignore;
    declare_func "closure_apply" i64_type [| i64_type; i64_type; i64_type |] |> ignore;
    define_ibinop "+" i64_type build_add;
    define_ibinop "-" i64_type build_sub;
    define_ibinop "*" i64_type build_mul;
    define_ibinop "/" i64_type build_sdiv;
    define_ibinop "<" i1_type (build_icmp Llvm.Icmp.Slt);
    define_ibinop ">" i1_type (build_icmp Llvm.Icmp.Sgt);
    define_ibinop "<=" i1_type (build_icmp Llvm.Icmp.Sle);
    define_ibinop ">=" i1_type (build_icmp Llvm.Icmp.Sge);
    define_ibinop "=" i1_type (build_icmp Llvm.Icmp.Eq)

let emit_create_closure func args = 
     let arity = params func |> Array.length in
    let argc = List.length args in
     let create_closure = lookup_func_exn "create_closure" in
     let func = build_pointercast func i64_type ~name:"func_toi64_cast" in 
     let argc_lv = const_int i64_type argc in
     let argv_lv = build_array_alloca ~name:"create_closure_argv" i64_type argc_lv in
     args |> List.iteri
     (fun i a ->
         let el_ptr = build_gep argv_lv [| (const_int i64_type i) |] in
         build_store a el_ptr |> ignore);
     let argv_lv = build_pointercast argv_lv i64_type ~name:"args_arr_toi64_cast" in 
     let arity_lv = const_int i64_type arity in
     build_call (lookup_func_type_exn "create_closure") create_closure [ func; arity_lv; argc_lv; argv_lv ]

let emit_immexpr binds = 
    function
  | Anf.ImmNum n -> const_int i64_type n
  | Anf.ImmId s ->
    (match Map.find binds s with
     | Some lv -> lv
     | None -> (match lookup_func s with
        | Some f -> emit_create_closure f []
        | None -> failf "Unbound variable %s" s))

let emit_capp binds name args = 
    let app_type = match lookup_func name with
    (** binops are defined inside llvm ir and processed as regular functions
        they will be inlined **)
    | Some func -> `Fun (func, params func |> Array.length)
    | None -> (match Map.find binds name with
        | Some closure -> `Closure closure
        | None -> failf "Unbound application %s" name)
    in
    let argc = List.length args
    in 
    match app_type with
    | `Fun (func, arity) when argc == arity ->
        let args_lv = args |> List.map
         (fun a ->
             emit_immexpr binds a)
         in
         let typ = lookup_func_type_exn name in
         build_call typ func args_lv
    | `Fun (func, arity) when argc < arity ->
        let args = args |> List.map
        (fun a -> emit_immexpr binds a) in
        emit_create_closure func args
    | `Fun (_, arity) ->
            failf
           "Too many arguments (%d) are passed for the function %s, expected %d"
           argc
           name
           arity
    | `Closure closure ->
        let args_lv = args |> List.map
         (fun a ->
             emit_immexpr binds a)
         in
         let closure_apply = lookup_func_exn "closure_apply" in
         let argc_lv = const_int i64_type argc in
         let argv_lv = build_array_alloca ~name:"closure_apply_argv" i64_type argc_lv in
         args_lv |> List.iteri
         (fun i a ->
             let el_ptr = build_gep argv_lv [| (const_int i64_type i) |] in
             build_store a el_ptr |> ignore);
         let argv_lv = build_pointercast argv_lv i64_type ~name:"args_arr_toi64_cast" in 
         let apply_args = [ closure; argc_lv; argv_lv ] in
         let typ = lookup_func_type_exn "closure_apply" in
         build_call typ closure_apply apply_args

let rec emit_cexpr binds =
    function
    | Anf.CImm imm -> emit_immexpr binds imm
    | Anf.CIte (cond_, then_, else_) ->
        let cond_lv = emit_immexpr binds cond_ in
        let zero = const_int i1_type 0 in
        build_icmp Llvm.Icmp.Ne cond_lv zero |> ignore;

        let start_bb = insertion_block () in
        let the_function = block_parent start_bb in

        let then_bb = append_block ~name:"then" the_function in
        position_at_end then_bb;
        let then_lv = emit_aexpr binds then_ in
        let new_then_bb = insertion_block () in

        let else_bb = append_block ~name:"else" the_function in
        position_at_end else_bb;
        let else_lv = emit_aexpr binds else_ in
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
        emit_capp binds name args

and emit_aexpr binds = function
    | Anf.AExpr expr -> emit_cexpr binds expr
    | Anf.ALet (pattern, bind, body) -> 
        let bind_lv = emit_cexpr binds bind in
        let binds = Map.update binds pattern ~f:(fun _ -> bind_lv) in
        emit_aexpr binds body


let emit_decl (decl: Anf.decl) = 
    match decl with
    | Anf.Decl (rec_flag, name, par, body) ->
        (if has_toplevel_func name then failf "Function redefinition %s" name);
        let declare () = List.map (fun _ -> i64_type) par |> Array.of_list |> declare_func name i64_type in
        let f = (match rec_flag with
        | Ast.Rec -> declare ()
        | Ast.NonRec -> failf "todo") in
        let par_binds = par |>
            List.mapi (fun i a -> (i, a)) |> 
            List.fold_left (fun acc (i, a) -> 
                (a, (params f).(i)) :: acc) [ ] |> Map.of_alist_exn in
        let entry_bb = append_block ~name:"entry" f in
        position_at_end entry_bb;
        let body = emit_aexpr par_binds body in
        (match rec_flag with
        | Ast.Rec -> ()
        | Ast.NonRec -> failf "todo");
        build_ret body |> ignore;
        Llvm_analysis.assert_valid_function f;
        f
;;

let emit_ir ?(triple = "x86_64-pc-linux-gnu") program  =
    assert (Llvm_executionengine.initialize ());
    Llvm.set_target_triple triple the_module;
    emit_builtins ();
    List.iter (fun d -> emit_decl d |> ignore) program;
    the_module

let optimize_ir ?(triple = "x86_64-pc-linux-gnu") module_ =
    let target = Llvm_target.Target.by_triple triple in
    let machine =
      Llvm_target.TargetMachine.create
        ~triple:triple target in
    let options = Llvm_passbuilder.create_passbuilder_options () in
    Llvm_passbuilder.passbuilder_options_set_verify_each options true;
    Llvm_passbuilder.passbuilder_options_set_debug_logging options true;
    Llvm_passbuilder.passbuilder_options_set_slp_vectorization options true;
    Llvm_passbuilder.passbuilder_options_set_merge_functions options true;
    Llvm_passbuilder.passbuilder_options_set_inliner_threshold options 2;
      (match Llvm_passbuilder.run_passes module_ "default<O2>" machine options with
      | Error e -> failf "Optimization error %s" e
      | Ok () -> ());
    Llvm_passbuilder.dispose_passbuilder_options options

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

    declare void @print_int(i64)

    declare i64 @create_closure(i64, i64, i64, i64)

    declare i64 @apply_closure(i64, i64, i64)

    define i64 @"+"(i64 %0, i64 %1) {
    entry:
      %2 = add i64 %0, %1
      ret i64 %2
    }

    define i64 @-(i64 %0, i64 %1) {
    entry:
      %2 = sub i64 %0, %1
      ret i64 %2
    }

    define i64 @"*"(i64 %0, i64 %1) {
    entry:
      %2 = mul i64 %0, %1
      ret i64 %2
    }

    define i64 @"/"(i64 %0, i64 %1) {
    entry:
      %2 = sdiv i64 %0, %1
      ret i64 %2
    }

    define i1 @"<"(i64 %0, i64 %1) {
    entry:
      %2 = icmp slt i64 %0, %1
      ret i1 %2
    }

    define i1 @">"(i64 %0, i64 %1) {
    entry:
      %2 = icmp sgt i64 %0, %1
      ret i1 %2
    }

    define i1 @"<="(i64 %0, i64 %1) {
    entry:
      %2 = icmp sle i64 %0, %1
      ret i1 %2
    }

    define i1 @">="(i64 %0, i64 %1) {
    entry:
      %2 = icmp sge i64 %0, %1
      ret i1 %2
    }

    define i1 @"="(i64 %0, i64 %1) {
    entry:
      %2 = icmp eq i64 %0, %1
      ret i1 %2
    }

    define i64 @f(i64 %0) {
    entry:
      %1 = call i1 @"="(i64 1, i64 %0)
      %2 = icmp ne i1 %1, false
      br i1 %1, label %then, label %else

    then:                                             ; preds = %entry
      br label %merge

    else:                                             ; preds = %entry
      %3 = call i64 @-(i64 1, i64 %0)
      %4 = call i64 @f(i64 %3)
      %5 = call i64 @"*"(i64 %0, i64 %4)
      br label %merge

    merge:                                            ; preds = %else, %then
      %ifphi = phi i64 [ 1, %then ], [ %5, %else ]
      ret i64 %ifphi
    }
    |}]
;;
