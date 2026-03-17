(** Copyright 2026,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Middleend.Anf
open Common.Ast
open Target
open Common.Parser

let context = Llvm.global_context ()
let i64_type = Llvm.i64_type context
let gl_align = Target.word_size
let void_type = Llvm.void_type context
let gcheader_type = Llvm.struct_type context [| i64_type |]
let block_elms_type = Llvm.array_type i64_type 0
let block_type = Llvm.struct_type context [| gcheader_type; i64_type; block_elms_type |]
let ptr_type = Llvm.pointer_type context
let builder = Llvm.builder context
let subst_main = "__user_main"

module ParamMap = Map.Make (String)

module FuncMap = struct
  module K = struct
    type t = ident

    let compare = Stdlib.compare
  end

  module M = Map.Make (K)

  type status =
    | User
    | Lib

  type t = (Llvm.llvalue * Llvm.lltype * status) M.t

  let empty () : t = M.empty

  let bind (t : t) (x : ident) (ll : Llvm.llvalue * Llvm.lltype * status) : t =
    M.add x ll t
  ;;

  let find (t : t) (x : ident) : (Llvm.llvalue * Llvm.lltype * status) option =
    M.find_opt x t
  ;;

  let find_exn (t : t) (x : ident) : Llvm.llvalue * Llvm.lltype * status = M.find x t
  let keys = M.bindings

  let print_fmap (t : t) =
    let _ =
      List.map
        (fun (id, (fval, _, s)) ->
           let stat =
             match s with
             | User -> "user"
             | Lib -> "lib"
           in
           Format.printf
             "Id: %s Arity: %d Status: %s\n"
             id
             (Array.length (Llvm.params fval))
             stat)
        (keys t)
    in
    ()
  ;;
end

(* Return types from runtime.c *)
let initial_fmap the_mod =
  let decl fmap id retty argtyps =
    let ftyp = Llvm.function_type retty argtyps in
    let fval = Llvm.declare_function id ftyp the_mod in
    FuncMap.bind fmap id (fval, ftyp, FuncMap.Lib)
  in
  let fmap = FuncMap.empty () in
  let fmap = decl fmap "print_int" void_type [| i64_type |] in
  let fmap = decl fmap "alloc_block" i64_type (*ptr*) [| i64_type |] in
  let fmap =
    decl fmap "alloc_closure" i64_type (*ptr*) [| i64_type (*ptr*); i64_type |]
  in
  let fmap =
    decl fmap "apply1" i64_type (*ptr or int*) [| i64_type (*ptr*); i64_type |]
  in
  let fmap = decl fmap "print_gc_status" void_type [||] in
  let fmap = decl fmap "collect" void_type [||] in
  let fmap = decl fmap "create_tuple" i64_type (*ptr*) [| i64_type |] in
  let fmap =
    decl fmap "create_tuple_init" i64_type (*ptr*) [| i64_type; i64_type (*ptr*) |]
  in
  let fmap = decl fmap "field" i64_type (*ptr or int*) [| i64_type (*ptr*); i64_type |] in
  let fmap = decl fmap "rt_init" void_type [| i64_type |] in
  fmap
;;

let build_call_mb_void ftype fval argvs name =
  match Llvm.return_type ftype with
  | ty when ty = void_type ->
    let _ = Llvm.build_call ftype fval argvs "" builder in
    Llvm.const_int i64_type 0
  | _ -> Llvm.build_call ftype fval argvs name builder
;;

let decl_and_bind fmap the_mod id retty argc =
  let id = if id = "main" then subst_main else id in
  match FuncMap.find fmap id with
  | Some (_, _, FuncMap.Lib) -> fmap
  | _ when argc = 0 -> fmap
  | _ ->
    let argtyps = Array.make argc i64_type in
    let ftyp = Llvm.function_type retty argtyps in
    let fval = Llvm.declare_function id ftyp the_mod in
    FuncMap.bind fmap id (fval, ftyp, FuncMap.User)
;;

let prefill_fmap (fmap0 : FuncMap.t) the_mod (program : aprogram) : FuncMap.t =
  List.fold_left
    (fun fm -> function
       | Anf_str_value (_rf, name, anf_expr) ->
         (match anf_expr with
          | Anf_let (_, _, Comp_func (ps, _), _) | Anf_comp_expr (Comp_func (ps, _)) ->
            decl_and_bind fm the_mod name i64_type (List.length ps)
          | _ -> decl_and_bind fm the_mod name i64_type 0)
       | _ -> fm)
    fmap0
    program
;;

let build_alloc_closure fmap func =
  let acval, actyp, _ = FuncMap.find_exn fmap "alloc_closure" in
  let argc = Array.length (Llvm.params func) in
  let argc = Llvm.const_int i64_type argc in
  let func_as_i64 = Llvm.build_pointercast func i64_type "func_as_i64" builder in
  Llvm.build_call actyp acval [| func_as_i64; argc |] "closure_tmp" builder
;;

let gen_im_expr_ir fmap env = function
  | Imm_num n -> Llvm.const_int i64_type ((n lsl 1) lor 1)
  | Imm_ident id ->
    (match ParamMap.find_opt id env with
     | Some v ->
       let temp = Llvm.build_load i64_type v id builder in
       Llvm.set_alignment gl_align temp;
       temp
     | None ->
       let id = if id = "main" then subst_main else id in
       (match FuncMap.find fmap id with
        | Some (fval, ftyp, _) ->
          if Array.length (Llvm.params fval) = 0
          then build_call_mb_void ftyp fval [||] "calltmp"
          else build_alloc_closure fmap fval
        | None -> invalid_arg ("Name not bound: " ^ id)))
;;

let create_entry_alloca the_fun var_name =
  let builder = Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block the_fun)) in
  Llvm.build_alloca i64_type var_name builder
;;

(* working with tagged integers *)
let gen_tagged_binop fmap env op lhs rhs =
  let left = gen_im_expr_ir fmap env lhs in
  let right = gen_im_expr_ir fmap env rhs in
  let one = Llvm.const_int i64_type 1 in
  match op with
  | "+" ->
    let temp = Llvm.build_add left right "addtmp1" builder in
    Llvm.build_sub temp one "addtmp2" builder
  | "-" ->
    let temp = Llvm.build_sub left right "subtmp1" builder in
    Llvm.build_add temp one "subtmp2" builder
  | "*" ->
    let left' = Llvm.build_lshr left one "multmp1" builder in
    let right' = Llvm.build_sub right one "multmp2" builder in
    let temp = Llvm.build_mul left' right' "multmp3" builder in
    Llvm.build_add temp one "multmp4" builder
  | "<" ->
    let temp = Llvm.build_icmp Llvm.Icmp.Slt left right "slttmp" builder in
    Llvm.build_zext temp i64_type "slttmp_as_i64" builder
  | "<=" ->
    let temp = Llvm.build_icmp Llvm.Icmp.Sle left right "sletmp" builder in
    Llvm.build_zext temp i64_type "sletmp_as_i64" builder
  | ">" ->
    let temp = Llvm.build_icmp Llvm.Icmp.Sgt left right "sgttmp" builder in
    Llvm.build_zext temp i64_type "sgttmp_as_i64" builder
  | ">=" ->
    let temp = Llvm.build_icmp Llvm.Icmp.Sge left right "sgetmp" builder in
    Llvm.build_zext temp i64_type "sgetmp_as_i64" builder
  | "=" ->
    let temp = Llvm.build_icmp Llvm.Icmp.Eq left right "eqtmp" builder in
    Llvm.build_zext temp i64_type "eqtmp_as_i64" builder
  | "<>" ->
    let temp = Llvm.build_icmp Llvm.Icmp.Ne left right "neqtmp" builder in
    Llvm.build_zext temp i64_type "neqtmp_as_i64" builder
  | _ -> invalid_arg ("Unsupported binary operator: " ^ op)
;;

let build_apply_part fmap fclos args =
  let apval, aptyp, _ = FuncMap.find_exn fmap "apply1" in
  List.fold_left
    (fun clos arg ->
       let clos_as_i64 = Llvm.build_pointercast clos i64_type "clos_as_i64" builder in
       build_call_mb_void aptyp apval [| clos_as_i64; arg |] "apptmp")
    fclos
    args
;;

let rec gen_comp_expr_ir fmap env = function
  | Comp_imm imm -> gen_im_expr_ir fmap env imm
  | Comp_binop (op, lhs, rhs) -> gen_tagged_binop fmap env op lhs rhs
  | Comp_app (Imm_ident op, [ arg1; arg2 ]) when is_operator_char op.[0] ->
    gen_tagged_binop fmap env op arg1 arg2
  | Comp_app (Imm_ident f, args) ->
    let f_map = if f = "main" then subst_main else f in
    (match FuncMap.find fmap f_map with
     | Some (fval, ftype, _) ->
       let pvs = Llvm.params fval in
       let argvs = List.map (fun arg -> gen_im_expr_ir fmap env arg) args in
       if List.length args = Array.length pvs
       then build_call_mb_void ftype fval (Array.of_list argvs) "calltmp"
       else (
         let fclos = build_alloc_closure fmap fval in
         build_apply_part fmap fclos argvs)
     | None ->
       (match ParamMap.find_opt f env with
        | Some clos_ptr ->
          let clos_val = Llvm.build_load i64_type clos_ptr (f ^ "_val") builder in
          Llvm.set_alignment gl_align clos_val;
          let argvs = List.map (fun arg -> gen_im_expr_ir fmap env arg) args in
          build_apply_part fmap clos_val argvs
        | None -> invalid_arg ("Id: " ^ f ^ " not found")))
  | Comp_app (Imm_num _, _) -> invalid_arg "cannot apply number as a function"
  | Comp_branch (cond, br_then, br_else) ->
    let cv = gen_im_expr_ir fmap env cond in
    let zero = Llvm.const_int i64_type 0 in
    let cond_val = Llvm.build_icmp Llvm.Icmp.Ne cv zero "cond" builder in
    let start_bb = Llvm.insertion_block builder in
    let the_fun = Llvm.block_parent start_bb in
    let then_bb = Llvm.append_block context "then" the_fun in
    Llvm.position_at_end then_bb builder;
    let then_val, _ = gen_anf_expr fmap env br_then in
    let new_then_bb = Llvm.insertion_block builder in
    let else_bb = Llvm.append_block context "else" the_fun in
    Llvm.position_at_end else_bb builder;
    let else_val, _ = gen_anf_expr fmap env br_else in
    let new_else_bb = Llvm.insertion_block builder in
    let merge_bb = Llvm.append_block context "ifcont" the_fun in
    Llvm.position_at_end merge_bb builder;
    let incoming = [ then_val, new_then_bb; else_val, new_else_bb ] in
    let phi = Llvm.build_phi incoming "iftmp" builder in
    Llvm.position_at_end start_bb builder;
    let _ = Llvm.build_cond_br cond_val then_bb else_bb builder in
    Llvm.position_at_end new_then_bb builder;
    let _ = Llvm.build_br merge_bb builder in
    Llvm.position_at_end new_else_bb builder;
    let _ = Llvm.build_br merge_bb builder in
    Llvm.position_at_end merge_bb builder;
    phi
  | Comp_alloc imms | Comp_tuple imms ->
    let ctval, cttyp, _ = FuncMap.find_exn fmap "create_tuple_init" in
    let argc = Llvm.const_int i64_type (List.length imms) in
    let argv = List.map (fun im -> gen_im_expr_ir fmap env im) imms in
    let alloca = Llvm.build_array_alloca i64_type argc "tuple_vals_alloca" builder in
    List.iteri
      (fun i elem ->
         let ptr_to_elem =
           Llvm.build_gep
             i64_type
             alloca
             [| Llvm.const_int i64_type i |]
             "ptr_to_elem"
             builder
         in
         let store = Llvm.build_store elem ptr_to_elem builder in
         Llvm.set_alignment gl_align store;
         ())
      argv;
    let alloca_as_i64 = Llvm.build_pointercast alloca i64_type "alloca_as_i64" builder in
    Llvm.build_call cttyp ctval [| argc; alloca_as_i64 |] "tuple_tmp" builder
  | Comp_load (imexpr, offset) ->
    let vbase = gen_im_expr_ir fmap env imexpr in
    let voffst = Llvm.const_int i64_type (offset / Target.word_size) in
    let fifn, fity, _ = FuncMap.find_exn fmap "field" in
    Llvm.build_call fity fifn [| vbase; voffst |] "load_tmp" builder
  | Comp_func (_, _) -> invalid_arg "anonymous functions should be lambda-lifted"

and gen_anf_expr fmap env = function
  | Anf_comp_expr comp ->
    let v = gen_comp_expr_ir fmap env comp in
    v, env
  | Anf_let (_, name, comp_expr, body) ->
    let init_val = gen_comp_expr_ir fmap env comp_expr in
    let the_fun = Llvm.block_parent (Llvm.insertion_block builder) in
    let alloca = create_entry_alloca the_fun name in
    let store = Llvm.build_store init_val alloca builder in
    Llvm.set_alignment gl_align store;
    let new_env = ParamMap.add name alloca env in
    gen_anf_expr fmap new_env body
;;

let gen_function fmap the_mod name params body =
  let name = if name = "main" then subst_main else name in
  let param_types = Array.map (fun _ -> i64_type) (Array.of_list params) in
  let f_type = Llvm.function_type i64_type param_types in
  let the_fun =
    match Llvm.lookup_function name the_mod with
    | None -> Llvm.declare_function name f_type the_mod
    | Some f ->
      if Array.length (Llvm.basic_blocks f) = 0
      then ()
      else invalid_arg ("Redefinition of function: " ^ name);
      if Array.length (Llvm.params f) = List.length params
      then ()
      else invalid_arg ("Redefinition of function with different number of args: " ^ name);
      f
  in
  let bb = Llvm.append_block context "entry" the_fun in
  Llvm.position_at_end bb builder;
  let env =
    List.fold_left2
      (fun env name pval ->
         Llvm.set_value_name name pval;
         let alloca = create_entry_alloca the_fun name in
         let store = Llvm.build_store pval alloca builder in
         Llvm.set_alignment gl_align store;
         ParamMap.add name alloca env)
      ParamMap.empty
      params
      (Array.to_list (Llvm.params the_fun))
  in
  let ret_val, _ = gen_anf_expr fmap env body in
  let _ = Llvm.build_ret ret_val builder in
  if Llvm_analysis.verify_function the_fun
  then ()
  else (
    Stdlib.Format.printf
      "invalid function generated\n%s\n"
      (Llvm.string_of_llvalue the_fun);
    Llvm_analysis.assert_valid_function the_fun);
  the_fun
;;

let gen_astructure_item fmap the_mod main_fn env = function
  | Anf_str_eval expr ->
    Llvm.position_at_end (Llvm.entry_block main_fn) builder;
    let _, new_env = gen_anf_expr fmap env expr in
    new_env
  | Anf_str_value (_, name, Anf_comp_expr (Comp_func (params, body))) ->
    let _ = gen_function fmap the_mod name params body in
    Llvm.position_at_end (Llvm.entry_block main_fn) builder;
    env
  | Anf_str_value (_, name, expr) ->
    Llvm.position_at_end (Llvm.entry_block main_fn) builder;
    let value, _ = gen_anf_expr fmap env expr in
    let alloca = create_entry_alloca main_fn name in
    let store = Llvm.build_store value alloca builder in
    Llvm.set_alignment gl_align store;
    ParamMap.add name alloca env
;;

let optimize_ir the_mod (triple : string) (opt : string option) =
  let target = Llvm_target.Target.by_triple triple in
  let machine = Llvm_target.TargetMachine.create ~triple target in
  let opts = Llvm_passbuilder.create_passbuilder_options () in
  let optflag =
    match opt with
    | Some opt -> opt
    | _ -> "O0"
  in
  let optflag = "default<" ^ optflag ^ ">" in
  (match Llvm_passbuilder.run_passes the_mod optflag machine opts with
   | Error e -> invalid_arg e
   | Ok () -> ());
  Llvm_passbuilder.dispose_passbuilder_options opts
;;

let gen_program_ir (program : aprogram) (triple : string) (opt : string option) =
  let the_module = Llvm.create_module context "main" in
  Llvm_all_backends.initialize ();
  Llvm.set_target_triple triple the_module;
  assert (Llvm_executionengine.initialize ());
  let fmap = prefill_fmap (initial_fmap the_module) the_module program in
  let main_ty = Llvm.function_type i64_type [||] in
  let main_fn = Llvm.define_function "main" main_ty the_module in
  Llvm.position_at_end (Llvm.entry_block main_fn) builder;
  let initfn, initty, _ = FuncMap.find_exn fmap "rt_init" in
  let _ =
    build_call_mb_void initty initfn [| Llvm.const_int i64_type (5 * 1024) |] "inittmp"
  in
  let env = ParamMap.empty in
  let _ =
    List.fold_left
      (fun env item -> gen_astructure_item fmap the_module main_fn env item)
      env
      program
  in
  let bbs = Llvm.basic_blocks main_fn in
  Llvm.position_at_end bbs.(Array.length bbs - 1) builder;
  let col_fn, col_ty, _ = FuncMap.find_exn fmap "collect" in
  let _ = build_call_mb_void col_ty col_fn [||] "_" in
  let _ = Llvm.build_ret (Llvm.const_int i64_type 0) builder in
  optimize_ir the_module triple opt;
  match Llvm_analysis.verify_module the_module with
  | Some r -> invalid_arg r
  | None -> Llvm.string_of_llmodule the_module
;;
