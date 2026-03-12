(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm
open Runtime.Primitives
open Architecture
open Llvm_backend
open Analysis
open Frontend.Ast
open Middleend.Anf
open Generator_state

let tag_int n = 1 + (n lsl 1)
let tag_bool b = if b then 4 else 2
let tag_char c = tag_int (Char.code c)
let context = global_context ()
let builder = builder context
let int_t = i64_type context
let i32_t = i32_type context
let void_t = void_type context
let ptr_t = pointer_type context

let lltype_of_arg = function
  | Ptr -> ptr_t
  | Int -> int_t
  | I32 -> i32_t
;;

let lltype_of_ret = function
  | RPtr -> ptr_t
  | RInt -> int_t
  | RVoid -> void_t
;;

let predefined_funcs =
  List.map
    (fun { name; ret; args } ->
       let ret_t = lltype_of_ret ret in
       let arg_t = Array.of_list (List.map lltype_of_arg args) in
       name, function_type ret_t arg_t)
    predefined_runtime_funcs
;;

let predefined_init current_module =
  List.fold_left
    (fun (value_env, type_env) (function_name, function_type) ->
       let function_value = declare_function function_name function_type current_module in
       ( Base.Map.set value_env ~key:function_name ~data:function_value
       , Base.Map.set type_env ~key:function_name ~data:function_type ))
    (Base.Map.empty (module Base.String), Base.Map.empty (module Base.String))
    predefined_funcs
;;

let emit_void builder instr : (unit, string) Result.t =
  match emit builder instr with
  | _ -> Ok ()
;;

let emit_void_st builder instr =
  match emit_void builder instr with
  | Ok () -> return ()
  | Error e -> fail e
;;

let with_optional_value = function
  | Some value -> return value
  | None -> fail "Llvm_backend: expected value"
;;

let lookup_func name =
  let* value_opt = find_value_opt name in
  match value_opt with
  | Some func -> return func
  | None ->
    let* state = get in
    (match lookup_function name state.current_module with
     | Some func -> return func
     | None -> fail ("Couldn't find value for key: " ^ name))
;;

let lookup_type name =
  let* type_opt = find_type_opt name in
  match type_opt with
  | Some ty -> return ty
  | None -> fail ("Couldn't find type for key: " ^ name)
;;

let lookup_func_type name =
  let* func_value = lookup_func name in
  let* func_type = lookup_type name in
  return (func_value, func_type)
;;

let gen_simple_type name args =
  let* func_value, func_type = lookup_func_type name in
  let* res =
    with_optional_value (call builder func_type func_value args ("boxed_" ^ name))
  in
  return res
;;

let imm_unit =
  let* v =
    with_optional_value (inttoptr builder (const_int int_t (tag_int 0)) ptr_t "unit")
  in
  return v
;;

let imm_tagged_int i =
  let* v =
    with_optional_value
      (inttoptr builder (const_int int_t (tag_int i)) ptr_t "tagged_int")
  in
  return v
;;

let imm_tagged_bool b =
  let* v =
    with_optional_value
      (inttoptr builder (const_int int_t (tag_bool b)) ptr_t "tagged_bool")
  in
  return v
;;

let imm_tagged_char c =
  let* v =
    with_optional_value
      (inttoptr builder (const_int int_t (tag_char c)) ptr_t "tagged_char")
  in
  return v
;;

let untag_int_val tagged_val =
  let* raw = with_optional_value (ptrtoint builder tagged_val int_t "raw_int") in
  let* minus1 = with_optional_value (sub builder raw (const_int int_t 1) "minus1") in
  let* result =
    with_optional_value (sdiv builder minus1 (const_int int_t 2) "untagged")
  in
  return result
;;

let tag_int_result i =
  let* twice = with_optional_value (mul builder i (const_int int_t 2) "twice") in
  let* tagged = with_optional_value (add builder twice (const_int int_t 1) "tagged") in
  let* v = with_optional_value (inttoptr builder tagged ptr_t "result_int") in
  return v
;;

let untag_bool_val tagged_val =
  let* raw = with_optional_value (ptrtoint builder tagged_val int_t "raw_bool") in
  let* result =
    with_optional_value (icmp builder Icmp.Eq raw (const_int int_t 4) "is_true")
  in
  return result
;;

let tag_bool_result cond_value =
  let tagged_i64 =
    build_select cond_value (const_int int_t 4) (const_int int_t 2) "tagged_bool" builder
  in
  let* v = with_optional_value (inttoptr builder tagged_i64 ptr_t "result_bool") in
  return v
;;

let rec gen_imm = function
  | ImmediateConst (ConstInt i) -> imm_tagged_int i
  | ImmediateConst (ConstBool b) -> imm_tagged_bool b
  | ImmediateConst (ConstChar c) -> imm_tagged_char c
  | ImmediateConst (ConstString _s) -> imm_unit
  | ImmediateVar id ->
    let* value =
      let* gc_allocas = get_gc_allocas in
      match gc_allocas with
      | Some allocas ->
        (match Generator_state.map_find_opt allocas id with
         | Some alloca ->
           let* v = with_optional_value (load builder ptr_t alloca id) in
           return v
         | None ->
           let* value_opt = find_value_opt id in
           (match value_opt with
            | Some v -> return v
            | None ->
              let* resolved_value = resolved_find_value_opt id in
              (match resolved_value with
               | None -> fail ("Unbound variable: " ^ id)
               | Some v -> return v)))
      | None ->
        let* value_opt = find_value_opt id in
        (match value_opt with
         | Some v -> return v
         | None ->
           let* resolved_value = resolved_find_value_opt id in
           (match resolved_value with
            | None -> fail ("Unbound variable: " ^ id)
            | Some v -> return v))
    in
    (match classify_value value with
     | ValueKind.Function when Array.length (params value) = 0 ->
       let* type_opt = resolved_find_type_opt id in
       let* func_type =
         match type_opt with
         | Some ty -> return ty
         | None -> fail ("Missing type for: " ^ id)
       in
       let* v = with_optional_value (call builder func_type value [||] "call_0") in
       return v
     | _ ->
       let* arity_opt = get_resolved_arity id in
       maybe_closure value arity_opt)

and get_resolved_arity name =
  let* state = get in
  return
    (match state.resolve with
     | Some resolver ->
       (match resolver state.current_func_index name with
        | Some (_, arity) -> Some arity
        | None -> None)
     | None -> None)

and maybe_closure value arity_opt =
  match classify_value value with
  | ValueKind.Function ->
    let arity = Option.value arity_opt ~default:(Array.length (params value)) in
    let* function_ptr =
      with_optional_value (bitcast builder value ptr_t "func_ptr_cast")
    in
    gen_simple_type "alloc_closure" [| function_ptr; const_int int_t arity |]
  | _ -> return value
;;

let gen_binop_native op left_v right_v =
  match op with
  | Plus ->
    let* l = untag_int_val left_v in
    let* r = untag_int_val right_v in
    let* v = with_optional_value (add builder l r "add") in
    tag_int_result v
  | Minus ->
    let* l = untag_int_val left_v in
    let* r = untag_int_val right_v in
    let* v = with_optional_value (sub builder l r "sub") in
    tag_int_result v
  | Multiply ->
    let* l = untag_int_val left_v in
    let* r = untag_int_val right_v in
    let* v = with_optional_value (mul builder l r "mul") in
    tag_int_result v
  | Division ->
    let* l = untag_int_val left_v in
    let* r = untag_int_val right_v in
    let* v = with_optional_value (sdiv builder l r "sdiv") in
    tag_int_result v
  | GreatestEqual ->
    let* l = untag_int_val left_v in
    let* r = untag_int_val right_v in
    let* v = with_optional_value (icmp builder Icmp.Sge l r "icmp_sge") in
    tag_bool_result v
  | LowestEqual ->
    let* l = untag_int_val left_v in
    let* r = untag_int_val right_v in
    let* v = with_optional_value (icmp builder Icmp.Sle l r "icmp_sle") in
    tag_bool_result v
  | GreaterThan ->
    let* l = untag_int_val left_v in
    let* r = untag_int_val right_v in
    let* v = with_optional_value (icmp builder Icmp.Sgt l r "icmp_sgt") in
    tag_bool_result v
  | LowerThan ->
    let* l = untag_int_val left_v in
    let* r = untag_int_val right_v in
    let* v = with_optional_value (icmp builder Icmp.Slt l r "icmp_slt") in
    tag_bool_result v
  | Equal ->
    let* l = untag_int_val left_v in
    let* r = untag_int_val right_v in
    let* v = with_optional_value (icmp builder Icmp.Eq l r "icmp_eq") in
    tag_bool_result v
  | NotEqual ->
    let* l = untag_int_val left_v in
    let* r = untag_int_val right_v in
    let* v = with_optional_value (icmp builder Icmp.Ne l r "icmp_ne") in
    tag_bool_result v
  | And ->
    let* l = untag_bool_val left_v in
    let* r = untag_bool_val right_v in
    let* v = with_optional_value (and_ builder l r "and") in
    tag_bool_result v
  | Or ->
    let* l = untag_bool_val left_v in
    let* r = untag_bool_val right_v in
    let* v = with_optional_value (or_ builder l r "or") in
    tag_bool_result v
  | Custom _ -> fail "Custom operator must be compiled to application"
;;

let gen_unop_native op tagged_val =
  match op with
  | Negative ->
    let* int_val = untag_int_val tagged_val in
    let* result = with_optional_value (neg builder int_val "neg") in
    tag_int_result result
  | Not ->
    let* bool_val = untag_bool_val tagged_val in
    let* result = with_optional_value (not builder bool_val "not") in
    tag_bool_result result
;;

let rec gen_cexpr = function
  | ComplexImmediate imm -> gen_imm imm
  | ComplexUnit -> imm_unit
  | ComplexBinOper (op, left, right) ->
    let* left_v = gen_imm left in
    let* right_v = gen_imm right in
    gen_binop_native op left_v right_v
  | ComplexUnarOper (op, imm) ->
    let* v = gen_imm imm in
    gen_unop_native op v
  | ComplexTuple (e1, e2, rest) ->
    let* args =
      List.fold_left
        (fun acc imm ->
           let* vs = acc in
           let* v = gen_imm imm in
           return (vs @ [ v ]))
        (return [])
        (e1 :: e2 :: rest)
    in
    let len = List.length args in
    let arr_len = if len = 0 then 1 else len in
    let arr_ty = Llvm.array_type ptr_t arr_len in
    let* alloca_arr =
      with_optional_value (Some (Llvm.build_alloca arr_ty "tuple_args" builder))
    in
    let* () =
      Base.List.foldi args ~init:(return ()) ~f:(fun i acc v ->
        let* () = acc in
        let* elem_ptr =
          with_optional_value
            (Some
               (Llvm.build_gep
                  arr_ty
                  alloca_arr
                  [| Llvm.const_int i32_t 0; Llvm.const_int i32_t i |]
                  "tuple_elem"
                  builder))
        in
        let () = Llvm_backend.store builder v elem_ptr in
        return ())
    in
    let* args_ptr =
      with_optional_value
        (Some
           (Llvm.build_gep
              arr_ty
              alloca_arr
              [| Llvm.const_int i32_t 0; Llvm.const_int i32_t 0 |]
              "tuple_args_ptr"
              builder))
    in
    let* create_tuple_func, create_tuple_type = lookup_func_type "create_tuple" in
    with_optional_value
      (call
         builder
         create_tuple_type
         create_tuple_func
         [| const_int int_t len; args_ptr |]
         "boxed_create_tuple")
  | ComplexField (tuple_imm, idx) ->
    let* tuple_val = gen_imm tuple_imm in
    let* field_func, field_type = lookup_func_type "field" in
    let* v =
      with_optional_value
        (call
           builder
           field_type
           field_func
           [| tuple_val; const_int int_t (tag_int idx) |]
           "field")
    in
    return v
  | ComplexApp (ImmediateVar fname, first, rest) ->
    let args_list = first :: rest in
    if fname = "print_int" && List.length args_list = 1
    then
      let* arg = gen_imm first in
      let* tagged_i64 =
        with_optional_value (ptrtoint builder arg int_t "print_int_arg")
      in
      let* print_int_func, print_int_type = lookup_func_type "print_int" in
      let* () =
        emit_void_st builder (Call (print_int_type, print_int_func, [| tagged_i64 |], ""))
      in
      imm_unit
    else
      let* callee_value, callee_from_alloca =
        let resolve_callee () =
          let* resolved_value = resolved_find_value_opt fname in
          match resolved_value with
          | Some v -> return (v, false)
          | None ->
            let* value_opt = find_value_opt fname in
            (match value_opt with
             | Some v -> return (v, false)
             | None -> fail ("Unbound function: " ^ fname))
        in
        let* gc_allocas = get_gc_allocas in
        match gc_allocas with
        | Some allocas ->
          (match Generator_state.map_find_opt allocas fname with
           | Some alloca ->
             let* v = with_optional_value (load builder ptr_t alloca fname) in
             return (v, true)
           | None -> resolve_callee ())
        | None -> resolve_callee ()
      in
      let* args =
        List.fold_left
          (fun acc imm ->
             let* vs = acc in
             let* v = gen_imm imm in
             return (vs @ [ v ]))
          (return [])
          args_list
      in
      let args_values = Array.of_list args in
      let num_args = Array.length args_values in
      let is_direct_func =
        match classify_value callee_value with
        | ValueKind.Function -> Array.length (params callee_value) = num_args
        | _ -> false
      in
      let is_zero_arg_with_unit =
        match classify_value callee_value with
        | ValueKind.Function -> Array.length (params callee_value) = 0 && num_args = 1
        | _ -> false
      in
      let use_direct =
        callee_from_alloca = false
        && (is_direct_func || is_zero_arg_with_unit)
        && num_args <= 6
      in
      if use_direct
      then
        let* type_opt = resolved_find_type_opt fname in
        let* func_type =
          match type_opt with
          | Some ty -> return ty
          | None -> fail ("Missing type for: " ^ fname)
        in
        let args_for_call = if is_zero_arg_with_unit then [||] else args_values in
        with_optional_value
          (call builder func_type callee_value args_for_call ("direct_" ^ fname))
      else
        let* arity_opt = get_resolved_arity fname in
        let* closure_value = maybe_closure callee_value arity_opt in
        let* eml_applyN_func, eml_applyN_type = lookup_func_type "eml_applyN" in
        if num_args = 0
        then (
          let arr_ty = Llvm.array_type ptr_t 1 in
          let* alloca_arr =
            with_optional_value (Some (Llvm.build_alloca arr_ty "apply_args" builder))
          in
          let* args_ptr =
            with_optional_value
              (Some
                 (Llvm.build_gep
                    arr_ty
                    alloca_arr
                    [| Llvm.const_int i32_t 0; Llvm.const_int i32_t 0 |]
                    "apply_args_ptr"
                    builder))
          in
          with_optional_value
            (call
               builder
               eml_applyN_type
               eml_applyN_func
               [| closure_value; const_int int_t 0; args_ptr |]
               "boxed_eml_applyN"))
        else
          let* _then_name, _else_name, merge_name = fresh_blocks in
          let current_func = block_parent (insertion_block builder) in
          let merge_block = append_block context merge_name current_func in
          let blocks =
            Array.init num_args (fun i ->
              append_block context ("apply_step_" ^ Int.to_string i) current_func)
          in
          let apply_one_step closure arg name =
            let one_ty = Llvm.array_type ptr_t 1 in
            let a = Llvm.build_alloca one_ty "apply_one" builder in
            let p =
              Llvm.build_gep
                one_ty
                a
                [| Llvm.const_int i32_t 0; Llvm.const_int i32_t 0 |]
                "one_elem"
                builder
            in
            Llvm_backend.store builder arg p;
            Llvm_backend.call
              builder
              eml_applyN_type
              eml_applyN_func
              [| closure; const_int int_t 1; p |]
              name
          in
          let result_vals = Array.make num_args (Llvm.const_null ptr_t) in
          let* () = emit_void_st builder (Br blocks.(0)) in
          let rec loop step_index =
            if step_index >= num_args
            then return ()
            else (
              let () = position_at_end blocks.(step_index) builder in
              let* cur =
                if step_index = 0
                then return closure_value
                else
                  with_optional_value
                    (Llvm_backend.phi
                       builder
                       [ result_vals.(step_index - 1), blocks.(step_index - 1) ]
                       ("cur_" ^ Int.to_string step_index))
              in
              let* step_result =
                with_optional_value
                  (apply_one_step
                     cur
                     args_values.(step_index)
                     ("apply_step_" ^ Int.to_string step_index))
              in
              let () = result_vals.(step_index) <- step_result in
              let* () =
                emit_void_st
                  builder
                  (if step_index < num_args - 1
                   then Br blocks.(step_index + 1)
                   else Br merge_block)
              in
              loop (step_index + 1))
          in
          let* () = loop 0 in
          let () = position_at_end merge_block builder in
          let* final_val =
            with_optional_value
              (Llvm_backend.phi
                 builder
                 [ result_vals.(num_args - 1), blocks.(num_args - 1) ]
                 "apply_result")
          in
          return final_val
  | ComplexApp (_, _, _) ->
    fail "LLVM codegen: ComplexApp with non-variable function not supported"
  | ComplexBranch (cond_imm, then_e, else_e) ->
    let* cond_val = gen_imm cond_imm in
    let* bool_val = untag_bool_val cond_val in
    let current_func = block_parent (insertion_block builder) in
    let* then_name, else_name, merge_name = fresh_blocks in
    let then_block = append_block context then_name current_func in
    let else_block = append_block context else_name current_func in
    let merge_block = append_block context merge_name current_func in
    let* () = emit_void_st builder (CondBr (bool_val, then_block, else_block)) in
    position_at_end then_block builder;
    let* then_val = gen_anf then_e in
    let* () = emit_void_st builder (Br merge_block) in
    let then_bb = insertion_block builder in
    position_at_end else_block builder;
    let* else_val = gen_anf else_e in
    let* () = emit_void_st builder (Br merge_block) in
    let else_bb = insertion_block builder in
    position_at_end merge_block builder;
    let* v =
      with_optional_value
        (phi builder [ then_val, then_bb; else_val, else_bb ] "ite_result")
    in
    return v
  | ComplexList _ | ComplexOption _ | ComplexLambda _ ->
    fail "LLVM codegen: List/Option/Lambda not yet implemented"

and gen_anf = function
  | AnfExpr cexp -> gen_cexpr cexp
  | AnfLet (_, name, cexp, cont) ->
    let* value = gen_cexpr cexp in
    let* () =
      let* gc_allocas = get_gc_allocas in
      match gc_allocas with
      | Some allocas ->
        let* alloca =
          match Generator_state.map_find_opt allocas name with
          | Some a -> return a
          | None ->
            let* entry_opt = get_gc_entry_block in
            let* entry_block =
              match entry_opt with
              | Some blk -> return blk
              | None -> fail "gen_anf: gc_entry_block not set"
            in
            let current_block = insertion_block builder in
            let* alloca_ptr =
              if current_block = entry_block
              then with_optional_value (Llvm_backend.alloca builder ptr_t name)
              else (
                let () =
                  match Llvm.instr_begin entry_block with
                  | Llvm.Before first -> position_before first builder
                  | Llvm.At_end _ -> position_at_end entry_block builder
                in
                let* alloca_in_entry =
                  with_optional_value (Llvm_backend.alloca builder ptr_t name)
                in
                let () = position_at_end current_block builder in
                return alloca_in_entry)
            in
            let* () =
              set_gc_allocas (Some (Base.Map.set allocas ~key:name ~data:alloca_ptr))
            in
            return alloca_ptr
        in
        let* () = set_gc_allocas (Some (Base.Map.set allocas ~key:name ~data:alloca)) in
        emit_void_st builder (Store (value, alloca))
      | None -> set_value name value
    in
    gen_anf cont
;;

let declare_function (func_layout : function_layout) state =
  let arg_types = Array.make (List.length func_layout.params) ptr_t in
  let func_type = function_type ptr_t arg_types in
  let func = declare_function func_layout.asm_name func_type state.current_module in
  { state with
    value_env = Base.Map.set state.value_env ~key:func_layout.asm_name ~data:func
  ; type_env = Base.Map.set state.type_env ~key:func_layout.asm_name ~data:func_type
  }
;;

let emit_gc_prologue =
  let* init_gc_func, init_gc_type = lookup_func_type "init_gc" in
  let* set_ptr_func, set_ptr_type = lookup_func_type "set_ptr_stack" in
  let* frameaddr_func, frameaddr_type = lookup_func_type "llvm.frameaddress.p0" in
  let* () = emit_void_st builder (Call (init_gc_type, init_gc_func, [||], "")) in
  let* frame_ptr =
    with_optional_value
      (call builder frameaddr_type frameaddr_func [| const_int i32_t 0 |] "frame")
  in
  emit_void_st builder (Call (set_ptr_type, set_ptr_func, [| frame_ptr |], ""))
;;

let gen_function
      (func_layout : function_layout)
      ~enable_gc
      ~is_entry
      ~func_index
      initial_state
  =
  let comp =
    let* state = get in
    let* () = put { state with current_func_index = func_index } in
    let* func, _ = lookup_func_type func_layout.asm_name in
    let entry_block = append_block context "entry" func in
    position_at_end entry_block builder;
    let* () = if enable_gc && is_entry then emit_gc_prologue else return () in
    let* () =
      if enable_gc
      then set_gc_allocas (Some (Base.Map.empty (module Base.String)))
      else return ()
    in
    let* () = if enable_gc then set_gc_entry_block (Some entry_block) else return () in
    let* () = set_value func_layout.asm_name func in
    let* state_before_params = get in
    let func_params = params func in
    let* () =
      Base.List.foldi func_layout.params ~init:(return ()) ~f:(fun param_index acc arg ->
        let* () = acc in
        match arg with
        | ImmediateVar name ->
          let* param_value =
            if param_index >= 0 && param_index < Array.length func_params
            then return (Array.get func_params param_index)
            else fail "gen_function: param index out of bounds"
          in
          set_value_name name param_value;
          if enable_gc
          then (
            let* gc_allocas = get_gc_allocas in
            let* allocas_map =
              match gc_allocas with
              | Some map -> return map
              | None -> fail "gen_function: enable_gc but gc_allocas not set"
            in
            let* alloca_ptr = with_optional_value (alloca builder ptr_t name) in
            store builder param_value alloca_ptr;
            set_gc_allocas (Some (Base.Map.set allocas_map ~key:name ~data:alloca_ptr)))
          else set_value name param_value
        | ImmediateConst _ -> return ())
    in
    let* body_value = gen_anf func_layout.body in
    let* () = emit_void_st builder (Ret (Some body_value)) in
    let* () = if enable_gc then set_gc_allocas None else return () in
    let* () = if enable_gc then set_gc_entry_block None else return () in
    let* state = get in
    let value_env =
      let without_params =
        List.fold_left
          (fun env -> function
             | ImmediateVar name -> Base.Map.remove env name
             | _ -> env)
          state.value_env
          func_layout.params
      in
      List.fold_left
        (fun env -> function
           | ImmediateVar name ->
             (match Generator_state.map_find_opt state_before_params.value_env name with
              | Some v -> Base.Map.set env ~key:name ~data:v
              | None -> env)
           | _ -> env)
        without_params
        func_layout.params
    in
    put
      { state with
        value_env = Base.Map.set value_env ~key:func_layout.asm_name ~data:func
      }
  in
  run comp initial_state
;;

let gen_program ~output_file ~enable_gc (program : anf_program) =
  let llvm_module = create_module context "EML" in
  let value_env, type_env = predefined_init llvm_module in
  let { functions; resolve; _ } = analyze program in
  let initial_state : Generator_state.state =
    { value_env
    ; type_env
    ; current_module = llvm_module
    ; gc_allocas = None
    ; gc_entry_block = None
    ; naming_state = Default_naming.init
    ; resolve = Some resolve
    ; current_func_index = 0
    }
  in
  (* [functions] is never empty: synthetic main is added when missing. *)
  let entry_name =
    match List.find_opt (fun func -> func.func_name = "main") functions with
    | Some _ -> "main"
    | None -> (List.rev functions |> List.hd).func_name
  in
  let state_after_declares =
    List.fold_left (fun state func -> declare_function func state) initial_state functions
  in
  match
    Base.List.foldi
      functions
      ~init:(Ok state_after_declares)
      ~f:(fun idx acc func_layout ->
        match acc with
        | Error _ -> acc
        | Ok state ->
          let is_entry = func_layout.func_name = entry_name in
          (match gen_function func_layout ~enable_gc ~is_entry ~func_index:idx state with
           | Ok ((), state') -> Ok state'
           | Error err -> Error err))
  with
  | Error err -> Error err
  | Ok _ ->
    print_module output_file llvm_module;
    Ok ()
;;
