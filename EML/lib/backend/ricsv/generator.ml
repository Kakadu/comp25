(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Frontend.Ast
open Middleend.Anf
open Architecture
open Analysis
open Riscv_backend
open Generator_state
open Auxillary

let alloc_frame_slot =
  let* () =
    modify (fun state -> { state with frame_offset = state.frame_offset + word_size })
  in
  let* state = get in
  return (fp, -state.frame_offset)
;;

let store_reg_into_frame source_register =
  let* slot = alloc_frame_slot in
  let* () = append (sd source_register slot) in
  return (Loc_mem slot)
;;

let load_into_reg destination_register source_location =
  let instructions =
    match source_location with
    | Loc_reg source_register when equal_reg source_register destination_register -> []
    | Loc_reg source_register -> mv destination_register source_register
    | Loc_mem source_offset -> ld destination_register source_offset
  in
  let* () = append instructions in
  return ()
;;

let spill_params_to_frame params_reg =
  Base.List.foldi params_reg ~init:(return ()) ~f:(fun index acc param ->
    let* () = acc in
    match param with
    | ImmediateVar name ->
      let argument_register = List.nth arg_regs index in
      let* slot = store_reg_into_frame argument_register in
      modify_env (fun env -> Base.Map.set env ~key:name ~data:slot)
    | _ -> return ())
;;

let spill_caller_saved_vars_to_frame =
  let* env = get_env in
  let caller_saved_variables = vars_in_caller_saved_regs env in
  let frame_bytes = List.length caller_saved_variables * word_size in
  let* () = if frame_bytes > 0 then append (addi sp sp (-frame_bytes)) else return () in
  let rec spill environment = function
    | [] -> return environment
    | (variable_name, register) :: remaining_variables ->
      let* new_location = store_reg_into_frame register in
      spill
        (Base.Map.set environment ~key:variable_name ~data:new_location)
        remaining_variables
  in
  let* updated_environment = spill env caller_saved_variables in
  set_env updated_environment
;;

let evacuate_reg destination_register =
  let is_register_used environment register =
    Base.Map.exists environment ~f:(function
      | Loc_reg mapped_register -> equal_reg register mapped_register
      | Loc_mem _ -> false)
  in
  let rewrite_location_in_environment environment from_register to_location =
    Base.Map.map environment ~f:(function
      | Loc_reg mapped_register when equal_reg mapped_register from_register ->
        to_location
      | location -> location)
  in
  let* env = get_env in
  if not (is_register_used env destination_register)
  then return ()
  else (
    match
      List.find_opt
        (fun candidate_register -> not (is_register_used env candidate_register))
        candidate_regs_for_spill
    with
    | Some free_register ->
      let* () = append (mv free_register destination_register) in
      let updated_environment =
        rewrite_location_in_environment env destination_register (Loc_reg free_register)
      in
      set_env updated_environment
    | None ->
      let* spilled_location = store_reg_into_frame destination_register in
      let updated_environment =
        rewrite_location_in_environment env destination_register spilled_location
      in
      set_env updated_environment)
;;

let resolve_call_symbol name =
  let* state = get in
  match state.symbol_resolve state.current_func_index name with
  | Some (asm_name, _) -> return asm_name
  | None -> return name
;;

let resolve_symbol_and_arity state name =
  match state.symbol_resolve state.current_func_index name with
  | Some (asm_name, arity_val) -> asm_name, arity_val
  | None ->
    (match Base.Map.find state.arity_map name with
     | Some arity_val -> name, arity_val
     | None -> name, -1)
;;

let gen_imm dst = function
  | ImmediateConst (ConstInt n) -> append (li dst (tag_int n))
  | ImmediateConst (ConstBool b) -> append (li dst (if b then tag_int 1 else tag_int 0))
  | ImmediateConst (ConstChar c) -> append (li dst (tag_int (Char.code c)))
  | ImmediateConst (ConstString _) -> fail "String constants not yet supported in codegen"
  | ImmediateVar name ->
    let* env = get_env in
    (match Base.Map.find env name with
     | Some loc -> load_into_reg dst loc
     | None ->
       let* state = get in
       let sym, arity = resolve_symbol_and_arity state name in
       if arity < 0
       then fail ("unbound variable: " ^ name)
       else (
         match arity with
         | 0 -> append (call sym)
         | nargs ->
           let* () = append (la result_reg sym) in
           let* () = append (li (List.nth arg_regs 1) nargs) in
           append (call "alloc_closure")))
;;

let copy_result_to dst =
  if equal_reg dst result_reg then return () else append (mv dst result_reg)
;;

let spill_dangerous_args state arguments =
  let dangerous_argument_indices = indices_of_args_to_spill state arguments in
  let spill_slots = List.length dangerous_argument_indices * word_size in
  let* () = if spill_slots > 0 then append (addi sp sp (-spill_slots)) else return () in
  Base.List.foldi
    arguments
    ~init:(return (Base.Map.empty (module Base.Int)))
    ~f:(fun argument_index acc argument ->
      let* spilled_locations_by_index = acc in
      if List.mem argument_index dangerous_argument_indices
      then
        let* () = gen_imm result_reg argument in
        let* spilled_location = store_reg_into_frame result_reg in
        return
          (Base.Map.add_exn
             spilled_locations_by_index
             ~key:argument_index
             ~data:spilled_location)
      else return spilled_locations_by_index)
;;

let load_immediates_into_registers
      spilled_locations
      argument_registers
      immediate_arguments
  =
  let immediate_count_to_load =
    min (List.length immediate_arguments) (List.length argument_registers)
  in
  Base.List.foldi
    (Base.List.take immediate_arguments immediate_count_to_load)
    ~init:(return ())
    ~f:(fun argument_index acc immediate_argument ->
      let* () = acc in
      let destination_register = List.nth argument_registers argument_index in
      match Base.Map.find spilled_locations argument_index with
      | Some spilled_location -> load_into_reg destination_register spilled_location
      | None -> gen_imm destination_register immediate_argument)
;;

let emit_arguments_to_stack spilled_arguments arguments =
  Base.List.foldi arguments ~init:(return ()) ~f:(fun argument_index acc argument ->
    let* () = acc in
    let stack_offset = argument_index * word_size in
    let* () =
      match Base.Map.find spilled_arguments argument_index with
      | Some spilled_location -> load_into_reg t0 spilled_location
      | None -> gen_imm t0 argument
    in
    append (sd t0 (sp, stack_offset)))
;;

let push_stack_args stack_args =
  let stack_argument_count = List.length stack_args in
  if stack_argument_count = 0
  then return 0
  else (
    let stack_bytes = stack_argument_count * word_size in
    let* () = append (addi sp sp (-stack_bytes)) in
    let no_spilled_arguments = Base.Map.empty (module Base.Int) in
    let* () = emit_arguments_to_stack no_spilled_arguments stack_args in
    return stack_bytes)
;;

let gen_call_with_regs
      destination_register
      argument_registers
      call_arguments
      spilled_arguments
      function_symbol
  =
  let* () =
    load_immediates_into_registers spilled_arguments argument_registers call_arguments
  in
  let stack_arguments = Base.List.drop call_arguments (List.length argument_registers) in
  let* reserved_stack_bytes = push_stack_args stack_arguments in
  let* () = append (call function_symbol) in
  let* () = copy_result_to destination_register in
  if reserved_stack_bytes > 0 then append (addi sp sp reserved_stack_bytes) else return ()
;;

(*  let foo = ... in
    foo () *)
let gen_nullary destination_register function_name =
  let* resolved_symbol = resolve_call_symbol function_name in
  let* () = append (call resolved_symbol) in
  copy_result_to destination_register
;;

let gen_direct_call destination_register function_name call_arguments spilled_arguments =
  let* resolved_symbol = resolve_call_symbol function_name in
  gen_call_with_regs
    destination_register
    arg_regs
    call_arguments
    spilled_arguments
    resolved_symbol
;;

let gen_via_apply_nargs
      destination_register
      function_name
      argument_count
      call_arguments
      spilled_arguments
  =
  let argv_bytes = argument_count * word_size in
  let* () = gen_imm a0 (ImmediateVar function_name) in
  let* () = append (li a1 argument_count) in
  let* () = append (addi sp sp (-argv_bytes)) in
  let* () = emit_arguments_to_stack spilled_arguments call_arguments in
  let* () = append (mv a2 sp) in
  let* () = append (call "eml_applyN") in
  let* () = copy_result_to destination_register in
  append (addi sp sp argv_bytes)
;;

let rec gen_invocation destination_register function_name call_arguments =
  let* () = spill_caller_saved_vars_to_frame in
  let* state = get in
  let* spilled_arguments = spill_dangerous_args state call_arguments in
  let argument_count = List.length call_arguments in
  let callee_arity_opt = Base.Map.find state.arity_map function_name in
  let style =
    classify_call
      ~argument_count
      ~callee_arity_opt
      ~function_name
      ~arguments:call_arguments
  in
  match style with
  | Nullary resolved_function_name ->
    gen_nullary destination_register resolved_function_name
  | CurryChain { function_name; arity; initial_arguments; remaining_arguments } ->
    gen_curried_call
      destination_register
      function_name
      arity
      initial_arguments
      remaining_arguments
  | Direct { function_name; arguments } ->
    gen_direct_call destination_register function_name arguments spilled_arguments
  | ViaApplyNargs { function_name; argument_count; arguments } ->
    gen_via_apply_nargs
      destination_register
      function_name
      argument_count
      arguments
      spilled_arguments

and gen_curried_call
      destination_register
      function_name
      _arity
      initial_arguments
      remaining_arguments
  =
  let* part_name = fresh_partial in
  let* () =
    gen_cexpr
      destination_register
      (ComplexApp
         (ImmediateVar function_name, List.hd initial_arguments, List.tl initial_arguments))
  in
  let* partial_function_location = store_reg_into_frame destination_register in
  let* () =
    modify_env (fun environment ->
      Base.Map.set environment ~key:part_name ~data:partial_function_location)
  in
  let rec apply_remaining_arguments = function
    | [] -> return ()
    | [ argument ] ->
      gen_cexpr destination_register (ComplexApp (ImmediateVar part_name, argument, []))
    | argument :: remaining_arguments_tail ->
      let* () =
        gen_cexpr destination_register (ComplexApp (ImmediateVar part_name, argument, []))
      in
      let* updated_partial_location = store_reg_into_frame destination_register in
      let* () =
        modify_env (fun environment ->
          Base.Map.set environment ~key:part_name ~data:updated_partial_location)
      in
      apply_remaining_arguments remaining_arguments_tail
  in
  apply_remaining_arguments remaining_arguments

and gen_unit dst = append (li dst (tag_int 0))

and gen_neg dst op =
  let* () = gen_imm t0 op in
  let* () = append (li dst (tag_int 0)) in
  append (sub dst dst t0)

and gen_not dst op =
  let* () = gen_imm t0 op in
  append (xori dst t0 (tag_int 1))

and gen_binop dst binary_operator left_operand right_operand =
  let* () = gen_imm t0 left_operand in
  let* () = gen_imm t1 right_operand in
  let* () = evacuate_reg dst in
  match bin_op dst (bin_oper_to_string binary_operator) t0 t1 with
  | Ok instrs -> append instrs
  | Error msg -> fail msg

and gen_branch dst cond then_e else_e =
  let* () = gen_imm t0 cond in
  let* else_lbl, end_lbl = fresh_branch in
  let* () = append (li t1 (tag_int 0)) in
  let* () = append (beq t0 t1 else_lbl) in
  let* state_before_then = get in
  let frame_offset_before_then = state_before_then.frame_offset in
  let* () = gen_anf dst then_e in
  let* () = append (j end_lbl) in
  let* state_after_then = get in
  let* () =
    put
      { state_before_then with
        frame_offset = frame_offset_before_then
      ; instr_buffer = state_after_then.instr_buffer
      }
  in
  let* () = append (label else_lbl) in
  let* () = gen_anf dst else_e in
  append (label end_lbl)

and spill_tuple_var_if_in_reg = function
  | ImmediateVar name ->
    let* env = get_env in
    (match Base.Map.find env name with
     | Some (Loc_reg _) ->
       let* loc = store_reg_into_frame result_reg in
       modify_env (fun env -> Base.Map.set env ~key:name ~data:loc)
     | _ -> return ())
  | _ -> return ()

and gen_field dst tuple_imm idx =
  let* () = gen_imm result_reg tuple_imm in
  let* () = spill_tuple_var_if_in_reg tuple_imm in
  let* () = append (li (List.nth arg_regs 1) (tag_int idx)) in
  let* () = append (call "field") in
  copy_result_to dst

and gen_list dst = function
  | [] -> append (li dst (tag_int 0))
  | hd :: tl ->
    let* () = gen_list dst tl in
    let* tail_loc = store_reg_into_frame dst in
    let* () = gen_imm t0 hd in
    let* () = spill_caller_saved_vars_to_frame in
    let* () = append (li result_reg 2) in
    let* () = load_into_reg (List.nth arg_regs 1) (Loc_reg t0) in
    let* () = load_into_reg (List.nth arg_regs 2) tail_loc in
    let* () = append (call "create_tuple") in
    copy_result_to dst

and gen_tuple dst e1 e2 rest =
  let elts = e1 :: e2 :: rest in
  let argc = List.length elts in
  let* () = spill_caller_saved_vars_to_frame in
  let* state = get in
  let* spilled = spill_dangerous_args state elts in
  let array_bytes = argc * word_size in
  let* () = append (addi sp sp (-array_bytes)) in
  let* () = emit_arguments_to_stack spilled elts in
  let* () = append (li result_reg argc) in
  let* () = append (addi (List.nth arg_regs 1) sp 0) in
  let* () = append (call "create_tuple") in
  let* () = append (addi sp sp array_bytes) in
  copy_result_to dst

and gen_app dst fname first rest = gen_invocation dst fname (first :: rest)

and gen_cexpr dst = function
  | ComplexUnit -> gen_unit dst
  | ComplexImmediate imm -> gen_imm dst imm
  | ComplexUnarOper (Negative, op) -> gen_neg dst op
  | ComplexUnarOper (Not, op) -> gen_not dst op
  | ComplexBinOper (Custom _, _, _) ->
    fail "Custom operator must be compiled to application"
  | ComplexBinOper (op, left, right) -> gen_binop dst op left right
  | ComplexBranch (cond, then_e, else_e) -> gen_branch dst cond then_e else_e
  | ComplexField (tuple_imm, idx) -> gen_field dst tuple_imm idx
  | ComplexTuple (e1, e2, rest) -> gen_tuple dst e1 e2 rest
  | ComplexApp (ImmediateVar name, first, rest) -> gen_app dst name first rest
  | ComplexApp (_, _, _) -> fail "ComplexApp: function must be a variable"
  | ComplexLambda _ | ComplexOption _ -> fail "gen_cexpr: Lambda/Option not implemented"
  | ComplexList imm_list -> gen_list dst imm_list

and gen_anf dst = function
  | AnfExpr cexp -> gen_cexpr dst cexp
  | AnfLet (_, name, rhs, cont) ->
    let* () = evacuate_reg result_reg in
    let* () = gen_cexpr result_reg rhs in
    let* loc = store_reg_into_frame result_reg in
    let* () = modify_env (fun env -> Base.Map.set env ~key:name ~data:loc) in
    gen_anf dst cont
;;

let bind_param_to_reg env i = function
  | ImmediateVar name ->
    let register = List.nth arg_regs i in
    return (Base.Map.set env ~key:name ~data:(Loc_reg register))
  | _ -> fail "unsupported pattern"
;;

let bind_param_to_stack env i = function
  | ImmediateVar name ->
    let stack_offset = (i + 2) * word_size in
    return (Base.Map.set env ~key:name ~data:(Loc_mem (fp, stack_offset)))
  | _ -> fail "unsupported pattern"
;;

let flush_instr_buffer ~enable_peephole ppf =
  let* state = get in
  let instruction_buffer = state.instr_buffer in
  let* () = put { state with instr_buffer = [] } in
  let instructions = List.rev instruction_buffer in
  let instructions =
    if enable_peephole then Peephole.optimize instructions else instructions
  in
  let () = List.iter (fun item -> format_item ppf item) instructions in
  return ()
;;

let gen_func ~enable_gc ~enable_peephole asm_name params body frame_sz ppf =
  fprintf ppf "\n  .globl %s\n  .type %s, @function\n" asm_name asm_name;
  let args = List.length params in
  let params_reg, params_stack =
    ( Base.List.take params (min args arg_regs_count)
    , Base.List.drop params (min args arg_regs_count) )
  in
  let env0 = Base.Map.empty (module Base.String) in
  let* env =
    Base.List.foldi params_reg ~init:(return env0) ~f:(fun i acc p ->
      let* e = acc in
      bind_param_to_reg e i p)
  in
  let* env =
    Base.List.foldi params_stack ~init:(return env) ~f:(fun i acc p ->
      let* e = acc in
      bind_param_to_stack e i p)
  in
  let* () = set_env env in
  let* () = append (prologue ~enable_gc ~name:asm_name ~stack_size:frame_sz) in
  let* state = get in
  let* () = put { state with frame_offset = 0 } in
  let* () = spill_params_to_frame params_reg in
  let* () = gen_anf result_reg body in
  let* () = append (epilogue ~enable_gc ~is_main:(String.equal asm_name "main")) in
  let* () = flush_instr_buffer ~enable_peephole ppf in
  return ()
;;

let gen_program ~enable_gc ~enable_peephole ppf (analysis : analysis_result) =
  fprintf ppf ".section .text";
  let base = Runtime.Primitives.runtime_primitive_arities in
  let arity_map =
    List.fold_left
      (fun map (name, arity) -> Base.Map.set map ~key:name ~data:arity)
      analysis.arity_map
      base
  in
  let init =
    { frame_offset = 0
    ; naming_state = Default_naming.init
    ; arity_map
    ; env = Base.Map.empty (module Base.String)
    ; instr_buffer = []
    ; current_func_index = 0
    ; symbol_resolve = analysis.resolve
    }
  in
  let comp =
    Base.List.foldi analysis.functions ~init:(return ()) ~f:(fun function_index acc fn ->
      let frame_sz = (2 + fn.slots_count) * word_size in
      let* () = acc in
      let* () =
        modify (fun state -> { state with current_func_index = function_index })
      in
      gen_func ~enable_gc ~enable_peephole fn.asm_name fn.params fn.body frame_sz ppf)
  in
  match run comp init with
  | Ok ((), _) ->
    pp_print_flush ppf ();
    Ok ()
  | Error msg -> Error msg
;;
