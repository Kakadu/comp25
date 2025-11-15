(** Copyright 2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Sofya Kozyreva <k81sofia@gmail.com>, Vyacheslav Kochergin <vyacheslav.kochergin1@gmail.com> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Anf
open TreeAnalysis
open CodegenTypes
module InfoMap = Map.Make (String)
(* module RegStackMap = Map.Make (Int) *)

type state =
  { label_factory : int (* for creating unique ite and function labels *)
  ; is_start_label_put : bool
    (* for now, this is the only way to write _start label at suitable place and do it exactly once *)
  ; a_regs : reg list
  ; free_regs : reg list
  ; stack : int
  ; frame : int
  ; info : meta_info InfoMap.t
  ; compiled : instr list
  }

module type StateErrorMonadType = sig
  type ('s, 'a) t

  val return : 'a -> ('s, 'a) t
  val ( >>= ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
  val fail : string -> ('s, 'a) t
  val read : ('s, 's) t
  val write : 's -> ('s, unit) t
  val run : ('s, 'a) t -> 's -> ('s * 'a, string) result

  module Syntax : sig
    val ( let* ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
  end
end

module StateErrorMonad : StateErrorMonadType = struct
  type ('s, 'a) t = 's -> ('s * 'a, string) result

  let return x s = Ok (s, x)

  let ( >>= ) m f s =
    match m s with
    | Ok (s', x) -> f x s'
    | Error e -> Error e
  ;;

  let fail e _ = Error e
  let read s = Ok (s, s)
  let write s _ = Ok (s, ())
  let run m = m

  module Syntax = struct
    let ( let* ) = ( >>= )
  end
end

open StateErrorMonad
open StateErrorMonad.Syntax

let clear_a_regs = List.init 8 (fun i -> Arg i)

let init_state =
  let label_factory = 0 in
  let is_start_label_put = false in
  let a_regs = clear_a_regs in
  let t_regs = List.init 7 (fun i -> Temp i) in
  (* s0-s10 registers are available, but not s11 - it is saved for function return value *)
  let s_regs = List.init 11 (fun i -> Saved i) in
  let free_regs = t_regs @ s_regs in
  (* a_regs are used as a "bridge" to new values, so it is unstable to use them for storing *)
  let stack = 0 in
  let frame = 0 in
  let info = InfoMap.empty in
  let info = InfoMap.add "print_int" (Func ("print_int", 1)) info in
  let info = InfoMap.add "get_heap_start" (Func ("get_heap_start", 0)) info in
  let info = InfoMap.add "get_heap_fin" (Func ("get_heap_fin", 0)) info in
  let info = InfoMap.add "get_heap_free_size" (Func ("get_heap_free_size", 0)) info in
  let info = InfoMap.add "collect" (Func ("collect", 0)) info in
  let info = InfoMap.add "print_gc_status" (Func ("print_gc_status", 0)) info in
  let compiled = [] in
  { label_factory; is_start_label_put; a_regs; free_regs; stack; frame; info; compiled }
;;

let start_label = "_start"

let update_info new_info =
  let* state = read in
  let new_state = { state with info = new_info } in
  write new_state
;;

let update_a_regs new_a_regs =
  let* state = read in
  let new_state = { state with a_regs = new_a_regs } in
  write new_state
;;

let update_free_regs new_free_regs =
  let* state = read in
  let new_state = { state with free_regs = new_free_regs } in
  write new_state
;;

let update_stack new_stack =
  let* state = read in
  let new_state = { state with stack = new_stack } in
  write new_state
;;

let update_frame new_frame =
  let* state = read in
  let new_state = { state with frame = new_frame } in
  write new_state
;;

let update_is_start_label_put new_value =
  let* state = read in
  let new_state = { state with is_start_label_put = new_value } in
  write new_state
;;

(* increment stack by size being allocated *)
let extend_stack size =
  let* state = read in
  let curr_stack = state.stack in
  let* () = update_stack (curr_stack + size) in
  return curr_stack
;;

let extend_frame size =
  let* state = read in
  let curr_frame = state.frame in
  let* () = update_frame (curr_frame + size) in
  return curr_frame
;;

(* info will probably be used later, when there is logic for pushing something into stack *)
let find_free_reg =
  let* state = read in
  match state.free_regs with
  | reg :: tail ->
    let new_state = { state with free_regs = tail } in
    let* () = write new_state in
    return reg
  | [] -> fail "Empty reg list!"
;;

let make_label name =
  let* state = read in
  let label = Printf.sprintf ".%s_%d" name state.label_factory in
  let new_label_factory = state.label_factory + 1 in
  let new_state = { state with label_factory = new_label_factory } in
  let* () = write new_state in
  return label
;;

let add_instr instr =
  let* state = read in
  let new_state = { state with compiled = instr :: state.compiled } in
  write new_state
;;

let codegen_immexpr immexpr =
  let* state = read in
  let a_regs_hd = List.hd state.a_regs in
  match immexpr with
  | ImmNum n ->
    (* turn int into tagged int *)
    let* () = add_instr (Pseudo (LI (a_regs_hd, Num n))) in
    let* () = add_instr (True (IType (SLLI, a_regs_hd, a_regs_hd, Num 1))) in
    add_instr (True (IType (ADDI, a_regs_hd, a_regs_hd, Num 1)))
  | ImmId (Ident name) ->
    (match InfoMap.find_opt name state.info with
     | None -> fail "Panic: undefined var in codegen!"
     | Some (Var (o, var_type)) ->
       let is_arg =
         match var_type with
         | Argument -> true
         | Local -> false
       in
       let is_inside_function_body = state.frame > 0 in
       (* order of values on frame: first value -> RA -> old FP -> other values *)
       let is_not_frame_start = o <> 0 in
       let is_local_variable =
         is_inside_function_body && is_not_frame_start && not is_arg
       in
       let space_for_ra = if state.is_start_label_put then 0 else 8 in
       let space_for_old_fp = if state.is_start_label_put then 0 else 8 in
       let remaining_frame_offset = space_for_ra + space_for_old_fp + o in
       let reg = if is_inside_function_body then Fp else Sp in
       let o = if is_local_variable then -remaining_frame_offset else o in
       add_instr (True (StackType (LD, a_regs_hd, Stack (o, reg))))
       (* change back to Arg 0 here? *)
     | Some (Func (l, arity)) ->
       (* for function identifier: create a closure via runtime *)
       (* load the function label address into a0, put arity into a1 *)
       let* () = add_instr (Pseudo (MV (Saved 11, Arg 0))) in
       let* () = add_instr (Pseudo (LA (Arg 0, l))) in
       let* () = add_instr (Pseudo (LI (Arg 1, Num arity))) in
       add_instr (Pseudo (CALL "alloc_closure"))
     | Some (Value reg) -> add_instr (Pseudo (MV (a_regs_hd, reg))))
;;

(* change back to Arg 0 here? *)

let find_argument =
  let* state = read in
  match state.a_regs with
  | a_reg :: rest ->
    let new_state = { state with a_regs = rest } in
    let* () = write new_state in
    return a_reg
  | _ -> fail "argument storing on stack is not yet implemented"
;;

let codegen_binop_tagged a_regs_hd fst snd = function
  | CPlus ->
    (* fst + snd - 1 *)
    let* () = add_instr (True (RType (ADD, a_regs_hd, fst, snd))) in
    add_instr (True (IType (ADDI, a_regs_hd, a_regs_hd, Num (-1))))
  | CMinus ->
    (* fst - snd + 1 *)
    let* () = add_instr (True (RType (SUB, a_regs_hd, fst, snd))) in
    add_instr (True (IType (ADDI, a_regs_hd, a_regs_hd, Num 1)))
  | CMul ->
    (* (fst >> 1) * (snd - 1) + 1 *)
    let* () = add_instr (True (IType (SRLI, fst, fst, Num 1))) in
    let* () = add_instr (True (IType (ADDI, snd, snd, Num (-1)))) in
    let* () = add_instr (True (RType (MUL, a_regs_hd, fst, snd))) in
    add_instr (True (IType (ADDI, a_regs_hd, a_regs_hd, Num 1)))
  | CDiv ->
    (* (fst >> 1) / (snd >> 1) << 1 + 1 *)
    let* () = add_instr (True (IType (SRLI, fst, fst, Num 1))) in
    let* () = add_instr (True (IType (SRLI, snd, snd, Num 1))) in
    let* () = add_instr (True (RType (DIV, a_regs_hd, fst, snd))) in
    let* () = add_instr (True (IType (SLLI, a_regs_hd, a_regs_hd, Num 1))) in
    add_instr (True (IType (ADDI, a_regs_hd, a_regs_hd, Num 1)))
  | CEq ->
    let* () = add_instr (True (RType (XOR, a_regs_hd, fst, snd))) in
    add_instr (Pseudo (SEQZ (a_regs_hd, a_regs_hd)))
  | CNeq ->
    let* () = add_instr (True (RType (SUB, a_regs_hd, fst, snd))) in
    add_instr (Pseudo (SNEZ (a_regs_hd, a_regs_hd)))
  | CLt -> add_instr (True (RType (SLT, a_regs_hd, fst, snd)))
  | CLte ->
    let* () = add_instr (True (RType (SLT, a_regs_hd, snd, fst))) in
    add_instr (True (IType (XORI, a_regs_hd, a_regs_hd, Num 1)))
  | CGt -> add_instr (True (RType (SLT, a_regs_hd, snd, fst)))
  | CGte ->
    let* () = add_instr (True (RType (SLT, Arg 0, fst, snd))) in
    add_instr (True (IType (XORI, a_regs_hd, a_regs_hd, Num 1)))
;;

let rec codegen_cexpr cexpr =
  let* state = read in
  let a_regs_hd = List.hd state.a_regs in
  match cexpr with
  | CBinop (op, i1, i2) ->
    let reg_fst = a_regs_hd in
    let* () = codegen_immexpr i1 in
    let* reg_fst_free = find_free_reg in
    let* () = add_instr (Pseudo (MV (reg_fst_free, reg_fst))) in
    let reg_snd = a_regs_hd in
    let* () = codegen_immexpr i2 in
    let* reg_snd_free = find_free_reg in
    let* () = add_instr (Pseudo (MV (reg_snd_free, reg_snd))) in
    codegen_binop_tagged a_regs_hd reg_fst_free reg_snd_free op
  | CImmexpr i ->
    (* TODO maybe replace it into another register? *)
    codegen_immexpr i
  | CIte (cond, thn, els) ->
    let old_a_regs = state.a_regs in
    let old_free_regs = state.free_regs in
    let* () = codegen_cexpr cond in
    let* () = update_a_regs old_a_regs in
    let* () = update_free_regs old_free_regs in
    let* reg_cond = find_free_reg in
    let old_free_regs = state.free_regs in
    let* () = add_instr (Pseudo (MV (reg_cond, a_regs_hd))) in
    (match els with
     | Some els ->
       let* label_else = make_label "else" in
       let* label_join = make_label "join" in
       (* because we want to jump into else when beq Zero 0 => cond should be reversed *)
       (*let compiled = True (IType (XORI, reg_cond, reg_cond, 1)) :: compiled in*)
       let* () = add_instr (True (BType (BEQ, Zero, reg_cond, label_else))) in
       let* () = codegen_aexpr thn in
       let* () = update_a_regs old_a_regs in
       let* () = update_free_regs old_free_regs in
       let* () = add_instr (Pseudo (J label_join)) in
       let* () = add_instr (True (Label label_else)) in
       let* () = codegen_aexpr els in
       let* () = update_a_regs old_a_regs in
       let* () = update_free_regs old_free_regs in
       add_instr (True (Label label_join))
     | None ->
       let* label_join = make_label "join" in
       let* () = add_instr (True (BType (BEQ, Zero, reg_cond, label_join))) in
       let* () = codegen_aexpr thn in
       let* () = update_a_regs old_a_regs in
       let* () = update_free_regs old_free_regs in
       let* () = add_instr (Pseudo (J label_join)) in
       add_instr (True (Label label_join)))
  | CLam (Ident name, ae) ->
    let* _ = extend_stack 8 in
    let* cur_offset = extend_frame 8 in
    let new_info = InfoMap.add name (Var (cur_offset, Argument)) state.info in
    let* () = update_info new_info in
    let* state = read in
    let new_a_regs =
      match ae with
      | ACExpr (CLam (_, _)) -> state.a_regs
      | _ ->
        (* if next expr isnt lambda, then args are all set and all a* registers can be used for codegen again *)
        clear_a_regs
    in
    let* () = update_a_regs new_a_regs in
    codegen_aexpr ae
  (* TODO: technically, name can be digit. do something about it? *)
  | CApp (func, args) ->
    (* we may need to have previous function return value in a0 *)
    let* () = add_instr (Pseudo (MV (Arg 0, Saved 11))) in
    (* find all t* registers that should be stored *)
    let used_temps =
      InfoMap.bindings state.info
      |> List.filter_map (fun (name, sp) ->
        match sp with
        | Value (Temp i) -> Some (name, Temp i)
        | _ -> None)
    in
    (* store them on stack with mapping of who is who *)
    let* save_map =
      List.fold_left
        (fun acc (name, reg) ->
           let* saved = acc in
           let* cur_offset = extend_stack 8 in
           let* () = add_instr (True (StackType (SD, reg, Stack (cur_offset, Sp)))) in
           return ((name, (reg, cur_offset)) :: saved))
        (return [])
        used_temps
    in
    let nargs = List.length args in
    let* buf_offset = extend_stack (8 * nargs) in
    let old_a_regs = state.a_regs in
    let* () =
      List.fold_left
        (fun acc (i, arg) ->
           let* () = acc in
           let* () = codegen_immexpr arg in
           let* arg_reg = find_argument in
           let* () = update_a_regs old_a_regs in
           (* store args on stack so we can pass the pointer to them and apply via runtime *)
           let* () =
             add_instr (True (StackType (SD, arg_reg, Stack (buf_offset + (i * 8), Sp))))
           in
           return ())
        (return ())
        (List.mapi (fun i arg -> i, arg) args)
    in
    let* () = codegen_immexpr func in
    (* so pointer to closure in a0, arity in a1, pointer to args in a2, number of applied args in a3 *)
    let* () = add_instr (True (IType (ADDI, Arg 2, Sp, Num buf_offset))) in
    let* () = add_instr (Pseudo (LI (Arg 3, Num nargs))) in
    (* call runtime *)
    let* () = add_instr (Pseudo (CALL "apply")) in
    let* state = read in
    let new_stack = state.stack - (8 * nargs) in
    let* () = update_stack new_stack in
    (* put all values back into corresponding registers, cleaning stack back *)
    let* () =
      List.fold_left
        (fun acc (_, (reg, offset)) ->
           let* () = acc in
           let* () = add_instr (True (StackType (LD, reg, Stack (offset, Fp)))) in
           let* _ = extend_stack (-8) in
           return ())
        (return ())
        save_map
    in
    (* restore stack by freeing the space used for saved registers *)
    let total_saved_size = List.length save_map * 8 in
    let* _ =
      if total_saved_size > 0 then extend_stack (-total_saved_size) else return 0
    in
    (* after application, all a* can be used again - reset a_regs *)
    update_a_regs clear_a_regs

and codegen_aexpr = function
  | ACExpr ce -> codegen_cexpr ce
  | ALet (Ident name, cexpr, body) ->
    let* state = read in
    let old_a_regs = state.a_regs in
    let old_free_regs = state.free_regs in
    let* () = codegen_cexpr cexpr in
    let* () = update_a_regs old_a_regs in
    let* () = update_free_regs old_free_regs in
    let* _ = extend_stack 8 in
    let* cur_offset = extend_frame 8 in
    let* state = read in
    let new_info = InfoMap.add name (Var (cur_offset, Local)) state.info in
    let* () = update_info new_info in
    let is_not_frame_start = cur_offset <> 0 in
    let space_for_ra = if state.is_start_label_put then 0 else 8 in
    let space_for_old_fp = if state.is_start_label_put then 0 else 8 in
    let remaining_frame_offset = space_for_ra + space_for_old_fp + cur_offset in
    let cur_offset = if is_not_frame_start then -remaining_frame_offset else cur_offset in
    let* () =
      add_instr (True (StackType (SD, List.hd state.a_regs, Stack (cur_offset, Fp))))
    in
    let* () = codegen_aexpr body in
    let* () = update_a_regs old_a_regs in
    update_free_regs old_free_regs
;;

let codegen_astatement astmt =
  let* state = read in
  let required_stack_size = analyze_astatement 0 astmt * 8 in
  match astmt with
  | Ident name, st when is_function st ->
    let* func_label = make_label name in
    let arity, _ = lambda_arity_of_aexpr st in
    let* () = add_instr (True (Label func_label)) in
    let new_info = InfoMap.add name (Func (func_label, arity)) state.info in
    let* () = update_info new_info in
    let* () = add_instr (True (IType (ADDI, Sp, Sp, Num (-required_stack_size)))) in
    let* () =
      add_instr (True (StackType (SD, Ra, Stack (required_stack_size - 8, Sp))))
    in
    let* () =
      add_instr (True (StackType (SD, Fp, Stack (required_stack_size - 16, Sp))))
    in
    let* () = add_instr (True (IType (ADDI, Fp, Sp, Num required_stack_size))) in
    let fresh_stack = -8 in
    (* uninitialized stack *)
    let fresh_frame = 0 in
    let* () = update_stack fresh_stack in
    let* () = update_frame fresh_frame in
    let old_a_regs = state.a_regs in
    let old_free_regs = state.free_regs in
    let* () = codegen_aexpr st in
    let* () = update_a_regs old_a_regs in
    let* () = update_free_regs old_free_regs in
    let* () = update_frame fresh_frame in
    let* () = add_instr (Pseudo (MV (Saved 11, Arg 0))) in
    let* () =
      add_instr (True (StackType (LD, Ra, Stack (required_stack_size - 8, Sp))))
    in
    let* () =
      add_instr (True (StackType (LD, Fp, Stack (required_stack_size - 16, Sp))))
    in
    let* () = add_instr (True (IType (ADDI, Sp, Sp, Num required_stack_size))) in
    add_instr (Pseudo RET)
    (* if statement is not a function and label start isnt put yet, initialize global stack and put start label before it *)
  | Ident _, st ->
    let* is_global =
      if state.is_start_label_put
      then return false
      else
        let* () = update_is_start_label_put true in
        let* () = add_instr (True (Label start_label)) in
        let* () = add_instr (Pseudo (MV (Fp, Sp))) in
        let* () = add_instr (True (IType (ADDI, Sp, Sp, Num (-required_stack_size)))) in
        (* initialize s11 with 0 for further saving return value into it *)
        let* () = add_instr (Pseudo (LI (Saved 11, Num 0))) in
        let* () = add_instr (Pseudo (CALL "init_start_heap")) in
        return true
    in
    let* () = update_stack 0 in
    (* TODO: maybe put it in info here? *)
    let old_a_regs = state.a_regs in
    let old_free_regs = state.free_regs in
    let* () = codegen_aexpr st in
    let* () = update_a_regs old_a_regs in
    let* () = update_free_regs old_free_regs in
    if is_global
    then
      let* () = add_instr (True (IType (ADDI, Sp, Sp, Num required_stack_size))) in
      add_instr (Pseudo (CALL "free_heap"))
    else return ()
;;

let codegen_aconstruction aconstr =
  let* state = read in
  let required_stack_size = analyze_aconstr 0 aconstr * 8 in
  match aconstr with
  | AExpr ae ->
    let* is_global =
      if state.is_start_label_put
      then return false
      else
        let* () = update_is_start_label_put true in
        let* () = add_instr (True (Label start_label)) in
        let* () = add_instr (Pseudo (MV (Fp, Sp))) in
        let* () = add_instr (True (IType (ADDI, Sp, Sp, Num (-required_stack_size)))) in
        let* () = add_instr (Pseudo (LI (Saved 11, Num 0))) in
        let* () = add_instr (Pseudo (CALL "init_start_heap")) in
        return true
    in
    let old_a_regs = state.a_regs in
    let old_free_regs = state.free_regs in
    let* () = codegen_aexpr ae in
    let* () = update_a_regs old_a_regs in
    let* () = update_free_regs old_free_regs in
    if is_global
    then
      let* () = add_instr (True (IType (ADDI, Sp, Sp, Num required_stack_size))) in
      add_instr (Pseudo (CALL "free_heap"))
    else return ()
  | AStatement (_, st_list) ->
    List.fold_left (fun _ -> codegen_astatement) (return ()) st_list
;;

let codegen_aconstructions acs =
  let* () =
    List.fold_left
      (fun acc c ->
         let* () = acc in
         codegen_aconstruction c)
      (return ())
      acs
  in
  let* () = add_instr (Pseudo (LI (Arg 7, Num 93))) in
  let* () = add_instr (True Ecall) in
  let* state = read in
  return (List.rev state.compiled)
;;

let codegen_program acs =
  let state = init_state in
  run (codegen_aconstructions acs) state
;;
