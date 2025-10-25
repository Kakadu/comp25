[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Machine
open Anf.Anf_core
open Base
open Stdlib.Format

module Platform = struct
  let arg_regs_count = 8
  let word_size = 8
end

(** Environment context: maps variables to registers or stack offsets *)
type location =
  | Loc_reg of reg
  | Loc_mem of offset

let pp_location ppf = function
  | Loc_reg r -> fprintf ppf "Reg(%a)" pp_reg r
  | Loc_mem ofs -> fprintf ppf "Mem(%a)" pp_offset ofs
;;

type env = (ident, location, String.comparator_witness) Map.t

let pp_env ppf env =
  let bindings = Map.to_alist env in
  fprintf ppf "{";
  List.iteri bindings ~f:(fun i (name, loc) ->
    if i > 0 then fprintf ppf "; ";
    fprintf ppf "%s -> %a" name pp_location loc);
  fprintf ppf "}"
;;

type codegen_state =
  { frame_offset : int
    (* Stores the current offset from FP for local variables and some caller-regs *)
  ; fresh_id : int
  ; arity_map : (ident, int, String.comparator_witness) Map.t
  }

module State = struct
  type 'a t = codegen_state -> 'a * codegen_state

  let return x st = x, st

  let bind m f =
    fun state ->
    let x, st = m state in
    f x st
  ;;

  let ( let* ) = bind
  let get st = st, st
  let put st = fun _ -> (), st

  let modify_frame_offset f =
    let* st = get in
    put { st with frame_offset = f st.frame_offset }
  ;;

  let modify_fresh_id f =
    let* st = get in
    put { st with fresh_id = f st.fresh_id }
  ;;

  let fresh =
    let* st = get in
    let* () = modify_fresh_id Int.succ in
    return st.fresh_id
  ;;
end

open State

module Emission = struct
  let code : (instr * string) Queue.t = Queue.create ()
  let emit ?(comm = "") instr = instr (fun i -> Queue.enqueue code (i, comm))

  let flush_queue ppf =
    while not (Queue.is_empty code) do
      let i, comm = Queue.dequeue_exn code in
      (match i with
       | Label _ -> fprintf ppf "%a" pp_instr i
       | _ -> fprintf ppf "  %a" pp_instr i);
      if String.(comm <> "") then fprintf ppf " # %s" comm;
      fprintf ppf "\n"
    done
  ;;

  let emit_bin_op dst op r1 r2 =
    match op with
    | "+" -> emit add dst r1 r2
    | "-" -> emit sub dst r1 r2
    | "*" -> emit mul dst r1 r2
    | "<=" ->
      emit slt dst r2 r1;
      emit xori dst dst 1
    | ">=" ->
      emit slt dst r1 r2;
      emit xori dst dst 1
    | "=" ->
      emit xor dst r1 r2;
      emit seqz dst dst
    | "<>" ->
      emit xor dst r1 r2;
      emit snez dst dst
    | "<" -> emit slt dst r1 r2
    | ">" -> emit slt dst r2 r1
    | _ -> failwith ("unsupported binary operator: " ^ op)
  ;;

  let emit_load_reg (dst_reg : reg) = function
    | Loc_reg src_reg when equal_reg src_reg dst_reg -> emit mv dst_reg src_reg
    | Loc_mem ofs -> emit ld dst_reg ofs
    | _ -> ()
  ;;

  let emit_store ?(comm = "") reg =
    let* () = modify_frame_offset (fun fr_ofs -> fr_ofs + Platform.word_size) in
    let* state = get in
    let ofs = -state.frame_offset in
    emit sd reg (S 0, ofs) ~comm;
    return (Loc_mem (S 0, ofs))
  ;;

  (* save 'live' registers from env to stack *)
  let emit_save_caller_regs env =
    let regs =
      Map.to_alist env
      |> List.filter_map ~f:(fun (name, loc) ->
        match loc with
        | Loc_reg r ->
          (match r with
           | A _ | T _ -> Some (name, r)
           | _ -> None)
        | _ -> None)
    in
    let spill_count = List.length regs in
    let frame_size = spill_count * Platform.word_size in
    if frame_size > 0 then emit addi SP SP (-frame_size) ~comm:"Saving 'live' regs";
    List.fold regs ~init:(return env) ~f:(fun acc (name, r) ->
      let* env = acc in
      let* new_loc = emit_store r in
      return (Map.set env ~key:name ~data:new_loc))
  ;;

  let emit_fn_prologue name stack_size =
    (* allocate space on stack, store RA, old FP (S0) and make a new FP *)
    emit label name;
    emit addi SP SP (-stack_size);
    emit sd RA (SP, stack_size - Platform.word_size);
    emit sd (S 0) (SP, stack_size - (2 * Platform.word_size));
    emit addi (S 0) SP (stack_size - (2 * Platform.word_size)) ~comm:"Prologue ends"
  ;;

  let emit_fn_epilogue is_main =
    (* restore SP, S0 and RA using FP (S0) as reference *)
    emit addi SP (S 0) (2 * Platform.word_size) ~comm:"Epilogue starts";
    emit ld RA (S 0, Platform.word_size);
    emit ld (S 0) (S 0, 0);
    if is_main
    then (
      emit li (A 0) 0;
      emit ret)
    else emit ret
  ;;
end

open Emission

let reg_is_used env r =
  Map.exists env ~f:(function
    | Loc_reg r' -> equal_reg r r'
    | Loc_mem _ -> false)
;;

(* If dst contains a live variable, it moves it to another location. *)
let ensure_reg_free env dst =
  let relocate env ~(from : reg) ~(to_ : location) =
    Map.map env ~f:(function
      | Loc_reg r when equal_reg r from -> to_
      | loc -> loc)
  in
  if not (reg_is_used env dst)
  then return env
  else (
    let candidate_regs = List.init 8 ~f:(fun i -> A i) in
    match List.find candidate_regs ~f:(fun r -> not (reg_is_used env r)) with
    | Some new_reg ->
      emit mv new_reg dst;
      return (relocate env ~from:dst ~to_:(Loc_reg new_reg))
    | None ->
      let* new_loc = emit_store dst in
      return (relocate env ~from:dst ~to_:new_loc))
;;

let rec gen_i_exp env dst = function
  | IExp_constant (Const_integer n) ->
    emit li dst n;
    return env
  | IExp_ident x ->
    (match Map.find env x with
     | Some (Loc_reg r) ->
       if equal_reg r dst
       then return env
       else (
         emit mv dst r;
         return env)
     | Some (Loc_mem ofs) ->
       emit ld dst ofs;
       return env
     | None ->
       let* state = get in
       (match Map.find state.arity_map x with
        | Some 0 ->
          emit call x;
          return env
        | Some arity ->
          emit la (A 0) x;
          emit li (A 1) arity;
          emit call "alloc_closure";
          return env
        | _ -> failwith ("unbound variable: " ^ x)))
  | _ -> failwith "GenIExp: Not implemented"

and gen_c_exp env dst = function
  | CIExp i_exp -> gen_i_exp env dst i_exp
  | CExp_apply (IExp_ident op, i_exp1, [ i_exp2 ]) when Ast.is_bin_op op ->
    let* env = gen_i_exp env (T 0) i_exp1 in
    let* env = gen_i_exp env (T 1) i_exp2 in
    let* env = ensure_reg_free env dst in
    emit_bin_op dst op (T 0) (T 1);
    return env
  | CExp_apply (IExp_ident fname, i_exp, i_exp_list) ->
    let args = i_exp :: i_exp_list in
    let* env = emit_save_caller_regs env in
    let* state = get in
    let arity = Map.find state.arity_map fname in
    (match List.length args, arity with
     | args_received, Some args_count when args_received = args_count ->
       let* env =
         List.foldi args ~init:(return env) ~f:(fun i acc arg ->
           let* env = acc in
           if i < Platform.arg_regs_count
           then gen_i_exp env (A i) arg
           else failwith "too many args")
       in
       emit call fname;
       if not (equal_reg dst (A 0)) then emit mv dst (A 0);
       return env
     | args_received, _ ->
       let arg_regs = [ A 2; A 3; A 4; A 5; A 6; A 7 ] in
       (* determine which args can overwrite regs *)
       let is_rewrites_regs = function
         | IExp_constant _ -> false
         | IExp_ident id ->
           (match Map.find state.arity_map id with
            | Some _ -> true
            | None -> false)
         | _ -> false
       in
       (* save all “dangerous” args on the stack and remember where *)
       let rw_arg = List.filter args ~f:is_rewrites_regs in
       let rw_arg_size = List.length rw_arg in
       if rw_arg_size > 0
       then emit addi SP SP (-rw_arg_size * 8) ~comm:"Saving 'dangerous' args";
       let* rw_arg_locs =
         List.filter args ~f:is_rewrites_regs
         |> List.fold ~init:(return Map.Poly.empty) ~f:(fun acc arg ->
           let* acc = acc in
           let* _ = gen_i_exp env (A 0) arg in
           let* loc = emit_store (A 0) in
           return (Map.set acc ~key:arg ~data:loc))
       in
       let* env = gen_i_exp env (A 0) (IExp_ident fname) in
       emit li (A 1) args_received;
       (* load args into regs *)
       let num_reg_args = min args_received (List.length arg_regs) in
       let* env =
         List.foldi (List.take args num_reg_args) ~init:(return env) ~f:(fun i acc arg ->
           let* env = acc in
           if is_rewrites_regs arg
           then (
             emit_load_reg (List.nth_exn arg_regs i) (Map.find_exn rw_arg_locs arg);
             return env)
           else
             let* env = gen_i_exp env (List.nth_exn arg_regs i) arg in
             return env)
       in
       (* if there are stack args, prepare space for them *)
       let stack_args = List.drop args num_reg_args in
       let stack_size = List.length stack_args in
       if stack_size > 0
       then emit addi SP SP (-stack_size * 8) ~comm:"Stack space for variadic args";
       let* env =
         List.foldi stack_args ~init:(return env) ~f:(fun i acc arg ->
           let* env = acc in
           let offset = i * 8 in
           let* env = gen_i_exp env (T 0) arg in
           emit sd (T 0) (SP, offset);
           return env)
       in
       emit call "applyN";
       if stack_size > 0
       then emit addi SP SP (stack_size * 8) ~comm:"Restore stack after applyN";
       if rw_arg_size > 0
       then emit addi SP SP (rw_arg_size * 8) ~comm:"Restore stack after 'dangerous' args";
       if not (equal_reg dst (A 0)) then emit mv dst (A 0);
       return env)
  | CExp_ifthenelse (cond, then_e, Some else_e) ->
    let* env = gen_c_exp env (T 0) cond in
    let* id = fresh in
    let else_lbl = Printf.sprintf "else_%d" id
    and end_lbl = Printf.sprintf "end_%d" id in
    emit beq (T 0) Zero else_lbl;
    (* then case *)
    let* _ = gen_a_exp env dst then_e in
    emit j end_lbl;
    (* else case *)
    emit label else_lbl;
    let* _ = gen_a_exp env dst else_e in
    emit label end_lbl;
    return env
  | _ -> failwith "GenCExp: Not implemented"

and gen_a_exp env dst = function
  | ACExp c_exp -> gen_c_exp env dst c_exp
  | AExp_let (_, Pat_var id, exp, exp_in) ->
    let* env = gen_c_exp env (A 0) exp in
    let* loc = emit_store (A 0) ~comm:id in
    let env = Map.set env ~key:id ~data:loc in
    gen_a_exp env dst exp_in
  | _ -> failwith "GenAExp: Not implemented"
;;

let rec count_loc_vars_i_exp = function
  | IExp_ident _ | IExp_constant _ -> 0
  | IExp_fun (_, a_exp) -> count_loc_vars_a_exp a_exp

and count_loc_vars_c_exp = function
  | CIExp i_exp -> count_loc_vars_i_exp i_exp
  | CExp_tuple (i_exp1, i_exp2, i_exp_list) ->
    List.fold_left (i_exp1 :: i_exp2 :: i_exp_list) ~init:0 ~f:(fun acc e ->
      acc + count_loc_vars_i_exp e)
  | CExp_apply (i_exp1, i_exp2, i_exp_list) ->
    List.fold_left (i_exp1 :: i_exp2 :: i_exp_list) ~init:0 ~f:(fun acc e ->
      acc + count_loc_vars_i_exp e)
  | CExp_ifthenelse (c_exp_if, a_exp_then, None) ->
    count_loc_vars_c_exp c_exp_if + count_loc_vars_a_exp a_exp_then
  | CExp_ifthenelse (c_exp_if, a_exp_then, Some a_exp_else) ->
    count_loc_vars_c_exp c_exp_if
    + count_loc_vars_a_exp a_exp_then
    + count_loc_vars_a_exp a_exp_else

and count_loc_vars_a_exp = function
  | ACExp c_exp -> count_loc_vars_c_exp c_exp
  | AExp_let (_, pat, c_exp, a_exp) ->
    let count_vars_in_pat =
      match pat with
      | Pat_var _ -> 1
      | _ -> 0
    in
    count_vars_in_pat + count_loc_vars_c_exp c_exp + count_loc_vars_a_exp a_exp
;;

let gen_a_func f_id arg_list body_exp ppf state =
  fprintf ppf "\n  .globl %s\n  .type %s, @function\n" f_id f_id;
  let arity = List.length arg_list in
  let reg_params, stack_params =
    List.split_n arg_list (min arity Platform.arg_regs_count)
  in
  let stack_size = (2 + count_loc_vars_a_exp body_exp) * Platform.word_size in
  let env = Map.empty (module String) in
  let env =
    List.foldi reg_params ~init:env ~f:(fun i env -> function
      | APat_var name -> Map.set env ~key:name ~data:(Loc_reg (A i))
      | _ -> failwith "unsupported pattern")
  in
  let env =
    List.foldi stack_params ~init:env ~f:(fun i env -> function
      | APat_var name ->
        let offset = (i + 2) * Platform.word_size in
        Map.set env ~key:name ~data:(Loc_mem (S 0, offset))
      | _ -> failwith "unsupported pattern")
  in
  emit_fn_prologue f_id stack_size;
  let init_state = { state with frame_offset = 0 } in
  let _, state = gen_a_exp env (A 0) body_exp init_state in
  emit_fn_epilogue (String.equal f_id "main");
  flush_queue ppf;
  state
;;

let init_arity_map ast =
  let env =
    List.fold
      ast
      ~init:(Map.empty (module String))
      ~f:(fun env -> function
        | AStruct_value (_, Pat_var f_id, a_exp) ->
          let rec extract_f_params exp ~acc =
            match exp with
            | ACExp (CIExp (IExp_fun (_, a_exp))) ->
              let acc = acc + 1 in
              extract_f_params a_exp ~acc
            | _ -> acc
          in
          let params_count = extract_f_params a_exp ~acc:0 in
          Map.set env ~key:f_id ~data:params_count
        | _ -> env)
  in
  env
;;

let gen_a_structure ppf ast =
  fprintf ppf ".section .text";
  let arity_map = init_arity_map ast in
  let arity_map = Map.set arity_map ~key:"print_int" ~data:1 in
  let init_state = { frame_offset = 0; fresh_id = 0; arity_map } in
  let _ =
    List.fold ast ~init:init_state ~f:(fun state -> function
      | AStruct_value (_, Pat_var f_id, body_exp) ->
        let extract_fun_params body_exp =
          let rec helper acc exp =
            match exp with
            | ACExp (CIExp (IExp_fun (pat, body))) -> helper (pat :: acc) body
            | other -> List.rev acc, other
          in
          helper [] body_exp
        in
        let pat_list, body_exp = extract_fun_params body_exp in
        gen_a_func f_id pat_list body_exp ppf state
      | _ -> failwith "unsupported structure item")
  in
  pp_print_flush ppf ()
;;
