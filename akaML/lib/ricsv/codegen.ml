[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Ast.Expression
open Machine
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
    | _ -> failwith ("unsupported binary operator: " ^ op)
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
      emit li (A 7) 93;
      emit ecall)
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

let rec gen_exp env dst = function
  | Exp_constant (Const_integer n) ->
    emit li dst n;
    return env
  | Exp_ident x ->
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
     | None -> failwith ("unbound variable: " ^ x))
  | Exp_ifthenelse (cond, then_e, Some else_e) ->
    let* env = gen_exp env (T 0) cond in
    let* id = fresh in
    let else_lbl = Printf.sprintf "else_%d" id
    and end_lbl = Printf.sprintf "end_%d" id in
    emit beq (T 0) Zero else_lbl;
    (* then case *)
    let* env = gen_exp env dst then_e in
    emit j end_lbl;
    (* else case *)
    emit label else_lbl;
    let* env = gen_exp env dst else_e in
    emit label end_lbl;
    return env
  | Exp_apply _ as e ->
    let rec collect_apps acc = function
      | Exp_apply (f, arg) -> collect_apps (arg :: acc) f
      | fn -> fn, acc
    in
    let fn, args = collect_apps [] e in
    (match fn, args with
     | Exp_ident op, [ a1; a2 ] when Parser.is_operator op ->
       let* env = gen_exp env (T 0) a1 in
       let* env = gen_exp env (T 1) a2 in
       let* env = ensure_reg_free env dst in
       emit_bin_op dst op (T 0) (T 1);
       return env
     | Exp_ident fname, args ->
       let* env =
         List.foldi args ~init:(return env) ~f:(fun i acc arg ->
           let* env = acc in
           if i < Platform.arg_regs_count
           then gen_exp env (A i) arg
           else failwith "too many args")
       in
       let* env = emit_save_caller_regs env in
       emit call fname;
       if not (equal_reg dst (A 0)) then emit mv dst (A 0);
       return env
     | _ -> failwith "unsupported function application")
  | Exp_let (_, { pat = Pat_var id; exp }, [], exp_in) ->
    let* env = gen_exp env (A 0) exp in
    let* loc = emit_store (A 0) ~comm:id in
    let env = Map.set env ~key:id ~data:loc in
    gen_exp env dst exp_in
  | _ -> failwith "expression not supported yet"
;;

let rec count_local_vars = function
  | Exp_ident _ | Exp_constant _ | Exp_construct (_, None) -> 0
  | Exp_let (_, vb, vb_list, body) ->
    let count_one_vb { pat; exp } =
      let count_vars_in_pat =
        match pat with
        | Pat_var _ -> 1
        | _ -> 0
      in
      count_vars_in_pat + count_local_vars exp
    in
    List.fold_left (vb :: vb_list) ~init:0 ~f:(fun acc vb -> acc + count_one_vb vb)
    + count_local_vars body
  | Exp_fun (_, _, exp) | Exp_construct (_, Some exp) | Exp_constraint (exp, _) ->
    count_local_vars exp
  | Exp_apply (exp1, exp2) -> count_local_vars exp1 + count_local_vars exp2
  | Exp_ifthenelse (cond, then_exp, Some else_exp) ->
    count_local_vars cond + count_local_vars then_exp + count_local_vars else_exp
  | Exp_ifthenelse (cond, then_exp, None) ->
    count_local_vars cond + count_local_vars then_exp
  | Exp_function (case, case_list) ->
    let count_case { left = _; right } = count_local_vars right in
    List.fold_left (case :: case_list) ~init:0 ~f:(fun acc c -> acc + count_case c)
  | Exp_match (scrut, case, case_list) ->
    let count_case { left = _; right } = count_local_vars right in
    count_local_vars scrut
    + List.fold_left (case :: case_list) ~init:0 ~f:(fun acc c -> acc + count_case c)
  | Exp_tuple (exp1, exp2, exp_list) ->
    List.fold_left (exp1 :: exp2 :: exp_list) ~init:0 ~f:(fun acc e ->
      acc + count_local_vars e)
  | Exp_sequence (exp1, exp2) -> count_local_vars exp1 + count_local_vars exp2
;;

let gen_func f_id arg_list body_exp ppf state =
  let f_id = if String.equal f_id "main" then "_start" else f_id in
  fprintf ppf "\n  .globl %s\n  .type %s, @function\n" f_id f_id;
  let arity = List.length arg_list in
  let reg_params, stack_params =
    List.split_n arg_list (min arity Platform.arg_regs_count)
  in
  let stack_size = (2 + count_local_vars body_exp) * Platform.word_size in
  let env = Map.empty (module String) in
  let env =
    List.foldi reg_params ~init:env ~f:(fun i env -> function
      | Pat_var name -> Map.set env ~key:name ~data:(Loc_reg (A i))
      | _ -> failwith "unsupported pattern")
  in
  let env =
    List.foldi stack_params ~init:env ~f:(fun i env -> function
      | Pat_var name ->
        let offset = (i + 2) * Platform.word_size in
        Map.set env ~key:name ~data:(Loc_mem (S 0, offset))
      | _ -> failwith "unsupported pattern")
  in
  emit_fn_prologue f_id stack_size;
  let init_state = { state with frame_offset = 0 } in
  let _, state = gen_exp env (A 0) body_exp init_state in
  emit_fn_epilogue (String.equal f_id "_start");
  flush_queue ppf;
  state
;;

let gen_structure ppf ast =
  fprintf ppf ".section .text";
  let init_state = { frame_offset = 0; fresh_id = 0 } in
  let _ =
    List.fold ast ~init:init_state ~f:(fun state -> function
      | Struct_value
          (Recursive, { pat = Pat_var f_id; exp = Exp_fun (p, p_list, body_exp) }, _) ->
        gen_func f_id (p :: p_list) body_exp ppf state
      | Struct_value (Nonrecursive, { pat = Pat_var f_id; exp = body_exp }, _) ->
        gen_func f_id [] body_exp ppf state
      | _ -> failwith "unsupported structure item")
  in
  pp_print_flush ppf ()
;;
