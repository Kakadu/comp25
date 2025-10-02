(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Ast.Expression
open Machine
open Base
open Stdlib.Format

module Platform = struct
  let arg_regs_count = 8
  let word_size = 8
end

(* Stores the current offset from FP for local variables *)
let frame_offset = ref 0
let state = ref 0

(* fresh id generator *)
let fresh () =
  Int.incr state;
  !state
;;

(** Environment context: maps variables to registers or stack offsets *)
type location =
  | Loc_reg of reg
  | Loc_mem of offset

type env = (ident, location, String.comparator_witness) Map.t

module Emission = struct
  (* maybe in sep file? *)
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
    | "==" ->
      emit xor dst r1 r2;
      emit seqz dst dst
    | "<>" ->
      emit xor dst r1 r2;
      emit snez dst dst
    | _ -> failwith ("unsupported binary operator: " ^ op)
  ;;

  let emit_store ?(comm = "") reg =
    frame_offset := !frame_offset + Platform.word_size;
    let ofs = - !frame_offset in
    emit sd reg (S 0, ofs) ~comm;
    Loc_mem (S 0, ofs)
  ;;

  (* save 'live' registers from env to stack *)
  (* Currently, T n registers are restored after the called function returns,
  but A n registers are not restored, so their location in env changes. *)
  let emit_save_caller_regs (env : env) : env =
    let compare_reg_for_save r1 r2 =
      match r1, r2 with
      | A i, A j -> Int.compare i j
      | T i, T j -> Int.compare i j
      | T _, _ -> -1
      | _, T _ -> 1
      | _, _ -> 0
    in
    let regs =
      Map.to_alist env
      |> List.filter_map ~f:(fun (name, loc) ->
        match loc with
        | Loc_reg r ->
          (match r with
           | A _ | T _ -> Some (name, r)
           | _ -> None)
        | _ -> None)
      |> List.sort ~compare:(fun (_, r1) (_, r2) -> compare_reg_for_save r1 r2)
    in
    let spill_count = List.length regs in
    let frame_size = spill_count * Platform.word_size in
    if frame_size > 0 then emit addi SP SP (-frame_size) ~comm:"spill live regs";
    List.foldi regs ~init:env ~f:(fun i env (name, r) ->
      let ofs = i * Platform.word_size in
      emit sd r (SP, ofs);
      match r with
      | A _ -> Map.set env ~key:name ~data:(Loc_mem (SP, ofs))
      | _ -> env)
  ;;

  (* Currently restores only T n registers. *)
  let emit_restore_caller_regs (env : env) =
    let t_regs =
      Map.to_alist env
      |> List.filter_map ~f:(fun (_, loc) ->
        match loc with
        | Loc_reg (T rnum) -> Some rnum
        | _ -> None)
      |> List.sort ~compare:Int.compare
    in
    let frame_size = List.length t_regs * Platform.word_size in
    if frame_size > 0 then emit addi SP SP frame_size ~comm:"unspill live regs";
    List.iteri t_regs ~f:(fun i rnum ->
      let ofs = i * Platform.word_size in
      emit ld (T rnum) (SP, ofs))
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

let rec collect_apps acc = function
  | Exp_apply (f, arg) -> collect_apps (arg :: acc) f
  | fn -> fn, acc
;;

let reg_is_used (env : env) (r : reg) : bool =
  Map.exists env ~f:(fun loc ->
    match loc with
    | Loc_reg r' -> equal_reg r r'
    | Loc_mem _ -> false)
;;

let find_free_reg (env : env) (regs : reg list) : reg option =
  List.find_map regs ~f:(fun r -> if not (reg_is_used env r) then Some r else None)
;;

(* Ensures that dst is usable. If dst contains a live variable, 
  it moves it to another location. Returns the updated environment. *)
let ensure_reg_free (env : env) (dst : reg) : env =
  let relocate (env : env) ~(from : reg) ~(to_ : location) : env =
    Map.map env ~f:(function
      | Loc_reg r when equal_reg r from -> to_
      | loc -> loc)
  in
  if not (reg_is_used env dst)
  then env
  else (
    let candidate_regs = List.init 8 ~f:(fun i -> A i) @ List.init 7 ~f:(fun i -> T i) in
    match find_free_reg env candidate_regs with
    | Some new_reg ->
      emit mv new_reg dst;
      relocate env ~from:dst ~to_:(Loc_reg new_reg)
    | None ->
      let new_loc = emit_store dst ~comm:"spill" in
      relocate env ~from:dst ~to_:new_loc)
;;

let rec gen_exp (env : env) (dst : reg) = function
  | Exp_constant (Const_integer n) ->
    emit li dst n;
    env
  | Exp_ident x ->
    (match Map.find env x with
     | Some (Loc_reg r) ->
       if equal_reg r dst
       then env
       else (
         emit mv dst r;
         env)
     | Some (Loc_mem ofs) ->
       emit ld dst ofs;
       env
     | None -> failwith ("unbound variable: " ^ x))
  | Exp_ifthenelse (cond, then_e, Some else_e) ->
    let env = gen_exp env (T 0) cond in
    let else_lbl = Printf.sprintf "else_%d" (fresh ()) in
    let end_lbl = Printf.sprintf "end_%d" (fresh ()) in
    emit beq (T 0) Zero else_lbl;
    (* then case *)
    let env = gen_exp env dst then_e in
    emit j end_lbl;
    (* else case *)
    emit label else_lbl;
    let env = gen_exp env dst else_e in
    emit label end_lbl;
    env
  | Exp_apply _ as e ->
    let fn, args = collect_apps [] e in
    (match fn, args with
     | Exp_ident op, [ a1; a2 ] when Parser.is_operator op ->
       let env = gen_exp env (T 0) a1 in
       let env = gen_exp env (T 1) a2 in
       let env = ensure_reg_free env dst in
       emit_bin_op dst op (T 0) (T 1);
       env
     | Exp_ident fname, args ->
       let env =
         List.foldi args ~init:env ~f:(fun i env arg ->
           if i < Platform.arg_regs_count
           then (
             let env = gen_exp env (A i) arg in
             env)
           else failwith "too many args")
       in
       let env = emit_save_caller_regs env in
       emit call fname;
       emit_restore_caller_regs env;
       if not (equal_reg dst (A 0)) then emit mv dst (A 0);
       env
     | _ -> failwith "unsupported function application")
  | Exp_let (_, { pat = Pat_var id; exp }, [], exp_in) ->
    let env = gen_exp env (A 0) exp in
    let loc = emit_store (A 0) ~comm:id in
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

let gen_func (f : ident) (args : pattern list) (body : Expression.t) ppf : unit =
  frame_offset := 0;
  let f = if String.equal f "main" then "_start" else f in
  let () = fprintf ppf "\n  .globl %s\n  .type %s, @function\n" f f in
  let arity = List.length args in
  let reg_params, stack_params = List.split_n args (min arity Platform.arg_regs_count) in
  let stack_size = (2 + count_local_vars body) * Platform.word_size in
  let env = Map.empty (module String) in
  let env =
    List.foldi reg_params ~init:env ~f:(fun i env -> function
      | Pat_var name -> Map.set env ~key:name ~data:(Loc_reg (A i))
      | _ -> failwith "unsupported pattern")
  in
  let env =
    List.foldi stack_params ~init:env ~f:(fun i env -> function
      | Pat_var name ->
        let offset = (i + 1) * Platform.word_size in
        Map.set env ~key:name ~data:(Loc_mem (SP, offset))
      | _ -> failwith "unsupported pattern")
  in
  emit_fn_prologue f stack_size;
  let _ = gen_exp env (A 0) body in
  emit_fn_epilogue (String.equal f "_start");
  flush_queue ppf
;;

let gen_structure ppf (s : structure) =
  let () = fprintf ppf ".section .text" in
  List.iter
    ~f:(function
      | Struct_value (Recursive, { pat = Pat_var f; exp = Exp_fun (p, ps, body) }, _) ->
        gen_func f (p :: ps) body ppf
      | Struct_value (Nonrecursive, { pat = Pat_var f; exp = body }, _) ->
        gen_func f [] body ppf
      | _ -> failwith "unsupported structure item")
    s;
  pp_print_flush ppf ()
;;
