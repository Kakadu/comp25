(** Copyright 2024, Mikhail Gavrilenko, Danila Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Middleend.Anf
open Format
open Target
open Machine
open Emission.Emission

let label_counter = ref 0

(* hold the deferred functions *)
let deferred_functions : (string * ident list * anf_expr) list ref = ref []

let fresh_label prefix =
  let n = !label_counter in
  incr label_counter;
  prefix ^ "_" ^ string_of_int n
;;

type loc =
  | Reg of reg
  | Stack_offset of int

module Env = struct
  type t = (string, loc) Hashtbl.t

  let empty () = Hashtbl.create 16
  let bind t x loc = Hashtbl.replace t x loc
  let find t x = Hashtbl.find_opt t x
  let copy = Hashtbl.copy
end

module ArityMap = struct
  type t = (ident, int) Hashtbl.t

  let empty () = Hashtbl.create 16
  let bind (t : t) (x : ident) (arity : int) = Hashtbl.replace t x arity
  let find (t : t) (x : ident) = Hashtbl.find_opt t x
  let copy = Hashtbl.copy
end

type cg_state =
  { env : Env.t
  ; stack_offset : int
  ; arity : ArityMap.t
  }

let gen_im_expr (state : cg_state) (dst : reg) (imm : im_expr) : unit =
  match imm with
  | Imm_num n -> emit li dst n
  | Imm_ident x ->
    (match Env.find state.env x with
     | Some (Reg r) -> if not (equal_reg r dst) then emit mv dst r
     | Some (Stack_offset offset) -> emit ld dst (S 0, offset)
     | None ->
       (match ArityMap.find state.arity x with
        | Some 0 ->
          emit call x;
          if not (equal_reg (A 0) dst) then emit mv dst (A 0)
        | Some arity ->
          emit la (A 0) x;
          emit li (A 1) arity;
          emit call "alloc_closure";
          if not (equal_reg (A 0) dst) then emit mv dst (A 0)
        | None -> failwith ("Unbound identifier during codegen: " ^ x)))
;;

let rec gen_anf_expr state dst (aexpr : anf_expr) =
  match aexpr with
  | Anf_let (_rec_flag, name, comp_expr, body) ->
    let state_after_cexpr = gen_comp_expr state (T 0) comp_expr in
    let new_offset = state_after_cexpr.stack_offset - Target.word_size in
    emit sd (T 0) (S 0, new_offset);
    Env.bind state_after_cexpr.env name (Stack_offset new_offset);
    (* generate for let body*)
    let new_state = { state_after_cexpr with stack_offset = new_offset } in
    gen_anf_expr new_state dst body
  | Anf_comp_expr comp_expr ->
    (*end of let's*)
    gen_comp_expr state dst comp_expr

and gen_comp_expr state dst (cexpr : comp_expr) =
  match cexpr with
  | Comp_imm imm ->
    gen_im_expr state dst imm;
    state
  | Comp_binop (op, v1, v2) ->
    gen_im_expr state (T 0) v1;
    gen_im_expr state (T 1) v2;
    emit_bin_op op dst (T 0) (T 1);
    state
| Comp_app (func_imm, args_imms) ->
    let live_regs_to_save =
      Hashtbl.fold
        (fun _ loc acc ->
           match loc with
           | Reg r when not (equal_reg r (S 0)) -> r :: acc
           | _ -> acc)
        state.env
        []
    in
    List.iter
      (fun reg ->
         emit addi SP SP (-Target.word_size);
         emit sd reg (SP, 0))
      live_regs_to_save;
    let m = List.length args_imms in
    let fname_opt, n_opt =
      match func_imm with
      | Imm_ident name -> (Some name, ArityMap.find state.arity name)
      | _ -> (None, None)
    in
    begin match fname_opt, n_opt with
    | Some fname, Some n ->
      (*сразу вызываем чтобы не плодить closure*)
        if m = n then (
          List.iteri
            (fun i arg_imm ->
               if i < Array.length Target.arg_regs
               then gen_im_expr state (A i) arg_imm
               else failwith "Stack arguments for direct call not implemented")
            args_imms;
          emit call fname;
          emit mv (T 0) (A 0)
        ) else if m < n then (
          (*создаем замыкание если передано меньше аргументов*)
          emit la (A 0) fname;
          emit li (A 1) n;
          emit call "alloc_closure";
          emit mv (T 0) (A 0);
          List.iter
            (fun arg_imm ->
               if not (equal_reg (A 0) (T 0)) then emit mv (A 0) (T 0);
               gen_im_expr state (A 1) arg_imm;
               emit call "apply1";
               emit mv (T 0) (A 0))
            args_imms
        ) else (
          (*если больше аргументов - я считаю ошибка но стоит подумать могут ли быть другие случаи*)
          failwith
            (Printf.sprintf
               "Too many arguments for function %s: expected %d, got %d"
               fname n m)
        )

    | Some fname, None ->
        List.iteri
          (fun i arg_imm ->
             if i < Array.length Target.arg_regs
             then gen_im_expr state (A i) arg_imm
             else failwith "Stack arguments for external calls not implemented")
          args_imms;
        emit call fname;
        emit mv (T 0) (A 0)

    | _ ->
        gen_im_expr state (T 0) func_imm;
        List.iter
          (fun arg_imm ->
             if not (equal_reg (A 0) (T 0)) then emit mv (A 0) (T 0);
             gen_im_expr state (A 1) arg_imm;
             emit call "apply1";
             emit mv (T 0) (A 0))
          args_imms
    end;
    List.iter
      (fun reg ->
         emit ld reg (SP, 0);
         emit addi SP SP Target.word_size)
      (List.rev live_regs_to_save);

    if not (equal_reg dst (T 0)) then emit mv dst (T 0);
    state
  | Comp_branch (cond_imm, then_anf, else_anf) ->
    let lbl_else = fresh_label "else" in
    let lbl_end = fresh_label "endif" in
    gen_im_expr state (T 0) cond_imm;
    emit beq (T 0) Zero lbl_else;
    let then_state = { state with env = Env.copy state.env } in
    let _ = gen_anf_expr then_state dst then_anf in
    emit j lbl_end;
    emit label lbl_else;
    let else_state = { state with env = Env.copy state.env } in
    let _ = gen_anf_expr else_state dst else_anf in
    emit label lbl_end;
    state
  | Comp_func (params, body) ->
    let func_label = fresh_label "lambda" in
    ArityMap.bind state.arity func_label (List.length params);
    deferred_functions := (func_label, params, body) :: !deferred_functions;
    emit la (A 0) func_label;
    emit li (A 1) (List.length params);
    emit call "alloc_closure";
    if not (equal_reg dst (A 0)) then emit mv dst (A 0);
    state
  | Comp_tuple _ ->
    (* Tuples would require heap allocation.
         The test cases do not involve tuples. *)
    failwith "Tuple values are not yet implemented"
;;

(* counts the number of let bindings to allocate space on stack *)
let rec count_locals_in_anf (aexpr : anf_expr) : int =
  match aexpr with
  | Anf_let (_, _, comp_expr, body) ->
    let locals_in_comp = count_locals_in_comp comp_expr in
    let locals_in_body = count_locals_in_anf body in
    max locals_in_comp (1 + locals_in_body)
  | Anf_comp_expr comp_expr -> count_locals_in_comp comp_expr

and count_locals_in_comp (cexpr : comp_expr) : int =
  match cexpr with
  | Comp_imm _ | Comp_binop _ | Comp_app _ | Comp_func _ | Comp_tuple _ -> 0
  | Comp_branch (_, then_anf, else_anf) ->
    let locals_in_then = count_locals_in_anf then_anf in
    let locals_in_else = count_locals_in_anf else_anf in
    max locals_in_then locals_in_else
;;

let gen_func ~arity_map func_name params body_anf ppf =
  let env = Env.empty () in
  List.iteri
    (fun i param_name ->
       if i < Array.length Target.arg_regs
       then Env.bind env param_name (Reg (A i))
       else failwith "Too many arguments for register passing")
    params;
  let local_count = count_locals_in_anf body_anf in
  let stack_size = (2 + local_count) * Target.word_size in
  emit_prologue func_name stack_size;
  let initial_state = { env; stack_offset = 0; arity = arity_map } in
  let _ = gen_anf_expr initial_state (A 0) body_anf in
  flush_queue ppf;
  emit_epilogue stack_size
;;

let gen_start ppf =
  fprintf ppf ".section .text\n";
  fprintf ppf ".global main\n";
  fprintf ppf ".type main, @function\n"
;;

let gen_program ppf (program : aprogram) =
  label_counter := 0;
  let has_main =
    List.exists
      (function
        | Anf_str_value (_, "main", _) | Anf_str_eval _ -> true
        | _ -> false)
      program
  in
  if has_main then gen_start ppf;
  let arity = ArityMap.empty () in
  List.iter
    (function
      | Anf_str_value (_rf, name, Anf_let (_, _, Comp_func (ps, _), _)) ->
        ArityMap.bind arity name (List.length ps)
      | Anf_str_value (_rf, name, Anf_comp_expr (Comp_func (ps, _))) ->
        ArityMap.bind arity name (List.length ps)
      | _ -> ())
    program;
  List.iter
    (function
      | Anf_str_eval anf_expr -> gen_func ~arity_map:arity "main" [] anf_expr ppf
      | Anf_str_value (_rec_flag, name, anf_expr) ->
        let params, body =
          match anf_expr with
          | Anf_let (_, _, Comp_func (ps, b), _) -> ps, b
          | Anf_comp_expr (Comp_func (ps, b)) -> ps, b
          | _ -> [], anf_expr
        in
        gen_func ~arity_map:arity name params body ppf)
    program;
  List.iter
    (fun (name, params, body) -> gen_func ~arity_map:arity name params body ppf)
    (List.rev !deferred_functions);
  flush_queue ppf
;;
