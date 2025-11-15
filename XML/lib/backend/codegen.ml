(** Copyright 2024,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Middleend.Anf
open Format
open Target
open Machine
open Emission.Emission

type loc =
  | Reg of reg
  | Stack_offset of int

module Env = struct
  module M = Map.Make (String)

  type t = loc M.t

  let empty () : t = M.empty
  let bind (t : t) (x : string) (loc : loc) : t = M.add x loc t
  let find (t : t) (x : string) : loc option = M.find_opt x t
  let fold (f : string -> loc -> 'a -> 'a) (t : t) (acc : 'a) : 'a = M.fold f t acc
end

module ArityMap = struct
  module K = struct
    type t = ident

    let compare = Stdlib.compare
  end

  module M = Map.Make (K)

  type t = int M.t

  let empty () : t = M.empty
  let bind (t : t) (x : ident) (arity : int) : t = M.add x arity t
  let find (t : t) (x : ident) : int option = M.find_opt x t
end

let initial_arity_map =
  let arity_map = ArityMap.empty () in
  let arity_map = ArityMap.bind arity_map "print_int" 1 in
  let arity_map = ArityMap.bind arity_map "alloc_block" 1 in
  let arity_map = ArityMap.bind arity_map "alloc_closure" 2 in
  let arity_map = ArityMap.bind arity_map "apply1" 2 in
  let arity_map = ArityMap.bind arity_map "print_gc_status" 0 in
  let arity_map = ArityMap.bind arity_map "collect" 0 in
  arity_map
;;

type cg_state =
  { env : Env.t
  ; stack_offset : int (* current offset for new local variables *)
  ; arity : ArityMap.t
  ; next_label : int
  ; deferred : (string * ident list * anf_expr) list
  ; gc_stats : bool
  }

type cg_error =
  [ `Unbound_identifier of string
  | `Stack_args_not_impl_direct
  | `Stack_args_not_impl_external
  | `Too_many_args of string * int * int
  | `Call_non_function
  | `Tuple_not_impl
  ]

type 'a r = ('a, cg_error) result

let ok x = Ok x
let err e = Error e
let ( let* ) = Result.bind
let ( let+ ) x f = Result.map f x

let fresh_label (prefix : string) (st : cg_state) : string * cg_state =
  let n = st.next_label in
  prefix ^ "_" ^ string_of_int n, { st with next_label = n + 1 }
;;

let gen_im_expr (state : cg_state) (dst : reg) (imm : im_expr) : unit r =
  match imm with
  | Imm_num n ->
    (* tagged integer: (n<<1)|1 *)
    emit li dst ((n lsl 1) lor 1);
    ok ()
  | Imm_ident x ->
    (match Env.find state.env x with
     | Some (Reg r) ->
       if not (equal_reg r dst) then emit mv dst r;
       ok ()
     | Some (Stack_offset offset) ->
       emit ld dst (S 0, offset);
       ok ()
     | None ->
       (match ArityMap.find state.arity x with
        | Some 0 ->
          emit call x;
          if not (equal_reg (A 0) dst) then emit mv dst (A 0);
          ok ()
        | Some arity ->
          emit la (A 0) x;
          emit li (A 1) arity;
          emit call "alloc_closure";
          if not (equal_reg (A 0) dst) then emit mv dst (A 0);
          ok ()
        | None -> err (`Unbound_identifier x)))
;;

let rec gen_anf_expr (state : cg_state) (dst : reg) (aexpr : anf_expr) : cg_state r =
  match aexpr with
  | Anf_let (_rec_flag, name, comp_expr, body) ->
    let* state_after_cexpr = gen_comp_expr state (T 0) comp_expr in
    let new_offset = state_after_cexpr.stack_offset - Target.word_size in
    emit sd (T 0) (S 0, new_offset);
    let env' = Env.bind state_after_cexpr.env name (Stack_offset new_offset) in
    let new_state = { state_after_cexpr with stack_offset = new_offset; env = env' } in
    gen_anf_expr new_state dst body
  | Anf_comp_expr comp_expr -> gen_comp_expr state dst comp_expr

and gen_comp_expr (state : cg_state) (dst : reg) (cexpr : comp_expr) : cg_state r =
  match cexpr with
  | Comp_imm imm ->
    let* () = gen_im_expr state dst imm in
    ok state
  | Comp_binop (op, v1, v2) ->
    let* () = gen_im_expr state (T 0) v1 in
    let* () = gen_im_expr state (T 1) v2 in
    emit_tagged_binop op dst (T 0) (T 1);
    ok state
  | Comp_app (func_imm, args_imms) ->
    let live_regs_to_save =
      Env.fold
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
    let* state_after_call =
      let apply_chain closure_reg args st =
        let rec loop current_closure_reg_inner = function
          | [] -> ok st
          | arg_imm :: tl ->
            let* () = gen_im_expr st (T 1) arg_imm in
            emit mv (A 0) current_closure_reg_inner;
            emit mv (A 1) (T 1);
            emit call "apply1";
            emit mv (T 0) (A 0);
            loop (T 0) tl
        in
        loop closure_reg args
      in
      match func_imm with
      | Imm_ident fname ->
        (match Env.find state.env fname with
         | Some _ ->
           let* () = gen_im_expr state (T 0) func_imm in
           apply_chain (T 0) args_imms state
         | None ->
           (match ArityMap.find state.arity fname with
            | Some n when n = 0 ->
              emit call fname;
              emit mv (T 0) (A 0);
              apply_chain (T 0) args_imms state
            | Some n when List.length args_imms = n ->
              let num_args = List.length args_imms in
              let num_reg_args = Array.length Target.arg_regs in
              (* process all args and save it on the stack *)
              let* () =
                List.fold_left
                  (fun acc_res arg_imm ->
                     let* () = acc_res in
                     let* () = gen_im_expr state (T 0) arg_imm in
                     emit addi SP SP (-Target.word_size);
                     emit sd (T 0) (SP, 0);
                     ok ())
                  (ok ())
                  args_imms
              in
              (* load args from stack to the a0-a7 and stack *)
              List.iteri
                (fun i _ ->
                   if i < num_reg_args
                   then (
                     let arg_reg = A i in
                     let stack_offset = (num_args - 1 - i) * Target.word_size in
                     emit ld arg_reg (SP, stack_offset)))
                args_imms;
              let num_stack_args_to_pass = max 0 (num_args - num_reg_args) in
              if num_reg_args < num_args
              then emit addi SP SP (num_reg_args * Target.word_size);
              emit call fname;
              emit mv (T 0) (A 0);
              (* clear the stack *)
              if num_stack_args_to_pass = 0
              then emit addi SP SP (num_args * Target.word_size);
              ok state
            | Some n when List.length args_imms < n ->
              let m = List.length args_imms in
              if m > 0 then emit addi SP SP (-m * Target.word_size);
              let* () =
                List.mapi
                  (fun i arg_imm ->
                     let* () = gen_im_expr state (T 1) arg_imm in
                     emit sd (T 1) (SP, i * Target.word_size);
                     ok ())
                  args_imms
                |> List.fold_left
                     (fun acc r ->
                        let* () = acc in
                        r)
                     (ok ())
              in
              emit la (A 0) fname;
              emit li (A 1) n;
              emit call "alloc_closure";
              emit mv (T 0) (A 0);
              let rec apply_saved i =
                if i >= m
                then ok ()
                else (
                  emit ld (T 1) (SP, i * Target.word_size);
                  emit mv (A 0) (T 0);
                  emit mv (A 1) (T 1);
                  emit call "apply1";
                  emit mv (T 0) (A 0);
                  apply_saved (i + 1))
              in
              let* () = apply_saved 0 in
              if m > 0 then emit addi SP SP (m * Target.word_size);
              ok state
            | Some n -> err (`Too_many_args (fname, n, List.length args_imms))
            | None -> err (`Unbound_identifier fname)))
      | Imm_num _ -> err `Call_non_function
    in
    List.iter
      (fun reg ->
         emit ld reg (SP, 0);
         emit addi SP SP Target.word_size)
      (List.rev live_regs_to_save);
    if not (equal_reg dst (T 0)) then emit mv dst (T 0);
    ok state_after_call
  | Comp_branch (cond_imm, then_anf, else_anf) ->
    let* () = gen_im_expr state (T 0) cond_imm in
    let lbl_else, state_after_labels = fresh_label "else" state in
    let lbl_end, final_state = fresh_label "endif" state_after_labels in
    emit beq (T 0) Zero lbl_else;
    let* state_then = gen_anf_expr state dst then_anf in
    emit j lbl_end;
    emit label lbl_else;
    let* state_else = gen_anf_expr state dst else_anf in
    emit label lbl_end;
    let final_stack_offset = min state_then.stack_offset state_else.stack_offset in
    ok { final_state with stack_offset = final_stack_offset }
  | Comp_func (params, body) ->
    let func_label, state = fresh_label "lambda" state in
    let arity' = ArityMap.bind state.arity func_label (List.length params) in
    let state =
      { state with
        arity = arity'
      ; deferred = (func_label, params, body) :: state.deferred
      }
    in
    emit la (A 0) func_label;
    emit li (A 1) (List.length params);
    emit call "alloc_closure";
    if not (equal_reg dst (A 0)) then emit mv dst (A 0);
    ok state
  | Comp_tuple _ -> err `Tuple_not_impl
  | Comp_alloc imms ->
    (* allocate Block(len) and store elements at base+16 (GCHeader+len) *)
    let len = List.length imms in
    emit li (A 0) len;
    emit call "alloc_block";
    (* A0 = Block* *)
    emit addi (T 2) (A 0) 16;
    (* T2 = &elems[0] *)
    let* () =
      List.mapi
        (fun i imm ->
           let* () = gen_im_expr state (T 1) imm in
           emit sd (T 1) (T 2, i * Target.word_size);
           ok ())
        imms
      |> List.fold_left
           (fun acc r ->
              let* () = acc in
              r)
           (ok ())
    in
    if not (equal_reg dst (A 0)) then emit mv dst (A 0);
    ok state
  | Comp_load (addr_imm, offset) ->
    let* () = gen_im_expr state (T 0) addr_imm in
    emit addi (T 0) (T 0) 16;
    (* skip header *)
    emit ld dst (T 0, offset);
    ok state
;;

let rec count_locals_in_anf (aexpr : anf_expr) : int =
  match aexpr with
  | Anf_let (_, _, comp_expr, body) ->
    let locals_in_comp = count_locals_in_comp comp_expr in
    let locals_in_body = count_locals_in_anf body in
    max locals_in_comp (1 + locals_in_body)
  | Anf_comp_expr comp_expr -> count_locals_in_comp comp_expr

and count_locals_in_comp (cexpr : comp_expr) : int =
  match cexpr with
  | Comp_imm _
  | Comp_binop _
  | Comp_app _
  | Comp_func _
  | Comp_tuple _
  | Comp_alloc _
  | Comp_load _ -> 0
  | Comp_branch (_, then_anf, else_anf) ->
    let locals_in_then = count_locals_in_anf then_anf in
    let locals_in_else = count_locals_in_anf else_anf in
    max locals_in_then locals_in_else
;;

let gen_func
      ~arity_map
      (func_name : string)
      (params : ident list)
      (body_anf : anf_expr)
      ppf
      (st : cg_state)
  : cg_state r
  =
  let env_params_res =
    let num_reg_args = Array.length Target.arg_regs in
    let rec go i env = function
      | [] -> ok env
      | p :: ps when i < num_reg_args -> go (i + 1) (Env.bind env p (Reg (A i))) ps
      | p :: ps ->
        let stack_offset = (2 + i - num_reg_args) * Target.word_size in
        go (i + 1) (Env.bind env p (Stack_offset stack_offset)) ps
    in
    go 0 (Env.empty ()) params
  in
  let* env_params = env_params_res in
  let local_count = count_locals_in_anf body_anf in
  let initial_frame_size = (2 + local_count + 5) * Target.word_size in
  emit_prologue func_name initial_frame_size;
  (* Initialize GC heap in main (64 MiB by default) *)
  if func_name = "main"
  then (
    emit li (A 0) (5 * 1024);
    emit call "rt_init";
    if st.gc_stats
    then (
      emit call "print_gc_status";
      emit call "collect";
      emit call "print_gc_status"));
  let initial_state_for_body =
    { st with env = env_params; stack_offset = 0; arity = arity_map }
  in
  let* state_after = gen_anf_expr initial_state_for_body (A 0) body_anf in
  if func_name = "main" && st.gc_stats then emit call "print_gc_status";
  flush_queue ppf;
  emit_epilogue initial_frame_size;
  ok { st with next_label = state_after.next_label; deferred = state_after.deferred }
;;

let gen_start ppf =
  fprintf ppf ".section .text\n";
  fprintf ppf ".global main\n";
  fprintf ppf ".type main, @function\n"
;;

let prefill_arities (arity_map0 : ArityMap.t) (program : aprogram) : ArityMap.t =
  List.fold_left
    (fun am -> function
       | Anf_str_value (_rf, name, anf_expr) ->
         (match anf_expr with
          | Anf_let (_, _, Comp_func (ps, _), _) -> ArityMap.bind am name (List.length ps)
          | Anf_comp_expr (Comp_func (ps, _)) -> ArityMap.bind am name (List.length ps)
          | _ -> ArityMap.bind am name 0)
       | _ -> am)
    arity_map0
    program
;;

let gen_program_res_with_gc ~gc_stats ppf (program : aprogram) : unit r =
  let has_main =
    List.exists
      (function
        | Anf_str_value (_, "main", _) | Anf_str_eval _ -> true
        | _ -> false)
      program
  in
  if has_main then gen_start ppf;
  let arity_map = prefill_arities initial_arity_map program in
  let st0 =
    { env = Env.empty ()
    ; stack_offset = 0
    ; arity = arity_map
    ; next_label = 0
    ; deferred = []
    ; gc_stats
    }
  in
  let* st1 =
    List.fold_left
      (fun acc_res item ->
         let* st = acc_res in
         match item with
         | Anf_str_eval anf_expr -> gen_func ~arity_map "main" [] anf_expr ppf st
         | Anf_str_value (_rec_flag, name, anf_expr) ->
           let params, body =
             match anf_expr with
             | Anf_let (_, _, Comp_func (ps, b), _) -> ps, b
             | Anf_comp_expr (Comp_func (ps, b)) -> ps, b
             | _ -> [], anf_expr
           in
           gen_func ~arity_map name params body ppf st)
      (ok st0)
      program
  in
  let rec drain st =
    match st.deferred with
    | [] -> ok st
    | defs ->
      let st' = { st with deferred = [] } in
      let* st'' =
        List.fold_left
          (fun acc_res (name, ps, body) ->
             let* st_acc = acc_res in
             gen_func ~arity_map name ps body ppf st_acc)
          (ok st')
          (List.rev defs)
      in
      drain st''
  in
  let* _st_final = drain st1 in
  flush_queue ppf;
  ok ()
;;

let gen_program_with_gc_stats ~gc_stats ppf (program : aprogram) =
  match gen_program_res_with_gc ~gc_stats ppf program with
  | Ok () -> ()
  | Error (`Unbound_identifier x) ->
    invalid_arg ("Unbound identifier during codegen: " ^ x)
  | Error `Stack_args_not_impl_direct ->
    invalid_arg "Stack arguments for direct call not implemented"
  | Error `Stack_args_not_impl_external ->
    invalid_arg "Stack arguments for external calls not implemented"
  | Error (`Too_many_args (fname, expected, got)) ->
    invalid_arg
      (Printf.sprintf
         "Too many arguments for function %s: expected %d, got %d"
         fname
         expected
         got)
  | Error `Call_non_function -> invalid_arg "Runtime error: attempted to call a number."
  | Error `Tuple_not_impl -> invalid_arg "Tuple values are not yet implemented"
;;

let gen_program ppf (program : aprogram) =
  gen_program_with_gc_stats ~gc_stats:false ppf program
;;
