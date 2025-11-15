[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Machine
open Middleend.Anf_core
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
  type 'a t = codegen_state -> ('a * codegen_state, string) result

  let return x st = Ok (x, st)
  let fail e = fun _ -> Error e

  let bind m f =
    fun state ->
    match m state with
    | Ok (x, st') -> f x st'
    | Error e -> Error e
  ;;

  let ( let* ) = bind
  let get st = Ok (st, st)
  let put st = fun _ -> Ok ((), st)

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

  let run m = m
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
    | "+" ->
      return
      @@
      (emit add dst r1 r2;
       emit addi dst dst (-1))
    | "-" ->
      return
      @@
      (emit sub dst r1 r2;
       emit addi dst dst 1)
    | "*" ->
      return
      @@
      (emit srli r1 r1 1;
       emit addi r2 r2 (-1);
       emit mul dst r1 r2;
       emit addi dst dst 1)
    | "<=" ->
      emit slt dst r2 r1;
      return @@ emit xori dst dst 1
    | ">=" ->
      emit slt dst r1 r2;
      return @@ emit xori dst dst 1
    | "=" ->
      emit xor dst r1 r2;
      return @@ emit seqz dst dst
    | "<>" ->
      emit xor dst r1 r2;
      return @@ emit snez dst dst
    | "<" -> return @@ emit slt dst r1 r2
    | ">" -> return @@ emit slt dst r2 r1
    | _ -> fail ("unsupported binary operator: " ^ op)
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

  let emit_fn_prologue ~enable_gc name stack_size =
    (* allocate space on stack, store RA, old FP (S0) and make a new FP *)
    emit label name;
    emit addi SP SP (-stack_size);
    emit sd RA (SP, stack_size - Platform.word_size);
    emit sd (S 0) (SP, stack_size - (2 * Platform.word_size));
    emit addi (S 0) SP (stack_size - (2 * Platform.word_size)) ~comm:"Prologue ends";
    if enable_gc && String.equal name "main"
    then (
      emit call "init_gc";
      emit mv (A 0) (S 0);
      emit call "set_ptr_stack")
  ;;

  let emit_fn_epilogue ~enable_gc is_main =
    if enable_gc && is_main then emit call "destroy_gc";
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
    emit li dst (to_tag_integer n);
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
        | _ -> fail ("unbound variable: " ^ x)))
  | _ -> fail "GenIExp: Not implemented"

and gen_c_exp env dst = function
  | CIExp i_exp -> gen_i_exp env dst i_exp
  | CExp_apply (IExp_ident fname, IExp_unit, []) ->
    emit call fname;
    return env
  | CExp_apply (IExp_ident op, i_exp1, [ i_exp2 ]) when Ast.is_bin_op op ->
    let* env = gen_i_exp env (T 0) i_exp1 in
    let* env = gen_i_exp env (T 1) i_exp2 in
    let* env = ensure_reg_free env dst in
    let* () = emit_bin_op dst op (T 0) (T 1) in
    return env
  | CExp_apply (IExp_ident fname, i_exp, i_exp_list) ->
    let args = i_exp :: i_exp_list in
    let* env = emit_save_caller_regs env in
    let* state = get in
    (* Determine which args can overwrite regs *)
    let is_rewrites_regs_cache =
      List.mapi args ~f:(fun i arg ->
        let rew =
          match arg with
          | IExp_constant _ -> false
          | IExp_ident id ->
            (match Map.find state.arity_map id with
             | Some _ -> true
             | None -> false)
          | _ -> false
        in
        i, rew)
    in
    let rw_idxs =
      is_rewrites_regs_cache
      |> List.filter_map ~f:(fun (i, rew) -> if rew then Some i else None)
    in
    let rw_count = List.length rw_idxs in
    (* Save all 'dangerous' args on the stack and remember where *)
    let dangerous_bytes = rw_count * Platform.word_size in
    if dangerous_bytes > 0
    then emit addi SP SP (-dangerous_bytes) ~comm:"Saving 'dangerous' args";
    let* rw_locs =
      List.foldi args ~init:(return Map.Poly.empty) ~f:(fun i acc arg ->
        let* acc = acc in
        if List.mem rw_idxs i ~equal:Int.equal
        then
          let* _ = gen_i_exp env (A 0) arg in
          let* loc = emit_store (A 0) in
          return (Map.set acc ~key:i ~data:loc)
        else return acc)
    in
    (* helper: prepare registers for first N args from 'arg_regs' *)
    let load_into_regs env arg_regs args_list =
      let num_reg_args = min (List.length args_list) (List.length arg_regs) in
      List.foldi
        (List.take args_list num_reg_args)
        ~init:(return env)
        ~f:(fun i acc arg ->
          let* env = acc in
          let reg = List.nth_exn arg_regs i in
          if Map.mem rw_locs i
          then (
            let loc = Map.find_exn rw_locs i in
            emit_load_reg reg loc;
            return env)
          else
            let* env = gen_i_exp env reg arg in
            return env)
    in
    (* helper: push stack args (those exceeding register count) *)
    let push_stack_args env stack_args =
      let stack_count = List.length stack_args in
      if stack_count = 0
      then return (env, 0)
      else (
        let stack_bytes = stack_count * Platform.word_size in
        emit addi SP SP (-stack_bytes) ~comm:"Stack space for variadic args";
        let* env =
          List.foldi stack_args ~init:(return env) ~f:(fun i acc arg ->
            let* env = acc in
            let offset = i * Platform.word_size in
            let* env = gen_i_exp env (T 0) arg in
            emit sd (T 0) (SP, offset);
            return env)
        in
        return (env, stack_bytes))
    in
    let arity = Map.find state.arity_map fname in
    (match List.length args, arity with
     (* Branch in which the arity of fname is known and is less than args_received *)
     | args_received, Some arity when args_received > arity ->
       let first_args = List.take args arity in
       let rest_args = List.drop args arity in
       let* tmp_id = fresh in
       let tmp_name = Printf.sprintf "%d_partial" tmp_id in
       let* env =
         gen_c_exp
           env
           dst
           (CExp_apply (IExp_ident fname, List.hd_exn first_args, List.tl_exn first_args))
       in
       let* loc = emit_store dst ~comm:tmp_name in
       let env = Map.set env ~key:tmp_name ~data:loc in
       gen_c_exp
         env
         dst
         (CExp_apply (IExp_ident tmp_name, List.hd_exn rest_args, List.tl_exn rest_args))
     (* Branch in which the arity of fname is known and it exactly matches args_received *)
     | args_received, Some args_count when args_received = args_count ->
       let arg_regs = [ A 0; A 1; A 2; A 3; A 4; A 5; A 6; A 7 ] in
       let env = load_into_regs env arg_regs args in
       let stack_args = List.drop args (List.length arg_regs) in
       let* env = env in
       let* env, extra_reserved = push_stack_args env stack_args in
       emit call fname;
       if not (equal_reg dst (A 0)) then emit mv dst (A 0);
       if extra_reserved > 0
       then emit addi SP SP extra_reserved ~comm:"Restore stack after call";
       return env
     (* Branch for closure *)
     | args_received, _ ->
       let arg_regs = [ A 2; A 3; A 4; A 5; A 6; A 7 ] in
       let* env = gen_i_exp env (A 0) (IExp_ident fname) in
       emit li (A 1) args_received;
       let env = load_into_regs env arg_regs args in
       let stack_args = List.drop args (List.length arg_regs) in
       let* env = env in
       let* env, extra_reserved = push_stack_args env stack_args in
       emit call "applyN";
       if extra_reserved > 0
       then emit addi SP SP extra_reserved ~comm:"Restore stack after applyN";
       if not (equal_reg dst (A 0)) then emit mv dst (A 0);
       return env)
  | CExp_ifthenelse (cond, then_e, Some else_e) ->
    let* env = gen_c_exp env (T 0) cond in
    let* id = fresh in
    let else_lbl = Printf.sprintf "else_%d" id
    and end_lbl = Printf.sprintf "end_%d" id in
    emit beq (T 0) Zero else_lbl;
    let* st = get in
    let old_frame_ofs = st.frame_offset in
    (* then case *)
    let* _ = gen_a_exp env dst then_e in
    emit j end_lbl;
    (* else case *)
    let* () = put { st with frame_offset = old_frame_ofs } in
    emit label else_lbl;
    let* _ = gen_a_exp env dst else_e in
    emit label end_lbl;
    return env
  | _ -> fail "GenCExp: Not implemented"

and gen_a_exp env dst = function
  | ACExp c_exp -> gen_c_exp env dst c_exp
  | AExp_let (_, Pat_var id, exp, exp_in) ->
    let* env = gen_c_exp env (A 0) exp in
    let* loc = emit_store (A 0) ~comm:id in
    let env = Map.set env ~key:id ~data:loc in
    gen_a_exp env dst exp_in
  | _ -> fail "GenAExp: Not implemented"
;;

let rec count_loc_vars_i_exp = function
  | IExp_ident _ | IExp_constant _ | IExp_unit -> 0
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

let gen_a_func ~enable_gc f_id arg_list body_exp ppf state =
  fprintf ppf "\n  .globl %s\n  .type %s, @function\n" f_id f_id;
  let arity = List.length arg_list in
  let reg_params, stack_params =
    List.split_n arg_list (min arity Platform.arg_regs_count)
  in
  let stack_size = (2 + count_loc_vars_a_exp body_exp) * Platform.word_size in
  let env = Map.empty (module String) in
  let* env =
    List.foldi reg_params ~init:(return env) ~f:(fun i acc -> function
      | APat_var name ->
        let* env = acc in
        return @@ Map.set env ~key:name ~data:(Loc_reg (A i))
      | _ -> fail "unsupported pattern")
  in
  let* env =
    List.foldi stack_params ~init:(return env) ~f:(fun i acc -> function
      | APat_var name ->
        let* env = acc in
        let offset = (i + 2) * Platform.word_size in
        return @@ Map.set env ~key:name ~data:(Loc_mem (S 0, offset))
      | _ -> fail "unsupported pattern")
  in
  emit_fn_prologue ~enable_gc f_id stack_size;
  let* st = get in
  let* () = put { st with frame_offset = 0 } in
  let* _ = gen_a_exp env (A 0) body_exp in
  emit_fn_epilogue ~enable_gc (String.equal f_id "main");
  flush_queue ppf;
  return state
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

let gen_a_structure ~enable_gc ppf anf_ast =
  fprintf ppf ".section .text";
  let arity_map =
    let print_arities = [ "print_int", 1; "print_endline", 1 ] in
    let gc_arities =
      [ "collect", 0; "get_heap_start", 0; "get_heap_final", 0; "print_gc_status", 0 ]
    in
    let all_arities = if enable_gc then print_arities @ gc_arities else print_arities in
    List.fold
      ~init:(init_arity_map anf_ast)
      ~f:(fun map (key, data) -> Map.set map ~key ~data)
      all_arities
  in
  let init_state = { frame_offset = 0; fresh_id = 0; arity_map } in
  let program =
    let* _ =
      List.fold anf_ast ~init:(return init_state) ~f:(fun acc -> function
        | AStruct_value (_, Pat_var f_id, body_exp) ->
          let* state = acc in
          let rec extract_fun_params acc = function
            | ACExp (CIExp (IExp_fun (pat, body))) -> extract_fun_params (pat :: acc) body
            | other -> List.rev acc, other
          in
          let pat_list, body_exp = extract_fun_params [] body_exp in
          gen_a_func ~enable_gc f_id pat_list body_exp ppf state
        | _ -> fail "unsupported structure item")
    in
    pp_print_flush ppf ();
    return ()
  in
  run program init_state
  |> function
  | Ok _ -> ()
  | Error msg ->
    Format.eprintf "Codegen error: %s\n%!" msg;
    exit 1
;;
