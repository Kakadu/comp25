(** Copyright 2025-2026, Rodion Suvorov, Dmitriy Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Machine
open Ast
open Ast.Pattern
open Middle.Anf

type location =
  | Loc_reg of reg
  | Loc_mem of reg

type env = (ident, location, String.comparator_witness) Map.t

type cg_state =
  { env : env
  ; frame_offset : int
  ; label_id : int
  ; instructions : instr list
  ; arity_map : (ident, int, String.comparator_witness) Map.t
  }

module Codegen = struct
  type 'a t = Cg of (cg_state -> 'a * cg_state)

  let run (state : cg_state) (Cg f) : 'a * cg_state = f state
  let return x = Cg (fun state -> x, state)

  let ( let* ) (Cg m) f =
    Cg
      (fun state ->
        let res, new_state = m state in
        let (Cg m') = f res in
        m' new_state)
  ;;

  let ( >> ) (m1 : unit t) (m2 : 'a t) : 'a t =
    let* () = m1 in
    m2
  ;;

  let get_state = Cg (fun state -> state, state)
  let set_state new_state = Cg (fun _ -> (), new_state)

  let emit instr =
    let k instr =
      let* state = get_state in
      set_state { state with instructions = instr :: state.instructions }
    in
    instr k
  ;;

  let rec map_m f = function
    | [] -> return ()
    | x :: xs ->
      let* () = f x in
      map_m f xs
  ;;
end

open Codegen

let fresh_label prefix =
  let* state = get_state in
  let id = state.label_id in
  let* () = set_state { state with label_id = id + 1 } in
  return (Printf.sprintf ".L%s_%d" prefix id)
;;

let allocate_local_var id =
  let* state = get_state in
  let new_offset = state.frame_offset + 8 in
  (* TODO: maybe 16? *)
  let location = ROff (-new_offset, fp) in
  let new_env = Map.set state.env ~key:id ~data:(Loc_mem location) in
  let* () = set_state { state with frame_offset = new_offset; env = new_env } in
  return location
;;

let a_gen_bin_op op dst r1 r2 =
  match op with
  | Add -> emit add dst r1 r2
  | Sub -> emit sub dst r1 r2
  | Mul -> emit mul dst r1 r2
  | Le -> emit slt dst r2 r1 >> emit xori dst dst 1
  | Lt -> emit slt dst r1 r2
  | Eq ->
    emit sub t2 r1 r2
    >> emit slt dst x0 t2
    >> emit slt t3 t2 x0
    >> emit add dst dst t3
    >> emit xori dst dst 1
  | Neq ->
    emit sub t2 r1 r2 >> emit slt dst x0 t2 >> emit slt t3 t2 x0 >> emit add dst dst t3
;;

(* collect caller-saved registers that are currently live in the env.
   exclude a0 because runtime/targets use it for return/first arg *)
let live_caller_regs_excluding_a0 : reg list Codegen.t =
  let* st = get_state in
  Map.to_alist st.env
  |> List.filter_map ~f:(fun (_, loc) ->
    match loc with
    | Loc_reg ((A _ | T _) as r) -> Some r
    | _ -> None)
  |> List.dedup_and_sort ~compare:Poly.compare
  |> List.filter ~f:(fun r -> not (equal_reg r a0))
  |> return
;;

(* save a list of regs in stack, in order. Returns the same list *)
let save_regs (rs : reg list) : reg list Codegen.t =
  map_m (fun r -> emit addi sp sp (-8) >> emit sd r (ROff (0, sp))) rs >> return rs
;;

(* restore regs from the stack in reverse order *)
let restore_regs (rs : reg list) : unit Codegen.t =
  map_m (fun r -> emit ld r (ROff (0, sp)) >> emit addi sp sp 8) (List.rev rs)
  >> return ()
;;

(* Utility: run some code with caller-saved registers saved and restored *)
let with_saved_caller_regs (body : reg list -> unit Codegen.t) : unit Codegen.t =
  let* lives = live_caller_regs_excluding_a0 in
  let* lives = save_regs lives in
  let* () = body lives in
  restore_regs lives
;;

(* load first up to 8 buffered args from a temporary base (t3) into A0..A7. *)
let load_reg_args_from_buffer (count : int) : unit Codegen.t =
  map_m (fun i -> emit ld (A i) (ROff (i * 8, t3))) (List.init count ~f:Fn.id)
  >> return ()
;;

let rec push_args_array (args : immexpr list) : int Codegen.t =
  let n = List.length args in
  emit addi sp sp (-(8 * n))
  >> map_m
       (fun (i, arg) -> a_gen_immexpr t0 arg >> emit sd t0 (ROff (i * 8, sp)))
       (List.mapi args ~f:(fun i a -> i, a))
  >> return (n * 8)

and a_gen_immexpr (dst : reg) (immexpr : immexpr) =
  match immexpr with
  | ImmNum i -> emit li dst i
  | ImmId id ->
    let* st = get_state in
    (match Map.find st.env id with
     | Some loc ->
       (* local identifier: either in a register or at a frame slot *)
       (match loc with
        | Loc_reg r -> emit mv dst r
        | Loc_mem m -> emit ld dst m)
     | None ->
       (* global identifier.
          if it's a function with positive arity --- produce a closure pointer.
          otherwise just load the address (label) *)
       (match Map.find st.arity_map id with
        | Some arity when arity > 0 ->
          emit la a0 id
          >> emit li a1 arity
          >> emit call "closure_alloc"
          >> if not (equal_reg dst a0) then emit mv dst a0 else return ()
        | _ -> emit la dst id))
;;

let rec a_gen_expr (dst : reg) (aexpr : aexpr) : unit Codegen.t =
  match aexpr with
  | ACE cexpr -> a_gen_cexpr dst cexpr
  | ALet (_rec_flag, id, cexpr, body) ->
    a_gen_cexpr t0 cexpr
    >>
    let* loc = allocate_local_var id in
    emit sd t0 loc >> a_gen_expr dst body

and a_gen_cexpr (dst : reg) (cexpr : cexpr) : unit Codegen.t =
  match cexpr with
  | CImm imm -> a_gen_immexpr dst imm
  | CBinop (bop, imm1, imm2) ->
    a_gen_immexpr t0 imm1 >> a_gen_immexpr t1 imm2 >> a_gen_bin_op bop dst t0 t1
  | CApp (ImmId fname, args) ->
    let* st = get_state in
    let provided_arity = List.length args in
    (match Map.find st.env fname with
     | Some _ ->
       (* ----- indirect call ----- *)
       (* place args as a contiguous array on the stack *)
       let* _bytes = push_args_array args in
       (* perform call with automatic save/restore of live caller-saved regs *)
       with_saved_caller_regs (fun lives ->
         let saved_cnt = List.length lives in
         (* load closure into a0, set_state a1=argc, a2=pointer to args *)
         a_gen_immexpr a0 (ImmId fname)
         >> emit li a1 provided_arity
         >> emit addi a2 sp (8 * saved_cnt)
         (* call runtime. Result is in a0 *)
         >> emit call "closure_apply"
         (* restore and pop the temporary arg array *)
         >> emit addi sp sp (8 * provided_arity)
         (* move result if needed *)
         >> if not (equal_reg dst a0) then emit mv dst a0 else return ())
     | None ->
       (* ----- direct/global call ----- *)
       let total_arity = Map.find_exn st.arity_map fname in
       if total_arity = provided_arity
       then (
         (* ----- full application ----- *)
         let indexed = List.mapi args ~f:(fun i imm -> imm, i) in
         let reg_args, stack_args = List.partition_tf indexed ~f:(fun (_, i) -> i < 8) in
         let n_reg = List.length reg_args in
         (* pre-buffer register arguments so we can call runtime/save regs safely before jal/jalr *)
         (if n_reg > 0 then emit addi sp sp (-8 * n_reg) else return ())
         >> emit addi t3 sp 0
         >> map_m
              (fun (arg_imm, i) ->
                 a_gen_immexpr t0 arg_imm >> emit sd t0 (ROff (i * 8, t3)))
              reg_args
         (* push stack arguments in reverse order *)
         >> map_m
              (fun (arg_imm, _) ->
                 a_gen_immexpr t0 arg_imm
                 >> emit addi sp sp (-8)
                 >> emit sd t0 (ROff (0, sp)))
              (List.rev stack_args)
         (* perform call with automatic save/restore of live caller-saved regs *)
         >> with_saved_caller_regs (fun _ ->
           (* move buffered reg-args into A0.. and perform the call *)
           load_reg_args_from_buffer n_reg
           >> (match Map.find st.env fname with
             | Some (Loc_reg r) -> emit jalr r
             | Some (Loc_mem m) -> emit ld t0 m >> emit jalr t0
             | None -> emit call fname)
           (* move result, restore regs, and pop stack args/reg buffer *)
           >> emit mv t0 a0 (* save call result immediately *)
           >> (if not (equal_reg dst t0) then emit mv dst t0 else return ())
           >> (if not (List.is_empty stack_args)
               then emit addi sp sp (8 * List.length stack_args)
               else return ())
           >> if n_reg > 0 then emit addi sp sp (8 * n_reg) else return ()))
       else if provided_arity < total_arity
       then
         (* ----- partial application ----- *)
         (* push the provided args as an array *)
         let* _bytes = push_args_array args in
         (* perform call with automatic save/restore of live caller-saved regs *)
         with_saved_caller_regs (fun lives ->
           let saved_cnt = List.length lives in
           (* alignment: keep stack 16-byte aligned across runtime calls.
          add one 8-byte slot when (#saved_regs + #args) is odd *)
           let need_pad = (provided_arity + saved_cnt) land 1 = 1 in
           (if need_pad
            then emit addi sp sp (-8) >> emit sd x0 (ROff (0, sp))
            else return ())
           (* allocate empty closure for (fname, total_arity) *)
           >> emit la a0 fname
           >> emit li a1 total_arity
           >> emit call "closure_alloc"
           (* apply the provided args into that closure *)
           >> emit li a1 provided_arity
           >> emit addi a2 sp (8 * (saved_cnt + if need_pad then 1 else 0))
           >> emit call "closure_apply"
           (* drop pad, pop args array; result (closure pointer) in a0 *)
           >> (if need_pad then emit addi sp sp 8 else return ())
           >> emit addi sp sp (8 * provided_arity)
           >> if not (equal_reg dst a0) then emit mv dst a0 else return ())
       else failwith "Too many arguments")
  | CApp (ImmNum _, _) -> failwith "unreachable"
  (* TODO: ite without else *)
  | CIte (cond_imm, then_aexpr, else_aexpr) ->
    let* l_else = fresh_label "else" in
    let* l_end = fresh_label "endif" in
    a_gen_immexpr t0 cond_imm
    >> emit beq t0 x0 l_else
    >> a_gen_expr dst then_aexpr
    >> emit j l_end
    >> emit label l_else
    >> a_gen_expr dst else_aexpr
    >> emit label l_end
  | CFun (_, _) -> failwith "TODO"
;;

let rec a_count_local_vars = function
  | ALet (_, _, _, body) -> 1 + a_count_local_vars body
  | ACE cexpr ->
    (match cexpr with
     | CIte (_, then_expr, else_expr) ->
       Int.max (a_count_local_vars then_expr) (a_count_local_vars else_expr)
     | _ -> 0)
;;

let a_gen_func name args body =
  let is_main = String.equal name "main" in
  let func_label = name in
  let locals_count = a_count_local_vars body in
  let spill_count = Int.min (List.length args) 8 in
  let stack_size = 16 + ((locals_count + spill_count) * 8) in
  emit directive (Printf.sprintf ".globl %s" func_label)
  >> emit directive (Printf.sprintf ".type %s, @function" func_label)
  >> emit label func_label
  >> emit addi sp sp (-stack_size)
  >> emit sd ra (ROff (stack_size - 8, sp))
  >> emit sd fp (ROff (stack_size - 16, sp))
  >> emit addi fp sp stack_size
  >>
  let* global_state = get_state in
  let f i env (pat : Ast.Pattern.t) =
    match pat with
    | Pat_var id when i < 8 ->
      (* bind first up to 8 params directly to A registers *)
      Map.set env ~key:id ~data:(Loc_reg (A i))
    | Pat_var id ->
      let offset = (i - 8) * 8 in
      Map.set env ~key:id ~data:(Loc_mem (ROff (offset, fp)))
    | _ -> failwith "unreachable"
  in
  let initial_env = List.foldi args ~init:(Map.empty (module String)) ~f in
  let initial_cg_state = { global_state with env = initial_env; frame_offset = 16 } in
  set_state initial_cg_state
  >>
  (* spill first up to 8 params from A registers into fresh frame slots and
   bind parameter names to those Loc_mem slots *)
  let nspill = Int.min (List.length args) 8 in
  map_m
    (fun i ->
       match List.nth args i with
       | Some (Pat_var id) ->
         let* loc = allocate_local_var id in
         (* updates env := Loc_mem loc *)
         emit sd (A i) loc (* store A(i) into its slot *)
       | _ -> return ())
    (List.init nspill ~f:Fn.id)
  >> a_gen_expr a0 body
  >>
  let* final_state = get_state in
  set_state
    { global_state with
      label_id = final_state.label_id
    ; instructions = final_state.instructions
    }
  >> emit label (name ^ "_end")
  >> emit ld ra (ROff (stack_size - 8, sp))
  >> emit ld fp (ROff (stack_size - 16, sp))
  >> emit addi sp sp stack_size
  >> if is_main then emit li a0 0 >> emit li (A 7) 93 >> emit ecall else emit ret
;;

let codegen ppf (s : aprogram) =
  let arity_map =
    List.fold
      s
      ~init:(Map.empty (module String))
      ~f:(fun acc -> function
        | AStr_value (_, name, expr) ->
          let rec extract_fun_params_body (aexp : aexpr) =
            match aexp with
            | ACE (CFun (param, body)) ->
              let params, final_body = extract_fun_params_body body in
              Ast.Pattern.Pat_var param :: params, final_body
            | _ -> [], aexp
          in
          let params, _ = extract_fun_params_body expr in
          Map.set acc ~key:name ~data:(List.length params)
        | _ -> acc)
  in
  let arity_map = Map.set arity_map ~key:"print_int" ~data:1 in
  let initial_state =
    { env = Map.empty (module String)
    ; frame_offset = 0
    ; label_id = 0
    ; instructions = []
    ; arity_map
    }
  in
  (* let initial_state =
    { env = Map.empty (module String); frame_offset = 0; label_id = 0; instructions = [] }
  in *)
  let program_gen =
    emit directive ".text"
    >> map_m
         (function
           | AStr_value (_rec_flag, name, expr) ->
             let rec extract_fun_params_body (aexp : aexpr) =
               match aexp with
               | ACE (CFun (param, body)) ->
                 let params, final_body = extract_fun_params_body body in
                 Ast.Pattern.Pat_var param :: params, final_body
               | _ -> [], aexp
             in
             let params, body = extract_fun_params_body expr in
             a_gen_func name params body
           | AStr_eval expr -> a_gen_func "main" [] expr)
         s
  in
  let (), final_state = Codegen.run initial_state program_gen in
  pp_instrs ppf final_state.instructions
;;
