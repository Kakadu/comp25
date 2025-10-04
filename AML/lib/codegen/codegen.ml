(** Copyright 2025-2026, Rodion Suvorov, Dmitriy Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Machine
open Ast
open Ast.Expression
open Ast.Pattern

type location =
  | Loc_reg of reg
  | Loc_mem of reg

type env = (ident, location, String.comparator_witness) Map.t

type cg_state =
  { env : env
  ; frame_offset : int
  ; label_id : int
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

  let get = Cg (fun state -> state, state)
end

open Codegen

let get_state = Cg (fun state -> state, state)
let set_state new_state = Cg (fun _ -> (), new_state)

let fresh_label prefix =
  let* state = get_state in
  let id = state.label_id in
  let* () = set_state { state with label_id = id + 1 } in
  return (Printf.sprintf ".L%s_%d" prefix id)
;;

let allocate_local_var id =
  let* state = get_state in
  let new_offset = state.frame_offset + 8 in
  let location = ROff (-new_offset, fp) in
  let new_env = Map.set state.env ~key:id ~data:(Loc_mem location) in
  let* () = set_state { state with frame_offset = new_offset; env = new_env } in
  return location
;;

let lookup_var id =
  let* state = get_state in
  match Map.find state.env id with
  | Some loc -> return loc
  | None -> failwith ("Unbound variable: " ^ id)
;;

let gen_bin_op op dst r1 r2 =
  match op with
  | "+" -> emit add dst r1 r2
  | "-" -> emit sub dst r1 r2
  | "*" -> emit mul dst r1 r2
  | "<=" ->
    emit slt dst t1 t0;
    emit xori dst dst 1
  | _ -> failwith ("Unsupported binary operator: " ^ op)
;;

let rec gen_expr (dst : reg) (expr : Ast.Expression.t) : unit Codegen.t =
  match expr with
  | Exp_constant (Const_integer i) -> return (emit li dst i)
  | Exp_ident id ->
    let* loc = lookup_var id in
    return
      (match loc with
       | Loc_reg r -> emit mv dst r
       | Loc_mem m -> emit ld dst m)
  | Exp_apply (f, arg) ->
    (match f, arg with
     | Exp_ident op, Exp_tuple (arg1, arg2, []) when is_not_keyword op ->
       let* () = gen_expr t0 arg1 in
       let* () = gen_expr t1 arg2 in
       return (gen_bin_op op dst t0 t1)
     | Exp_ident fname, arg_exp ->
       let* state = get in
       let live_caller_regs =
         Map.to_alist state.env
         |> List.filter_map ~f:(fun (_, loc) ->
           match loc with
           | Loc_reg ((A _ | T _) as r) -> Some r
           | _ -> None)
         |> List.dedup_and_sort ~compare:Poly.compare
       in
       let* () =
         return
           (List.iter live_caller_regs ~f:(fun r ->
              emit addi sp sp (-8);
              emit sd r (ROff (0, sp))))
       in
       let* () = gen_expr a0 arg_exp in
       let* () = return (emit jal ra fname) in
       let* () = if not (equal_reg dst a0) then return (emit mv dst a0) else return () in
       let* () =
         return
           (List.iter (List.rev live_caller_regs) ~f:(fun r ->
              emit ld r (ROff (0, sp));
              emit addi sp sp 8))
       in
       return ()
     | _ -> failwith "TODO: general function application not done yet")
  | Exp_let (_, ({ pat = Pat_var id; expr }, []), body) ->
    let* () = gen_expr t0 expr in
    let* loc = allocate_local_var id in
    let* () = return (emit sd t0 loc) in
    gen_expr dst body
  | Exp_if (cond, then_exp, Some else_exp) ->
    let* else_label = fresh_label "else" in
    let* end_label = fresh_label "endif" in
    let* () = gen_expr t0 cond in
    let* () = return (emit beq t0 x0 else_label) in
    let* () = gen_expr dst then_exp in
    let* () = return (emit j end_label) in
    let* () = return (emit label else_label) in
    let* () = gen_expr dst else_exp in
    return (emit label end_label)
  | _ -> failwith "TODO: expr"
;;

let rec count_local_vars = function
  | Exp_let (_, _, body) -> 1 + count_local_vars body
  | Exp_if (c, t, Some e) -> count_local_vars c + count_local_vars t + count_local_vars e
  | Exp_apply (f, a) -> count_local_vars f + count_local_vars a
  | _ -> 0
;;

let gen_func name args body =
  let is_main = String.equal name "main" in
  let func_label = if is_main then "_start" else name in
  emit directive (Printf.sprintf ".globl %s" func_label);
  emit directive (Printf.sprintf ".type %s, @function" func_label);
  emit label func_label;
  let locals_count = count_local_vars body in
  let stack_size = 16 + (locals_count * 8) in
  emit addi sp sp (-stack_size);
  emit sd ra (ROff (stack_size - 8, sp));
  emit sd fp (ROff (stack_size - 16, sp));
  emit addi fp sp stack_size;
  let f i env = function
    | Pat_var id when i < 8 -> Map.set env ~key:id ~data:(Loc_reg (A i))
    | _ -> failwith "not yet"
  in
  let initial_env = List.foldi args ~init:(Map.empty (module String)) ~f in
  let initial_cg_state = { env = initial_env; frame_offset = 16; label_id = 0 } in
  let (), _final_state = Codegen.run initial_cg_state (gen_expr a0 body) in
  let () = emit label (name ^ "_end") in
  let () = emit ld ra (ROff (stack_size - 8, sp)) in
  let () = emit ld fp (ROff (stack_size - 16, sp)) in
  let () = emit addi sp sp stack_size in
  let () =
    if is_main
    then (
      emit li (A 7) 93;
      emit ecall)
    else emit ret
  in
  ()
;;

let codegen ppf (s : Structure.structure_item list) =
  let open Structure in
  emit directive ".text";
  List.iter s ~f:(function
    | Str_value (Recursive, ({ pat = Pat_var f; expr = Exp_fun ((p, ps), body) }, [])) ->
      gen_func f (p :: ps) body
    | Str_value (Nonrecursive, ({ pat = Pat_var f; expr = body }, [])) ->
      gen_func f [] body
    | _ -> failwith "Unsupported toplevel structure item ");
  flush_queue ppf
;;
