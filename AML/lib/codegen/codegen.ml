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

  let get = Cg (fun state -> state, state)
  let set new_state = Cg (fun _ -> (), new_state)

  let emit instr =
    let k instr =
      let* state = get in
      set { state with instructions = instr :: state.instructions }
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

let get_state = get
let set_state = set

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

let a_gen_bin_op op dst r1 r2 =
  match op with
  | Add -> emit add dst r1 r2
  | Sub -> emit sub dst r1 r2
  | Mul -> emit mul dst r1 r2
  | Le -> emit slt dst t1 t0 >> emit xori dst dst 1
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

let a_gen_immexpr (dst : reg) (immexpr : immexpr) =
  match immexpr with
  | ImmNum i -> emit li dst i
  | ImmId id ->
    let* loc = lookup_var id in
    (match loc with
     | Loc_reg r -> emit mv dst r
     | Loc_mem m -> emit ld dst m)
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
    let* state = get in
    let live_caller_regs =
      Map.to_alist state.env
      |> List.filter_map ~f:(fun (_, loc) ->
        match loc with
        | Loc_reg ((A _ | T _) as r) -> Some r
        | _ -> None)
      |> List.dedup_and_sort ~compare:Poly.compare
    in
    map_m (fun r -> emit addi sp sp (-8) >> emit sd r (ROff (0, sp))) live_caller_regs
    >> map_m
         (fun (arg_imm, i) ->
            if i < 8
            then a_gen_immexpr (A i) arg_imm
            else failwith "Functions with more than 8 arguments are not supported")
         (List.mapi args ~f:(fun i imm -> imm, i))
    >> emit call fname
    >> (if not (equal_reg dst a0) then emit mv dst a0 else return ())
    >> map_m
         (fun r -> emit ld r (ROff (0, sp)) >> emit addi sp sp 8)
         (List.rev live_caller_regs)
  | CApp (ImmNum _, _) -> failwith "unreachable"
  | CIte (cond_imm, then_aexpr, else_aexpr) ->
    let* else_label = fresh_label "else" in
    let* end_label = fresh_label "endif" in
    a_gen_immexpr t0 cond_imm
    >> emit beq t0 x0 else_label
    >> a_gen_expr dst then_aexpr
    >> emit j end_label
    >> emit label else_label
    >> a_gen_expr dst else_aexpr
    >> emit label end_label
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
  let stack_size = 16 + (locals_count * 8) in
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
    | Pat_var id when i < 8 -> Map.set env ~key:id ~data:(Loc_reg (A i))
    | _ -> failwith "Unsupported argument pattern or too many arguments"
  in
  let initial_env = List.foldi args ~init:(Map.empty (module String)) ~f in
  let initial_cg_state = { global_state with env = initial_env; frame_offset = 16 } in
  set_state initial_cg_state
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
  let initial_state =
    { env = Map.empty (module String); frame_offset = 0; label_id = 0; instructions = [] }
  in
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
