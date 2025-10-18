(** Copyright 2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Sofya Kozyreva <k81sofia@gmail.com>, Vyacheslav Kochergin <vyacheslav.kochergin1@gmail.com> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Anf
open CodegenTypes

(* for creating unique ite and function labels *)
let label_factory = ref 0

(* for now, this is the only way to write _start label at suitable place and do it exactly once *)
let is_start_label_put = ref false
let start_label = "_start"
let t_regs = [ Temp 0; Temp 1; Temp 2; Temp 3; Temp 4; Temp 5; Temp 6 ]

let s_regs =
  [ Saved 0
  ; Saved 1
  ; Saved 2
  ; Saved 3
  ; Saved 4
  ; Saved 5
  ; Saved 6
  ; Saved 7
  ; Saved 8
  ; Saved 9
  ; Saved 10
  ; Saved 11
  ]
;;

let a_regs_global = [ Arg 0; Arg 1; Arg 2; Arg 3; Arg 4; Arg 5; Arg 6; Arg 7 ]

(* a_regs are used as a "bridge" to new values, so it is unstable to use them for storing *)
let regs = t_regs @ s_regs

module PlacementMap = Map.Make (String)
module RegStackMap = Map.Make (Int)

(* return current stack tail and increment it by size being allocated *)
let extend_stack stack size =
  let cur_offset = stack in
  stack + size, cur_offset
;;

(* placement will probably be used later, when there is logic for pushing something into stack *)
let find_free_reg free_regs (*placement*) =
  match free_regs with
  | reg :: tail -> reg, tail
  | [] -> failwith "Empty reg list!"
;;

let make_label name =
  let label = Printf.sprintf ".%s_%d" name !label_factory in
  label_factory := !label_factory + 1;
  label
;;

let codegen_immexpr a_regs placement compiled = function
  | ImmNum n ->
    let instr = Pseudo (LI (List.hd a_regs, n)) in
    a_regs, placement, instr :: compiled
  | ImmId (Ident name) ->
    (match PlacementMap.find_opt name placement with
     | None -> failwith "Panic: undefined var in codegen!"
     | Some (Offset o) ->
       (* change back to Arg 0 here? *)
       let instr = True (StackType (LD, List.hd a_regs, Stack o)) in
       a_regs, placement, instr :: compiled
     | Some (FuncLabel l) ->
       let instr = Pseudo (CALL l) in
       a_regs, placement, instr :: compiled
     | Some (Register reg) ->
       (* change back to Arg 0 here? *)
       let instr = Pseudo (MV (List.hd a_regs, reg)) in
       a_regs, placement, instr :: compiled)
;;

let find_argument = function
  | a_reg :: rest -> a_reg, rest
  | _ -> failwith "argument storing on stack is not yet implemented"
;;

let rec codegen_cexpr a_regs free_regs stack placement compiled = function
  | CBinop (op, i1, i2) ->
    let reg_fst = List.hd a_regs in
    let _, _, compiled = codegen_immexpr a_regs placement compiled i1 in
    let reg_fst_free, free_regs = find_free_reg free_regs in
    let compiled = Pseudo (MV (reg_fst_free, reg_fst)) :: compiled in
    let reg_snd = List.hd a_regs in
    let _, _, compiled = codegen_immexpr a_regs placement compiled i2 in
    let reg_snd_free, free_regs = find_free_reg free_regs in
    let compiled = Pseudo (MV (reg_snd_free, reg_snd)) :: compiled in
    let compiled =
      match op with
      | CPlus ->
        True (RType (ADD, List.hd a_regs, reg_fst_free, reg_snd_free)) :: compiled
      | CMinus ->
        True (RType (SUB, List.hd a_regs, reg_fst_free, reg_snd_free)) :: compiled
      | CMul -> True (RType (MUL, List.hd a_regs, reg_fst_free, reg_snd_free)) :: compiled
      | CDiv -> True (RType (DIV, List.hd a_regs, reg_fst_free, reg_snd_free)) :: compiled
      (* TODO check logic for eq and neq *)
      | CEq ->
        Pseudo (SEQZ (List.hd a_regs, List.hd a_regs))
        :: True (RType (XOR, List.hd a_regs, reg_fst_free, reg_snd_free))
        :: compiled
      | CNeq ->
        Pseudo (SNEZ (List.hd a_regs, List.hd a_regs))
        :: True (RType (SUB, List.hd a_regs, reg_fst_free, reg_snd_free))
        :: compiled
      | CLt -> True (RType (SLT, List.hd a_regs, reg_fst_free, reg_snd_free)) :: compiled
      | CLte ->
        True (IType (XORI, List.hd a_regs, List.hd a_regs, 1))
        :: True (RType (SLT, List.hd a_regs, reg_snd_free, reg_fst_free))
        :: compiled
      | CGt -> True (RType (SLT, List.hd a_regs, reg_snd_free, reg_fst_free)) :: compiled
      | CGte ->
        True (IType (XORI, List.hd a_regs, List.hd a_regs, 1))
        :: True (RType (SLT, Arg 0, reg_fst_free, reg_snd_free))
        :: compiled
    in
    a_regs, free_regs, stack, placement, compiled
  | CImmexpr i ->
    (* TODO maybe replace it into another register? *)
    let a_regs, placement, compiled = codegen_immexpr a_regs placement compiled i in
    a_regs, free_regs, stack, placement, compiled
  | CIte (cond, thn, els) ->
    let reg_hd = List.hd a_regs in
    let _, _, stack, placement, compiled =
      codegen_cexpr a_regs free_regs stack placement compiled cond
    in
    let reg_cond, free_regs = find_free_reg free_regs in
    let compiled = Pseudo (MV (reg_cond, reg_hd)) :: compiled in
    (match els with
     | Some els ->
       let label_else = make_label "else" in
       let label_join = make_label "join" in
       (* because we want to jump into else when beq Zero 0 => cond should be reversed *)
       (*let compiled = True (IType (XORI, reg_cond, reg_cond, 1)) :: compiled in*)
       let compiled = True (BType (BEQ, Zero, reg_cond, label_else)) :: compiled in
       let _, _, stack, placement, compiled =
         codegen_aexpr a_regs free_regs stack placement compiled thn
       in
       let compiled = Pseudo (J label_join) :: compiled in
       let compiled = True (Label label_else) :: compiled in
       let _, _, stack, placement, compiled =
         codegen_aexpr a_regs free_regs stack placement compiled els
       in
       let compiled = True (Label label_join) :: compiled in
       a_regs, free_regs, stack, placement, compiled
     | None ->
       let label_join = make_label "join" in
       let compiled = True (BType (BEQ, Zero, reg_cond, label_join)) :: compiled in
       let _, _, stack, placement, compiled =
         codegen_aexpr a_regs free_regs stack placement compiled thn
       in
       let compiled = Pseudo (J label_join) :: compiled in
       let compiled = True (Label label_join) :: compiled in
       a_regs, free_regs, stack, placement, compiled)
  | CLam (Ident name, ae) ->
    let arg_reg, a_regs_new = find_argument a_regs in
    let stack, cur_offset = extend_stack stack 8 in
    let placement = PlacementMap.add name (Offset cur_offset) placement in
    let compiled = True (StackType (SD, arg_reg, Stack cur_offset)) :: compiled in
    (match ae with
     | ACExpr (CLam (_, _)) ->
       codegen_aexpr a_regs_new free_regs stack placement compiled ae
     | _ ->
       (* if next expr isnt lambda, then args are all set and all a* registers can be used for codegen again *)
       codegen_aexpr a_regs_global free_regs stack placement compiled ae)
  (* TODO: technically, name can be digit. do something about it? *)
  | CApp (func, args) ->
    (* find all t* registers that should be stored *)
    let used_temps =
      PlacementMap.bindings placement
      |> List.filter_map (fun (name, sp) ->
        match sp with
        | Register (Temp i) -> Some (name, Temp i)
        | _ -> None)
    in
    (* store them on stack with mapping of who is who *)
    let stack, compiled, save_map =
      List.fold_left
        (fun (stack, compiled, save_map) (name, reg) ->
           let stack, cur_offset = extend_stack stack 8 in
           let instr = True (StackType (SD, reg, Stack cur_offset)) in
           stack, instr :: compiled, (name, (reg, cur_offset)) :: save_map)
        (stack, compiled, [])
        used_temps
    in
    (* codegen arguments and function expr *)
    let a_regs_inner, placement, compiled =
      List.fold_left
        (fun (a_regs, placement, compiled) c ->
           let _, a_regs_next = find_argument a_regs in
           let _, placement, compiled = codegen_immexpr a_regs placement compiled c in
           a_regs_next, placement, compiled)
        (a_regs, placement, compiled)
        args
    in
    let _, placement, compiled = codegen_immexpr a_regs_inner placement compiled func in
    (* put all values back into corresponding registers, cleaning stack back *)
    let stack, compiled =
      List.fold_left
        (fun (stack, compiled) (_, (reg, offset)) ->
           let instr = True (StackType (LD, reg, Stack offset)) in
           stack - 8, instr :: compiled)
        (stack, compiled)
        save_map
    in
    (* after application, all a* can be used again *)
    a_regs_global, free_regs, stack, placement, compiled

and codegen_aexpr a_regs free_regs stack placement compiled = function
  | ACExpr ce -> codegen_cexpr a_regs free_regs stack placement compiled ce
  | ALet (Ident name, cexpr, body) ->
    let _, _, stack, placement, compiled =
      codegen_cexpr a_regs free_regs stack placement compiled cexpr
    in
    let stack, cur_offset = extend_stack stack 8 in
    let placement = PlacementMap.add name (Offset cur_offset) placement in
    let compiled = True (StackType (SD, List.hd a_regs, Stack cur_offset)) :: compiled in
    let _, _, stack, placement, compiled =
      codegen_aexpr a_regs free_regs stack placement compiled body
    in
    a_regs, free_regs, stack, placement, compiled
;;

let is_function = function
  | ACExpr (CLam (Ident _, _)) -> true
  | _ -> false
;;

let codegen_astatement a_regs free_regs placement compiled = function
  | Ident name, st ->
    (match is_function st with
     | true ->
       let func_label = make_label name in
       let compiled = True (Label func_label) :: compiled in
       let placement = PlacementMap.add name (FuncLabel func_label) placement in
       let required_stack = 64 in
       let compiled = True (IType (ADDI, Sp, Sp, -required_stack)) :: compiled in
       let compiled = True (StackType (SD, Ra, Stack 0)) :: compiled in
       let fresh_stack = 8 in
       let _, _, stack, placement, compiled =
         codegen_aexpr a_regs free_regs fresh_stack placement compiled st
       in
       let compiled =
         Pseudo RET
         :: True (IType (ADDI, Sp, Sp, required_stack))
         :: True (StackType (LD, Ra, Stack 0))
         :: compiled
       in
       a_regs, free_regs, stack, placement, compiled
     (* if statement is not a function and label start isnt put yet, initialize global stack and put start label before it *)
     | false ->
       let is_global, compiled =
         match !is_start_label_put with
         | true -> false, compiled
         | false ->
           is_start_label_put := true;
           true, True (IType (ADDI, Sp, Sp, -64)) :: True (Label start_label) :: compiled
       in
       (* TODO: maybe put it in placement here? *)
       let _, _, stack, placement, compiled =
         codegen_aexpr a_regs free_regs 0 placement compiled st
       in
       let compiled =
         if is_global then True (IType (ADDI, Sp, Sp, 64)) :: compiled else compiled
       in
       a_regs, free_regs, stack, placement, compiled)
;;

let codegen_aconstruction a_regs free_regs stack placement compiled = function
  | AExpr ae ->
    let is_global, compiled =
      match !is_start_label_put with
      | true -> false, compiled
      | false ->
        is_start_label_put := true;
        true, True (IType (ADDI, Sp, Sp, -64)) :: True (Label start_label) :: compiled
    in
    let _, _, stack, placement, compiled =
      codegen_aexpr a_regs free_regs stack placement compiled ae
    in
    let compiled =
      if is_global then True (IType (ADDI, Sp, Sp, 64)) :: compiled else compiled
    in
    a_regs, free_regs, stack, placement, compiled
  | AStatement (_, st_list) ->
    let a_regs, free_regs, stack, placement, compiled =
      List.fold_left
        (fun (a_regs, free_regs, _, placement, compiled) s ->
           codegen_astatement a_regs free_regs placement compiled s)
        (a_regs, free_regs, stack, placement, compiled)
        st_list
    in
    a_regs, free_regs, stack, placement, compiled
;;

let codegen_aconstructions acs =
  let placement = PlacementMap.empty in
  let placement = PlacementMap.add "print_int" (FuncLabel "print_int") placement in
  let free_regs = regs in
  let global_stack = 0 in
  let _, _, _, _, instructions =
    List.fold_left
      (fun (a_regs, free_regs, stack, placement, compiled) c ->
         codegen_aconstruction a_regs free_regs stack placement compiled c)
      (a_regs_global, free_regs, global_stack, placement, [])
      acs
  in
  let instructions = True Ecall :: Pseudo (LI (Arg 7, 93)) :: instructions in
  List.rev instructions
;;
