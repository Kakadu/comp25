(** Copyright 2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Sofya Kozyreva <k81sofia@gmail.com> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Anf
open CodegenTypes

(* for creating unique ite and function labels *)
let label_factory = ref 0

(* for giving some names to intermediate results stored in registers *)
let fresh_name = ref 0

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

let a_regs = [ Arg 0; Arg 1; Arg 2; Arg 3; Arg 4; Arg 5; Arg 6; Arg 7 ]

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

let codegen_pat a_regs free_regs stack placement compiled = function
  (* add information about pattern storage into placement *)
  | PVar (Ident name) ->
    let stack, cur_offset = extend_stack stack 8 in
    let place = Offset cur_offset in
    let placement = PlacementMap.add name place placement in
    a_regs, free_regs, stack, placement, compiled
  | _ -> failwith "Pattern is not yet implemented"
;;

(* for every func arg, move them on stack and update placement via codegen_pat *)
let codegen_func_arg arg i (a_regs, free_regs, stack, placement, compiled) =
  let _, _, stack, placement, compiled =
    codegen_pat a_regs free_regs stack placement compiled arg
  in
  let compiled = True (StackType (SD, Arg i, Stack ((i + 1) * 8))) :: compiled in
  a_regs, free_regs, stack, placement, compiled
;;

(* used before function application to put all values in t* registers on stack to prevent overwriting *)
let add_unnamed_value_to_placement value_place placement =
  let name = string_of_int !fresh_name in
  fresh_name := !fresh_name + 1;
  PlacementMap.add name value_place placement
;;

let change_register old_r new_r placement =
  let name = PlacementMap.find old_r placement in
  PlacementMap.add name new_r placement
;;

let rec codegen_expr a_regs free_regs stack placement compiled e =
  match e with
  (* load const into a* register *)
  | Const (Int_lt n) ->
    let instr = Pseudo (LI (List.hd a_regs, n)) in
    a_regs, free_regs, stack, placement, instr :: compiled
  (* if var is value on stack, load it into Arg 0, if var is function name (ergo has label), call it *)
  | Variable (Ident name) ->
    (match PlacementMap.find_opt name placement with
     | None -> failwith "Panic: undefined var in codegen!"
     | Some (Offset o) ->
       let instr = True (StackType (LD, Arg 0, Stack o)) in
       a_regs, free_regs, stack, placement, instr :: compiled
     | Some (FuncLabel l) ->
       let instr = Pseudo (CALL l) in
       a_regs, free_regs, stack, placement, instr :: compiled
     | Some (Register reg) ->
       let instr = Pseudo (MV (Arg 0, reg)) in
       a_regs, free_regs, stack, placement, instr :: compiled)
  | Bin_expr (op, e1, e2) ->
    (* codegen fst expr, move it into t* or s* register and add it to placement in case of application *)
    let reg_fst = List.hd a_regs in
    let _, _, stack, placement, compiled =
      codegen_expr a_regs free_regs stack placement compiled e1
    in
    let reg_fst_free, free_regs = find_free_reg free_regs in
    let compiled = Pseudo (MV (reg_fst_free, reg_fst)) :: compiled in
    let placement = add_unnamed_value_to_placement (Register reg_fst_free) placement in
    let reg_snd = List.hd a_regs in
    let _, _, stack, placement, compiled =
      codegen_expr a_regs free_regs stack placement compiled e2
    in
    let reg_snd_free, free_regs = find_free_reg free_regs in
    let compiled = Pseudo (MV (reg_snd_free, reg_snd)) :: compiled in
    let placement = add_unnamed_value_to_placement (Register reg_snd_free) placement in
    (* codegen binop with two pre-codegened exprs *)
    let compiled =
      match op with
      | Binary_add -> True (RType (ADD, Arg 0, reg_fst_free, reg_snd_free)) :: compiled
      | Binary_subtract ->
        True (RType (SUB, Arg 0, reg_fst_free, reg_snd_free)) :: compiled
      | Binary_multiply ->
        True (RType (MUL, Arg 0, reg_fst_free, reg_snd_free)) :: compiled
      | Binary_less_or_equal ->
        True (IType (XORI, Arg 0, Arg 0, 1))
        :: True (RType (SLT, Arg 0, reg_fst_free, reg_snd_free))
        :: compiled
      | Binary_greater_or_equal ->
        True (IType (XORI, Arg 0, Arg 0, 1))
        :: True (RType (SLT, Arg 0, reg_snd_free, reg_fst_free))
        :: compiled
      | Binary_less -> True (RType (SLT, Arg 0, reg_fst_free, reg_snd_free)) :: compiled
      | Binary_greater ->
        True (RType (SLT, Arg 0, reg_snd_free, reg_fst_free)) :: compiled
      | _ -> failwith "Binary op is not yet implemented"
    in
    a_regs, free_regs, stack, placement, compiled
  | If_then_else (cond, thn, els) ->
    (* codegen cond expr and store it into t* or s* register *)
    let reg_hd = List.hd a_regs in
    let _, _, stack, placement, compiled =
      codegen_expr a_regs free_regs stack placement compiled cond
    in
    let reg_cond, free_regs = find_free_reg free_regs in
    let compiled = Pseudo (MV (reg_cond, reg_hd)) :: compiled in
    let placement = add_unnamed_value_to_placement (Register reg_cond) placement in
    (match els with
     | Some els ->
       let label_else = make_label "else" in
       let label_join = make_label "join" in
       (* because we want to jump into else when beq Zero 0 => cond should be reversed *)
       let compiled = True (IType (XORI, reg_cond, reg_cond, 1)) :: compiled in
       let compiled = True (BType (BEQ, Zero, reg_cond, label_else)) :: compiled in
       let _, _, stack, placement, compiled =
         codegen_expr a_regs free_regs stack placement compiled thn
       in
       let compiled = Pseudo (J label_join) :: compiled in
       let compiled = True (Label label_else) :: compiled in
       let _, _, stack, placement, compiled =
         codegen_expr a_regs free_regs stack placement compiled els
       in
       let compiled = True (Label label_join) :: compiled in
       a_regs, free_regs, stack, placement, compiled
     | None ->
       let label_join = make_label "join" in
       let compiled = True (BType (BEQ, Zero, reg_cond, label_join)) :: compiled in
       let _, _, stack, placement, compiled =
         codegen_expr a_regs free_regs stack placement compiled thn
       in
       let compiled = Pseudo (J label_join) :: compiled in
       let compiled = True (Label label_join) :: compiled in
       a_regs, free_regs, stack, placement, compiled)
  | Apply (func, arg) ->
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
    (* codegen both argument and function exprs *)
    let _, _, stack, placement, compiled =
      codegen_expr a_regs free_regs stack placement compiled arg
    in
    let _, _, stack, placement, compiled =
      codegen_expr a_regs free_regs stack placement compiled func
    in
    (* put all values back into corresponding registers, cleaning stack back *)
    let stack, compiled =
      List.fold_left
        (fun (stack, compiled) (_, (reg, offset)) ->
           let instr = True (StackType (LD, reg, Stack offset)) in
           stack - 8, instr :: compiled)
        (stack, compiled)
        save_map
    in
    a_regs, free_regs, stack, placement, compiled
  | LetIn (is_rec, let_b, [], expr) ->
    let _, _, stack, placement, compiled =
      codegen_let_bind a_regs free_regs stack placement compiled (is_rec, let_b)
    in
    let _, _, stack, placement, compiled =
      codegen_expr a_regs free_regs stack placement compiled expr
    in
    a_regs, free_regs, stack, placement, compiled
  | _ -> failwith "Expr is not yet codegened"

and codegen_let_bind a_regs free_regs global_stack placement compiled = function
  | _, Let_bind (PVar (Ident name), [], expr) ->
    (*let compiled = if (not !is_start_label_put) then
      True(Label(start_label)) :: compiled
    else compiled
    in
    is_start_label_put := true;*)

    (*let required_stack = 64 in
    let fresh_stack = global_stack in
    let compiled = True(IType(ADDI, Sp, Sp, -required_stack)) :: compiled in*)
    let _, _, global_stack, placement, compiled =
      codegen_expr a_regs free_regs global_stack placement compiled expr
    in
    let global_stack, cur_offset = extend_stack global_stack 8 in
    let placement = PlacementMap.add name (Offset cur_offset) placement in
    let instr = True (StackType (SD, Arg 0, Stack cur_offset)) in
    a_regs, free_regs, global_stack, placement, instr :: compiled
  | _, Let_bind (PVar (Ident name), args, expr) ->
    (* manage function label *)
    let func_label = make_label name in
    let compiled = True (Label func_label) :: compiled in
    let placement = PlacementMap.add name (FuncLabel func_label) placement in
    (* (number of args + ra placement) * 32 bytes -- temporary *)
    let required_stack = (List.length args + 1) * 32 in
    let fresh_stack = 0 in
    let compiled = True (IType (ADDI, Sp, Sp, -required_stack)) :: compiled in
    (* put ra and args on stack *)
    let compiled = True (StackType (SD, Ra, Stack 0)) :: compiled in
    let fresh_stack = fresh_stack + 8 in
    let a_regs, free_regs, stack, new_placement, compiled =
      List.fold_left
        (fun acc (i, arg) -> codegen_func_arg arg i acc)
        (a_regs, free_regs, fresh_stack, placement, compiled)
        (List.mapi (fun i arg -> i, arg) args)
    in
    let _, _, _, _, compiled =
      codegen_expr a_regs free_regs stack new_placement compiled expr
    in
    let compiled = True (StackType (LD, Ra, Stack 0)) :: compiled in
    let compiled = True (IType (ADDI, Sp, Sp, required_stack)) :: compiled in
    let compiled = Pseudo RET :: compiled in
    a_regs, free_regs, global_stack, placement, compiled
  | _ -> failwith "Let bind type is not yet implemented"
;;

let codegen_statement a_regs free_regs global_stack placement compiled = function
  | Let (is_rec, let_b, []) ->
    let is_global, compiled =
      match !is_start_label_put, let_b with
      | false, Let_bind (PVar _, [], _) ->
        is_start_label_put := true;
        (* initializing global stack *)
        true, True (IType (ADDI, Sp, Sp, -64)) :: True (Label start_label) :: compiled
      | _, _ -> false, compiled
    in
    let _, _, global_stack, placement, compiled =
      codegen_let_bind a_regs free_regs global_stack placement compiled (is_rec, let_b)
    in
    let compiled =
      if is_global then True (IType (ADDI, Sp, Sp, 64)) :: compiled else compiled
    in
    a_regs, free_regs, global_stack, placement, compiled
  | _ -> failwith "Statement is not yet implemented"
;;

let codegen_construction a_regs free_regs global_stack placement compiled = function
  | Expr e -> codegen_expr a_regs free_regs global_stack placement compiled e
  | Statement s -> codegen_statement a_regs free_regs global_stack placement compiled s
;;

let codegen cs =
  let placement = PlacementMap.empty in
  let placement = PlacementMap.add "print_int" (FuncLabel "print_int") placement in
  let free_regs = regs in
  let global_stack = 64 in
  let _, _, _, _, instructions =
    List.fold_left
      (fun (a_regs, free_regs, stack, placement, compiled) c ->
         codegen_construction a_regs free_regs stack placement compiled c)
      (a_regs, free_regs, global_stack, placement, [])
      cs
  in
  let instructions = True Ecall :: Pseudo (LI (Arg 7, 93)) :: instructions in
  List.rev instructions
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

    let compiled = match op with
      | CPlus -> True (RType (ADD, List.hd a_regs, reg_fst_free, reg_snd_free)) :: compiled
      | CMinus -> True (RType (SUB, List.hd a_regs, reg_fst_free, reg_snd_free)) :: compiled
      | CMul -> True (RType (MUL, List.hd a_regs, reg_fst_free, reg_snd_free)) :: compiled
      | CDiv -> True (RType (DIV, List.hd a_regs, reg_fst_free, reg_snd_free)) :: compiled
      (* TODO check logic for eq and neq *)
      | CEq -> True (IType (SLTI, List.hd a_regs, List.hd a_regs, 1)) :: True (RType (SUB, List.hd a_regs, reg_fst_free, reg_snd_free)) :: compiled
      | CNeq -> Pseudo(SNEZ (List.hd a_regs, List.hd a_regs)) :: True (RType (SUB, List.hd a_regs, reg_fst_free, reg_snd_free)) :: compiled
      | CLt ->  True (RType (SLT, List.hd a_regs, reg_fst_free, reg_snd_free)) :: compiled
      | CLte -> True (IType (XORI, List.hd a_regs, List.hd a_regs, 1))
      :: True (RType (SLT, List.hd a_regs, reg_fst_free, reg_snd_free))
      :: compiled
      | CGt -> True (RType (SLT, List.hd a_regs, reg_fst_free, reg_snd_free)) :: compiled
      | CGte -> True (IType (XORI, List.hd a_regs, List.hd a_regs, 1))
      :: True (RType (SLT, Arg 0, reg_fst_free, reg_snd_free))
      :: compiled
    in a_regs, free_regs, stack, placement, compiled
  | CImmexpr i -> 
    (* TODO maybe replace it into anpther register? *)
    let a_regs, placement, compiled = codegen_immexpr a_regs placement compiled i in
    (a_regs, free_regs, stack, placement, compiled)
  (*| CIte (cond, thn els) -> *)
  | _ -> failwith "cexpr NYI"

and codegen_aexpr a_regs free_regs stack placement compiled = function
  | ACExpr ce -> codegen_cexpr a_regs free_regs stack placement compiled ce
  | ALet (Ident (name), cexpr, body) ->
    let _, _, stack, placement, compiled = codegen_cexpr a_regs free_regs stack placement compiled cexpr in
    let stack, cur_offset = extend_stack stack 8 in
    let placement = PlacementMap.add name (Offset cur_offset) placement in
    let compiled = True (StackType (SD, List.hd a_regs, Stack cur_offset)) :: compiled in

    let _, _, stack, placement, compiled = codegen_aexpr a_regs free_regs stack placement compiled body in
    a_regs, free_regs, stack, placement, compiled
;;

let codegen_aconstruction a_regs free_regs stack placement compiled = function
  | AExpr ae -> codegen_aexpr a_regs free_regs stack placement compiled ae
  | _ -> failwith "aconstr NYI"

let codegen_aconstructions acs =
  let placement = PlacementMap.empty in
  let placement = PlacementMap.add "print_int" (FuncLabel "print_int") placement in
  let free_regs = regs in
  let global_stack = 64 in
  let _, _, _, _, instructions =
    List.fold_left
      (fun (a_regs, free_regs, stack, placement, compiled) c ->
         codegen_aconstruction a_regs free_regs stack placement compiled c)
      (a_regs, free_regs, global_stack, placement, [])
      acs
  in
  let instructions = True Ecall :: Pseudo (LI (Arg 7, 93)) :: instructions in
  List.rev instructions
;;

