(** Copyright 2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Sofya Kozyreva <k81sofia@gmail.com>, Vyacheslav Kochergin <vyacheslav.kochergin1@gmail.com> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

type value =
  | Num of int
  | Ptr of int

type reg =
  | Zero
  | Ra
  | Fp
  | Sp
  | Stack of int * reg (* offset from sp or fp *)
  | Temp of int
  | Saved of int
  | Arg of int

type variable_type =
  | Argument
  | Local

(* for mapping names with the way they can be reached *)
type meta_info =
  | Var of int * variable_type
    (* represents stack offset of a variable, and bool if it is an argument *)
  | Func of string * int (* represents label and arity of a function *)
  | Value of reg (* represents value in a register *)

let temp i = Temp i
let saved i = Saved i
let arg i = Arg i

(* small subset enough to codegen factorial  *)
type rtype_op =
  | ADD
  | SUB
  | MUL
  | DIV
  | AND
  | OR
  | XOR
  | SLT

type itype_op =
  | ADDI
  (* | LI *)
  | JALR
  | SLTI
  | XORI
  | SLLI (* for converting ints into tagged ints *)
  | SRLI (* and back *)

type stack_op =
  | LW
  | SW
  | LD
  | SD

(* type stype_op = SW *)

type btype_op =
  | BEQ
  | BNE
  | BLE

type utype_op =
  | LUI
  | AUIPC

type jtype_op = JAL

type pseudo_instr =
  | LI of reg * value
  | LA of reg * string
  | MV of reg * reg
  | J of string
  | RET
  | CALL of string
  | SNEZ of reg * reg (* puts 0 in rd if rs equals 0, else puts 1. is used for <> binop *)
  | SEQZ of reg * reg (* puts 1 in rd if rs equals 0, else puts 0. is used for = binop *)

type true_instr =
  | RType of rtype_op * reg * reg * reg (* op rd rs1 rs2 *)
  | IType of itype_op * reg * reg * value (* op rd rs1 imm *)
  | StackType of stack_op * reg * reg (* op rd offset(rs) *)
  (*| SType of stype_op * reg * reg * int (* op rs2 rs1 imm *) *)
  | BType of btype_op * reg * reg * string (* op rs1 rs2 imm/label offset *)
  | UType of utype_op * reg * value (* op rd imm *)
  | JType of jtype_op * reg * value (* op rd imm/label offset *)
  | Label of string
  | Ecall

type instr =
  | True of true_instr
  | Pseudo of pseudo_instr

let pp_value fmt = function
  | Num imm -> fprintf fmt "%d" imm
  | Ptr imm -> fprintf fmt "%d" imm
;;

let rec pp_reg fmt = function
  | Zero -> fprintf fmt "x0"
  | Ra -> fprintf fmt "ra"
  | Fp -> fprintf fmt "fp"
  | Sp -> fprintf fmt "sp"
  | Stack (o, reg) ->
    let () = fprintf fmt "%d(" o in
    let () = pp_reg fmt reg in
    fprintf fmt ")"
  | Temp i -> fprintf fmt "t%d" i
  | Saved i -> fprintf fmt "s%d" i
  | Arg i -> fprintf fmt "a%d" i
;;

let pp_rtype_op fmt = function
  | ADD -> fprintf fmt "add"
  | SUB -> fprintf fmt "sub"
  | MUL -> fprintf fmt "mul"
  | DIV -> fprintf fmt "div"
  | AND -> fprintf fmt "and"
  | OR -> fprintf fmt "or"
  | XOR -> fprintf fmt "xor"
  | SLT -> fprintf fmt "slt"
;;

let pp_itype_op fmt = function
  | ADDI -> fprintf fmt "addi"
  (* | LI -> fprintf fmt "li" *)
  | JALR -> fprintf fmt "jalr"
  | SLTI -> fprintf fmt "slti"
  | XORI -> fprintf fmt "xori"
  | SLLI -> fprintf fmt "slli"
  | SRLI -> fprintf fmt "srli"
;;

let pp_stack_op fmt = function
  | LW -> fprintf fmt "lw"
  | SW -> fprintf fmt "sw"
  | LD -> fprintf fmt "ld"
  | SD -> fprintf fmt "sd"
;;

(* let pp_stype_op fmt = function
  | SW -> fprintf fmt "sw"
;; *)

let pp_btype_op fmt = function
  | BEQ -> fprintf fmt "beq"
  | BNE -> fprintf fmt "bne"
  | BLE -> fprintf fmt "ble"
;;

let pp_utype_op fmt = function
  | LUI -> fprintf fmt "lui"
  | AUIPC -> fprintf fmt "auipc"
;;

let pp_jtype_op fmt = function
  | JAL -> fprintf fmt "jal"
;;

let pp_pseudo_instr fmt = function
  | LI (r, imm) -> fprintf fmt "@[\tli %a, %a@]@." pp_reg r pp_value imm
  | LA (r, l) -> fprintf fmt "@[\tla %a, %s@]@." pp_reg r l
  | MV (r1, r2) -> fprintf fmt "@[\tmv %a, %a@]@." pp_reg r1 pp_reg r2
  | J l -> fprintf fmt "@[\tj %s@]@." l
  | RET -> fprintf fmt "@[\tret @]@."
  | CALL l -> fprintf fmt "@[\tcall %s@]@." l
  | SNEZ (r1, r2) -> fprintf fmt "@[\tsnez %a, %a@]@." pp_reg r1 pp_reg r2
  | SEQZ (r1, r2) -> fprintf fmt "@[\tseqz %a, %a@]@." pp_reg r1 pp_reg r2
;;

let pp_true_instr fmt = function
  | RType (op, rd, rs1, rs2) ->
    fprintf fmt "@[\t%a %a, %a, %a@]@." pp_rtype_op op pp_reg rd pp_reg rs1 pp_reg rs2
  | IType (op, rd, rs1, imm) ->
    fprintf fmt "@[\t%a %a, %a, %a@]@." pp_itype_op op pp_reg rd pp_reg rs1 pp_value imm
  | StackType (op, rd, rs) ->
    fprintf fmt "@[\t%a %a, %a@]@." pp_stack_op op pp_reg rd pp_reg rs
  (* | SType (op, rs2, rs1, imm) ->
    fprintf fmt "@[\t%a %a, %d(%a)@]@." pp_stype_op op pp_reg rs2 imm pp_reg rs1 *)
  | BType (op, rs1, rs2, l) ->
    fprintf fmt "@[\t%a %a, %a, %s@]@." pp_btype_op op pp_reg rs1 pp_reg rs2 l
  | UType (op, rd, imm) ->
    fprintf fmt "@[\t%a %a, %a@]@." pp_utype_op op pp_reg rd pp_value imm
  | JType (op, rd, imm) ->
    fprintf fmt "@[\t%a %a, %a@]@." pp_jtype_op op pp_reg rd pp_value imm
  | Label l -> fprintf fmt "%s:@." l
  | Ecall -> fprintf fmt "ecall@."
;;

let pp_instr = function
  | True i -> pp_true_instr std_formatter i
  | Pseudo i -> pp_pseudo_instr std_formatter i
;;
