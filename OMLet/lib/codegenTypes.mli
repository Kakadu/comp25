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
  | Stack of int * reg
  | Temp of int
  | Saved of int
  | Arg of int

type variable_type =
  | Argument
  | Local

type meta_info =
  | Var of int * variable_type
  | Func of string * int
  | Value of reg

val temp : int -> reg
val saved : int -> reg
val arg : int -> reg

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
  | JALR
  | SLTI
  | XORI
  | SLLI
  | SRLI

type stack_op =
  | LW
  | SW
  | LD
  | SD

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
  | SNEZ of reg * reg
  | SEQZ of reg * reg

type true_instr =
  | RType of rtype_op * reg * reg * reg
  | IType of itype_op * reg * reg * value
  | StackType of stack_op * reg * reg
  | BType of btype_op * reg * reg * string
  | UType of utype_op * reg * value
  | JType of jtype_op * reg * value
  | Label of string
  | Ecall

type instr =
  | True of true_instr
  | Pseudo of pseudo_instr

val pp_reg : formatter -> reg -> unit
val pp_rtype_op : formatter -> rtype_op -> unit
val pp_itype_op : formatter -> itype_op -> unit
val pp_stack_op : formatter -> stack_op -> unit
val pp_btype_op : formatter -> btype_op -> unit
val pp_utype_op : formatter -> utype_op -> unit
val pp_jtype_op : formatter -> jtype_op -> unit
val pp_pseudo_instr : formatter -> pseudo_instr -> unit
val pp_true_instr : formatter -> true_instr -> unit
val pp_instr : instr -> unit
