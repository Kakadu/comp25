(** Copyright 2025-2026, Rodion Suvorov, Dmitriy Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type reg =
  | X0
  | A of int
  | RA
  | SP
  | T of int
  | S of int
  | ROff of int * reg
[@@deriving eq]

val x0 : reg
val ra : reg
val sp : reg
val fp : reg
val a0 : reg
val a1 : reg
val a2 : reg
val t0 : reg
val t1 : reg
val t2 : reg
val t3 : reg
val pp_reg : Format.formatter -> reg -> unit

type instr =
  | Addi of reg * reg * int
  | Add of reg * reg * reg
  | Sub of reg * reg * reg
  | Mul of reg * reg * reg
  | Slt of reg * reg * reg
  | Xori of reg * reg * int
  | Srai of reg * reg * int
  | Slli of reg * reg * int
  | Beq of reg * reg * string
  | Bne of reg * reg * string
  | Blt of reg * reg * string
  | Jal of reg * string
  | J of string
  | Jalr of reg
  | Ret
  | Ld of reg * reg
  | Sd of reg * reg
  | Li of reg * int
  | La of reg * string
  | Ecall
  | Label of string
  | Directive of string
  | Call of string

val pp_instr : Format.formatter -> instr -> unit
val addi : (instr -> 'a) -> reg -> reg -> int -> 'a
val add : (instr -> 'a) -> reg -> reg -> reg -> 'a
val sub : (instr -> 'a) -> reg -> reg -> reg -> 'a
val mul : (instr -> 'a) -> reg -> reg -> reg -> 'a
val slt : (instr -> 'a) -> reg -> reg -> reg -> 'a
val xori : (instr -> 'a) -> reg -> reg -> int -> 'a
val srai : (instr -> 'a) -> reg -> reg -> int -> 'a
val slli : (instr -> 'a) -> reg -> reg -> int -> 'a
val beq : (instr -> 'a) -> reg -> reg -> string -> 'a
val bne : (instr -> 'a) -> reg -> reg -> string -> 'a
val blt : (instr -> 'a) -> reg -> reg -> string -> 'a
val ecall : (instr -> 'a) -> 'a
val ret : (instr -> 'a) -> 'a
val jal : (instr -> 'a) -> reg -> string -> 'a
val j : (instr -> 'a) -> string -> 'a
val ld : (instr -> 'a) -> reg -> reg -> 'a
val sd : (instr -> 'a) -> reg -> reg -> 'a
val li : (instr -> 'a) -> reg -> int -> 'a
val label : (instr -> 'a) -> string -> 'a
val directive : (instr -> 'a) -> string -> 'a
val mv : (instr -> 'a) -> reg -> reg -> 'a
val call : (instr -> 'a) -> string -> 'a
val la : (instr -> 'a) -> reg -> string -> 'a
val jalr : (instr -> 'a) -> reg -> 'a
val pp_instrs : Format.formatter -> instr list -> unit
