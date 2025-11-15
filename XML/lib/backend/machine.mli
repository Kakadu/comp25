(** Copyright 2024,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type reg =
  | A of int (*function args*)
  | T of int (*temporary*)
  | S of int (*saved*)
  | RA
  | SP
  | Zero

val equal_reg : reg -> reg -> bool
val pp_reg : Format.formatter -> reg -> unit

type offset = reg * int

type instr =
  | Addi of reg * reg * int (* ADD immediate *)
  | Add of reg * reg * reg (* ADD *)
  | Sub of reg * reg * reg (* SUB *)
  | Mul of reg * reg * reg (* MUL *)
  | Slt of reg * reg * reg (* SLT: set less than (signed) *)
  | Seqz of reg * reg (* SEQZ: set equal zero *)
  | Snez of reg * reg (* SNEZ: set not equal zero *)
  | Xor of reg * reg * reg (* XOR *)
  | Xori of reg * reg * int (* XOR immediate *)
  | Beq of reg * reg * string (* BEQ: branch if equal *)
  | Blt of reg * reg * string (* BLT: branch if less than *)
  | Ble of reg * reg * string (* BLE: branch if less or equal *)
  | Lla of reg * string (* LLA: load address *)
  | Li of reg * int (* LI: load immediate *)
  | Ld of reg * offset (* LD: load doubleword *)
  | Sd of reg * offset (* SD: store doubleword *)
  | Mv of reg * reg (* MV: move *)
  | Comment of string (* Assembler comment *)
  | Label of string (* Assembler label *)
  | Call of string (* CALL *)
  | J of string (* J: jump *)
  | Ecall (* ECALL *)
  | Ret (* return *)
  | La of reg * string (* Load Address of labeled function into the reg *)
  | Slli of reg * reg * int (* << imm *)
  | Srai of reg * reg * int (* >> imm (arith) *)

val pp_instr : Format.formatter -> instr -> unit
val addi : (instr -> 'a) -> reg -> reg -> int -> 'a
val add : (instr -> 'a) -> reg -> reg -> reg -> 'a
val sub : (instr -> 'a) -> reg -> reg -> reg -> 'a
val mul : (instr -> 'a) -> reg -> reg -> reg -> 'a
val slt : (instr -> 'a) -> reg -> reg -> reg -> 'a
val seqz : (instr -> 'a) -> reg -> reg -> 'a
val snez : (instr -> 'a) -> reg -> reg -> 'a
val xor : (instr -> 'a) -> reg -> reg -> reg -> 'a
val xori : (instr -> 'a) -> reg -> reg -> int -> 'a
val li : (instr -> 'a) -> reg -> int -> 'a
val ecall : (instr -> 'a) -> 'a
val call : (instr -> 'a) -> string -> 'a
val ret : (instr -> 'a) -> 'a
val lla : (instr -> 'a) -> reg -> string -> 'a
val ld : (instr -> 'a) -> reg -> offset -> 'a
val sd : (instr -> 'a) -> reg -> offset -> 'a
val mv : (instr -> 'a) -> reg -> reg -> 'a
val beq : (instr -> 'a) -> reg -> reg -> string -> 'a
val blt : (instr -> 'a) -> reg -> reg -> string -> 'a
val ble : (instr -> 'a) -> reg -> reg -> string -> 'a
val j : (instr -> 'a) -> string -> 'a
val la : (instr -> 'a) -> reg -> string -> 'a
val comment : (instr -> 'a) -> string -> 'a
val label : (instr -> 'a) -> string -> 'a
val slli : (instr -> 'a) -> reg -> reg -> int -> 'a
val srai : (instr -> 'a) -> reg -> reg -> int -> 'a
