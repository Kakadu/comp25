(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type reg =
  | Zero (** Always zero *)
  | RA (** Return address *)
  | SP (** Stack pointer *)
  | A of int (** Arguments A0..A7 *)
  | T of int (** Temporary T0..T6 *)
  | S of int (** Saved S0..S11 *)

val equal_reg : reg -> reg -> bool

type offset = reg * int

val pp_reg : Format.formatter -> reg -> unit
val pp_offset : Format.formatter -> reg * int -> unit

type instr =
  | Addi of reg * reg * int
  (** [addi rd, rs1, imm] Adds register [rs1] to constant [imm], result in [rd] *)
  | Add of reg * reg * reg
  (** [add rd, rs1, rs2] Adds registers [rs1] and [rs2], result in [rd] *)
  | Sub of reg * reg * reg
  (** [sub rd, rs1, rs2] Subtracts [rs2] from [rs1], result in [rd] *)
  | Mul of reg * reg * reg
  (** [mul rd, rs1, rs2] Multiplies [rs1] by [rs2], result in [rd] *)
  | Li of reg * int (** [li rd, imm] Loads constant [imm] into register [rd] *)
  | Ecall
  (** [ecall] Environment call (system call). The system call number is passed in A7,
    arguments in A0–A6, result returned in A0. *)
  | Call of string
  (** [call label] Calls a function by [label], return address is stored in RA *)
  | Ret (** [ret] Returns from function to address stored in RA *)
  | Ld of reg * offset
  (** [ld rd, offset(base)] Loads value from memory at address [base + offset] into register [rd] *)
  | Sd of reg * offset
  (** [sd rs, offset(base)] Stores value of register [rs] into memory at address [base + offset] *)
  | Mv of reg * reg
  (** [mv rd, rs] Copies the value from register [rs] into register [rd] *)
  | Beq of reg * reg * string
  (** [beq rs1, rs2, label] Jumps to [label] if [rs1] == [rs2] *)
  | Blt of reg * reg * string
  (** [blt rs1, rs2, label] Jumps to [label] if [rs1] < [rs2] *)
  | Ble of reg * reg * string
  (** [ble rs1, rs2, label] Jumps to [label] if [rs1] <= [rs2] *)
  | Slt of reg * reg * reg
  (** [slt rd, rs1, rs2] Sets rd to 1 if rs1 < rs2, else sets rd to 0 *)
  | Xori of reg * reg * int (** [xori rd, rs1, imm] Sets [rd] to [rs1] xor [imm] *)
  | J of string (** [j label] Unconditional jump to [label] *)
  | Label of string (** Label for jumps and function calls *)

val pp_instr : Format.formatter -> instr -> unit
val addi : (instr -> 'a) -> reg -> reg -> int -> 'a
val add : (instr -> 'a) -> reg -> reg -> reg -> 'a
val sub : (instr -> 'a) -> reg -> reg -> reg -> 'a
val mul : (instr -> 'a) -> reg -> reg -> reg -> 'a
val li : (instr -> 'a) -> reg -> int -> 'a
val ecall : (instr -> 'a) -> 'a
val call : (instr -> 'a) -> string -> 'a
val ret : (instr -> 'a) -> 'a
val ld : (instr -> 'a) -> reg -> offset -> 'a
val sd : (instr -> 'a) -> reg -> offset -> 'a
val mv : (instr -> 'a) -> reg -> reg -> 'a
val beq : (instr -> 'a) -> reg -> reg -> string -> 'a
val blt : (instr -> 'a) -> reg -> reg -> string -> 'a
val ble : (instr -> 'a) -> reg -> reg -> string -> 'a
val slt : (instr -> 'a) -> reg -> reg -> reg -> 'a
val xori : (instr -> 'a) -> reg -> reg -> int -> 'a
val j : (instr -> 'a) -> string -> 'a
val label : (instr -> 'a) -> string -> 'a
