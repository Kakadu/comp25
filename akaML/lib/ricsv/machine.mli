[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

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
  (* === Arithmetic instructions === *)
  | Addi of reg * reg * int
  (** [addi rd, rs, imm] Adds register [rs] and immediate [imm], result in [rd] *)
  | Add of reg * reg * reg
  (** [add rd, rs1, rs2] Adds registers [rs1] and [rs2], result in [rd] *)
  | Sub of reg * reg * reg
  (** [sub rd, rs1, rs2] Subtracts [rs2] from [rs1], result in [rd] *)
  | Mul of reg * reg * reg
  (** [mul rd, rs1, rs2] Multiplies [rs1] by [rs2], result in [rd] *)
  (* === Logical / bitwise instructions === *)
  | Srli of reg * reg * int
  (** [slli rd, rs, imm] Logical shift right of [rs] by [imm] bits, result in [rd] *)
  | Slli of reg * reg * int
  (** [slli rd, rs, imm] Logical shift left of [rs] by [imm] bits, result in [rd] *)
  | Xori of reg * reg * int
  (** [xori rd, rs, imm] Bitwise exclusive-or of [rs] and immediate [imm], result in [rd] *)
  | Xor of reg * reg * reg
  (** [xor rd, rs1, rs2] Bitwise exclusive-or of [rs1] and [rs2], result in [rd] *)
  | Slt of reg * reg * reg
  (** [slt rd, rs1, rs2] Sets [rd] = 1 if [rs1] < [rs2], else [rd] = 0 *)
  | Seqz of reg * reg (** [seqz rd, rs] Sets [rd] = 1 if [rs] == 0, else [rd] = 0 *)
  | Snez of reg * reg (** [snez rd, rs] Sets [rd] = 1 if [rs] != 0, else [rd] = 0 *)
  (* === Immediate loading === *)
  | Li of reg * int (** [li rd, imm] Loads immediate [imm] into register [rd] *)
  | La of reg * string
  (** [la rd, label] Loads the address of [label] into register [rd] *)
  | Mv of reg * reg
  (** [mv rd, rs] Copies the value from register [rs] into register [rd] *)
  (* === Memory access === *)
  | Ld of reg * offset
  (** [ld rd, offset(base)] Loads value from memory [base + offset] into register [rd] *)
  | Sd of reg * offset
  (** [sd rs, offset(base)] Stores value of register [rs] into memory [base + offset] *)
  (* === Control flow: jumps and branches === *)
  | Beq of reg * reg * string
  (** [beq rs1, rs2, label] Jumps to [label] if [rs1] == [rs2] *)
  | J of string (** [j label] Unconditional jump to [label] *)
  | Label of string (** [label:] Declares a label for jumps and function calls *)
  (* === Function calls and system calls === *)
  | Call of string
  (** [call label] Calls a function at [label], return address stored in RA *)
  | Ret (** [ret] Returns from function to address stored in RA *)
  | Ecall
  (** [ecall] Environment call (system call).
      The syscall number is passed in A7, arguments in A0–A6, result in A0. *)

val pp_instr : Format.formatter -> instr -> unit
val addi : (instr -> 'a) -> reg -> reg -> int -> 'a
val add : (instr -> 'a) -> reg -> reg -> reg -> 'a
val sub : (instr -> 'a) -> reg -> reg -> reg -> 'a
val mul : (instr -> 'a) -> reg -> reg -> reg -> 'a
val srli : (instr -> 'a) -> reg -> reg -> int -> 'a
val slli : (instr -> 'a) -> reg -> reg -> int -> 'a
val xori : (instr -> 'a) -> reg -> reg -> int -> 'a
val xor : (instr -> 'a) -> reg -> reg -> reg -> 'a
val slt : (instr -> 'a) -> reg -> reg -> reg -> 'a
val seqz : (instr -> 'a) -> reg -> reg -> 'a
val snez : (instr -> 'a) -> reg -> reg -> 'a
val li : (instr -> 'a) -> reg -> int -> 'a
val la : (instr -> 'a) -> reg -> string -> 'a
val mv : (instr -> 'a) -> reg -> reg -> 'a
val ld : (instr -> 'a) -> reg -> offset -> 'a
val sd : (instr -> 'a) -> reg -> offset -> 'a
val beq : (instr -> 'a) -> reg -> reg -> string -> 'a
val j : (instr -> 'a) -> string -> 'a
val label : (instr -> 'a) -> string -> 'a
val call : (instr -> 'a) -> string -> 'a
val ret : (instr -> 'a) -> 'a
val ecall : (instr -> 'a) -> 'a
val to_tag_integer : int -> int
