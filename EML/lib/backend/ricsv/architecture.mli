(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Riscv_backend : sig
  type reg =
    | Zero
    | RA
    | SP
    | A of int
    | T of int
    | S of int
  [@@deriving eq]

  type offset = reg * int

  type instr =
    | Addi of reg * reg * int
    | Ld of reg * offset
    | Sd of reg * offset
    | Mv of reg * reg
    | Li of reg * int
    | Add of reg * reg * reg
    | Sub of reg * reg * reg
    | Call of string
    | Ret
    | Beq of reg * reg * string
    | J of string
    | Label of string
    | La of reg * string
    | Slt of reg * reg * reg
    | Seqz of reg * reg
    | Snez of reg * reg
    | Xori of reg * reg * int
    | Xor of reg * reg * reg
    | Mul of reg * reg * reg
    | Div of reg * reg * reg
    | Slli of reg * reg * int
    | Srli of reg * reg * int

  val pp_reg : Format.formatter -> reg -> unit
  val pp_offset : Format.formatter -> offset -> unit
  val pp_instr : Format.formatter -> instr -> unit
  val tag_int : int -> int
  val fp : reg
  val sp : reg
  val ra : reg
  val zero : reg
  val a0 : reg
  val a1 : reg
  val a2 : reg
  val a3 : reg
  val a4 : reg
  val a5 : reg
  val a6 : reg
  val a7 : reg
  val t0 : reg
  val t1 : reg
  val result_reg : reg
  val addi : reg -> reg -> int -> instr list
  val ld : reg -> offset -> instr list
  val sd : reg -> offset -> instr list
  val mv : reg -> reg -> instr list
  val li : reg -> int -> instr list
  val add : reg -> reg -> reg -> instr list
  val sub : reg -> reg -> reg -> instr list
  val call : string -> instr list
  val ret : unit -> instr list
  val beq : reg -> reg -> string -> instr list
  val j : string -> instr list
  val label : string -> instr list
  val la : reg -> string -> instr list
  val slt : reg -> reg -> reg -> instr list
  val seqz : reg -> reg -> instr list
  val snez : reg -> reg -> instr list
  val xori : reg -> reg -> int -> instr list
  val xor : reg -> reg -> reg -> instr list
  val mul : reg -> reg -> reg -> instr list
  val div : reg -> reg -> reg -> instr list
  val slli : reg -> reg -> int -> instr list
  val srli : reg -> reg -> int -> instr list
  val add_tag_items : reg -> int -> instr list
  val arg_regs : reg list
  val candidate_regs_for_spill : reg list
  val arg_regs_count : int
  val word_size : int
  val stack_align : int
  val frame_header_size : int
  val saved_fp_offset : int
  val saved_ra_offset : int
  val max_addi_imm : int
  val sub_sp : int -> instr list
  val addi_or_li_add : reg -> reg -> int -> instr list
  val sd_at_sp_offset : reg -> int -> instr list

  type location =
    | Loc_reg of reg
    | Loc_mem of offset

  val prologue : enable_gc:bool -> name:string -> stack_size:int -> instr list
  val epilogue : enable_gc:bool -> is_main:bool -> instr list
  val format_item : Format.formatter -> instr -> unit
end
