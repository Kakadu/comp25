(** Copyright 2024,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

(* gens program on riscv asm from the ast *)
val gen_program : formatter -> Middleend.Anf.aprogram -> unit

val gen_program_with_gc_stats
  :  gc_stats:bool
  -> formatter
  -> Middleend.Anf.aprogram
  -> unit
