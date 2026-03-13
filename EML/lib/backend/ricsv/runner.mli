(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val gen_program
  :  ?enable_gc:bool
  -> ?enable_peephole:bool
  -> Format.formatter
  -> Middleend.Anf.anf_program
  -> (unit, string) result
