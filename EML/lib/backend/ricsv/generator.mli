(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val gen_program
  :  enable_gc:bool
  -> enable_peephole:bool
  -> Format.formatter
  -> Analysis.analysis_result
  -> (unit, string) Result.t
