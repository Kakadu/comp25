(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Middleend.Anf
open Analysis

let gen_program ?(enable_gc = false) ?(enable_peephole = true) ppf (program : anf_program)
  =
  program |> analyze |> Generator.gen_program ~enable_gc ~enable_peephole ppf
;;
