[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

val gen_a_structure
  :  enable_gc:bool
  -> Format.formatter
  -> Middleend.Anf_core.a_structure_item list
  -> unit
