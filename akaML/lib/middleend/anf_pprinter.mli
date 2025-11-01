[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Anf_core

val pp_a_pat : Format.formatter -> a_pat -> unit
val pp_i_exp : Format.formatter -> i_exp -> unit
val pp_c_exp : Format.formatter -> c_exp -> unit
val pp_a_exp : Format.formatter -> a_exp -> unit
val pp_a_structure_item : Format.formatter -> a_structure_item -> unit
val pp_a_structure : Format.formatter -> a_structure_item list -> unit
