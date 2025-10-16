[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Default : sig
  val gen_structure : Format.formatter -> Ast.structure_item list -> unit
end

module Anf : sig
  val gen_a_structure : Format.formatter -> Anf.Anf_core.a_structure_item list -> unit
end
