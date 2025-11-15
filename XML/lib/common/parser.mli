(** Copyright 2024,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

val parse : string -> (program, string) result
val parse_str : string -> program
val pass_ws : unit Angstrom.t
val pass_ws1 : unit Angstrom.t
val token : string -> string Angstrom.t
val pparenth : 'a Angstrom.t -> 'a Angstrom.t
val pident_cap : string Angstrom.t
val pident_lc : string Angstrom.t
val pconstint : Constant.t Angstrom.t
val pconstchar : Constant.t Angstrom.t
val pconststring : Constant.t Angstrom.t
val pconst : Constant.t Angstrom.t
val ptype : TypeExpr.t Angstrom.t
val ptype_adt : TypeExpr.t Angstrom.t
val ppattern : Pattern.t Angstrom.t
val ppatvar : Pattern.t Angstrom.t
val pexpr : Expression.t Angstrom.t
val pstr_item : Structure.structure_item Angstrom.t
val pstructure : program Angstrom.t
