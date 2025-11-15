[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type a_pat =
  | APat_var of string
  | APat_constant of Ast.constant

val show_a_pat : a_pat -> string

type i_exp =
  | IExp_ident of string
  | IExp_constant of Ast.constant
  | IExp_unit
  | IExp_fun of a_pat * a_exp

and c_exp =
  | CIExp of i_exp
  | CExp_tuple of i_exp * i_exp * i_exp list
  | CExp_apply of i_exp * i_exp * i_exp list
  | CExp_ifthenelse of c_exp * a_exp * a_exp option

and a_exp =
  | ACExp of c_exp
  | AExp_let of Ast.rec_flag * Ast.pattern * c_exp * a_exp

val show_i_exp : i_exp -> string
val show_c_exp : c_exp -> string
val show_a_exp : a_exp -> string

type a_structure_item =
  | AStruct_eval of a_exp
  | AStruct_value of Ast.rec_flag * Ast.pattern * a_exp

val show_a_structure_item : a_structure_item -> string

type a_structure = a_structure_item list

val show_a_structure : a_structure -> string
val anf_structure : Ast.structure -> (a_structure_item list, string) result
