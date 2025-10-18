(** Copyright 2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Sofya Kozyreva <k81sofia@gmail.com>, Vyacheslav Kochergin <vyacheslav.kochergin1@gmail.com> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type ident = Ident of string

val show_ident : ident -> string
val gen_ident_uppercase : ident QCheck.Gen.t

type literal =
  | Int_lt of int
  | Bool_lt of bool
  | String_lt of string
  | Unit_lt

val show_literal : literal -> string

type binary_operator =
  | Binary_equal
  | Binary_unequal
  | Binary_less
  | Binary_less_or_equal
  | Binary_greater
  | Binary_greater_or_equal
  | Binary_add
  | Binary_subtract
  | Binary_multiply
  | Logical_or
  | Logical_and
  | Binary_divide
  | Binary_or_bitwise
  | Binary_xor_bitwise
  | Binary_and_bitwise
  | Binary_cons

val show_binary_operator : binary_operator -> string

type unary_operator =
  | Unary_minus
  | Unary_not

val show_unary_operator : unary_operator -> string

type pattern =
  | Wild
  | PList of pattern list
  | PCons of pattern * pattern
  | PTuple of pattern * pattern * pattern list
  | PConst of literal
  | PVar of ident
  | POption of pattern option
  | PConstraint of pattern * TypedTree.typ

val show_pattern : pattern -> string

type is_recursive =
  | Nonrec
  | Rec

val show_is_recursive : is_recursive -> string

type case = pattern * expr

and expr =
  | Const of literal
  | Tuple of expr * expr * expr list
  | List of expr list
  | Variable of ident
  | Unary_expr of unary_operator * expr
  | Bin_expr of binary_operator * expr * expr
  | If_then_else of expr * expr * expr option
  | Lambda of pattern * pattern list * expr
  | Apply of expr * expr
  | Function of case * case list
  | Match of expr * case * case list
  | LetIn of is_recursive * let_bind * let_bind list * expr
  | Option of expr option
  | EConstraint of expr * TypedTree.typ

and let_bind = Let_bind of pattern * pattern list * expr

val show_case : case -> string
val show_expr : expr -> string
val show_let_bind : let_bind -> string

type statement = Let of is_recursive * let_bind * let_bind list

val show_statement : statement -> string

type construction =
  | Expr of expr
  | Statement of statement

val show_construction : construction -> string

type constructions = construction list

val show_constructions : constructions -> string
val arb_constructions : construction list QCheck.arbitrary
