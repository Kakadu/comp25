(** Copyright 2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Sofya Kozyreva <k81sofia@gmail.com>, Vyacheslav Kochergin <vyacheslav.kochergin1@gmail.com> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open Ast

type immexpr =
  | ImmNum of int
  | ImmId of ident

type cbinop =
  | CPlus
  | CMinus
  | CMul
  | CDiv
  | CEq
  | CNeq
  | CLt
  | CLte
  | CGt
  | CGte

type cexpr =
  | CBinop of cbinop * immexpr * immexpr
  | CIte of cexpr * aexpr * aexpr option
  | CImmexpr of immexpr
  | CLam of ident * aexpr
  | CApp of immexpr * immexpr list

and aexpr =
  | ALet of ident * cexpr * aexpr
  | ACExpr of cexpr

type aconstruction =
  | AExpr of aexpr
  | AStatement of is_recursive * (ident * aexpr) list

type aconstructions = aconstruction list

val count : int ref
val gen_temp : string -> ident
val anf_constructions : construction list -> aconstructions
