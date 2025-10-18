(** Copyright 2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Sofya Kozyreva <k81sofia@gmail.com>, Vyacheslav Kochergin <vyacheslav.kochergin1@gmail.com> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type binder = int

type typ =
  | Primitive of string
  | Type_var of binder
  | Arrow of typ * typ
  | Type_list of typ
  | Type_tuple of typ * typ * typ list
  | TOption of typ
  | TActPat of string * typ
  | Choice of (string, typ, Base.String.comparator_witness) Base.Map.t

val choice_to_list : (string, typ, 'a) Base.Map.t -> typ list
val choice_set_many : ('a, 'b, 'c) Base.Map.t -> ('a * 'b) list -> ('a, 'b, 'c) Base.Map.t
val arrow_of_types : typ list -> typ -> typ
val gen_typ_primitive : typ QCheck.Gen.t

module VarSet : sig
  type t = Set.Make(Int).t

  val pp : Format.formatter -> t -> unit
end

type binder_set = VarSet.t
type scheme = Scheme of binder_set * typ

val int_typ : typ
val bool_typ : typ
val string_typ : typ
val unit_typ : typ
