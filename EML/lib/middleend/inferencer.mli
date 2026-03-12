(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Frontend.Ast

type error =
  | OccursCheck of string * ty
  | NoVariable of string
  | UnificationFailed of ty * ty
  | SeveralBounds of string
  | LHS of string
  | RHS of string
  | UnexpectedFunction of ty

val pp_error : Format.formatter -> error -> unit

module ResultMonad : sig
  type 'a t

  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  val fresh : int t
  val current_level : int t
  val enter_level : unit t
  val leave_level : unit t
  val set_var_level : string -> int -> unit t
  val get_var_level : string -> int option t
  val run : 'a t -> ('a, error) Result.t
end

module Substitution : sig
  type t

  val empty : t
end

module VarSet : Stdlib.Set.S with type elt = string

module Scheme : sig
  type t = Scheme of VarSet.t * ty
end

module TypeEnv : sig
  type t = (ident, Scheme.t, Base.String.comparator_witness) Base.Map.t

  val extend : t -> ident -> Scheme.t -> t
  val free_vars : t -> VarSet.t
  val apply : Substitution.t -> t -> t
  val find : t -> ident -> Scheme.t option
  val keys : t -> ident list
  val initial_env : t
  val env_with_gc : t
end

val infer_structure : TypeEnv.t -> program -> (Substitution.t * TypeEnv.t) ResultMonad.t
val infer_simple_expression : expr -> (ty, error) Result.t
val run_infer : program -> (TypeEnv.t, error) Result.t
