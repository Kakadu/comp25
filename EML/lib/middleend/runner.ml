(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Frontend.Ast
open Inferencer
open Resolve_builtins
open Cc
open Ll
open Anf

type error =
  | Infer of Inferencer.error
  | Closure of Cc.error
  | Lifting of Ll.error
  | Anf of string

let pp_error ppf = function
  | Infer e -> fprintf ppf "inference: %a" Inferencer.pp_error e
  | Closure e -> fprintf ppf "closure conversion: %a" Cc.pp_error e
  | Lifting e -> fprintf ppf "lambda lifting: %a" Ll.pp_error e
  | Anf s -> fprintf ppf "ANF: %s" s
;;

let run (program : program) (env : Inferencer.TypeEnv.t)
  : (anf_program * Inferencer.TypeEnv.t, error) Result.t
  =
  let ( >>= ) = Result.bind in
  let env' =
    match Inferencer.ResultMonad.run (infer_structure env program) with
    | Error (Inferencer.OccursCheck _) -> Ok env
    | Error e -> Error (Infer e)
    | Ok (_subst, env'') -> Ok env''
  in
  env'
  >>= fun env'' ->
  let initial_scope = Inferencer.TypeEnv.keys env in
  let program' = resolve_program program initial_scope in
  closure_conversion_result program'
  |> Result.map_error (fun e -> Closure e)
  >>= fun cc_ast ->
  lambda_lifting_result cc_ast
  |> Result.map_error (fun e -> Lifting e)
  >>= fun ll_ast ->
  anf_program ll_ast
  |> Result.map_error (fun e -> Anf e)
  >>= fun anf_ast -> Ok (anf_ast, env'')
;;
