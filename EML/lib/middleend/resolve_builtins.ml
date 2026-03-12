(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Frontend.Ast

let names_of_pattern p =
  let rec go = function
    | PatVariable x -> [ x ]
    | PatAny | PatConst _ | PatUnit -> []
    | PatType (q, _) -> go q
    | PatTuple (a, b, rest) -> go a @ go b @ List.concat_map rest ~f:go
    | PatList ps -> List.concat_map ps ~f:go
    | PatOption None -> []
    | PatOption (Some q) -> go q
    | PatConstruct (_, None) -> []
    | PatConstruct (_, Some q) -> go q
  in
  go p
;;

let names_of_bind (pat, _) = names_of_pattern pat

let rec resolve_expr scope = function
  | ExpBinOper (Custom op, e1, e2) ->
    let e1' = resolve_expr scope e1 in
    let e2' = resolve_expr scope e2 in
    (match List.mem scope op ~equal:String.equal, builtin_op_of_string op with
     | true, _ -> ExpBinOper (Custom op, e1', e2')
     | false, Some b -> ExpBinOper (b, e1', e2')
     | false, None -> ExpBinOper (Custom op, e1', e2'))
  | ExpIdent x -> ExpIdent x
  | ExpConst c -> ExpConst c
  | ExpBranch (c, t, o) ->
    ExpBranch
      (resolve_expr scope c, resolve_expr scope t, Option.map o ~f:(resolve_expr scope))
  | ExpUnarOper (u, e') -> ExpUnarOper (u, resolve_expr scope e')
  | ExpTuple (a, b, rest) ->
    ExpTuple
      (resolve_expr scope a, resolve_expr scope b, List.map rest ~f:(resolve_expr scope))
  | ExpList es -> ExpList (List.map es ~f:(resolve_expr scope))
  | ExpLambda (pat, pats, body) ->
    let scope' =
      scope @ names_of_pattern pat @ List.concat_map pats ~f:names_of_pattern
    in
    ExpLambda (pat, pats, resolve_expr scope' body)
  | ExpTypeAnnotation (e', ty) -> ExpTypeAnnotation (resolve_expr scope e', ty)
  | ExpLet (rec_flag, (pat, e1), binds, body) ->
    let names =
      names_of_pattern pat @ List.concat_map binds ~f:(fun (p, _) -> names_of_pattern p)
    in
    let scope' = scope @ names in
    let scope_rhs =
      match rec_flag with
      | Rec -> scope'
      | NonRec -> scope
    in
    ExpLet
      ( rec_flag
      , (pat, resolve_expr scope_rhs e1)
      , List.map binds ~f:(fun (p, e') -> p, resolve_expr scope_rhs e')
      , resolve_expr scope' body )
  | ExpApply (f, a) -> ExpApply (resolve_expr scope f, resolve_expr scope a)
  | ExpOption None -> ExpOption None
  | ExpOption (Some e') -> ExpOption (Some (resolve_expr scope e'))
  | ExpFunction (c, cases) ->
    let names = names_of_bind c @ List.concat_map cases ~f:names_of_bind in
    let scope' = scope @ names in
    ExpFunction
      ( (fst c, resolve_expr scope' (snd c))
      , List.map cases ~f:(fun (p, e') -> p, resolve_expr scope' e') )
  | ExpMatch (scrut, c, cases) ->
    let names = names_of_bind c @ List.concat_map cases ~f:names_of_bind in
    let scope' = scope @ names in
    ExpMatch
      ( resolve_expr scope scrut
      , (fst c, resolve_expr scope' (snd c))
      , List.map cases ~f:(fun (p, e') -> p, resolve_expr scope' e') )
  | ExpConstruct (c, o) -> ExpConstruct (c, Option.map o ~f:(resolve_expr scope))
  | ExpBinOper (b, e1, e2) ->
    let left_resolved = resolve_expr scope e1 in
    let right_resolved = resolve_expr scope e2 in
    let builtin_op_name = builtin_op_to_string b in
    if List.mem scope builtin_op_name ~equal:String.equal
    then ExpBinOper (Custom builtin_op_name, left_resolved, right_resolved)
    else ExpBinOper (b, left_resolved, right_resolved)
;;

let resolve_structure scope = function
  | SEval e -> SEval (resolve_expr scope e), scope
  | SValue (rec_flag, (pat, e1), binds) ->
    let names =
      names_of_pattern pat @ List.concat_map binds ~f:(fun (p, _) -> names_of_pattern p)
    in
    let scope' = scope @ names in
    let scope_rhs =
      match rec_flag with
      | Rec -> scope'
      | NonRec -> scope
    in
    let e1' = resolve_expr scope_rhs e1 in
    let binds' = List.map binds ~f:(fun (p, e') -> p, resolve_expr scope_rhs e') in
    SValue (rec_flag, (pat, e1'), binds'), scope'
;;

let resolve_program (program : program) (initial_scope : string list) : program =
  let _, rev_resolved =
    List.fold_left program ~init:(initial_scope, []) ~f:(fun (scope, acc) s ->
      let s', scope' = resolve_structure scope s in
      scope', s' :: acc)
  in
  List.rev rev_resolved
;;
