[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Ast.Expression
open Base
open Set.Poly

let rec free_vars_pat = function
  | Pat_any -> empty
  | Pat_var x -> singleton x
  | Pat_constant _ -> empty
  | Pat_tuple (pat1, pat2, pat_list) ->
    union_list (List.map (pat1 :: pat2 :: pat_list) ~f:free_vars_pat)
  | Pat_construct (_, None) -> empty
  | Pat_construct (_, Some pat) -> free_vars_pat pat
  | Pat_constraint (pat, _) -> free_vars_pat pat
;;

let rec free_vars_exp = function
  | Exp_ident id -> singleton id
  | Exp_constant _ -> empty
  | Exp_let (flag, vb, vb_list, body) ->
    let bound =
      List.fold_left
        ~f:union
        ~init:(free_vars_pat vb.pat)
        (List.map vb_list ~f:(fun vb -> free_vars_pat vb.pat))
    in
    let free_defs =
      let defs = (vb.pat, vb.exp) :: List.map vb_list ~f:(fun vb -> vb.pat, vb.exp) in
      match flag with
      | Recursive ->
        union_list (List.map defs ~f:(fun (_, exp) -> diff (free_vars_exp exp) bound))
      | Nonrecursive -> union_list (List.map defs ~f:(fun (_, exp) -> free_vars_exp exp))
    in
    let free_body = diff (free_vars_exp body) bound in
    union free_defs free_body
  | Exp_fun (pat, pat_list, exp) ->
    let bound_vars = union_list (List.map (pat :: pat_list) ~f:free_vars_pat) in
    diff (free_vars_exp exp) bound_vars
  | Exp_apply (exp1, exp2) -> union (free_vars_exp exp1) (free_vars_exp exp2)
  | Exp_function (case, case_list) ->
    let fvs_case { left; right } = union (free_vars_pat left) (free_vars_exp right) in
    let all_free =
      List.fold_left ~f:union ~init:(fvs_case case) (List.map case_list ~f:fvs_case)
    in
    let bound =
      List.fold_left
        ~f:union
        ~init:(free_vars_pat case.left)
        (List.map case_list ~f:(fun c -> free_vars_pat c.left))
    in
    diff all_free bound
  | Exp_match (exp, case, case_list) ->
    let exp_free_vars = free_vars_exp exp in
    let fvs_case { left; right } = union (free_vars_pat left) (free_vars_exp right) in
    let all_free =
      List.fold_left ~f:union ~init:(fvs_case case) (List.map case_list ~f:fvs_case)
    in
    let bound =
      List.fold_left
        ~f:union
        ~init:(free_vars_pat case.left)
        (List.map case_list ~f:(fun c -> free_vars_pat c.left))
    in
    union exp_free_vars (diff all_free bound)
  | Exp_ifthenelse (cond, then_exp, else_exp) ->
    let else_exp = Option.value_map else_exp ~f:free_vars_exp ~default:empty in
    union (free_vars_exp cond) (union (free_vars_exp then_exp) else_exp)
  | Exp_tuple (exp1, exp2, exp_list) ->
    union_list (List.map (exp1 :: exp2 :: exp_list) ~f:free_vars_exp)
  | Exp_construct (_, None) -> empty
  | Exp_construct (_, Some exp) -> free_vars_exp exp
  | Exp_sequence (exp1, exp2) -> union (free_vars_exp exp1) (free_vars_exp exp2)
  | Exp_constraint (exp, _) -> free_vars_exp exp
;;

let rec process_bindings globals flag vb vb_list =
  let bound =
    List.fold_left
      ~f:union
      ~init:(free_vars_pat vb.pat)
      (List.map vb_list ~f:(fun vb -> free_vars_pat vb.pat))
  in
  let globals_for_defs =
    match flag with
    | Recursive -> union globals bound
    | Nonrecursive -> globals
  in
  let vb' = { vb with exp = cc_exp globals_for_defs vb.exp } in
  let vb_list' =
    List.map vb_list ~f:(fun vb -> { vb with exp = cc_exp globals_for_defs vb.exp })
  in
  vb', vb_list', globals

and cc_exp globals = function
  | (Exp_ident _ | Exp_constant _) as e -> e
  | Exp_let (flag, vb, vb_list, body) ->
    let vb', vb_list', new_globals = process_bindings globals flag vb vb_list in
    Exp_let (flag, vb', vb_list', cc_exp new_globals body)
  | Exp_fun (pat, pat_list, body) as lam ->
    let all_params = pat :: pat_list in
    let fvs = free_vars_exp lam in
    let fvs = diff fvs globals in
    let fvs =
      diff
        fvs
        (List.fold_left ~f:union ~init:empty (List.map all_params ~f:free_vars_pat))
    in
    let body' = cc_exp globals body in
    if is_empty fvs
    then Exp_fun (pat, pat_list, body')
    else (
      let safe_tl = function
        | [] -> []
        | _ :: tail -> tail
      in
      let env_vars = to_list fvs in
      let env_pats = List.map env_vars ~f:(fun x -> Pat_var x) in
      let new_params = env_pats @ all_params in
      let new_fun = Exp_fun (List.hd_exn new_params, safe_tl new_params, body') in
      List.fold_left ~f:(fun acc x -> Exp_apply (acc, Exp_ident x)) ~init:new_fun env_vars)
  | Exp_apply (f, arg) -> Exp_apply (cc_exp globals f, cc_exp globals arg)
  | Exp_function (case, case_list) ->
    let case' = { case with right = cc_exp globals case.right } in
    let case_list' =
      List.map case_list ~f:(fun c -> { c with right = cc_exp globals c.right })
    in
    Exp_function (case', case_list')
  | Exp_match (exp, case, case_list) ->
    let exp' = cc_exp globals exp in
    let case' = { case with right = cc_exp globals case.right } in
    let case_list' =
      List.map case_list ~f:(fun c -> { c with right = cc_exp globals c.right })
    in
    Exp_match (exp', case', case_list')
  | Exp_ifthenelse (cond, then_exp, else_exp) ->
    let else_exp' = Option.map else_exp ~f:(cc_exp globals) in
    Exp_ifthenelse (cc_exp globals cond, cc_exp globals then_exp, else_exp')
  | Exp_tuple (exp1, exp2, exp_list) ->
    Exp_tuple
      (cc_exp globals exp1, cc_exp globals exp2, List.map exp_list ~f:(cc_exp globals))
  | Exp_construct (_, None) as exp -> exp
  | Exp_construct (tag, Some payload) -> Exp_construct (tag, Some (cc_exp globals payload))
  | Exp_sequence (exp1, exp2) -> Exp_sequence (cc_exp globals exp1, cc_exp globals exp2)
  | Exp_constraint (exp, typ) -> Exp_constraint (cc_exp globals exp, typ)
;;

let cc_structure_item globals = function
  | Struct_eval exp ->
    let exp' = cc_exp globals exp in
    globals, Struct_eval exp'
  | Struct_value (flag, vb, vb_list) ->
    let vb', vb_list', _ = process_bindings globals flag vb vb_list in
    let bound =
      List.fold_left
        ~f:union
        ~init:(free_vars_pat vb.pat)
        (List.map vb_list ~f:(fun vb -> free_vars_pat vb.pat))
    in
    let new_globals = union globals bound in
    new_globals, Struct_value (flag, vb', vb_list')
;;

let stdlib_globals = of_list ([ "print_int" ] @ un_op_list @ bin_op_list)

let closure_conversion (ast : structure) =
  let initial_globals = stdlib_globals in
  let rec helper globals acc = function
    | [] -> List.rev acc
    | item :: rest ->
      let globals, item' = cc_structure_item globals item in
      helper globals (item' :: acc) rest
  in
  helper initial_globals [] ast
;;
