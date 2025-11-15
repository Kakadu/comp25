[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Ast.Expression
open Base
open Set.Poly
module M = Map.Poly

let rec get_vars_pat = function
  | Pat_any -> empty
  | Pat_var x -> singleton x
  | Pat_constant _ -> empty
  | Pat_tuple (pat1, pat2, pat_list) ->
    union_list (List.map (pat1 :: pat2 :: pat_list) ~f:get_vars_pat)
  | Pat_construct (_, None) -> empty
  | Pat_construct (_, Some pat) -> get_vars_pat pat
  | Pat_constraint (pat, _) -> get_vars_pat pat
;;

let rec free_vars_exp = function
  | Exp_ident id -> singleton id
  | Exp_constant _ -> empty
  | Exp_let (flag, vb, vb_list, body) ->
    let bound =
      List.fold_left
        ~f:union
        ~init:(get_vars_pat vb.pat)
        (List.map vb_list ~f:(fun vb -> get_vars_pat vb.pat))
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
    let bound_vars = union_list (List.map (pat :: pat_list) ~f:get_vars_pat) in
    diff (free_vars_exp exp) bound_vars
  | Exp_apply (exp1, exp2) -> union (free_vars_exp exp1) (free_vars_exp exp2)
  | Exp_function (case, case_list) ->
    let fvs_case { left; right } = diff (free_vars_exp right) (get_vars_pat left) in
    let all_free =
      List.fold_left ~f:union ~init:(fvs_case case) (List.map case_list ~f:fvs_case)
    in
    all_free
  | Exp_match (exp, case, case_list) ->
    let exp_free_vars = free_vars_exp exp in
    let fvs_case { left; right } = diff (free_vars_exp right) (get_vars_pat left) in
    let all_free =
      List.fold_left ~f:union ~init:(fvs_case case) (List.map case_list ~f:fvs_case)
    in
    union exp_free_vars all_free
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

let safe_tl = function
  | [] -> []
  | _ :: tail -> tail
;;

let rec close_lambda ?(apply = false) globals env params body fvs =
  let body' = cc_exp globals env body in
  if Set.is_empty fvs
  then Exp_fun (List.hd_exn params, safe_tl params, body')
  else (
    let env_vars = Set.to_list fvs in
    let env_pats = List.map env_vars ~f:(fun x -> Pat_var x) in
    let all_pats = env_pats @ params in
    let base_fun = Exp_fun (List.hd_exn all_pats, safe_tl all_pats, body') in
    if apply
    then
      List.fold_left env_vars ~init:base_fun ~f:(fun acc v ->
        Exp_apply (acc, Exp_ident v))
    else base_fun)

and cc_exp globals env = function
  | Exp_ident id ->
    (match Map.find env id with
     | None -> Exp_ident id
     | Some fvs ->
       List.fold_left (Set.to_list fvs) ~init:(Exp_ident id) ~f:(fun acc fv ->
         Exp_apply (acc, Exp_ident fv)))
  | Exp_constant c -> Exp_constant c
  | Exp_let (flag, vb, vb_list, body) ->
    let vb', vb_list', globals, env = process_bindings globals env flag vb vb_list in
    Exp_let (flag, vb', vb_list', cc_exp globals env body)
  | Exp_fun (pat, pat_list, body) as lam ->
    let all_params = pat :: pat_list in
    let fvs = diff (free_vars_exp lam) globals in
    close_lambda globals env all_params body fvs ~apply:true
  | Exp_apply (f, arg) -> Exp_apply (cc_exp globals env f, cc_exp globals env arg)
  | Exp_function (case, case_list) ->
    let case' = { case with right = cc_exp globals env case.right } in
    let case_list' =
      List.map case_list ~f:(fun c -> { c with right = cc_exp globals env c.right })
    in
    Exp_function (case', case_list')
  | Exp_match (exp, case, case_list) ->
    let exp' = cc_exp globals env exp in
    let case' = { case with right = cc_exp globals env case.right } in
    let case_list' =
      List.map case_list ~f:(fun c -> { c with right = cc_exp globals env c.right })
    in
    Exp_match (exp', case', case_list')
  | Exp_ifthenelse (cond, then_exp, else_exp) ->
    let else_exp' = Option.map else_exp ~f:(cc_exp globals env) in
    Exp_ifthenelse (cc_exp globals env cond, cc_exp globals env then_exp, else_exp')
  | Exp_tuple (exp1, exp2, exp_list) ->
    Exp_tuple
      ( cc_exp globals env exp1
      , cc_exp globals env exp2
      , List.map exp_list ~f:(cc_exp globals env) )
  | Exp_construct (_, None) as exp -> exp
  | Exp_construct (tag, Some exp) -> Exp_construct (tag, Some (cc_exp globals env exp))
  | Exp_sequence (exp1, exp2) ->
    Exp_sequence (cc_exp globals env exp1, cc_exp globals env exp2)
  | Exp_constraint (exp, typ) -> Exp_constraint (cc_exp globals env exp, typ)

and process_bindings globals env flag vb vb_list =
  let rec add_pat_to_env env pat fvs =
    match pat with
    | Pat_any | Pat_constant _ | Pat_construct (_, None) -> env
    | Pat_var name -> Map.set env ~key:name ~data:fvs
    | Pat_tuple (pat1, pat2, pat_list) ->
      let env = add_pat_to_env env pat1 fvs in
      let env = add_pat_to_env env pat2 fvs in
      List.fold_left pat_list ~init:env ~f:(fun acc p -> add_pat_to_env acc p fvs)
    | Pat_construct (_, Some pat) | Pat_constraint (pat, _) -> add_pat_to_env env pat fvs
  in
  let bound =
    List.fold_left
      ~f:union
      ~init:(get_vars_pat vb.pat)
      (List.map vb_list ~f:(fun vb -> get_vars_pat vb.pat))
  in
  match flag with
  | Recursive ->
    let globals = union globals bound in
    let all_fvs =
      vb :: vb_list
      |> List.map ~f:(fun vb -> diff (free_vars_exp vb.exp) globals)
      |> union_list
    in
    let env =
      List.fold_left (vb :: vb_list) ~init:env ~f:(fun acc vb ->
        add_pat_to_env acc vb.pat all_fvs)
    in
    let vb_list' =
      List.map (vb :: vb_list) ~f:(fun vb ->
        let fvs = diff all_fvs (get_vars_pat vb.pat) in
        let new_fun' =
          match vb.exp with
          | Exp_fun (pat, pat_list, body) ->
            let new_fun = close_lambda globals env (pat :: pat_list) body fvs in
            new_fun
          | _ -> cc_exp globals env vb.exp
        in
        { vb with exp = new_fun' })
    in
    List.hd_exn vb_list', safe_tl vb_list', globals, env
  | Nonrecursive ->
    let env, vb_list_rev =
      List.fold_left (vb :: vb_list) ~init:(env, []) ~f:(fun acc vb ->
        let env, funs = acc in
        let fvs = diff (free_vars_exp vb.exp) globals in
        let env, new_fun' =
          match vb.exp with
          | Exp_fun (pat, pat_list, body) ->
            let new_fun = close_lambda globals env (pat :: pat_list) body fvs in
            add_pat_to_env env vb.pat fvs, new_fun
          | _ -> env, cc_exp globals env vb.exp
        in
        env, { vb with exp = new_fun' } :: funs)
    in
    let vb_list' = List.rev vb_list_rev in
    List.hd_exn vb_list', safe_tl vb_list', globals, env
;;

let cc_structure_item globals env = function
  | Struct_eval exp ->
    let exp' = cc_exp globals env exp in
    globals, Struct_eval exp'
  | Struct_value (flag, vb, vb_list) ->
    let vb', vb_list', _, _ = process_bindings globals env flag vb vb_list in
    let bound =
      List.fold_left
        ~f:union
        ~init:(get_vars_pat vb.pat)
        (List.map vb_list ~f:(fun vb -> get_vars_pat vb.pat))
    in
    let new_globals = union globals bound in
    new_globals, Struct_value (flag, vb', vb_list')
;;

let stdlib_globals =
  of_list
    ([ "print_int"; "collect"; "get_heap_start"; "get_heap_final"; "print_gc_status" ]
     @ un_op_list
     @ bin_op_list)
;;

let closure_conversion (ast : structure) =
  let initial_globals = stdlib_globals in
  let env = M.empty in
  let rec helper globals acc = function
    | [] -> List.rev acc
    | item :: rest ->
      let globals, item' = cc_structure_item globals env item in
      helper globals (item' :: acc) rest
  in
  helper initial_globals [] ast
;;
