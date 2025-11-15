(** Copyright 2024,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Common.Ast.Expression
open Common.Ast.Pattern
open Common.Ast.Structure
module SSet = Set.Make (String)
module SMap = Map.Make (String)

(* ---------- error monad ---------- *)
type cc_error = Empty_toplevel_let

let string_of_cc_error = function
  | Empty_toplevel_let -> "Cannot have empty let-binding at top level"
;;

let ( let* ) = Result.bind

let std_lib_names =
  [ "print_int"
  ; "alloc_block"
  ; "alloc_closure"
  ; "apply1"
  ; "+"
  ; "-"
  ; "*"
  ; "/"
  ; "="
  ; "<>"
  ; "<"
  ; ">"
  ; "<="
  ; ">="
  ; "&&"
  ; "||"
  ]
;;

let rec pattern_vars_list acc = function
  | Pat_var v -> v :: acc
  | Pat_construct (_, Some p) -> pattern_vars_list acc p
  | Pat_tuple (p1, p2, ps) -> List.fold_left pattern_vars_list acc (p1 :: p2 :: ps)
  | Pat_constraint (p, _) -> pattern_vars_list acc p
  | Pat_any | Pat_constant _ | Pat_construct (_, None) -> acc
;;

let pattern_vars p = pattern_vars_list [] p

let construct_fun patterns body =
  match patterns with
  | [] -> body
  | hd :: tl -> Exp_fun ((hd, tl), body)
;;

(* Recursively computes the set of free variables in expression. *)
let rec free_vars_in bound_vars = function
  | Exp_ident id when SSet.mem id bound_vars -> SSet.empty
  | Exp_ident id -> SSet.singleton id
  | Exp_constant _ | Exp_construct (_, None) -> SSet.empty
  | Exp_tuple (e1, e2, es) ->
    List.fold_left
      (fun acc e -> SSet.union acc (free_vars_in bound_vars e))
      SSet.empty
      (e1 :: e2 :: es)
  | Exp_apply (e1, e2) ->
    SSet.union (free_vars_in bound_vars e1) (free_vars_in bound_vars e2)
  | Exp_construct (_, Some e) -> free_vars_in bound_vars e
  | Exp_constraint (e, _) -> free_vars_in bound_vars e
  | Exp_fun ((p, ps), body) ->
    let fun_bound_vars =
      List.fold_left
        (fun acc p -> SSet.union acc (SSet.of_list (pattern_vars p)))
        SSet.empty
        (p :: ps)
    in
    free_vars_in (SSet.union bound_vars fun_bound_vars) body
  | Exp_if (e1, e2, e3_opt) ->
    let fv1 = free_vars_in bound_vars e1 in
    let fv2 = free_vars_in bound_vars e2 in
    let fv3 =
      match e3_opt with
      | Some e3 -> free_vars_in bound_vars e3
      | None -> SSet.empty
    in
    SSet.union fv1 (SSet.union fv2 fv3)
  | Exp_match (e, (case, cases)) ->
    let fv_e = free_vars_in bound_vars e in
    let all_cases = case :: cases in
    let fv_cases =
      List.fold_left
        (fun acc { first; second } ->
           let case_bound_vars = SSet.of_list (pattern_vars first) in
           let fv_second = free_vars_in (SSet.union bound_vars case_bound_vars) second in
           SSet.union acc fv_second)
        SSet.empty
        all_cases
    in
    SSet.union fv_e fv_cases
  | Exp_let (rec_flag, (vb, vbs), body) ->
    let bindings = vb :: vbs in
    let bound_in_let =
      List.fold_left
        (fun acc b -> SSet.union acc (SSet.of_list (pattern_vars b.pat)))
        SSet.empty
        bindings
    in
    let bound_for_rhss =
      if rec_flag = Recursive then SSet.union bound_vars bound_in_let else bound_vars
    in
    let fv_rhss =
      List.fold_left
        (fun acc b -> SSet.union acc (free_vars_in bound_for_rhss b.expr))
        SSet.empty
        bindings
    in
    let fv_body = free_vars_in (SSet.union bound_vars bound_in_let) body in
    SSet.union fv_rhss fv_body
  | Exp_function (case, cases) ->
    let all_cases = case :: cases in
    List.fold_left
      (fun acc { first; second } ->
         let case_bound_vars = SSet.of_list (pattern_vars first) in
         let fv_second = free_vars_in (SSet.union bound_vars case_bound_vars) second in
         SSet.union acc fv_second)
      SSet.empty
      all_cases
;;

(* main function *)
let rec closure_expr toplvl_set env expr =
  match expr with
  (* if id is already converted functions - apply it to free vars *)
  | Exp_ident id ->
    (match SMap.find_opt id env with
     | Some free_vars when not (SSet.is_empty free_vars) ->
       SSet.fold (fun fv acc -> Exp_apply (acc, Exp_ident fv)) free_vars (Exp_ident id)
     | _ -> expr)
  (* conversion `fun p1 ... -> body` *)
  | Exp_fun ((p, ps), body) ->
    let patterns = p :: ps in
    let fun_bound_vars =
      List.fold_left
        (fun acc p -> SSet.union acc (SSet.of_list (pattern_vars p)))
        SSet.empty
        patterns
    in
    let free_vars = free_vars_in fun_bound_vars body in
    let captured_vars = SSet.diff free_vars toplvl_set in
    let captured_vars_list = SSet.elements captured_vars in
    let new_pats_for_capture = List.map (fun v -> Pat_var v) captured_vars_list in
    let saturated_patterns = new_pats_for_capture @ patterns in
    let new_body = closure_expr toplvl_set env body in
    let new_fun = construct_fun saturated_patterns new_body in
    List.fold_left
      (fun acc_expr fv -> Exp_apply (acc_expr, Exp_ident fv))
      new_fun
      captured_vars_list
  | Exp_let (rec_flag, (vb, vbs), body) ->
    let bindings = vb :: vbs in
    let new_bindings, new_env = transform_bindings toplvl_set env rec_flag bindings in
    let new_body = closure_expr toplvl_set new_env body in
    (match new_bindings with
     | [] -> new_body
     | hd :: tl -> Exp_let (rec_flag, (hd, tl), new_body))
  | Exp_function (case, cases) ->
    let fresh_arg_name = "__fun_arg" in
    let desugared_expr =
      Exp_fun
        ((Pat_var fresh_arg_name, []), Exp_match (Exp_ident fresh_arg_name, (case, cases)))
    in
    closure_expr toplvl_set env desugared_expr
  | Exp_apply (e1, e2) ->
    Exp_apply (closure_expr toplvl_set env e1, closure_expr toplvl_set env e2)
  | Exp_tuple (e1, e2, es) ->
    let f = closure_expr toplvl_set env in
    Exp_tuple (f e1, f e2, List.map f es)
  | Exp_if (e1, e2, e3_opt) ->
    let f = closure_expr toplvl_set env in
    Exp_if (f e1, f e2, Option.map f e3_opt)
  | Exp_match (e, (case, cases)) ->
    let e' = closure_expr toplvl_set env e in
    let transform_case { first; second } =
      { first; second = closure_expr toplvl_set env second }
    in
    let case' = transform_case case in
    let cases' = List.map transform_case cases in
    Exp_match (e', (case', cases'))
  | Exp_constant _ -> expr
  | Exp_construct (id, Some e) -> Exp_construct (id, Some (closure_expr toplvl_set env e))
  | Exp_construct (_, None) -> expr
  | Exp_constraint (e, t) -> Exp_constraint (closure_expr toplvl_set env e, t)

and transform_bindings toplvl_set env rec_flag bindings =
  let transform_one binding env =
    let { pat; expr } = binding in
    match pat, expr with
    | Pat_var v, Exp_fun ((p, ps), body) ->
      let patterns = p :: ps in
      let bound_in_fun =
        List.fold_left
          (fun acc p -> SSet.union acc (SSet.of_list (pattern_vars p)))
          SSet.empty
          patterns
      in
      let bound_for_body =
        if rec_flag = Recursive then SSet.add v bound_in_fun else bound_in_fun
      in
      let free_vars = free_vars_in bound_for_body body in
      let captured_vars = SSet.diff free_vars toplvl_set in
      let captured_vars_list = SSet.elements captured_vars in
      let new_pats_for_capture = List.map (fun v -> Pat_var v) captured_vars_list in
      let saturated_patterns = new_pats_for_capture @ patterns in
      let env_for_body = SMap.add v captured_vars env in
      let new_body = closure_expr toplvl_set env_for_body body in
      let new_fun = construct_fun saturated_patterns new_body in
      let final_expr =
        List.fold_left
          (fun acc fv -> Exp_apply (acc, Exp_ident fv))
          new_fun
          captured_vars_list
      in
      let new_binding = { pat; expr = final_expr } in
      let final_env = SMap.add v captured_vars env in
      new_binding, final_env
    | _ ->
      let new_expr = closure_expr toplvl_set env expr in
      let new_binding = { pat; expr = new_expr } in
      let bound_vars = pattern_vars pat in
      let final_env =
        List.fold_left (fun acc v -> SMap.add v SSet.empty acc) env bound_vars
      in
      new_binding, final_env
  in
  if rec_flag = Nonrecursive
  then (
    let transformed, final_env =
      List.fold_left
        (fun (bindings_acc, current_env) b ->
           let new_b, next_env = transform_one b current_env in
           new_b :: bindings_acc, next_env)
        ([], env)
        bindings
    in
    List.rev transformed, final_env)
  else (
    let p_vars = List.concat_map (fun b -> pattern_vars b.pat) bindings in
    let env_rec = List.fold_left (fun e v -> SMap.add v SSet.empty e) env p_vars in
    let env_with_fvs, _ =
      List.fold_left
        (fun (env_acc, _) b ->
           let _, next_env = transform_one b env_acc in
           next_env, [])
        (env_rec, [])
        bindings
    in
    let transformed, final_env =
      List.fold_left
        (fun (bindings_acc, _) b ->
           let new_b, _ = transform_one b env_with_fvs in
           new_b :: bindings_acc, env_with_fvs)
        ([], env_with_fvs)
        bindings
    in
    List.rev transformed, final_env)
;;

let closure_structure_item_result toplvl_set = function
  | Str_eval e ->
    let e' = closure_expr toplvl_set SMap.empty e in
    Ok (Str_eval e', toplvl_set)
  | Str_value (rec_flag, (vb, vbs)) ->
    let bindings = vb :: vbs in
    let new_bindings, _ = transform_bindings toplvl_set SMap.empty rec_flag bindings in
    let new_bound_names = List.concat_map (fun b -> pattern_vars b.pat) new_bindings in
    let new_toplvl_set = SSet.union toplvl_set (SSet.of_list new_bound_names) in
    (match new_bindings with
     | [] -> Error Empty_toplevel_let
     | hd :: tl -> Ok (Str_value (rec_flag, (hd, tl)), new_toplvl_set))
  | Str_adt _ as item -> Ok (item, toplvl_set)
;;

let closure_structure_item toplvl_set item =
  match closure_structure_item_result toplvl_set item with
  | Ok x -> x
  | Error _e ->
    (match item with
     | Str_value (rec_flag, (vb, vbs)) ->
       let bindings = vb :: vbs in
       let bound_names = List.concat_map (fun b -> pattern_vars b.pat) bindings in
       let new_toplvl_set = SSet.union toplvl_set (SSet.of_list bound_names) in
       Str_value (rec_flag, (vb, vbs)), new_toplvl_set
     | _ -> item, toplvl_set)
;;

let cc_program_result (ast : program) : (program, cc_error) result =
  let toplvl0 = SSet.of_list std_lib_names in
  let rec go acc current_top = function
    | [] -> Ok (List.rev acc)
    | item :: rest ->
      let* new_item, next_top = closure_structure_item_result current_top item in
      go (new_item :: acc) next_top rest
  in
  go [] toplvl0 ast
;;

let cc_program (ast : program) : program =
  match cc_program_result ast with
  | Ok p -> p
  | Error _e -> ast
;;
