[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Ast.Expression
open Base

module State = struct
  type 'a t = int -> 'a * int

  let return x st = x, st

  let bind m f st =
    let x, st' = m st in
    f x st'
  ;;

  let ( let* ) = bind
  let fresh st = st, st + 1

  let rec state_map f = function
    | [] -> return []
    | x :: xs ->
      let* y = f x in
      let* ys = state_map f xs in
      return (y :: ys)
  ;;

  let run m = fst (m 0)
end

open State

let empty_bindings = Map.empty (module String)

let gen_name =
  let* n = fresh in
  return ("ll_" ^ Int.to_string n)
;;

let rec pat_bound_names = function
  | Pat_any -> []
  | Pat_var s -> [ s ]
  | Pat_constant _ -> []
  | Pat_tuple (p1, p2, ps) ->
    pat_bound_names p1 @ pat_bound_names p2 @ List.concat_map ps ~f:pat_bound_names
  | Pat_construct (_id, None) -> []
  | Pat_construct (_id, Some p) -> pat_bound_names p
  | Pat_constraint (p, _t) -> pat_bound_names p
;;

let safe_tl = function
  | [] -> []
  | _ :: tl -> tl
;;

let remove_names bindings names = List.fold names ~init:bindings ~f:Map.remove

let rec ll_exp ?(top_level = false) bindings = function
  | Exp_ident name as id ->
    let id =
      match Map.find bindings name with
      | Some new_name -> Exp_ident new_name
      | None -> id
    in
    return ([], id)
  | (Exp_constant _ | Exp_construct (_, None)) as e -> return ([], e)
  | Exp_let (Nonrecursive, vb, vb_list, body) ->
    let* defs_vb, vb_exp = ll_exp bindings vb.exp in
    let* vb_list_res = state_map (fun b -> ll_exp bindings b.exp) vb_list in
    let vb_list_exps, vb_list_defs_list = List.unzip vb_list_res in
    let vb_list_defs = List.concat vb_list_exps in
    let vb' = { vb with exp = vb_exp } in
    let vb_list' =
      List.map2_exn vb_list vb_list_defs_list ~f:(fun b e -> { b with exp = e })
    in
    let* body_defs, body_exp = ll_exp bindings body in
    return
      (defs_vb @ vb_list_defs @ body_defs, Exp_let (Nonrecursive, vb', vb_list', body_exp))
  | Exp_let (Recursive, vb, vb_list, body) ->
    let defs = vb :: vb_list in
    let* bindings =
      List.fold defs ~init:(return bindings) ~f:(fun acc b ->
        let* acc = acc in
        let def_names = pat_bound_names b.pat in
        let* freshes = state_map (fun _ -> gen_name) def_names in
        let acc =
          List.fold2_exn def_names freshes ~init:acc ~f:(fun acc n f ->
            Map.set acc ~key:n ~data:f)
        in
        return acc)
    in
    let* defs_res =
      List.fold
        defs
        ~init:(return ([], []))
        ~f:(fun acc b ->
          let* defs_acc, bodies_acc = acc in
          let* defs_body, body' = ll_exp bindings b.exp in
          let new_pat =
            let rec rename_pat = function
              | Pat_var s ->
                (match Map.find bindings s with
                 | Some new_s -> Pat_var new_s
                 | None -> Pat_var s)
              | Pat_tuple (p1, p2, ps) ->
                Pat_tuple (rename_pat p1, rename_pat p2, List.map ps ~f:rename_pat)
              | Pat_construct (id, p_opt) ->
                Pat_construct (id, Option.map p_opt ~f:rename_pat)
              | Pat_constraint (p, t) -> Pat_constraint (rename_pat p, t)
              | p -> p
            in
            rename_pat b.pat
          in
          return (defs_acc @ defs_body, { pat = new_pat; exp = body' } :: bodies_acc))
    in
    let decl_bodies, bodies = defs_res in
    let bodies = List.rev bodies in
    let* body_defs, body_exp = ll_exp bindings body in
    return
      ( decl_bodies
        @ [ Struct_value (Recursive, List.hd_exn bodies, safe_tl bodies) ]
        @ body_defs
      , body_exp )
  | Exp_fun (pat, pat_list, body) when top_level ->
    let* defs, body' = ll_exp bindings body in
    return (defs, Exp_fun (pat, pat_list, body'))
  | Exp_fun (pat, pat_list, body) ->
    let* fresh_name = gen_name in
    let args = pat :: pat_list in
    let names = List.concat_map args ~f:pat_bound_names in
    let bindings = remove_names bindings names in
    let* defs, body' = ll_exp bindings body in
    let def =
      Struct_value
        ( Nonrecursive
        , { pat = Pat_var fresh_name; exp = Exp_fun (pat, pat_list, body') }
        , [] )
    in
    return (defs @ [ def ], Exp_ident fresh_name)
  | Exp_apply (exp1, exp2) ->
    let* defs1, exp1' = ll_exp bindings exp1 in
    let* defs2, exp2' = ll_exp bindings exp2 in
    return (defs1 @ defs2, Exp_apply (exp1', exp2'))
  | Exp_function (case, case_list) when top_level ->
    let* c_defs, c_exp = ll_exp bindings case.right in
    let* cs_res = state_map (fun c -> ll_exp bindings c.right) case_list in
    let cs_defs_list, cs_exps = List.unzip cs_res in
    let cs_defs = List.concat cs_defs_list in
    let case' = { case with right = c_exp } in
    let case_list' =
      List.map2_exn case_list cs_exps ~f:(fun c e -> { c with right = e })
    in
    return (c_defs @ cs_defs, Exp_function (case', case_list'))
  | Exp_function (case, case_list) ->
    let* fresh_name = gen_name in
    let bound1 = pat_bound_names case.left in
    let bindings_case = remove_names bindings bound1 in
    let* defs1, body1 = ll_exp bindings_case case.right in
    let* cs_res =
      state_map
        (fun c ->
           let bound = pat_bound_names c.left in
           let bindings_c = remove_names bindings bound in
           ll_exp bindings_c c.right)
        case_list
    in
    let cs_defs_list, cs_exps = List.unzip cs_res in
    let cs_defs = List.concat cs_defs_list in
    let case' = { case with right = body1 } in
    let case_list' =
      List.map2_exn case_list cs_exps ~f:(fun c e -> { c with right = e })
    in
    let def =
      Struct_value
        ( Nonrecursive
        , { pat = Pat_var fresh_name; exp = Exp_function (case', case_list') }
        , [] )
    in
    return (defs1 @ cs_defs @ [ def ], Exp_ident fresh_name)
  | Exp_match (exp, case, case_list) ->
    let* e_defs, e_exp = ll_exp bindings exp in
    let* c_defs, c_exp = ll_exp bindings case.right in
    let* cs_res = state_map (fun c -> ll_exp bindings c.right) case_list in
    let cs_defs_list, cs_exps = List.unzip cs_res in
    let cs_defs = List.concat cs_defs_list in
    let case' = { case with right = c_exp } in
    let case_list' =
      List.map2_exn case_list cs_exps ~f:(fun c e -> { c with right = e })
    in
    return (e_defs @ c_defs @ cs_defs, Exp_match (e_exp, case', case_list'))
  | Exp_ifthenelse (exp1, exp2, exp3_opt) ->
    let* defs1, exp1 = ll_exp bindings exp1 in
    let* defs2, exp2 = ll_exp bindings exp2 in
    let* defs3, exp3_opt =
      match exp3_opt with
      | None -> return ([], None)
      | Some e3 ->
        let* d3, e3 = ll_exp bindings e3 in
        return (d3, Some e3)
    in
    return (defs1 @ defs2 @ defs3, Exp_ifthenelse (exp1, exp2, exp3_opt))
  | Exp_tuple (exp1, exp2, exp_list) ->
    let* defs1, exp1' = ll_exp bindings exp1 in
    let* defs2, exp2' = ll_exp bindings exp2 in
    let* res = state_map (ll_exp bindings) exp_list in
    let es_defs, es_exprs = List.unzip res in
    let es_defs = List.concat es_defs in
    return (defs1 @ defs2 @ es_defs, Exp_tuple (exp1', exp2', es_exprs))
  | Exp_construct (id, Some exp) ->
    let* defs, exp' = ll_exp bindings exp in
    return (defs, Exp_construct (id, Some exp'))
  | Exp_sequence (exp1, exp2) ->
    let* defs1, exp1' = ll_exp bindings exp1 in
    let* defs2, exp2' = ll_exp bindings exp2 in
    return (defs1 @ defs2, Exp_sequence (exp1', exp2'))
  | Exp_constraint (exp, typ) ->
    let* defs, exp' = ll_exp bindings exp in
    return (defs, Exp_constraint (exp', typ))
;;

let ll_toplevel = function
  | Struct_eval exp ->
    let* defs, exp' = ll_exp ~top_level:true empty_bindings exp in
    return (defs @ [ Struct_eval exp' ])
  | Struct_value (flag, vb, vb_list) ->
    let defs = vb :: vb_list in
    let* defs_res =
      List.fold
        defs
        ~init:(return ([], []))
        ~f:(fun acc vb ->
          let* acc_defs, acc_bodies = acc in
          let* defs_body, body' = ll_exp ~top_level:true empty_bindings vb.exp in
          let vb_rec = { vb with exp = body' } in
          return (acc_defs @ defs_body, vb_rec :: acc_bodies))
    in
    let defs_list, bodies = defs_res in
    let bodies = List.rev bodies in
    return (defs_list @ [ Struct_value (flag, List.hd_exn bodies, safe_tl bodies) ])
;;

let ll_program program =
  let rec helper = function
    | [] -> return []
    | hd :: tl ->
      let* defs1 = ll_toplevel hd in
      let* defs2 = helper tl in
      return (defs1 @ defs2)
  in
  helper program
;;

let lambda_lifting program = run (ll_program program)
