(** Copyright 2024, Mikhail Gavrilenko,
    Danila Rudnev-Stepanyan, Daniel Vlasenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Common.Parser
open Anf
module SSet = Set.Make (String)
module SMap = Map.Make (String)

type supply = int
type op_ctx = (ident * ident) list

let op_add ops pair = pair :: ops

(* we rename every (re)defined operator to generate is as a function.
  on later stages, real operators will have their names unchanged *)
let get_op_name op =
  if not (is_operator_char op.[0])
  then op
  else (
    let buf = Buffer.create (String.length op * 5) in
    String.iter
      (function
        | '+' -> Buffer.add_string buf "pls"
        | '*' -> Buffer.add_string buf "str"
        | '-' -> Buffer.add_string buf "min"
        | '/' -> Buffer.add_string buf "sls"
        | '%' -> Buffer.add_string buf "per"
        | '=' -> Buffer.add_string buf "eql"
        | '<' -> Buffer.add_string buf "lss"
        | '>' -> Buffer.add_string buf "gre"
        | '&' -> Buffer.add_string buf "amp"
        | '|' -> Buffer.add_string buf "pip"
        | '!' -> Buffer.add_string buf "ban"
        | '?' -> Buffer.add_string buf "que"
        | '@' -> Buffer.add_string buf "att"
        | '^' -> Buffer.add_string buf "hat"
        | '~' -> Buffer.add_string buf "tld"
        | ':' -> Buffer.add_string buf "cln"
        | '$' -> Buffer.add_string buf "dol"
        | '.' -> Buffer.add_string buf "dot"
        | ',' -> Buffer.add_string buf "com"
        | c ->
          Buffer.add_string buf "ch";
          Buffer.add_char buf c)
      op;
    Buffer.contents buf)
;;

let fresh_name (base : ident) (n : supply) : ident * supply =
  get_op_name base ^ "__ll$" ^ string_of_int n, n + 1
;;

let fv_im = function
  | Imm_num _ -> SSet.empty
  | Imm_ident x when is_operator_char x.[0] -> SSet.empty
  | Imm_ident x -> SSet.singleton x
;;

let rec fv_comp = function
  | Comp_imm i -> fv_im i
  | Comp_binop (_op, a, b) -> SSet.union (fv_im a) (fv_im b)
  | Comp_app (f, args) ->
    List.fold_left (fun s a -> SSet.union s (fv_im a)) (fv_im f) args
  | Comp_branch (c, t, e) -> SSet.union (fv_im c) (SSet.union (fv_anf t) (fv_anf e))
  | Comp_func (ps, body) ->
    let fvb = fv_anf body in
    List.fold_left (fun s p -> SSet.remove p s) fvb ps
  | Comp_tuple is | Comp_alloc is ->
    List.fold_left (fun s a -> SSet.union s (fv_im a)) SSet.empty is
  | Comp_load (addr, _off) -> fv_im addr

and fv_anf = function
  | Anf_comp_expr ce -> fv_comp ce
  | Anf_let (_rf, x, ce, body) -> SSet.union (fv_comp ce) (SSet.remove x (fv_anf body))
;;

let occurs_im x = function
  | Imm_ident y -> String.equal x y
  | _ -> false
;;

let rec escapes_comp x = function
  | Comp_func (ps, body) -> (not (List.mem x ps)) && SSet.mem x (fv_anf body)
  | Comp_branch (_c, t, e) -> escapes_anf x t || escapes_anf x e
  | Comp_app (_, _)
  | Comp_binop (_, _, _)
  | Comp_tuple _ | Comp_alloc _
  | Comp_load (_, _)
  | Comp_imm _ -> false

and escapes_anf x = function
  | Anf_comp_expr ce -> escapes_comp x ce
  | Anf_let (_rf, _y, ce, body) -> escapes_comp x ce || escapes_anf x body
;;

type ctx = (ident * ident list) SMap.t

let rec rewrite_app (ops : op_ctx) (env : ctx) (f : im_expr) (args : im_expr list)
  : im_expr * im_expr list
  =
  match f with
  | Imm_ident x when is_operator_char x.[0] ->
    (match List.assoc_opt x ops with
     | Some y -> rewrite_app ops env (Imm_ident y) args
     | None ->
       (match SMap.find_opt x env with
        | None -> f, args
        | Some (lf, fvs) ->
          let fv_atoms = List.map (fun v -> Imm_ident v) fvs in
          Imm_ident lf, fv_atoms @ args))
  | Imm_ident x ->
    (match SMap.find_opt x env with
     | None -> f, args
     | Some (lf, fvs) ->
       let fv_atoms = List.map (fun v -> Imm_ident v) fvs in
       Imm_ident lf, fv_atoms @ args)
  | _ -> f, args
;;

let rec lift_anf ops (env : ctx) (n : supply) (e : anf_expr)
  : op_ctx * (anf_expr * astructure_item list) * supply
  =
  match e with
  | Anf_comp_expr ce ->
    let ops, (ce', defs), n' = lift_comp ops env n ce in
    ops, (Anf_comp_expr ce', defs), n'
  | Anf_let (rf, x, ce, body) ->
    let x, n, ops =
      if is_operator_char x.[0]
      then (
        let new_x, n = fresh_name x n in
        let ops = op_add ops (x, new_x) in
        new_x, n, ops)
      else x, n, ops
    in
    (match ce with
     | Comp_func (ps, fbody) ->
       if escapes_anf x body || escapes_anf x fbody
       then (
         let ops, (fbody', d1), n1 = lift_anf ops env n fbody in
         let ops, (body', d2), n2 = lift_anf ops env n1 body in
         ops, (Anf_let (rf, x, Comp_func (ps, fbody'), body'), d1 @ d2), n2)
       else (
         let fvs =
           let all = fv_anf fbody in
           let all = List.fold_left (fun s p -> SSet.remove p s) all ps in
           let all = SSet.remove x all in
           SSet.elements all
         in
         let lifted_name, n1 = fresh_name x n in
         let env_body = SMap.add x (lifted_name, fvs) env in
         let ops, (fbody', defs_body), n2 = lift_anf ops env_body n1 fbody in
         let def_item =
           Anf_str_value
             (Nonrecursive, lifted_name, Anf_comp_expr (Comp_func (fvs @ ps, fbody')))
         in
         let ops, (body', defs_e2), n3 = lift_anf ops env_body n2 body in
         (* (body', defs_body @ (def_item :: defs_e2)), n3 *)
         let new_body =
           if SSet.mem x (fv_anf body')
           then
             (* create a closure with old name *)
             Anf_let (Nonrecursive, x, Comp_imm (Imm_ident lifted_name), body')
           else body'
         in
         ops, (new_body, defs_body @ (def_item :: defs_e2)), n3)
     | Comp_imm (Imm_ident y) ->
       let y =
         match List.assoc_opt y ops with
         | Some z -> z
         | None -> y
       in
       (match SMap.find_opt y env with
        | Some (lf, fvs) ->
          let env' = SMap.add x (lf, fvs) env in
          lift_anf ops env' n body
        | None ->
          let ops, (body', d2), n' = lift_anf ops env n body in
          ops, (Anf_let (rf, x, ce, body'), d2), n')
     | Comp_app (Imm_ident lf_id, args) ->
       let lf_id =
         match List.assoc_opt lf_id ops with
         | Some z -> z
         | None -> lf_id
       in
       let args_are vs =
         try
           List.for_all2
             (fun v -> function
                | Imm_ident y -> String.equal v y
                | _ -> false)
             vs
             args
         with
         | Invalid_argument _ -> false
       in
       let hit =
         SMap.fold
           (fun _ (lf, fvs) acc -> acc || (String.equal lf lf_id && args_are fvs))
           env
           false
       in
       if hit
       then (
         let lf_opt =
           SMap.fold
             (fun _ (lf, fvs) acc ->
                if acc = None && String.equal lf lf_id && args_are fvs
                then Some lf
                else acc)
             env
             None
         in
         let env' =
           match lf_opt with
           | Some lf -> SMap.add x (lf, []) env
           | None -> env
         in
         lift_anf ops env' n body)
       else (
         let ops, (ce', d1), n1 = lift_comp ops env n ce in
         let ops, (body', d2), n2 = lift_anf ops env n1 body in
         ops, (Anf_let (rf, x, ce', body'), d1 @ d2), n2)
     | _ ->
       let ops, (ce', d1), n1 = lift_comp ops env n ce in
       let ops, (body', d2), n2 = lift_anf ops env n1 body in
       ops, (Anf_let (rf, x, ce', body'), d1 @ d2), n2)

and lift_comp ops (env : ctx) (n : supply) (ce : comp_expr)
  : op_ctx * (comp_expr * astructure_item list) * supply
  =
  match ce with
  | Comp_imm _ | Comp_binop _ | Comp_tuple _ | Comp_alloc _ | Comp_load _ ->
    ops, (ce, []), n
  | Comp_app (f, args) ->
    let f', args' = rewrite_app ops env f args in
    ops, (Comp_app (f', args'), []), n
  | Comp_branch (c, t, e) ->
    let ops, (t', dt), n1 = lift_anf ops env n t in
    let ops, (e', de), n2 = lift_anf ops env n1 e in
    ops, (Comp_branch (c, t', e'), dt @ de), n2
  | Comp_func (ps, body) ->
    let ops, (body', defs), n' = lift_anf ops env n body in
    ops, (Comp_func (ps, body'), defs), n'
;;

let rec desugar_then_lift ops (env : ctx) (n : supply) (e : anf_expr)
  : op_ctx * (anf_expr * astructure_item list) * supply
  =
  match e with
  | Anf_let
      (Nonrecursive, tmp, Comp_func (ps, fbody), Anf_comp_expr (Comp_imm (Imm_ident tmp')))
    when String.equal tmp tmp' ->
    let ops, (fbody', d), n' = lift_anf ops env n fbody in
    ops, (Anf_comp_expr (Comp_func (ps, fbody')), d), n'
  | Anf_let
      ( Nonrecursive
      , tmp
      , Comp_func (ps, fbody)
      , Anf_let (_rf2, x, Comp_imm (Imm_ident tmp'), body) )
    when String.equal tmp tmp' ->
    if escapes_anf x body || escapes_anf x fbody
    then (
      let ops, (e', d), n' = lift_anf ops env n e in
      ops, (e', d), n')
    else (
      let fvs =
        let all = fv_anf fbody in
        let all = List.fold_left (fun s p -> SSet.remove p s) all ps in
        SSet.elements all
      in
      let lifted_name, n1 = fresh_name x n in
      let env' =
        env |> SMap.add x (lifted_name, fvs) |> SMap.add tmp (lifted_name, fvs)
      in
      let ops, (fbody', d1), n2 = lift_anf ops env' n1 fbody in
      let def_item =
        Anf_str_value
          (Nonrecursive, lifted_name, Anf_comp_expr (Comp_func (fvs @ ps, fbody')))
      in
      let ops, (body', d2), n3 = lift_anf ops env' n2 body in
      ops, (body', d1 @ (def_item :: d2)), n3)
  | Anf_let (rf, x, ce, body) ->
    let ops, (ce', d1), n1 = lift_comp ops env n ce in
    let ops, (body', d2), n2 = desugar_then_lift ops env n1 body in
    ops, (Anf_let (rf, x, ce', body'), d1 @ d2), n2
  | Anf_comp_expr ce ->
    let ops, (ce', d), n' = lift_comp ops env n ce in
    ops, (Anf_comp_expr ce', d), n'
;;

let lift_item ops (it : astructure_item) (n : supply)
  : op_ctx * (astructure_item * astructure_item list) * supply
  =
  match it with
  | Anf_str_eval e ->
    let ops, (e1, d1), n1 = desugar_then_lift ops SMap.empty n e in
    let ops, (e2, d2), n2 = lift_anf ops SMap.empty n1 e1 in
    ops, (Anf_str_eval e2, d1 @ d2), n2
  | Anf_str_value (rf, name, e) ->
    let is_operator = String.length name > 0 && is_operator_char name.[0] in
    let name, n, ops, e =
      if is_operator
      then (
        let fresh_op_name, n' = fresh_name name n in
        let ops' = op_add ops (name, fresh_op_name) in
        fresh_op_name, n', ops', e)
      else name, n, ops, e
    in
    let ops, (e1, d1), n1 = desugar_then_lift ops SMap.empty n e in
    let ops, (e2, d2), n2 = lift_anf ops SMap.empty n1 e1 in
    ops, (Anf_str_value (rf, name, e2), d1 @ d2), n2
;;

let lambda_lift_program (p : aprogram) : aprogram =
  let ops = [] in
  let (items_rev, defs), _, _ =
    List.fold_left
      (fun ((acc, dacc), ops, n) it ->
         let ops, (it', d), n' = lift_item ops it n in
         (it' :: acc, dacc @ d), ops, n')
      (([], []), ops, 1)
      p
  in
  List.rev items_rev @ defs
;;
