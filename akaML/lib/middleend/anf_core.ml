[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Ast.Expression
open Base

(* ANF pattern *)
type a_pat =
  | APat_var of ident
  | APat_constant of constant
[@@deriving show { with_path = false }]

(* Immediate/atom expression *)
type i_exp =
  | IExp_ident of ident
  | IExp_constant of constant
  | IExp_unit
  | IExp_fun of a_pat * a_exp
[@@deriving show { with_path = false }]

(* Computation/complex expression *)
and c_exp =
  | CIExp of i_exp
  | CExp_tuple of i_exp * i_exp * i_exp list
  | CExp_apply of i_exp * i_exp * i_exp list
  | CExp_ifthenelse of c_exp * a_exp * a_exp option
[@@deriving show { with_path = false }]

(* ANF expression *)
and a_exp =
  | ACExp of c_exp
  | AExp_let of rec_flag * pattern * c_exp * a_exp
[@@deriving show { with_path = false }]

(* ANF structure_item *)
type a_structure_item =
  | AStruct_eval of a_exp
  | AStruct_value of rec_flag * pattern * a_exp
[@@deriving show { with_path = false }]

type a_structure = a_structure_item list [@@deriving show { with_path = false }]

module State = struct
  type 'a t = int -> ('a * int, string) result

  let return x st = Ok (x, st)
  let fail e = fun _ -> Error e

  let bind m f =
    fun st ->
    match m st with
    | Error e -> Error e
    | Ok (x, st') -> f x st'
  ;;

  let ( let* ) = bind
  let fresh = fun st -> Ok (st, st + 1)

  let rec state_map f = function
    | [] -> return []
    | x :: xs ->
      let* y = f x in
      let* ys = state_map f xs in
      return (y :: ys)
  ;;

  let state_concat_map f l =
    let rec aux f acc = function
      | [] -> return (List.rev acc)
      | x :: xs ->
        let* ys = f x in
        aux f (List.rev_append ys acc) xs
    in
    aux f [] l
  ;;

  let run m =
    match m 0 with
    | Ok (x, _) -> Ok x
    | Error e -> Error e
  ;;
end

open State

let gen_ident =
  let* fresh_var = fresh in
  return ("temp" ^ Int.to_string fresh_var)
;;

let anf_pat = function
  | Pat_var var -> return @@ APat_var var
  | Pat_constant const -> return @@ APat_constant const
  | _ -> fail "Pat: Not implemented"
;;

let i_to_c_exp i = CIExp i
let i_to_a_exp i = return @@ ACExp (i_to_c_exp i)

let simplify_temp_binding name value body =
  match body with
  (* Case 1: let name = value in name -> value *)
  | ACExp (CIExp (IExp_ident name')) when String.equal name name' -> ACExp value
  (* Case 2: let name = value in let orig_name = name in body -> let orig_name = value in body *)
  | AExp_let (rf, Pat_var orig_name, CIExp (IExp_ident name'), body)
    when String.equal name name' -> AExp_let (rf, Pat_var orig_name, value, body)
  | _ -> AExp_let (Nonrecursive, Pat_var name, value, body)
;;

let a_exp_let_non c_exp k =
  let* id = gen_ident in
  let* body = k @@ IExp_ident id in
  return @@ simplify_temp_binding id c_exp body
;;

let rec collect_app_args = function
  | Exp_apply (f, arg) ->
    let fn, args = collect_app_args f in
    fn, args @ [ arg ]
  | exp -> exp, []
;;

let rec anf_exp exp k =
  match exp with
  | Exp_ident id -> k @@ IExp_ident id
  | Exp_constant const -> k @@ IExp_constant const
  | Exp_let (_, { pat = Pat_any; exp }, _, body) -> anf_exp exp (fun _ -> anf_exp body k)
  | Exp_let (_, { pat = Pat_construct ("()", None); exp }, _, body) ->
    anf_exp exp (fun _ -> anf_exp body k)
  | Exp_let (flag, { pat; exp }, _, body) ->
    anf_exp exp (fun a ->
      let* body_aexp = anf_exp body k in
      return (AExp_let (flag, pat, i_to_c_exp a, body_aexp)))
  | Exp_apply (Exp_apply (Exp_ident opr, exp1), exp2) when is_bin_op opr ->
    anf_exp exp1 (fun i_exp1 ->
      anf_exp exp2 (fun i_exp2 ->
        let c_exp = CExp_apply (IExp_ident opr, i_exp1, [ i_exp2 ]) in
        a_exp_let_non c_exp k))
  | Exp_apply (Exp_ident opr, exp) when is_unary_minus opr ->
    anf_exp exp (fun i_exp ->
      let c_exp = CExp_apply (IExp_ident opr, i_exp, []) in
      a_exp_let_non c_exp k)
  | Exp_apply (exp1, exp2) ->
    let safe_tl = function
      | [] -> []
      | _ :: tail -> tail
    in
    let rec anf_list exp_list k =
      match exp_list with
      | hd :: tl ->
        anf_exp hd (fun i_exp_head ->
          anf_list tl (fun i_exp_tail -> k (i_exp_head :: i_exp_tail)))
      | [] -> k []
    in
    let exp1, exp_list = collect_app_args (Exp_apply (exp1, exp2)) in
    anf_exp exp1 (fun i_exp1 ->
      anf_list exp_list (fun i_exp_list ->
        let c_exp = CExp_apply (i_exp1, List.hd_exn i_exp_list, safe_tl i_exp_list) in
        a_exp_let_non c_exp k))
  | Exp_ifthenelse (cond, then_exp, None) ->
    anf_exp cond (fun i_cond ->
      let* then_aexp = anf_exp then_exp i_to_a_exp in
      let c_exp = CExp_ifthenelse (i_to_c_exp i_cond, then_aexp, None) in
      a_exp_let_non c_exp k)
  | Exp_ifthenelse (cond, then_exp, Some else_exp) ->
    anf_exp cond (fun i_cond ->
      let* then_aexp = anf_exp then_exp i_to_a_exp in
      let* else_aexp = anf_exp else_exp i_to_a_exp in
      let c_exp = CExp_ifthenelse (i_to_c_exp i_cond, then_aexp, Some else_aexp) in
      a_exp_let_non c_exp k)
  | Exp_tuple (exp1, exp2, []) ->
    anf_exp exp1 (fun i_exp1 ->
      anf_exp exp2 (fun i_exp2 ->
        let c_exp = CExp_tuple (i_exp1, i_exp2, []) in
        a_exp_let_non c_exp k))
  | Exp_fun (pat, pat_list, body) ->
    let* body_aexp = anf_exp body (fun i_body -> a_exp_let_non (i_to_c_exp i_body) k) in
    let* folded =
      Base.List.fold_right
        ~init:(return body_aexp)
        ~f:(fun p acc ->
          let* acc = acc in
          let* i_pat = anf_pat p in
          return @@ ACExp (CIExp (IExp_fun (i_pat, acc))))
        (pat :: pat_list)
    in
    return folded
  | Exp_construct ("()", None) -> k IExp_unit
  | _ -> fail "Exp: Not implemented"
;;

let anf_structure_item = function
  | Struct_eval exp ->
    let* ae = anf_exp exp i_to_a_exp in
    return [ AStruct_eval ae ]
  | Struct_value (rec_flag, vb, vbs) ->
    let bindings = vb :: vbs in
    let* items =
      state_map
        (fun { pat; exp } ->
           let* ae = anf_exp exp i_to_a_exp in
           return (AStruct_value (rec_flag, pat, ae)))
        bindings
    in
    return items
;;

let anf_structure (ast : structure) = run (state_concat_map anf_structure_item ast)
