(** Copyright 2025-2026, Rodion Suvorov, Dmitriy Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.Expression
open Ast.Structure
open Ast

type immexpr =
  | ImmNum of int
  | ImmId of ident
[@@deriving eq, show { with_path = false }]

type binop =
  | Add
  | Sub
  | Mul
  | Le
  | Lt
  | Eq
  | Neq
[@@deriving eq, show { with_path = false }]

type cexpr =
  | CImm of immexpr
  | CBinop of binop * immexpr * immexpr
  | CApp of immexpr * immexpr list
  | CIte of immexpr * aexpr * aexpr
  | CFun of ident * aexpr
[@@deriving eq, show { with_path = false }]

and aexpr =
  | ACE of cexpr
  | ALet of rec_flag * ident * cexpr * aexpr
[@@deriving eq, show { with_path = false }]

type astructure_item =
  | AStr_value of rec_flag * ident * aexpr
  | AStr_eval of aexpr
[@@deriving eq, show { with_path = false }]

type aprogram = astructure_item list [@@deriving eq, show { with_path = false }]
type anf_state = { temps : int }

module ANFState = struct
  type 'a t = anf_state -> 'a * anf_state

  let return x st = x, st

  let bind : 'a t -> ('a -> 'b t) -> 'b t =
    fun t f st ->
    let a, tran_st = t st in
    let b, fin_st = f a tran_st in
    b, fin_st
  ;;

  let ( let* ) = bind
  let get st = st, st
  let put st _ = (), st

  let rec map_m f = function
    | [] -> return []
    | x :: xs ->
      let* y = f x in
      let* ys = map_m f xs in
      return (y :: ys)
  ;;

  let run m = fst (m { temps = 0 })
end

open ANFState

let fresh_temp =
  let* st = get in
  let name = Printf.sprintf "t_%d" st.temps in
  let* () = put { temps = st.temps + 1 } in
  return name
;;

let rec app_args_to_list = function
  | Exp_apply (f, arg) ->
    let args, fn = app_args_to_list f in
    args @ [ arg ], fn
  | e -> [], e
;;

let is_binop op =
  List.mem op [ "+"; "-"; "*"; "/"; "<"; "<="; ">"; ">="; "="; "<>"; "||"; "&&" ]
;;

let define_binop = function
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "<" -> Lt
  | "<=" -> Le
  | "=" -> Eq
  | "<>" -> Neq
  | s -> failwith ("unsupported binop: " ^ s)
;;

let get_pattern_name = function
  | Pattern.Pat_var name -> name
  | _ -> failwith "not supported"
;;

let rec transform_list
  : Expression.t list -> (immexpr list -> aexpr ANFState.t) -> aexpr ANFState.t
  =
  fun l k ->
  match l with
  | hd :: tl ->
    transform_expr hd (fun res_hd ->
      transform_list tl (fun res_tl -> k (res_hd :: res_tl)))
  | [] -> k []

and transform_expr expr k =
  match expr with
  | Exp_constant (Constant.Const_integer exp) -> k @@ ImmNum exp
  | Exp_ident exp -> k @@ ImmId exp
  | Exp_apply (Exp_ident op, Exp_tuple (exp1, exp2, [])) when is_binop op ->
    transform_expr exp1 (fun exp1_res ->
      transform_expr exp2 (fun exp2_res ->
        let* t = fresh_temp in
        let* rest = k @@ ImmId t in
        match rest with
        (* avoid `let t = e in t` *)
        | ACE (CImm (ImmId id)) when String.equal t id ->
          return @@ ACE (CBinop (define_binop op, exp1_res, exp2_res))
        | _ ->
          return
          @@ ALet (Nonrecursive, t, CBinop (define_binop op, exp1_res, exp2_res), rest)))
  | Exp_apply (exp1, exp2) ->
    let args, fn = app_args_to_list @@ Exp_apply (exp1, exp2) in
    transform_expr fn (fun fn_res ->
      transform_list args (fun args_res ->
        let* t = fresh_temp in
        let* rest = k (ImmId t) in
        match rest with
        (* avoid `let t = e in t` *)
        | ACE (CImm (ImmId id)) when String.equal t id ->
          return @@ ACE (CApp (fn_res, args_res))
        | _ -> return @@ ALet (Nonrecursive, t, CApp (fn_res, args_res), rest)))
  | Exp_let (flag, ({ pat = Pattern.Pat_var pat; expr }, _), exp) ->
    transform_expr expr (fun a ->
      let* res = transform_expr exp k in
      return (ALet (flag, pat, CImm a, res)))
  | Exp_let (flag, ({ pat = Pattern.Pat_construct ("()", None); expr }, _), exp) ->
    transform_expr expr (fun a ->
      let* res = transform_expr exp k in
      return (ALet (flag, "()", CImm a, res)))
  | Exp_if (cond, then_expr, Some else_expr) ->
    transform_expr cond (fun cond_res ->
      let* then_res = transform_expr then_expr k in
      let* else_res = transform_expr else_expr k in
      return (ACE (CIte (cond_res, then_res, else_res))))
  | Exp_fun ((pat_hd, pat_tl), exp) ->
    let* body_anf = transform_expr exp (fun exp_res -> return @@ ACE (CImm exp_res)) in
    let params =
      List.fold_right
        (fun pat acc -> ACE (CFun (get_pattern_name pat, acc)))
        (pat_hd :: pat_tl)
        body_anf
    in
    return params
  | _ -> failwith "other expression in future versions"
;;

let transform_str_item : structure_item -> astructure_item ANFState.t = function
  | Str_eval expr ->
    let* e_anf = transform_expr expr (fun v -> return @@ ACE (CImm v)) in
    return @@ AStr_eval e_anf
  | Str_value (recflag, ({ pat = Pattern.Pat_var pat; expr }, _)) ->
    let* e_anf = transform_expr expr (fun v -> return @@ ACE (CImm v)) in
    return @@ AStr_value (recflag, pat, e_anf)
  | _ -> failwith "ADT is not unsupported"
;;

let transform_str_item_list prog = map_m transform_str_item prog
let anf_transform (prog : program) = run (transform_str_item_list prog)
