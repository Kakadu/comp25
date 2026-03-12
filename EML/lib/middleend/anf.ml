(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Frontend.Ast
open Base
open Utils.Monads.ANFMonad
open Utils.Monads.ANFMonad.Syntax

type immediate =
  | ImmediateConst of const
  | ImmediateVar of ident
[@@deriving show { with_path = false }]

type complex_expr =
  | ComplexImmediate of immediate
  | ComplexUnit
  | ComplexBinOper of bin_oper * immediate * immediate
  | ComplexUnarOper of unar_oper * immediate
  | ComplexTuple of immediate * immediate * immediate list
  | ComplexField of immediate * int
  | ComplexList of immediate list
  | ComplexOption of immediate option
  | ComplexApp of immediate * immediate * immediate list
  | ComplexLambda of pattern list * anf_expr
  | ComplexBranch of immediate * anf_expr * anf_expr
[@@deriving show { with_path = false }]

and anf_expr =
  | AnfLet of is_rec * ident * complex_expr * anf_expr
  | AnfExpr of complex_expr
[@@deriving show { with_path = false }]

type arity = int

let pp_arity ppf (n : arity) = Stdlib.Format.pp_print_int ppf n

type anf_bind = ident * anf_expr [@@deriving show { with_path = false }]
type anf_fun_bind = ident * arity * anf_expr [@@deriving show { with_path = false }]

type anf_structure =
  | AnfEval of anf_expr
  | AnfValue of is_rec * anf_fun_bind * anf_fun_bind list
[@@deriving show { with_path = false }]

type anf_program = anf_structure list [@@deriving show { with_path = false }]

let rec anf_expr_arity = function
  | AnfExpr (ComplexLambda (pat_list, body)) -> List.length pat_list + anf_expr_arity body
  | AnfLet (_, _, _, body) -> anf_expr_arity body
  | _ -> 0
;;

let optimize_anf_let (is_rec, name1, expr, body) =
  match is_rec, body with
  | NonRec, AnfExpr (ComplexImmediate (ImmediateVar name2)) when String.equal name1 name2
    -> AnfExpr expr
  | _, AnfLet (is_rec', orig_name, ComplexImmediate (ImmediateVar name2), body')
    when String.equal name1 name2 -> AnfLet (is_rec', orig_name, expr, body')
  | _ -> AnfLet (is_rec, name1, expr, body)
;;

let bind_complex_expr complex_expr k =
  let* var = fresh in
  let* body_expr = k (ImmediateVar var) in
  return (optimize_anf_let (NonRec, var, complex_expr, body_expr))
;;

let get_var = function
  | PatVariable id -> return id
  | _ -> fresh
;;

let tuple_indices pats = List.mapi pats ~f:(fun i p -> i, p)

let match_list_cases cases =
  let is_nil = function
    | PatConstruct ("[]", None) | PatList [] -> true
    | _ -> false
  in
  let is_cons = function
    | PatConstruct ("::", Some (PatTuple (_, _, []))) -> true
    | _ -> false
  in
  let get_cons_pats = function
    | PatConstruct ("::", Some (PatTuple (head_pat, tail_pat, []))) ->
      Some (head_pat, tail_pat)
    | _ -> None
  in
  match cases with
  | [ (pat1, expr1); (pat2, expr2) ] when is_nil pat1 && is_cons pat2 ->
    (match get_cons_pats pat2 with
     | Some (head_pat, tail_pat) -> Some (expr1, head_pat, tail_pat, expr2)
     | None -> None)
  | [ (pat1, expr1); (pat2, expr2) ] when is_cons pat1 && is_nil pat2 ->
    (match get_cons_pats pat1 with
     | Some (head_pat, tail_pat) -> Some (expr2, head_pat, tail_pat, expr1)
     | None -> None)
  | _ -> None
;;

let build_tuple_lets tuple_var indices_pats body =
  let rec aux tuple_var indices_pats body =
    match indices_pats with
    | [] -> return body
    | (i, pat) :: rest ->
      let* bind_id = get_var pat in
      let* body_with_rest = aux tuple_var rest body in
      let* inner =
        match pat with
        | PatTuple (p1, p2, rest_pats) ->
          aux bind_id (tuple_indices (p1 :: p2 :: rest_pats)) body_with_rest
        | _ -> return body_with_rest
      in
      return (AnfLet (NonRec, bind_id, ComplexField (ImmediateVar tuple_var, i), inner))
  in
  aux tuple_var indices_pats body
;;

let build_tuple_top_level_bindings tuple_var indices_pats =
  let rec aux tuple_var = function
    | [] -> return []
    | (i, pat) :: rest ->
      let* bind_id = get_var pat in
      let cur = bind_id, AnfExpr (ComplexField (ImmediateVar tuple_var, i)) in
      let* inner =
        match pat with
        | PatTuple (p1, p2, rest_pats) ->
          aux bind_id (tuple_indices (p1 :: p2 :: rest_pats))
        | _ -> return []
      in
      let* rest_bindings = aux tuple_var rest in
      return ((cur :: inner) @ rest_bindings)
  in
  aux tuple_var indices_pats
;;

let rec anf (expr : expr) (k : immediate -> anf_expr t) : anf_expr t =
  match expr with
  | ExpConst c -> k (ImmediateConst c)
  | ExpIdent x -> k (ImmediateVar x)
  | ExpUnarOper (op, expr) ->
    anf expr (fun imm -> bind_complex_expr (ComplexUnarOper (op, imm)) k)
  | ExpBinOper (Custom op_name, exp1, exp2) ->
    anf exp1 (fun imm1 ->
      anf exp2 (fun imm2 ->
        bind_complex_expr (ComplexApp (ImmediateVar op_name, imm1, [ imm2 ])) k))
  | ExpBinOper (op, exp1, exp2) ->
    anf exp1 (fun imm1 ->
      anf exp2 (fun imm2 -> bind_complex_expr (ComplexBinOper (op, imm1, imm2)) k))
  | ExpBranch (cond, then_exp, else_exp_opt) ->
    anf cond (fun imm_cond ->
      let* then_aexp = anf_to_immediate_expr then_exp in
      let* else_aexp =
        match else_exp_opt with
        | None -> return (AnfExpr ComplexUnit)
        | Some else_exp -> anf_to_immediate_expr else_exp
      in
      bind_complex_expr (ComplexBranch (imm_cond, then_aexp, else_aexp)) k)
  | ExpLet (flag, (pat, expr), _, body) ->
    (match pat with
     | PatAny | PatUnit | PatConstruct ("()", None) -> anf expr (fun _ -> anf body k)
     | PatTuple (p1, p2, rest) ->
       let pats = p1 :: p2 :: rest in
       anf expr (fun tuple_imm ->
         let* tuple_var = fresh in
         let* body_anf_expr = anf body k in
         let* with_lets = build_tuple_lets tuple_var (tuple_indices pats) body_anf_expr in
         return (AnfLet (flag, tuple_var, ComplexImmediate tuple_imm, with_lets)))
     | PatVariable _ | PatConst _ ->
       anf expr (fun imm ->
         let* body_anf_expr = anf body k in
         let* var = get_var pat in
         return (AnfLet (flag, var, ComplexImmediate imm, body_anf_expr)))
     | _ -> fail "Complex patterns in let not supported")
  | ExpApply (exp1, exp2) ->
    let func, args_list =
      let rec collect_args acc = function
        | ExpApply (f, arg) -> collect_args (arg :: acc) f
        | f -> f, acc
      in
      collect_args [] (ExpApply (exp1, exp2))
    in
    anf func (fun immediate_func ->
      anf_list args_list (function
        | arg1 :: arg_tl ->
          bind_complex_expr (ComplexApp (immediate_func, arg1, arg_tl)) k
        | [] -> fail "application with no arguments"))
  | ExpTuple (exp1, exp2, exp_list) ->
    let all_exprs = exp1 :: exp2 :: exp_list in
    anf_list all_exprs (function
      | imm1 :: imm2 :: rest -> bind_complex_expr (ComplexTuple (imm1, imm2, rest)) k
      | _ -> fail "Invalid tuple")
  | ExpLambda (pat, pat_list, body) ->
    let params = pat :: pat_list in
    let* body_anf_expr = anf_to_immediate_expr body in
    let rec wrap_params current_body = function
      | [] -> return current_body
      | ((PatVariable _ | PatConst _) as param) :: remaining_params ->
        let* body_with_rest = wrap_params current_body remaining_params in
        return (AnfExpr (ComplexLambda ([ param ], body_with_rest)))
      | (PatAny | PatUnit | PatConstruct ("()", None)) :: remaining_params ->
        let* body_with_rest = wrap_params current_body remaining_params in
        let* ignored = fresh in
        return (AnfExpr (ComplexLambda ([ PatVariable ignored ], body_with_rest)))
      | PatType (inner_pat, _) :: remaining_params ->
        wrap_params current_body (inner_pat :: remaining_params)
      | PatTuple (p1, p2, rest_pats) :: remaining_params ->
        let* body_with_rest = wrap_params current_body remaining_params in
        let* var = fresh in
        let* body_with_tuple_destructured =
          build_tuple_lets var (tuple_indices (p1 :: p2 :: rest_pats)) body_with_rest
        in
        return
          (AnfExpr (ComplexLambda ([ PatVariable var ], body_with_tuple_destructured)))
      | _ -> fail "Only variable, constant and tuple patterns in lambda"
    in
    let* lambda_anf = wrap_params body_anf_expr params in
    (match lambda_anf with
     | AnfExpr (ComplexLambda (pats, body)) ->
       bind_complex_expr (ComplexLambda (pats, body)) k
     | _ -> fail "ExpLambda: wrap_params must return ComplexLambda")
  | ExpConstruct ("()", None) -> bind_complex_expr ComplexUnit k
  | ExpTypeAnnotation (e, _) -> anf e k
  | ExpList exprs ->
    anf_list exprs (fun imm_list -> bind_complex_expr (ComplexList imm_list) k)
  | ExpOption None -> bind_complex_expr ComplexUnit k
  | ExpOption (Some e) -> anf e k
  | ExpMatch (scrut, first_case, rest_cases) ->
    (match match_list_cases (first_case :: rest_cases) with
     | Some (nil_expr, head_pat, tail_pat, cons_expr) ->
       anf scrut (fun scrut_imm ->
         let* scrut_var = fresh in
         let* cond_var = fresh in
         let* nil_aexp = anf_to_immediate_expr nil_expr in
         let* cons_aexp_base = anf_to_immediate_expr cons_expr in
         let* cons_aexp =
           build_tuple_lets
             scrut_var
             (tuple_indices [ head_pat; tail_pat ])
             cons_aexp_base
         in
         let* branch_result =
           bind_complex_expr
             (ComplexBranch (ImmediateVar cond_var, nil_aexp, cons_aexp))
             k
         in
         return
           (AnfLet
              ( NonRec
              , scrut_var
              , ComplexImmediate scrut_imm
              , AnfLet
                  ( NonRec
                  , cond_var
                  , ComplexBinOper
                      (Equal, ImmediateVar scrut_var, ImmediateConst (ConstInt 0))
                  , branch_result ) )))
     | None -> fail "Only list match with [] and h::tl is supported")
  | ExpFunction _ -> fail "Match/function cases not implemented"
  | ExpConstruct ("[]", None) -> k (ImmediateConst (ConstInt 0))
  | ExpConstruct ("::", Some (ExpTuple (head_e, tail_e, []))) ->
    anf head_e (fun head_imm ->
      anf tail_e (fun tail_imm ->
        bind_complex_expr (ComplexTuple (head_imm, tail_imm, [])) k))
  | ExpConstruct _ -> fail "Constructors not implemented"

and anf_to_immediate_expr expr =
  anf expr (fun imm -> return (AnfExpr (ComplexImmediate imm)))

and anf_list (exprs : expr list) (k : immediate list -> anf_expr t) : anf_expr t =
  match exprs with
  | [] -> k []
  | hd :: tl ->
    anf hd (fun immediate_hd ->
      anf_list tl (fun immediate_tl -> k (immediate_hd :: immediate_tl)))
;;

let to_fun_bind (id, e) = id, anf_expr_arity e, e

let anf_structure_item (item : structure) : anf_structure list t =
  match item with
  | SEval expr ->
    let* result = anf_to_immediate_expr expr in
    return [ AnfEval result ]
  | SValue (rec_flag, (pat, expr), binds) ->
    let bindings = (pat, expr) :: binds in
    List.fold_left bindings ~init:(return []) ~f:(fun acc (pat, expr) ->
      let* acc_list = acc in
      let* anf_expr_body = anf_to_immediate_expr expr in
      match pat with
      | PatTuple (p1, p2, rest) ->
        let* tuple_var = fresh in
        let* component_bindings =
          build_tuple_top_level_bindings tuple_var (tuple_indices (p1 :: p2 :: rest))
        in
        let one_value (id, e) = AnfValue (NonRec, to_fun_bind (id, e), []) in
        let new_items =
          AnfValue (rec_flag, to_fun_bind (tuple_var, anf_expr_body), [])
          :: List.map component_bindings ~f:one_value
        in
        return (acc_list @ new_items)
      | _ ->
        let* var = get_var pat in
        return (acc_list @ [ AnfValue (rec_flag, to_fun_bind (var, anf_expr_body), []) ]))
;;

let anf_program (ast : program) : (anf_program, string) Result.t =
  let program' =
    List.fold_left ast ~init:(return []) ~f:(fun acc item ->
      let* acc_list = acc in
      let* item_anf = anf_structure_item item in
      return (acc_list @ item_anf))
  in
  run program'
;;
