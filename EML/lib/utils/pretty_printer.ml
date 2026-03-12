(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Frontend.Ast

let string_of_bin_op = function
  | Plus -> "+"
  | Minus -> "-"
  | Multiply -> "*"
  | Division -> "/"
  | And -> "&&"
  | Or -> "||"
  | GreatestEqual -> ">="
  | LowestEqual -> "<="
  | GreaterThan -> ">"
  | LowerThan -> "<"
  | Equal -> "="
  | NotEqual -> "<>"
  | Custom s -> s
;;

let string_of_unary_op = function
  | Negative -> "-"
  | Not -> "not"
;;

let pp_bin_op ppf op = Format.fprintf ppf "%s" (string_of_bin_op op)
let pp_unary_op ppf op = Format.fprintf ppf "%s" (string_of_unary_op op)

let pp_const ppf = function
  | ConstInt i -> Format.fprintf ppf "%d" i
  | ConstBool b -> Format.fprintf ppf "%b" b
  | ConstString s -> Format.fprintf ppf "%S" s
  | ConstChar c -> Format.fprintf ppf "'%c'" c
;;

let rec pp_pattern ppf = function
  | PatVariable v -> Format.fprintf ppf "%s" v
  | PatConst c -> pp_const ppf c
  | PatAny -> Format.fprintf ppf "_"
  | PatUnit -> Format.fprintf ppf "()"
  | PatType (p, t) -> Format.fprintf ppf "(%a : %a)" pp_pattern p pp_ty t
  | PatTuple (p1, p2, rest) ->
    Format.fprintf
      ppf
      "(%a)"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") pp_pattern)
      (p1 :: p2 :: rest)
  | PatList ps ->
    Format.fprintf
      ppf
      "[%a]"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ") pp_pattern)
      ps
  | PatOption None -> Format.fprintf ppf "None"
  | PatOption (Some p) -> Format.fprintf ppf "Some (%a)" pp_pattern p
  | PatConstruct ("[]", None) -> Format.fprintf ppf "[]"
  | PatConstruct ("::", Some (PatTuple (h, t, []))) ->
    Format.fprintf ppf "%a::%a" pp_pattern h pp_pattern t
  | PatConstruct (id, None) -> Format.fprintf ppf "%s" id
  | PatConstruct (id, Some p) -> Format.fprintf ppf "%s (%a)" id pp_pattern p

and pp_expr ppf = function
  | ExpIdent v -> Format.fprintf ppf "%s" v
  | ExpConst c -> pp_const ppf c
  | ExpBranch (c, t, None) -> Format.fprintf ppf "if %a then %a" pp_expr c pp_expr t
  | ExpBranch (c, t, Some e) ->
    Format.fprintf ppf "if %a then %a else %a" pp_expr c pp_expr t pp_expr e
  | ExpBinOper (op, l, r) ->
    Format.fprintf ppf "(%a %a %a)" pp_expr l pp_bin_op op pp_expr r
  | ExpUnarOper (op, e) -> Format.fprintf ppf "(%a %a)" pp_unary_op op pp_expr e
  | ExpTuple (e1, e2, rest) ->
    Format.fprintf
      ppf
      "(%a)"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") pp_expr)
      (e1 :: e2 :: rest)
  | ExpList es ->
    Format.fprintf
      ppf
      "[%a]"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ") pp_expr)
      es
  | ExpLambda (p, ps, body) ->
    Format.fprintf
      ppf
      "fun %a -> %a"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") pp_pattern)
      (p :: ps)
      pp_expr
      body
  | ExpTypeAnnotation (e, t) -> Format.fprintf ppf "(%a : %a)" pp_expr e pp_ty t
  | ExpLet (is_rec, bind, more, body) ->
    let kw =
      match is_rec with
      | Rec -> "let rec"
      | NonRec -> "let"
    in
    Format.fprintf ppf "%s %a in %a" kw pp_binds (bind, more) pp_expr body
  | ExpApply _ as e ->
    let f, args = flatten_apply e in
    Format.fprintf
      ppf
      "%a %a"
      pp_atomic_expr
      f
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") pp_atomic_expr)
      args
  | ExpOption None -> Format.fprintf ppf "None"
  | ExpOption (Some e) -> Format.fprintf ppf "Some (%a)" pp_expr e
  | ExpFunction (first, more) ->
    Format.fprintf
      ppf
      "function %a"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") pp_case)
      (first :: more)
  | ExpMatch (e, first, more) ->
    Format.fprintf
      ppf
      "match %a with %a"
      pp_expr
      e
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") pp_case)
      (first :: more)
  | ExpConstruct ("()", None) -> Format.fprintf ppf "()"
  | ExpConstruct ("[]", None) -> Format.fprintf ppf "[]"
  | ExpConstruct ("::", Some (ExpTuple (h, t, []))) ->
    Format.fprintf ppf "%a::%a" pp_expr h pp_expr t
  | ExpConstruct (id, None) -> Format.fprintf ppf "%s" id
  | ExpConstruct (id, Some e) -> Format.fprintf ppf "%s (%a)" id pp_expr e

and pp_atomic_expr ppf = function
  | (ExpIdent _ | ExpConst _ | ExpOption None | ExpConstruct (_, None)) as e ->
    pp_expr ppf e
  | e -> Format.fprintf ppf "(%a)" pp_expr e

and flatten_apply e =
  let rec go f args =
    match f with
    | ExpApply (f', a) -> go f' (a :: args)
    | _ -> f, args
  in
  go e []

and pp_case ppf (p, e) = Format.fprintf ppf "| %a -> %a" pp_pattern p pp_expr e
and pp_bind ppf (p, e) = Format.fprintf ppf "%a = %a" pp_pattern p pp_expr e

and pp_binds ppf (first, more) =
  Format.fprintf ppf "%a" pp_bind first;
  List.iter (fun b -> Format.fprintf ppf "@ and %a" pp_bind b) more
;;

let pp_structure_item ppf = function
  | SEval e -> Format.fprintf ppf "%a;;" pp_expr e
  | SValue (is_rec, bind, more) ->
    let kw =
      match is_rec with
      | Rec -> "let rec"
      | NonRec -> "let"
    in
    Format.fprintf ppf "%s %a;;" kw pp_binds (bind, more)
;;

let pp_structure ppf (lst : structure list) =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.fprintf ppf "@\n")
    pp_structure_item
    ppf
    lst
;;
