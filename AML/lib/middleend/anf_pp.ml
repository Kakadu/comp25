(** Copyright 2025-2026, Rodion Suvorov, Dmitriy Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Anf

let pp_binop ppf = function
  | Add -> pp_print_string ppf "+"
  | Sub -> pp_print_string ppf "-"
  | Mul -> pp_print_string ppf "*"
  | Le -> pp_print_string ppf "<="
  | Lt -> pp_print_string ppf "<"
  | Eq -> pp_print_string ppf "="
  | Neq -> pp_print_string ppf "<>"
;;

let pp_immexpr ppf = function
  | ImmNum i -> pp_print_int ppf i
  | ImmId id -> pp_print_string ppf id
;;

let pp_indent ppf indent = pp_print_string ppf (String.make indent ' ')

let rec pp_aexpr ppf indent = function
  | ACE cexpr ->
    pp_indent ppf indent;
    pp_cexpr ppf indent cexpr
  | ALet (flag, id, cexpr, body) ->
    let rec_str =
      match flag with
      | Recursive -> " rec"
      | _ -> ""
    in
    pp_indent ppf indent;
    fprintf ppf "let%s %s = " rec_str id;
    pp_cexpr ppf indent cexpr;
    fprintf ppf " in\n";
    pp_aexpr ppf indent body

and pp_cexpr ppf indent = function
  | CImm imm -> pp_immexpr ppf imm
  | CBinop (op, i1, i2) -> fprintf ppf "%a %a %a" pp_immexpr i1 pp_binop op pp_immexpr i2
  | CApp (f, args) ->
    fprintf ppf "%a" pp_immexpr f;
    List.iter (fprintf ppf " %a" pp_immexpr) args
  | CIte (cond, then_expr, else_expr) ->
    let new_indent = indent + 2 in
    fprintf ppf "if %a then\n" pp_immexpr cond;
    pp_aexpr ppf new_indent then_expr;
    fprintf ppf "\n";
    pp_indent ppf indent;
    fprintf ppf "else\n";
    pp_aexpr ppf new_indent else_expr
  | CFun (param, body) ->
    let new_indent = indent + 2 in
    fprintf ppf "fun %s ->\n" param;
    pp_aexpr ppf new_indent body
;;

let pp_astructure_item ppf indent = function
  | AStr_value (flag, name, expr) ->
    let rec_str =
      match flag with
      | Recursive -> " rec"
      | _ -> ""
    in
    pp_indent ppf indent;
    fprintf ppf "let%s %s =" rec_str name;
    fprintf ppf "\n";
    pp_aexpr ppf (indent + 2) expr
  | AStr_eval expr ->
    pp_indent ppf indent;
    pp_aexpr ppf (indent + 2) expr
;;

let pp_anf ppf anf =
  let pp_sep ppf () = fprintf ppf "\n\n" in
  let pp_item_with_indent ppf item = pp_astructure_item ppf 0 item in
  fprintf ppf "%a" (pp_print_list ~pp_sep pp_item_with_indent) anf
;;
