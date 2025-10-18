(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Common.Ast
open Anf

let pp_print_list_with_sep ~sep pp_item ppf items =
  let pp_separator ppf () = fprintf ppf "%s@ " sep in
  pp_print_list ~pp_sep:pp_separator pp_item ppf items
;;

let pp_rec_flag ppf = function
  | Expression.Recursive -> fprintf ppf "let rec"
  | Expression.Nonrecursive -> fprintf ppf "let"
;;

let print_immediate ppf = function
  | Imm_num n -> fprintf ppf "%d" n
  | Imm_ident id -> fprintf ppf "%s" id
;;

let rec print_anf_expr_impl ~needs_parens ppf expr =
  let print_complex ppf = function
    | Comp_imm imm -> print_immediate ppf imm
    | Comp_binop (op, v1, v2) ->
      fprintf ppf "(%a %s %a)" print_immediate v1 op print_immediate v2
    | Comp_app (func, args) ->
      fprintf
        ppf
        "%a (%a)"
        print_immediate
        func
        (pp_print_list_with_sep ~sep:"," print_immediate)
        args
    | Comp_branch (cond, then_expr, else_expr) ->
      open_box 2;
      fprintf ppf "if %a then@," print_immediate cond;
      print_anf_expr_impl ~needs_parens:false ppf then_expr;
      fprintf ppf "@,else@,";
      print_anf_expr_impl ~needs_parens:false ppf else_expr;
      close_box ()
    | Comp_func (params, body) ->
      open_box 2;
      fprintf
        ppf
        "fun %a ->@,"
        (pp_print_list ~pp_sep:pp_print_space pp_print_string)
        params;
      print_anf_expr_impl ~needs_parens:false ppf body;
      close_box ()
    | Comp_tuple items ->
      fprintf ppf "(%a)" (pp_print_list_with_sep ~sep:"," print_immediate) items
  in
  if needs_parens then fprintf ppf "(";
  (match expr with
   | Anf_comp_expr cexpr -> print_complex ppf cexpr
   | Anf_let (rec_flag, name, cexpr, body_expr) ->
     open_hvbox 2;
     fprintf ppf "%a %s =@ " pp_rec_flag rec_flag name;
     print_complex ppf cexpr;
     fprintf ppf "@ in@,";
     print_anf_expr_impl ~needs_parens:false ppf body_expr;
     close_box ());
  if needs_parens then fprintf ppf ")"
;;

let print_anf_expr ppf expr = print_anf_expr_impl ~needs_parens:false ppf expr

let print_anf_structure_item ppf = function
  | Anf_str_eval expr -> fprintf ppf "%a;;" print_anf_expr expr
  | Anf_str_value (rec_flag, name, expr) ->
    fprintf ppf "%a %s =@ %a;;" pp_rec_flag rec_flag name print_anf_expr expr
;;

let print_anf_program ppf (program : aprogram) =
  let pp_sep ppf () = fprintf ppf "@,@," in
  pp_print_list ~pp_sep print_anf_structure_item ppf program;
  pp_print_newline ppf ()
;;
