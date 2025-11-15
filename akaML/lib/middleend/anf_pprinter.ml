[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Format
open Anf_core
open Pprinter

let pp_sep ppf () = fprintf ppf " "

let pp_a_pat ppf = function
  | APat_var var -> pp_ident ppf var
  | APat_constant const -> pp_constant ppf const
;;

let rec pp_i_exp_deep need_parens ppf = function
  | IExp_ident id -> pp_ident ppf id
  | IExp_constant const -> pp_constant ppf const
  | IExp_unit -> pp_ident ppf "()"
  | IExp_fun (a_pat, a_exp) ->
    if need_parens then fprintf ppf "(";
    pp_open_box ppf 2;
    fprintf ppf "@[fun@ %a@ ->@]@ " pp_a_pat a_pat;
    fprintf ppf "@[<hv>%a@]" (pp_a_exp_deep true) a_exp;
    if need_parens then fprintf ppf ")";
    pp_close_box ppf ()

and pp_c_exp_deep need_parens ppf = function
  | CIExp i_exp -> pp_i_exp_deep need_parens ppf i_exp
  | CExp_tuple (i_exp1, i_exp2, i_exp_list) ->
    pp_open_hvbox ppf 0;
    if need_parens then fprintf ppf "( ";
    fprintf
      ppf
      "%a@]"
      (pp_print_list ~pp_sep:pp_comma (pp_i_exp_deep true))
      (i_exp1 :: i_exp2 :: i_exp_list);
    if need_parens then fprintf ppf " )"
  | CExp_apply (i_exp1, i_exp2, i_exp_list) ->
    pp_open_box ppf 2;
    (pp_c_exp_apply ~need_parens) ppf (i_exp1, i_exp2, i_exp_list);
    pp_close_box ppf ()
  | CExp_ifthenelse (c_exp, a_exp, None) ->
    if need_parens then fprintf ppf "(";
    pp_open_hvbox ppf 0;
    fprintf ppf "@[if %a@ @]" (pp_c_exp_deep false) c_exp;
    fprintf ppf "@[<v 2>then %a@]" (pp_a_exp_deep true) a_exp;
    if need_parens then fprintf ppf ")";
    pp_close_box ppf ()
  | CExp_ifthenelse (c_exp, a_exp1, Some a_exp2) ->
    if need_parens then fprintf ppf "(";
    pp_open_hvbox ppf 0;
    fprintf ppf "@[if %a@ @]" (pp_c_exp_deep false) c_exp;
    fprintf ppf "@[<v 2>then %a@]@ " (pp_a_exp_deep true) a_exp1;
    fprintf ppf "@[<hv 2>else %a@]" (pp_a_exp_deep true) a_exp2;
    if need_parens then fprintf ppf ")";
    pp_close_box ppf ()

and pp_c_exp_apply ?(need_parens = false) ppf = function
  | IExp_ident opr, i_exp, [] when is_unary_minus opr ->
    fprintf ppf "-%a" (pp_i_exp_deep need_parens) i_exp
  | IExp_ident opr, i_exp1, [ i_exp2 ] when is_bin_op opr ->
    fprintf ppf "%a" (pp_i_exp_deep need_parens) i_exp1;
    fprintf ppf " %s " opr;
    fprintf ppf "%a" (pp_i_exp_deep need_parens) i_exp2
  | IExp_ident f_exp, arg_exp, arg_exp_list ->
    fprintf ppf "%s" f_exp;
    fprintf
      ppf
      " %a"
      (pp_print_list ~pp_sep:pp_print_space (pp_i_exp_deep need_parens))
      (arg_exp :: arg_exp_list)
  | _ -> fprintf ppf "In progress"

and pp_a_exp_deep need_parens ppf = function
  | ACExp c_exp -> pp_c_exp_deep need_parens ppf c_exp
  | AExp_let (rec_flag, pat, c_exp, a_exp) ->
    if need_parens then fprintf ppf "(";
    fprintf ppf "@[<hv 2>";
    pp_rec_flag ppf rec_flag;
    fprintf ppf "@[%a =@]@ " pp_pattern pat;
    fprintf ppf "@[%a@] in@]@\n" (pp_c_exp_deep false) c_exp;
    fprintf ppf "@[%a@]" (pp_a_exp_deep false) a_exp;
    if need_parens then fprintf ppf ")"
;;

let pp_i_exp = pp_i_exp_deep false
let pp_c_exp = pp_c_exp_deep false
let pp_a_exp = pp_a_exp_deep false

let pp_a_structure_item ppf = function
  | AStruct_eval a_exp ->
    fprintf ppf "@[<hv>%a@];;" pp_a_exp a_exp;
    pp_print_flush ppf ()
  | AStruct_value (rec_flag, pat, a_exp) ->
    pp_open_hovbox ppf 2;
    pp_rec_flag ppf rec_flag;
    fprintf ppf "@[%a =@]@ " pp_pattern pat;
    fprintf ppf "@[%a@]" pp_a_exp a_exp;
    pp_print_if_newline ppf ();
    pp_print_cut ppf ();
    fprintf ppf ";;";
    pp_print_flush ppf ()
;;

let pp_a_structure ppf ast =
  if Base.List.is_empty ast
  then fprintf ppf ";;"
  else
    fprintf
      ppf
      "@[<v>%a@]"
      (pp_print_list ~pp_sep:pp_force_newline pp_a_structure_item)
      ast;
  pp_print_flush ppf ()
;;
