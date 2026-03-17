(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Common.Ast
open Anf
open Common.Parser

let pp_list ~sep pp ppf xs =
  let pp_sep ppf () = Format.fprintf ppf "%s@ " sep in
  Format.pp_print_list ~pp_sep pp ppf xs
;;

let pp_rec_flag ppf = function
  | Expression.Recursive -> fprintf ppf "let rec"
  | Expression.Nonrecursive -> fprintf ppf "let"
;;

let pp_immediate ppf = function
  | Imm_num n -> fprintf ppf "%d" n
  | Imm_ident id -> fprintf ppf "%s" id
;;

let pp_app ppf (f, args) =
  fprintf ppf "%a%t" pp_immediate f (fun ppf ->
    List.iter (fun a -> fprintf ppf " %a" pp_immediate a) args)
;;

let rec pp_anf_expr_impl ~parens ppf (e : anf_expr) =
  let pp_comp ppf = function
    | Comp_imm imm -> pp_immediate ppf imm
    | Comp_app (Imm_ident op, [ arg1; arg2 ]) when is_operator_char op.[0] ->
      fprintf ppf "(%a %s %a)" pp_immediate arg1 op pp_immediate arg2
    | Comp_binop (op, a, b) -> fprintf ppf "(%a %s %a)" pp_immediate a op pp_immediate b
    | Comp_app (f, args) -> pp_app ppf (f, args)
    | Comp_branch (c, t, e) ->
      fprintf
        ppf
        "@[<hov 2>if %a then %a else %a@]"
        pp_immediate
        c
        (pp_anf_expr_impl ~parens:false)
        t
        (pp_anf_expr_impl ~parens:false)
        e
    | Comp_func (params, body) ->
      fprintf
        ppf
        "@[<hov 2>fun %a -> %a@]"
        (pp_list ~sep:"" pp_print_string)
        params
        (pp_anf_expr_impl ~parens:false)
        body
    | Comp_tuple items -> fprintf ppf "(%a)" (pp_list ~sep:", " pp_immediate) items
    | Comp_alloc items -> fprintf ppf "alloc(%a)" (pp_list ~sep:", " pp_immediate) items
    | Comp_load (addr, off) -> fprintf ppf "%a[%d]" pp_immediate addr off
  in
  if parens then fprintf ppf "(";
  (match e with
   | Anf_comp_expr c -> pp_comp ppf c
   | Anf_let (rf, x, c, body) ->
     fprintf
       ppf
       "@[<hov 2>%a %s = %a@ in %a@]"
       pp_rec_flag
       rf
       x
       pp_comp
       c
       (pp_anf_expr_impl ~parens:false)
       body);
  if parens then fprintf ppf ")"
;;

let print_anf_expr ppf e = pp_anf_expr_impl ~parens:false ppf e

let print_anf_structure_item ppf = function
  | Anf_str_eval e -> fprintf ppf "@[<hov 2>%a@];;" print_anf_expr e
  | Anf_str_value (rf, name, e) when is_operator_char name.[0] ->
    fprintf ppf "@[<hov 2>%a (%s) = %a@];;" pp_rec_flag rf name print_anf_expr e
  | Anf_str_value (rf, name, e) ->
    fprintf ppf "@[<hov 2>%a %s = %a@];;" pp_rec_flag rf name print_anf_expr e
;;

let print_anf_program ppf (prog : aprogram) =
  pp_print_list
    ~pp_sep:(fun ppf () -> fprintf ppf "@,@,")
    print_anf_structure_item
    ppf
    prog;
  pp_print_newline ppf ()
;;
