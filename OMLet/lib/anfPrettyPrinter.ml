(** Copyright 2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Sofya Kozyreva <k81sofia@gmail.com>, Vyacheslav Kochergin <vyacheslav.kochergin1@gmail.com> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anf
open Format
open Ast

let rec pp_aexpr fmt = function
  | ALet (Ident name, cexpr, body) ->
    (* keep inner lets compact *)
    fprintf fmt "@[<v 2>let %s = %a in@,%a@]" name pp_cexpr cexpr pp_aexpr body
  | ACExpr cexpr -> fprintf fmt "%a" pp_cexpr cexpr

and pp_cbinop fmt l r = function
  | CPlus -> fprintf fmt "%a + %a" pp_imm l pp_imm r
  | CMinus -> fprintf fmt "%a - %a" pp_imm l pp_imm r
  | CMul -> fprintf fmt "%a * %a" pp_imm l pp_imm r
  | CDiv -> fprintf fmt "%a / %a" pp_imm l pp_imm r
  | CEq -> fprintf fmt "%a = %a" pp_imm l pp_imm r
  | CNeq -> fprintf fmt "%a <> %a" pp_imm l pp_imm r
  | CGte -> fprintf fmt "%a >= %a" pp_imm l pp_imm r
  | CLte -> fprintf fmt "%a <= %a" pp_imm l pp_imm r
  | CGt -> fprintf fmt "%a > %a" pp_imm l pp_imm r
  | CLt -> fprintf fmt "%a < %a" pp_imm l pp_imm r

and pp_cexpr fmt = function
  | CBinop (op, l, r) -> pp_cbinop fmt l r op
  | CImmexpr imm -> fprintf fmt "%a" pp_imm imm
  | CIte (c, t, Some e) ->
    fprintf fmt "@[<v 2>if %a@ then %a@ else %a@]" pp_cexpr c pp_aexpr t pp_aexpr e
  | CIte (c, t, None) -> fprintf fmt "@[<v 2>if %a@ then %a@]" pp_cexpr c pp_aexpr t
  | CLam _ as lam ->
    let rec collect_args acc = function
      | CLam (Ident arg, body) ->
        (match body with
         | ACExpr (CLam _) ->
           collect_args
             (arg :: acc)
             (match body with
              | ACExpr c -> c
              | _ -> assert false)
         | _ -> List.rev (arg :: acc), body)
      | _ -> failwith "NYI"
    in
    let args, body = collect_args [] lam in
    fprintf
      fmt
      "@[<2>fun %a -> @,%a@]"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_print_string)
      args
      pp_aexpr
      body
  | CApp (fn, args) ->
    fprintf
      fmt
      "@[<2>%a %a@]"
      pp_imm
      fn
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ") pp_imm)
      args

and pp_imm fmt = function
  | ImmNum n -> fprintf fmt "%d" n
  | ImmId (Ident x) -> fprintf fmt "%s" x

and pp_binding fmt (id, aexpr) =
  let open Ast in
  match id with
  | Ident name ->
    (* top-level lets print on a new line after '=' *)
    fprintf fmt "%s =@,%a" name pp_aexpr aexpr

and pp_astatement fmt (is_rec, bindings) =
  let open Ast in
  let keyword =
    match is_rec with
    | Rec -> "let rec"
    | Nonrec -> "let"
  in
  fprintf
    fmt
    "@[<v 0>%s %a@]"
    keyword
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ and ") pp_binding)
    bindings

and pp_aconstruction fmt = function
  | AExpr aexpr -> fprintf fmt "%a" pp_aexpr aexpr
  | AStatement (flag, bindings) -> pp_astatement fmt (flag, bindings)

and pp_aconstructions fmt (constrs : aconstructions) =
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,@,") pp_aconstruction fmt constrs
;;
