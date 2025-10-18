(** Copyright 2024, Mikhail Gavrilenko, Danila Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Common.Ast.Expression

(* Immediate, atomic expressions that do not require the reduction *)
type im_expr =
  | Imm_num of int
  | Imm_ident of ident

(* Complex/Computable expression *)
type comp_expr =
  | Comp_imm of im_expr
  | Comp_binop of ident * im_expr * im_expr (* x + y *)
  | Comp_app of im_expr * im_expr list (* f(x, y) *)
  | Comp_branch of im_expr * anf_expr * anf_expr (* if c then ... else ... *)
  | Comp_func of ident list * anf_expr (* fun x -> ... *)
  | Comp_tuple of im_expr list

and anf_expr =
  | Anf_comp_expr of comp_expr (* Atomic Computable Expression *)
  | Anf_let of rec_flag * ident * comp_expr * anf_expr (* let x = cexpr in anf_expr *)

type astructure_item =
  | Anf_str_eval of anf_expr
  | Anf_str_value of rec_flag * ident * anf_expr

type aprogram = astructure_item list

val anf_program : program -> astructure_item list
