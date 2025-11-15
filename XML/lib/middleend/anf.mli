(** Copyright 2024,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast.Expression
open Common.Ast.Structure
open Common.Ast

(** Immediate, atomic expressions that do not require the reduction *)
type im_expr =
  | Imm_num of int
  | Imm_ident of ident

(** Complex/Computable expression *)
type comp_expr =
  | Comp_imm of im_expr
  | Comp_binop of ident * im_expr * im_expr (* x + y *)
  | Comp_app of im_expr * im_expr list (* f(x, y) *)
  | Comp_branch of im_expr * anf_expr * anf_expr (* if c then ... else ... *)
  | Comp_func of ident list * anf_expr (* fun x y ... -> ... *)
  | Comp_tuple of im_expr list
  | Comp_alloc of
      im_expr list (* Allocate a memory block and initialize it with values. *)
  | Comp_load of
      im_expr * int (* Load a value from memory: Comp_load(address, byte_offset). *)

and anf_expr =
  | Anf_comp_expr of comp_expr
  | Anf_let of rec_flag * ident * comp_expr * anf_expr

type astructure_item =
  | Anf_str_eval of anf_expr
  | Anf_str_value of rec_flag * ident * anf_expr

type aprogram = astructure_item list

type anf_error =
  [ `Only_simple_var_params
  | `Func_no_params
  | `Let_and_not_supported
  | `Unsupported_let_pattern of string
  | `Unsupported_expr_in_normaliser
  | `Mutual_rec_not_supported
  | `Unsupported_toplevel_let
  | `Unsupported_toplevel_item
  ]

val pp_anf_error : anf_error -> string
val anf_program_res : structure_item list -> (aprogram, anf_error) result
val anf_program : structure_item list -> aprogram
