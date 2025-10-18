(** Copyright 2024, Mikhail Gavrilenko, Danila Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast.Expression
open Common.Ast.Constant
open Common.Ast.Structure
open Common.Ast

(* Immediate, atomic expressions that do not require the reduction *)
type im_expr =
  | Imm_num of int
  | Imm_ident of ident
[@@deriving eq, show { with_path = false }, qcheck]

(* Complex/Computable expression *)
type comp_expr =
  | Comp_imm of im_expr
  | Comp_binop of ident * im_expr * im_expr (* x + y *)
  | Comp_app of im_expr * im_expr list (* f(x, y) *)
  | Comp_branch of im_expr * anf_expr * anf_expr (* if c then ... else ... *)
  | Comp_func of ident list * anf_expr (* fun x y ... -> ... *)
  | Comp_tuple of im_expr list
[@@deriving eq, show { with_path = false }, qcheck]

and anf_expr =
  | Anf_comp_expr of comp_expr (* Atomic Computable Expression *)
  | Anf_let of rec_flag * ident * comp_expr * anf_expr (* let x = cexpr in anf_expr *)
[@@deriving eq, show { with_path = false }, qcheck]

type astructure_item =
  | Anf_str_eval of anf_expr
  | Anf_str_value of rec_flag * ident * anf_expr
[@@deriving eq, show { with_path = false }, qcheck]

type aprogram = astructure_item list [@@deriving eq, show { with_path = false }, qcheck]

let normalise_const = function
  | Const_integer e -> Imm_num e
  | Const_char e ->
    Imm_ident (String.make 1 e)
    (* i guess a standalone immediate type is redundant
    *)
  | Const_string e -> Imm_ident e
;;

let get_new_temp_reg =
  let counter = ref 0 in
  fun () ->
    let name = "t_" ^ string_of_int !counter in
    counter := !counter + 1;
    name
;;

let rec collect_params_and_body expr acc =
  match expr with
  | Exp_fun ((Pattern.Pat_var p, []), body) -> collect_params_and_body body (p :: acc)
  | _ -> List.rev acc, expr
;;

let rec norm_comp expr (k : comp_expr -> anf_expr) : anf_expr =
  match expr with
  | Exp_ident e -> k (Comp_imm (Imm_ident e))
  | Exp_constant c -> k (Comp_imm (normalise_const c))
  | Exp_tuple (expr1, expr2, rest_list) ->
    let all_exprs = expr1 :: expr2 :: rest_list in
    norm_list_to_imm all_exprs (fun imm_list -> k (Comp_tuple imm_list))
  | Exp_apply (Exp_ident op, Exp_tuple (expr1, expr2, []))
    when List.mem op [ "+"; "-"; "*"; "="; "<"; ">"; "<="; ">="; "<>" ] ->
    norm_to_imm expr1 (fun v1 ->
      norm_to_imm expr2 (fun v2 ->
        let ce =
          match v1, v2 with
          | Imm_num n1, Imm_num n2 ->
            (match op with
             | "+" -> Comp_imm (Imm_num (n1 + n2))
             | "-" -> Comp_imm (Imm_num (n1 - n2))
             | "*" -> Comp_imm (Imm_num (n1 * n2))
             | _ -> Comp_binop (op, v1, v2))
          | _, _ -> Comp_binop (op, v1, v2)
        in
        k ce))
  | Exp_apply (_, _) ->
    let rec collect_args_and_func expr acc =
      match expr with
      | Exp_apply (f, arg) -> collect_args_and_func f (arg :: acc)
      | f -> f, acc
    in
    let func_expr, args_exprs = collect_args_and_func expr [] in
    norm_to_imm func_expr (fun func_imm ->
      norm_list_to_imm args_exprs (fun args_imms -> k (Comp_app (func_imm, args_imms))))
  | Exp_if (cond, then_, Some else_) ->
    norm_to_imm cond (fun cond_imm ->
      let then_anf = norm_body then_ in
      let else_anf = norm_body else_ in
      k (Comp_branch (cond_imm, then_anf, else_anf)))
  | Exp_fun _ ->
    let params, body = collect_params_and_body expr [] in
    if params = [] then failwith "Function with no parameters found";
    let body_anf = norm_body body in
    k (Comp_func (params, body_anf))
  | Exp_let (rec_flag, (first_binding, other_bindings), body) ->
    if other_bindings <> [] then failwith "`let ... and ...` is not supported in ANF yet";
    let { pat; expr = vb_expr } = first_binding in
    (match pat with
     | Pattern.Pat_var x ->
       norm_comp vb_expr (function
         | (Comp_func _ | Comp_tuple _) as ce ->
           let temp_name = get_new_temp_reg () in
           let body_anf = norm_comp body k in
           Anf_let
             ( Nonrecursive
             , temp_name
             , ce
             , Anf_let (rec_flag, x, Comp_imm (Imm_ident temp_name), body_anf) )
         | (Comp_imm _ | Comp_binop _ | Comp_app _ | Comp_branch _) as ce ->
           let body_anf = norm_comp body k in
           Anf_let (rec_flag, x, ce, body_anf))
     | Pattern.Pat_construct ("()", None) ->
       norm_comp vb_expr (fun ce ->
         let body_anf = norm_comp body k in
         Anf_let (Nonrecursive, "_", ce, body_anf))
     | _ -> failwith ("Unsupported pattern in `let`-binding: " ^ Pattern.show pat))
  | _ -> failwith "unsupported expression in ANF normaliser"

and norm_to_imm expr (k : im_expr -> anf_expr) : anf_expr =
  norm_comp expr (fun ce ->
    match ce with
    | Comp_imm imm -> k imm
    | _ ->
      let temp_name = get_new_temp_reg () in
      Anf_let (Nonrecursive, temp_name, ce, k (Imm_ident temp_name)))

and norm_list_to_imm expr_list (k : im_expr list -> anf_expr) : anf_expr =
  match expr_list with
  | [] -> k []
  | hd :: tl ->
    norm_to_imm hd (fun result_hd ->
      norm_list_to_imm tl (fun result_tl -> k (result_hd :: result_tl)))

and norm_body expr = norm_to_imm expr (fun imm -> Anf_comp_expr (Comp_imm imm))

let norm_item (item : structure_item) : astructure_item =
  match item with
  | Str_eval expr ->
    let body_anf = norm_body expr in
    Anf_str_eval body_anf
  | Str_value (rec_flag, (first_binding, other_bindings)) ->
    if other_bindings <> []
    then failwith "Mutually recursive `let ... and ...` bindings are not supported yet.";
    let { pat; expr } = first_binding in
    (match pat with
     | Pattern.Pat_var name ->
       let body_anf = norm_body expr in
       Anf_str_value (rec_flag, name, body_anf)
     | _ ->
       failwith
         "Unsupported pattern in a top-level let-binding. Only simple variables are \
          allowed.")
  | _ -> failwith "Unsupported top-level structure item."
;;

let anf_program program = List.map norm_item program
