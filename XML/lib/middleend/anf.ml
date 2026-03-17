(** Copyright 2024,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast.Expression
open Common.Ast.Constant
open Common.Ast.Structure
open Common.Ast
open Common.Parser

(* Immediate, atomic expressions that do not require the reduction *)
type im_expr =
  | Imm_num of int
  | Imm_ident of ident
[@@deriving show { with_path = false }]

(* Complex/Computable expression *)
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
[@@deriving show { with_path = false }]

and anf_expr =
  | Anf_comp_expr of comp_expr
  | Anf_let of rec_flag * ident * comp_expr * anf_expr
[@@deriving show { with_path = false }]

type astructure_item =
  | Anf_str_eval of anf_expr
  | Anf_str_value of rec_flag * ident * anf_expr
[@@deriving show { with_path = false }]

type aprogram = astructure_item list [@@deriving show { with_path = false }]

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

type 'a r = ('a, anf_error) result

let ( let* ) = Result.bind
let ( let+ ) x f = Result.map f x
let ok x = Ok x
let err e = Error e

let pp_anf_error = function
  | `Only_simple_var_params ->
    "Only simple variable patterns are allowed in function parameters"
  | `Func_no_params -> "Function with no parameters found"
  | `Let_and_not_supported -> "let ... and ... is not supported in ANF yet"
  | `Unsupported_let_pattern s -> "Unsupported pattern in let-binding: " ^ s
  | `Unsupported_expr_in_normaliser -> "unsupported expression in ANF normaliser"
  | `Mutual_rec_not_supported ->
    "Mutually recursive let ... and ... bindings are not supported yet."
  | `Unsupported_toplevel_let ->
    "Unsupported pattern in a top-level let-binding. Only simple variables are allowed."
  | `Unsupported_toplevel_item -> "Unsupported top-level structure item."
;;

let normalise_const = function
  | Const_integer e -> Imm_num e
  | Const_char e -> Imm_ident (String.make 1 e)
  | Const_string e -> Imm_ident e
;;

(* FIX: Do not flatten tuples in arguments. Treat tuple as a single value. *)
let flatten_arg_expr e = [ e ]

let pat_vars = function
  | Pattern.Pat_var p -> ok [ p ]
  | _ -> err `Only_simple_var_params
;;

let rec collect_params_and_body expr acc =
  match expr with
  | Exp_fun ((first_pat, rest_pats), body) ->
    let* v1 = pat_vars first_pat in
    let* vrest =
      let rec go acc = function
        | [] -> ok (List.rev acc)
        | p :: tl ->
          let* vs = pat_vars p in
          go (List.rev_append vs acc) tl
      in
      go [] rest_pats
    in
    let* res = collect_params_and_body body (acc @ v1 @ vrest) in
    ok res
  | _ -> ok (acc, expr)
;;

type nstate = { next : int }

let initial_state = { next = 0 }

let fresh (st : nstate) : ident * nstate =
  let name = "t_" ^ string_of_int st.next in
  name, { next = st.next + 1 }
;;

(* Helper to generate bindings for patterns (tuples, etc) *)
let rec bind_pat (pat : Pattern.t) (source_imm : im_expr) (body : anf_expr) (st : nstate)
  : (anf_expr * nstate) r
  =
  match pat with
  | Pattern.Pat_var x -> ok (Anf_let (Nonrecursive, x, Comp_imm source_imm, body), st)
  | Pattern.Pat_any | Pattern.Pat_construct ("()", None) -> ok (body, st)
  | Pattern.Pat_tuple (p1, p2, ps) ->
    let pats = p1 :: p2 :: ps in
    let word_size = 8 in
    let rec loop i current_body st_iter remaining_pats =
      let helper = function
        | [] -> ok (current_body, st_iter)
        | p :: rest ->
          let* body_rest, st_next = loop (i + 1) current_body st_iter rest in
          let tmp_field, st_tmp = fresh st_next in
          (* Recursively bind the sub-pattern to the temp field *)
          let* body_bind, st_bind = bind_pat p (Imm_ident tmp_field) body_rest st_tmp in
          (* let tmp = source[offset] *)
          let offset = i * word_size in
          ok
            ( Anf_let (Nonrecursive, tmp_field, Comp_load (source_imm, offset), body_bind)
            , st_bind )
      in
      helper remaining_pats
    in
    loop 0 body st pats
  | _ -> err (`Unsupported_let_pattern (Pattern.show pat))
;;

let rec norm_comp expr (k : comp_expr -> nstate -> (anf_expr * nstate) r) (st : nstate)
  : (anf_expr * nstate) r
  =
  match expr with
  | Exp_ident e -> k (Comp_imm (Imm_ident e)) st
  | Exp_constant c -> k (Comp_imm (normalise_const c)) st
  | Exp_tuple (expr1, expr2, rest_list) ->
    let all_exprs = expr1 :: expr2 :: rest_list in
    norm_list_to_imm all_exprs (fun imm_list st -> k (Comp_alloc imm_list) st) st
  | Exp_apply (Exp_apply (Exp_ident op, expr1), expr2) when is_operator_char op.[0] ->
    norm_to_imm
      expr1
      (fun v1 ->
         norm_to_imm expr2 (fun v2 ->
           fun st ->
           let ce = Comp_app (Imm_ident op, [ v1; v2 ]) in
           k ce st))
      st
  | Exp_apply (func_expr, arg_expr) ->
    norm_to_imm
      func_expr
      (fun func_imm ->
         let flat_args = flatten_arg_expr arg_expr in
         norm_list_to_imm flat_args (fun args_imms st ->
           k (Comp_app (func_imm, args_imms)) st))
      st
  | Exp_if (cond, then_, Some else_) ->
    norm_to_imm
      cond
      (fun cond_imm ->
         fun st ->
         let* then_anf, st1 = norm_body then_ st in
         let* else_anf, st2 = norm_body else_ st1 in
         k (Comp_branch (cond_imm, then_anf, else_anf)) st2)
      st
  | Exp_fun _ ->
    let* params, body = collect_params_and_body expr [] in
    (match params with
     | [] -> err `Func_no_params
     | _ ->
       let* body_anf, st' = norm_body body st in
       k (Comp_func (params, body_anf)) st')
  | Exp_let (rec_flag, (first_binding, other_bindings), body) ->
    let* () =
      match other_bindings with
      | [] -> ok ()
      | _ -> err `Let_and_not_supported
    in
    let { pat; expr = vb_expr } = first_binding in
    norm_comp
      vb_expr
      (fun ce st ->
         let scrutinee, st1 = fresh st in
         let* body_anf, st2 = norm_comp body k st1 in
         let* match_anf, st3 = bind_pat pat (Imm_ident scrutinee) body_anf st2 in
         ok (Anf_let (rec_flag, scrutinee, ce, match_anf), st3))
      st
  | _ -> err `Unsupported_expr_in_normaliser

and norm_to_imm expr (k : im_expr -> nstate -> (anf_expr * nstate) r) (st : nstate)
  : (anf_expr * nstate) r
  =
  norm_comp
    expr
    (fun ce st' ->
       match ce with
       | Comp_imm imm -> k imm st'
       | _ ->
         let tmp, st1 = fresh st' in
         let* body, st2 = k (Imm_ident tmp) st1 in
         ok (Anf_let (Nonrecursive, tmp, ce, body), st2))
    st

and norm_list_to_imm
      expr_list
      (k : im_expr list -> nstate -> (anf_expr * nstate) r)
      (st : nstate)
  : (anf_expr * nstate) r
  =
  match expr_list with
  | [] -> k [] st
  | hd :: tl ->
    norm_to_imm
      hd
      (fun imm st1 ->
         let* res = norm_list_to_imm tl (fun imms st2 -> k (imm :: imms) st2) st1 in
         ok res)
      st

and norm_body expr (st : nstate) : (anf_expr * nstate) r =
  norm_to_imm expr (fun imm st -> ok (Anf_comp_expr (Comp_imm imm), st)) st
;;

let norm_item (item : structure_item) (st : nstate) : (astructure_item * nstate) r =
  match item with
  | Str_eval expr ->
    let* body_anf, st' = norm_body expr st in
    ok (Anf_str_eval body_anf, st')
  | Str_value (rec_flag, (first_binding, other_bindings)) ->
    let* () =
      match other_bindings with
      | [] -> ok ()
      | _ -> err `Mutual_rec_not_supported
    in
    let { pat; expr } = first_binding in
    (match pat with
     | Pattern.Pat_var name ->
       let* body_anf, st' = norm_body expr st in
       ok (Anf_str_value (rec_flag, name, body_anf), st')
     | _ -> err `Unsupported_toplevel_let)
;;

let anf_program_res (program : structure_item list) : (aprogram, anf_error) result =
  let step (st, acc) item =
    let* it, st' = norm_item item st in
    ok (st', it :: acc)
  in
  let* _, rev_items =
    List.fold_left
      (fun acc item ->
         let* st, xs = acc in
         step (st, xs) item)
      (ok (initial_state, []))
      program
  in
  ok (List.rev rev_items)
;;

let anf_program (program : structure_item list) : aprogram =
  match anf_program_res program with
  | Ok p -> p
  | Error e -> invalid_arg (pp_anf_error e)
;;
