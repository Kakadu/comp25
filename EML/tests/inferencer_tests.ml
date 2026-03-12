(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EML_lib.Middleend.Inferencer
open EML_lib.Frontend.Ast
open EML_lib.Frontend.Parser

let pretty_printer_parse_and_infer s =
  match parse s with
  | Ok parsed ->
    (match run_infer parsed with
     | Ok env ->
       let filtered_env =
         Base.Map.filter_keys env ~f:(fun key ->
           not (List.mem key [ "print_int"; "print_endline"; "print_bool" ]))
       in
       Base.Map.iteri filtered_env ~f:(fun ~key ~data:(Scheme.Scheme (_, ty)) ->
         Format.printf "val %s: %a\n" key pp_ty ty)
     | Error e -> Format.printf "Infer error. %a\n" pp_error e)
  | Error e -> Format.printf "Parsing error. %s\n" e
;;

let pretty_printer_infer_simple_expression expr =
  match infer_simple_expression expr with
  | Ok ty -> Format.printf "%a\n" pp_ty ty
  | Error e -> Format.printf "Infer error. %a\n" pp_error e
;;

let%expect_test "test_factorial" =
  pretty_printer_parse_and_infer
    {| let rec fac n =
  if n <= 1
  then 1
  else let n1 = n-1 in
       let m = fac n1 in
       n*m

let main = fac 4 |};
  [%expect
    {|
    val fac: int -> int
    val main: int|}]
;;

let%expect_test "test_primitives_and_data" =
  pretty_printer_parse_and_infer
    {| let a = 1 + 2
let b = true && false
let c = if true then 1 else 2
let d = (1, true, 'a')
let e = [1; 2; 3]
let f = Some 1
let g = None
|};
  [%expect
    {|
    val a: int
    val b: bool
    val c: int
    val d: (int * bool * char)
    val e: int list
    val f: int option
    val g: t1 option|}]
;;

let%expect_test "test_match_and_recursion" =
  pretty_printer_parse_and_infer
    {| let rec len xs =
  match xs with
  | [] -> 0
  | _::tl -> 1 + len tl

let main = len [1;2;3] |};
  [%expect
    {|
    val len: int list -> int
    val main: int|}]
;;

let%expect_test "test_mutual_recursion" =
  pretty_printer_parse_and_infer
    {| let rec even n =
  if n = 0 then true else odd (n - 1)
and odd n =
  if n = 0 then false else even (n - 1)

let main = even 4 |};
  [%expect
    {|
    val even: int -> bool
    val main: bool
    val odd: int -> bool|}]
;;

let%expect_test "test_annotations" =
  pretty_printer_parse_and_infer
    {| let id = ((fun x -> x) : int -> int)
let main = id 10 |};
  [%expect
    {|
    val id: int -> int
    val main: int|}]
;;

let%expect_test "test_rec_rhs_error" =
  pretty_printer_parse_and_infer {| let rec x = 1 |};
  [%expect
    {|Infer error. Right-hand side error: Right-hand side of let rec must be a lambda expression.|}]
;;

let%expect_test "test_list_constructors_and_match" =
  pretty_printer_parse_and_infer
    {| let rec head_or_zero xs =
  match xs with
  | [] -> 0
  | h::tl -> h

let x = 1 :: []
let main = head_or_zero x |};
  [%expect
    {|
    val head_or_zero: int list -> int
    val main: int
    val x: int list|}]
;;

let%expect_test "test_pattern_option_and_list" =
  pretty_printer_parse_and_infer
    {| let f = function
  | Some (h::tl) -> h
|};
  [%expect {|val f: t2 list option -> t2|}]
;;

let%expect_test "test_annotation_mismatch_error" =
  pretty_printer_parse_and_infer {| let x = (1 : bool) |};
  [%expect {|Infer error. Failed to unify types: int and bool.|}]
;;

let%expect_test "test_unexpected_function_error_branch" =
  pretty_printer_parse_and_infer {| let x = not 1 |};
  [%expect {|Infer error. Failed to unify types: int and bool.|}]
;;

let%expect_test "test_if_without_else_returns_unit_branch" =
  pretty_printer_parse_and_infer {| let x = if true then 1 |};
  [%expect {|Infer error. Failed to unify types: int and unit.|}]
;;

let%expect_test "test_unbound_var" =
  pretty_printer_parse_and_infer "let f = x";
  [%expect {|Infer error. Unbound variable 'x'.|}]
;;

let%expect_test "test_annotate" =
  pretty_printer_parse_and_infer "let sum = fun (x : int) (y : int) -> x + y";
  [%expect {|val sum: int -> int -> int|}]
;;

let%expect_test "test_annotate_fac" =
  pretty_printer_parse_and_infer
    "let rec fac = fun (n : int) (acc : int) -> if n < 2 then acc else fac (n-1) (acc * \
     n);;";
  [%expect {|val fac: int -> int -> int|}]
;;

let%expect_test "test_program_1" =
  pretty_printer_parse_and_infer
    "let div = fun x y -> x / y \n\
    \     let sum = fun x y -> x + y\n\
    \     let res = fun x y z -> div x (sum y z)";
  [%expect
    {|
    val div: int -> int -> int
    val res: int -> int -> int -> int
    val sum: int -> int -> int|}]
;;

let%expect_test "test_program_2" =
  pretty_printer_parse_and_infer
    "let square = fun x -> x * x\n\
    \                                  let result = square 10";
  [%expect
    {|
    val result: int
    val square: int -> int|}]
;;

let%expect_test "test_annotate_error" =
  pretty_printer_parse_and_infer "let sum (x : int) (y : string) = x + y";
  [%expect {|Infer error. Failed to unify types: string and int.|}]
;;

let%expect_test "test_unification_types" =
  pretty_printer_parse_and_infer "fun x -> x + true";
  [%expect {|Infer error. Failed to unify types: bool and int.|}]
;;

let%expect_test "test_option_type_error" =
  pretty_printer_parse_and_infer
    "let f x = Some (x + 1) in let g y = Some (y && true) in f = g";
  [%expect {|Infer error. Failed to unify types: bool and int.|}]
;;

let%expect_test "test_polymorphic_identity" =
  pretty_printer_parse_and_infer
    {| let id x = x
let a = id 1
let b = id true |};
  [%expect
    {|
    val a: int
    val b: bool
    val id: t0 -> t0|}]
;;

let%expect_test "test_polymorphic_tuple_use" =
  pretty_printer_parse_and_infer
    {| let id x = x
let pair = (id 1, id true) |};
  [%expect
    {|
    val id: t0 -> t0
    val pair: (int * bool)|}]
;;

let%expect_test "test_higher_order_function" =
  pretty_printer_parse_and_infer
    {| let apply f x = f x
let inc x = x + 1
let main = apply inc 10 |};
  [%expect
    {|
    val apply: (t1 -> t2) -> t1 -> t2
    val inc: int -> int
    val main: int|}]
;;

let%expect_test "test_lambda_returning_lambda" =
  pretty_printer_parse_and_infer
    {| let add x = fun y -> x + y
let f = add 5 |};
  [%expect
    {|
    val add: int -> int -> int
    val f: int -> int|}]
;;

let%expect_test "test_partial_application" =
  pretty_printer_parse_and_infer
    {| let add x y = x + y
let inc = add 1
let main = inc 10 |};
  [%expect
    {|
    val add: int -> int -> int
    val inc: int -> int
    val main: int|}]
;;

let%expect_test "test_tuple_pattern" =
  pretty_printer_parse_and_infer
    {| let sum_pair (x, y) = x + y
let main = sum_pair (3, 4) |};
  [%expect
    {|
    val main: int
    val sum_pair: (int * int) -> int|}]
;;

let%expect_test "test_nested_let_scope" =
  pretty_printer_parse_and_infer
    {| let x = 10
let f y =
  let x = y + 1 in
  x + y
let main = f 5 |};
  [%expect
    {|
    val f: int -> int
    val main: int
    val x: int|}]
;;

let%expect_test "test_function_composition" =
  pretty_printer_parse_and_infer
    {| let compose f g x = f (g x)
let inc x = x + 1
let double x = x * 2
let main = compose inc double 10 |};
  [%expect
    {|
    val compose: (t3 -> t4) -> (t2 -> t3) -> t2 -> t4
    val double: int -> int
    val inc: int -> int
    val main: int|}]
;;

let%expect_test "test_occurs_check_error" =
  pretty_printer_parse_and_infer {| fun x -> x x |};
  [%expect
    {|Infer error. Occurs check failed. Type variable 't0' occurs inside t0 -> t1.|}]
;;

let%expect_test "test_list_polymorphism" =
  pretty_printer_parse_and_infer
    {| let singleton x = [x]
let a = singleton 1
let b = singleton true |};
  [%expect
    {|
    val a: int list
    val b: bool list
    val singleton: t0 -> t0 list|}]
;;

let%expect_test "test_nonrec_tuple_pattern_binding" =
  pretty_printer_parse_and_infer
    {| let (x, y) = (1, true)
let main = x |};
  [%expect
    {|
    val main: int
    val x: int
    val y: bool|}]
;;

let%expect_test "test_match_option_none_some" =
  pretty_printer_parse_and_infer
    {| let unwrap_or_zero o =
  match o with
  | None -> 0
  | Some x -> x
let main = unwrap_or_zero None |};
  [%expect
    {|
    val main: int
    val unwrap_or_zero: int option -> int|}]
;;

let%expect_test "test_match_list_literal_pattern" =
  pretty_printer_parse_and_infer
    {| let sum2 xs =
  match xs with
  | [a; b] -> a + b
  | _ -> 0
let main = sum2 [1; 2] |};
  [%expect
    {|
    val main: int
    val sum2: int list -> int|}]
;;

let%expect_test "test_lambda_wildcard_and_unit_pattern" =
  pretty_printer_parse_and_infer
    {| let ignore_first _ y = y
let run () = ignore_first 1 42
let main = run () |};
  [%expect
    {|
    val ignore_first: t0 -> t1 -> t1
    val main: int
    val run: unit -> int|}]
;;

let%expect_test "test_rec_lhs_not_variable_error" =
  pretty_printer_parse_and_infer {| let rec Some x = Some 1 |};
  [%expect
    {|Infer error. Left-hand side error: Only variables are allowed on the left-hand side of let rec.|}]
;;

let%expect_test "test_expr_let_rec_in" =
  pretty_printer_parse_and_infer
    {| let main =
  let rec fact n = if n = 0 then 1 else n * fact (n - 1) in
  fact 4 |};
  [%expect
    {|
    val main: int|}]
;;

let%expect_test "test_expr_let_rec_and_in" =
  pretty_printer_infer_simple_expression
    (ExpLet
       ( Rec
       , (PatVariable "f", ExpLambda (PatVariable "x", [], ExpIdent "x"))
       , [ PatVariable "g", ExpLambda (PatVariable "y", [], ExpIdent "y") ]
       , ExpApply (ExpIdent "g", ExpConst (ConstInt 1)) ));
  [%expect {|int|}]
;;

let%expect_test "test_string_const_and_const_pattern" =
  pretty_printer_parse_and_infer
    {| let is_hi s =
  match s with
  | "hi" -> true
  | _ -> false
let main = is_hi "hello" |};
  [%expect
    {|
    val is_hi: string -> bool
    val main: bool|}]
;;

let%expect_test "test_ast_exp_list_empty" =
  pretty_printer_infer_simple_expression (ExpList []);
  [%expect {|t0 list|}]
;;

let%expect_test "test_ast_exp_list_non_empty" =
  pretty_printer_infer_simple_expression
    (ExpList [ ExpConst (ConstInt 1); ExpConst (ConstInt 2) ]);
  [%expect {|int list|}]
;;

let%expect_test "test_ast_exp_option_none" =
  pretty_printer_infer_simple_expression (ExpOption None);
  [%expect {|t0 option|}]
;;

let%expect_test "test_ast_exp_option_some" =
  pretty_printer_infer_simple_expression (ExpOption (Some (ExpConst (ConstInt 1))));
  [%expect {|int option|}]
;;

let%expect_test "test_ast_pattern_option_lambda" =
  pretty_printer_infer_simple_expression
    (ExpLambda (PatOption (Some (PatVariable "x")), [], ExpIdent "x"));
  [%expect {|t0 option -> t0|}]
;;

let%expect_test "test_ast_pattern_list_lambda" =
  pretty_printer_infer_simple_expression
    (ExpLambda (PatList [ PatVariable "x"; PatVariable "y" ], [], ExpIdent "x"));
  [%expect {|t1 list -> t1|}]
;;

let%expect_test "test_ast_pattern_unit_lambda" =
  pretty_printer_infer_simple_expression (ExpLambda (PatUnit, [], ExpConst (ConstInt 1)));
  [%expect {|unit -> int|}]
;;

let%expect_test "custom_infix_operator" =
  pretty_printer_parse_and_infer
    {| let ( ** ) x y = x * y
let main = 2 ** 3 |};
  [%expect
    {|
    val **: int -> int -> int
    val main: int|}]
;;

let%expect_test "custom_infix_bind_like" =
  pretty_printer_parse_and_infer
    {| let ( >>= ) n _ = if n <= 1 then 1 else n * (n - 1)
let main = 3 >>= 0 |};
  [%expect
    {|
    val >>=: int -> t1 -> int
    val main: int|}]
;;

let%expect_test "custom_infix_power" =
  pretty_printer_parse_and_infer
    {| let rec ( ^^ ) x n = if n <= 0 then 1 else x * (x ^^ (n - 1))
let main = 2 ^^ 10 |};
  [%expect
    {|
    val ^^: int -> int -> int
    val main: int|}]
;;

let%expect_test "custom_infix_compose" =
  pretty_printer_parse_and_infer
    {| let ( @@ ) f g = fun x -> f (g x)
let succ x = x + 1
let double x = x * 2
let main = (succ @@ double) 10 |};
  [%expect
    {|
    val @@: (t3 -> t4) -> (t2 -> t3) -> t2 -> t4
    val double: int -> int
    val main: int
    val succ: int -> int|}]
;;
