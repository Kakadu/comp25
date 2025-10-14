[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Anf
open Parser

let run str =
  reset_gen_id ();
  match parse str with
  | Ok ast -> Format.printf "%a" Style.pp_a_structure (anf_structure ast)
  | Error error -> Format.printf "%s" error
;;

let%expect_test "ANF constant" =
  run
    {|
  let a = 1;;
  |};
  [%expect
    {|
  let a = 1;;
  |}]
;;

let%expect_test "ANF binary operation" =
  run
    {|
  let a = 1 + 2;;
  |};
  [%expect
    {|
  let a = let temp1 = 1 + 2 in temp1;;
  |}]
;;

let%expect_test "ANF several binary operations" =
  run
    {|
  let a = 1 + 2 + 3;;
  |};
  [%expect
    {|
  let a = let temp1 = 1 + 2 in (let temp2 = temp1 + 3 in temp2);;
  |}]
;;

let%expect_test "ANF function with 1 argument" =
  run
    {|
  let f a = a;;
  let a = f 1;;
  |};
  [%expect
    {|
  let f = fun a -> (let temp1 = a in temp1);;
  let a = let temp2 = f 1 in temp2;;
  |}]
;;

let%expect_test "ANF function with 2 arguments" =
  run
    {|
  let f a b = a + b;;
  let a = f 1 2;;
  |};
  [%expect
    {|
  let f =
    fun a -> (fun b -> (let temp1 = a + b in (let temp2 = temp1 in temp2)))
  ;;
  let a = let temp3 = f 1 in (let temp4 = temp3 2 in temp4);;
  |}]
;;

let%expect_test "ANF factorial" =
  run
    {|
  let rec fac n = if n = 0 then 1 else n * fac (n - 1);;
  |};
  [%expect
    {|
  let rec fac =
    fun n -> (let temp1 = n = 0 in
    (let temp5 =
    (if temp1 then 1 else (let temp2 =
                       n - 1 in (let temp3 = fac temp2 in
    (let temp4 =
    n * temp3 in
    temp4)))) in (let temp6 = temp5 in temp6)));;
  |}]
;;

let%expect_test "ANF fibonacci" =
  run
    {|
  let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2);;
  |};
  [%expect
    {|
  let rec fib =
    fun n -> (let temp1 = n < 2 in
    (let temp7 =
    (if temp1 then n else (let temp2 =
                       n - 1 in (let temp3 = fib temp2 in
    (let temp4 =
    n - 2 in
    (let temp5 =
    fib temp4 in (let temp6 = temp3 + temp5 in
  temp6)))))) in (let temp8 = temp7 in temp8)));;
  |}]
;;
