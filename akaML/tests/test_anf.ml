[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Anf
open Parser

let run str =
  match parse str with
  | Ok ast -> Format.printf "%a" Anf_pprinter.pp_a_structure (Anf_core.anf_structure ast)
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
  let a = let temp0 = 1 + 2 in temp0;;
  |}]
;;

let%expect_test "ANF several binary operations" =
  run
    {|
  let a = 1 + 2 + 3;;
  |};
  [%expect
    {|
  let a = let temp0 = 1 + 2 in (let temp1 = temp0 + 3 in temp1);;
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
  let f = fun a -> (let temp0 = a in temp0);;
  let a = let temp1 = f 1 in temp1;;
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
    fun a -> (fun b -> (let temp0 = a + b in (let temp1 = temp0 in temp1)))
  ;;
  let a = let temp2 = f 1 in (let temp3 = temp2 2 in temp3);;
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
    fun n -> (let temp0 = n = 0 in
    (let temp4 =
    (if temp0 then 1 else (let temp1 =
                       n - 1 in (let temp2 = fac temp1 in
    (let temp3 =
    n * temp2 in
    temp3)))) in (let temp5 = temp4 in temp5)));;
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
    fun n -> (let temp0 = n < 2 in
    (let temp6 =
    (if temp0 then n else (let temp1 =
                       n - 1 in (let temp2 = fib temp1 in
    (let temp3 =
    n - 2 in
    (let temp4 =
    fib temp3 in (let temp5 = temp2 + temp4 in
  temp5)))))) in (let temp7 = temp6 in temp7)));;
  |}]
;;
