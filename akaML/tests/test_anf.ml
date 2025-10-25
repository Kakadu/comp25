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

let%expect_test "ANF Pat_any" =
  run
    {|
  let a =
    let _ = 3 in
    0
  ;;
  |};
  [%expect
    {|
  let a = 0;;
  |}]
;;

let%expect_test "ANF binary operation" =
  run
    {|
  let a = 1 + 2;;
  |};
  [%expect
    {|
  let a = 1 + 2;;
  |}]
;;

let%expect_test "ANF several binary operations" =
  run
    {|
  let a = 1 + 2 + 3;;
  |};
  [%expect
    {|
  let a = let temp0 = 1 + 2 in
          temp0 + 3;;
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
  let f = fun a -> a;;
  let a = f 1;;
  |}]
;;

let%expect_test "ANF ifthen" =
  run
    {|
  let foo n = if n < -5 then print_int 0
  |};
  [%expect
    {|
  let foo =
    fun n ->
      (let temp0 = -5 in
      let temp1 = n < temp0 in
      if temp1 then print_int 0);;
  |}]
;;

let%expect_test "ANF tuple" =
  run
    {|
  let tup a = 1, a
  |};
  [%expect
    {|
  let tup = fun a -> ( 1, a );;
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
  let f = fun a -> (fun b -> a + b);;
  let a = f 1 2;;
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
    fun n ->
      (let temp0 = n = 0 in
      if temp0 then 1
      else (let temp1 = n - 1 in
        let temp2 = fac temp1 in
        n * temp2));;
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
    fun n ->
      (let temp0 = n < 2 in
      if temp0 then n
      else (let temp1 = n - 1 in
        let temp2 = fib temp1 in
        let temp3 = n - 2 in
        let temp4 = fib temp3 in
        temp2 + temp4));;
  |}]
;;

let%expect_test "Check elimination: let name = value in name -> value" =
  run
    {|
  let foo =
    let x = 1 in
    let y = 2 in
    x + y
  ;;
  |};
  [%expect
    {|
  let foo = let x = 1 in
            let y = 2 in
            x + y;;
  |}]
;;

let%expect_test
    "Check elimination: let name = value in let orig_name = name in body -> let \
     orig_name = value in body"
  =
  run
    {|
  let foo =
    let a = 1 + 2 in
    let b = 3 - 4 in
    let c = 5 * 6 in
    let d = 7 <= 8 in
    let e = 9 >= 10 in
    let f = 11 = 12 in
    let g = 13 <> 14 in
    a
  ;;
  |};
  [%expect
    {|
  let foo =
    let a = 1 + 2 in
    let b = 3 - 4 in
    let c = 5 * 6 in
    let d = 7 <= 8 in
    let e = 9 >= 10 in
    let f = 11 = 12 in
    let g = 13 <> 14 in
    a;;
  |}]
;;
