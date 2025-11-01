[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Parser
open Pprinter
open Middleend

let run str =
  match parse str with
  | Ok ast ->
    Format.printf "%a \n" pp_structure (Closure_conversion.closure_conversion ast)
  | Error error -> Format.printf "%s" error
;;

let%expect_test "simple cc" =
  run
    {|
  let foo a =
    let fn b = a + b in
    fn 3
  ;;
  |};
  [%expect
    {|
  let foo a = let fn = (fun a b -> ( + ) a b) a in fn 3;;
  |}]
;;

let%expect_test "fac cc" =
  run
    {|
  let fac n =
    let rec fack n k = if n <= 1 then k 1 else fack (n - 1) (fun m -> k (m * n)) in
    fack n (fun x -> x)
  ;;
  |};
  [%expect
    {|
  let fac n =
    let rec fack n k =
    if ( <= ) n 1 then k 1
    else fack (( - ) n 1) ((fun k n m -> k (( * ) m n)) k n) in
    fack n (fun x -> x)
  ;;
  |}]
;;

let%expect_test "nonrecursive multiple lets" =
  run
    {|
  let foo x =
    let bar y = x + y
    and baz = 2 in
    bar 2 + baz
  ;;
  |};
  [%expect
    {|
  let foo x =
    let bar = (fun x y -> ( + ) x y) x
    and baz = 2 in ( + ) (bar 2) baz
  ;;
  |}]
;;

let%expect_test "recursive multiple lets" =
  run
    {|
  let foo x =
    let rec bar y = x + y
    and baz c = c + bar 5 in
    bar 5 + baz 6
  ;;
  |};
  [%expect
    {|
  let foo x =
    let rec bar = (fun x y -> ( + ) x y) x
    and baz c = ( + ) c (bar 5) in
    ( + ) (bar 5) (baz 6)
  ;;
  |}]
;;

let%expect_test "nested cc" =
  run
    {|
  let outer x =
    let mid y =
      let inner z = x + y + z in
      inner 3
    in
    mid 4
  ;;
  |};
  [%expect
    {|
  let outer x =
    let mid =
    (fun x y ->
       (let inner = (fun x y z -> ( + ) (( + ) x y) z) x y in inner 3)) x in
    mid 4
  ;;
  |}]
;;

let%expect_test "if then else with cc" =
  run
    {|
  let foo flag a b = if flag then fun x -> a + x else fun x -> b + x
  |};
  [%expect
    {|
  let foo flag a b =
    if flag then (fun a x -> ( + ) a x) a else (fun b x -> ( + ) b x) b
  ;;
  |}]
;;

let%expect_test "match exp cc" =
  run
    {|
  let f x =
    match x with
    | Some y -> fun z -> y + z
    | None -> fun z -> z
  ;;
  |};
  [%expect
    {|
  let f x =
    match x with
    | Some (y) -> (fun y z -> ( + ) y z) y
    | None -> (fun z -> z)
  ;;
  |}]
;;

let%expect_test "sequence with cc" =
  run
    {|
  let g x =
    print_int x;
    let h y = x + y in
    h 10
  ;;
  |};
  [%expect
    {|
  let g x = print_int x; (let h = (fun x y -> ( + ) x y) x in h 10);;
  |}]
;;

let%expect_test "tuple cc" =
  run
    {|
  let pair_sum a b =
    let f (x, y) = a + b + x + y in
    f (1, 2)
  ;;
  |};
  [%expect
    {|
  let pair_sum a b =
    let f = (fun a b ( x, y ) -> ( + ) (( + ) (( + ) a b) x) y) a b in
    f ( 1, 2 )
  ;;
  |}]
;;
