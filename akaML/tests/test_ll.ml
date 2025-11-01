[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Parser
open Pprinter
open Middleend

let run str =
  match parse str with
  | Ok ast -> Format.printf "%a \n" pp_structure (Lambda_lifting.lambda_lifting ast)
  | Error error -> Format.printf "%s" error
;;

let%expect_test "simple ll" =
  run
    {|
  let foo a =
    let fn = (fun a b -> a + b) a in
    fn 3
  ;;
  |};
  [%expect
    {|
  let ll_0 a b = ( + ) a b;;
  let foo a = let fn = ll_0 a in fn 3;;
  |}]
;;

let%expect_test "fac ll" =
  run
    {|
  let fac n =
    let rec fack n k =
    if ( <= ) n 1 then k 1
    else fack (( - ) n 1) ((fun k n m -> k (( * ) m n)) k n) in
    fack n (fun x -> x)
  ;;
  |};
  [%expect
    {|
  let ll_2 k n m = k (( * ) m n);;
  let ll_1 n k = if ( <= ) n 1 then k 1 else ll_0 (( - ) n 1) (ll_2 k n);;
  let rec ll_0 = ll_1;;
  let ll_3 x = x;;
  let fac n = ll_0 n ll_3;;
  |}]
;;

let%expect_test "nonrecursive multiple lets" =
  run
    {|
  let foo x =
    let bar = (fun x y -> x + y) x
    and baz = 2 in
    bar 2 + baz
  ;;
  |};
  [%expect
    {|
  let ll_0 x y = ( + ) x y;;
  let foo x = let bar = ll_0 x
              and baz = 2 in ( + ) (bar 2) baz;;
  |}]
;;

let%expect_test "recursive multiple lets" =
  run
    {|
  let foo x =
    let rec bar = (fun x y -> x + y) x
    and baz c = c + bar 5 in
    bar 5 + baz 6
  ;;
  |};
  [%expect
    {|
  let ll_2 x y = ( + ) x y;;
  let ll_3 c = ( + ) c (ll_0 5);;
  let rec ll_0 = ll_2 x
  and ll_1 = ll_3;;
  let foo x = ( + ) (ll_0 5) (ll_1 6);;
  |}]
;;

let%expect_test "nested ll" =
  run
    {|
  let outer x =
    let mid =
      (fun x y ->
        let inner = (fun x y z -> x + y + z) x y in
        inner 3)
        x
    in
    mid 4
  ;;
  |};
  [%expect
    {|
  let ll_1 x y z = ( + ) (( + ) x y) z;;
  let ll_0 x y = let inner = ll_1 x y in inner 3;;
  let outer x = let mid = ll_0 x in mid 4;;
  |}]
;;

let%expect_test "if then else with ll" =
  run
    {|
  let foo flag a b = if flag then (fun a x -> a + x) a else (fun b x -> b + x) b
  |};
  [%expect
    {|
  let ll_0 a x = ( + ) a x;;
  let ll_1 b x = ( + ) b x;;
  let foo flag a b = if flag then ll_0 a else ll_1 b;;
  |}]
;;

let%expect_test "function ll" =
  run
    {|
  let foo = function
    | 0 -> 0
    | _ ->
      let rec fn = function
        | 1 -> 1
        | a -> fn (a - 1)
      in
      fn 3
  ;;
  |};
  [%expect
    {|
  let ll_1 = function
             | 1 -> 1
             | a -> ll_0 (( - ) a 1);;
  let rec ll_0 = ll_1;;
  let foo = function
            | 0 -> 0
            | _ -> ll_0 3;;
  |}]
;;

let%expect_test "match exp ll" =
  run
    {|
  let f x =
    match x with
    | Some y -> (fun y z -> y + z) y
    | None -> fun z -> z
  ;;
  |};
  [%expect
    {|
  let ll_0 y z = ( + ) y z;;
  let ll_1 z = z;;
  let f x = match x with
            | Some (y) -> ll_0 y
            | None -> ll_1;;
  |}]
;;

let%expect_test "sequence with ll" =
  run
    {|
  let g x =
    print_int x;
    let h = (fun x y -> x + y) x in
    h 10
  ;;
  |};
  [%expect
    {|
  let ll_0 x y = ( + ) x y;;
  let g x = print_int x; (let h = ll_0 x in h 10);;
  |}]
;;

let%expect_test "tuple ll" =
  run
    {|
  let pair_sum a b =
    let f = (fun a b (x, y) -> a + b + x + y) a b in
    f (1, 2)
  ;;
  |};
  [%expect
    {|
  let ll_0 a b ( x, y ) = ( + ) (( + ) (( + ) a b) x) y;;
  let pair_sum a b = let f = ll_0 a b in f ( 1, 2 );;
  |}]
;;
