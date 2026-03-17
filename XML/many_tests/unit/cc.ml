(** Copyright 2026,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Middleend.Cc
open Common.Parser
open Common.Pprinter

let to_cc str =
    let prog = parse_str str in
    let ccprog = cc_program prog in
    (* let aprog = anf_program ccprog in
    let llprog = lambda_lift_program aprog in *)
    pprint_program Format.std_formatter ccprog


let%expect_test "020closures.pdf" =
to_cc {|
  let fac n =
    let rec fack n k =
      if n <= 1 then k 1
      else fack (n-1) (fun m -> k (m * n))
    in
    fack n (fun x -> x)
|};
[%expect {|
  let fac = (fun n -> (let rec fack = (fun n k -> (if n <= 1
    then (k 1)
    else ((fack n - 1) (((fun k n m -> (k m * n)) k) n)))) in ((fack n) (fun x -> x))));; |}]


let%expect_test "LN_CC_1" =
to_cc {|
  let f x =
    let g y = x
    in
    g
|};
[%expect {|
  let f = (fun x -> (let g = ((fun x y -> x) x) in (g x)));; |}]


let%expect_test "LN_CC_2" =
to_cc {|
  fun x -> x + a + b
|};
[%expect {|
  ((fun a b x -> x + a + b) a) b ;; |}]


let%expect_test "LN_CC_3" =
to_cc {|
  let mkPair = fun x ->
    let add = fun y -> x + y in
    let mul = fun z -> x * z in
    (add, mul)
|};
[%expect {|
  let mkPair = (fun x -> (let add = ((fun x y -> x + y) x) in (let mul = ((fun x z -> x * z) x) in ((add x), (mul x)))));; |}]


let%expect_test "if" =
to_cc {|
  let f =
    let ret1 = 1 in
    let ret2 = 2 in
    let greater_10 x = if x > 10 then ret1 else ret2 in
    greater_10
|};
[%expect {|
  let f = (let ret1 = 1 in (let ret2 = 2 in (let greater_10 = (((fun ret1 ret2 x -> (if x > 10
    then ret1
    else ret2)) ret1) ret2) in ((greater_10 ret1) ret2))));; |}]


let%expect_test "tuple" =
to_cc {|
  let tuples =
    let a, b = 10, 20 in
    let to_tuple x = x, a, b in
    to_tuple
|};
[%expect {|
  let tuples = (let (a, b) = (10, 20) in (let to_tuple = (((fun a b x -> (x, a, b)) a) b) in ((to_tuple a) b)));; |}]


let%expect_test "func" =
to_cc {|
  let f x y =
    ((fun x -> fun x -> y) x) x
|};
[%expect {|
  let f = (fun x y -> (((fun y x -> ((fun y x -> y) y)) y) x) x);; |}]


let%expect_test "load" =
to_cc {|
  let f x =
    let g y =
      let (a, b) = x, y in
      a in g
|};
[%expect{| let f = (fun x -> (let g = ((fun x y -> (let (a, b) = (x, y) in a)) x) in (g x)));; |}]


let%expect_test "match" =
to_cc {|
  let f x =
    let y = 10 in
    let g z =
      (match x with
      | Some _ -> y
      | _ -> z)
    in
    g
|};
[%expect{|
  let f = (fun x -> (let y = 10 in (let g = (((fun x y z -> (match x with
    | Some _ -> y
    | _ -> z)) x) y) in ((g x) y))));; |}]

  