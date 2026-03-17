(** Copyright 2026,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)


open Middleend.Cc
open Middleend.Anf
open Middleend.Pprinter
open Middleend.Ll
open Common.Parser

let to_ll str =
    let prog = parse_str str in
    let ccprog = cc_program prog in
    let aprog = anf_program ccprog in
    let llprog = lambda_lift_program aprog in
    print_anf_program Format.std_formatter llprog


let%expect_test "020closures.pdf" =
to_ll {|
  let fac n =
    let rec fack n k =
      if n <= 1 then k 1
      else fack (n-1) (fun m -> k (m * n))
    in
    fack n (fun x -> x)
|};
[%expect {|
  let fac = fun n -> let t_12 = t_11__ll$1 fack n
                       in let t_13 = t_13__ll$3 in let t_14 = t_12 t_13 in t_14;;
  let t_6__ll$2 = fun k n m -> let t_4 = (m * n) in let t_5 = k t_4 in t_5;;
  let t_11__ll$1 = fun fack n
                     k -> let t_0 = (n <= 1)
                            in let t_10 = if t_0 then let t_1 = k 1 in t_1 else
                                            let t_2 = (n - 1)
                                              in let t_3 = fack t_2
                                                   in let t_7 = t_6__ll$2 k
                                                        in let t_8 = t_7 n
                                                             in let t_9 = t_3 t_8
                                                                  in t_9
                                 in t_10;;
  let t_13__ll$3 = fun x -> x;; |}]


let%expect_test "LN_CC_1" =
to_ll {|
  let f x =
    let g y = x
    in
    g
|};
[%expect {|
  let f = fun x -> let t_1 = t_0__ll$1 x in let g = t_1 in let t_2 = g x in t_2;;

  let t_0__ll$1 = fun x y -> x;; |}]


let%expect_test "LN_CC_2" =
to_ll {|
  fun x -> x + a + b
|};
[%expect {|
  let t_3 = t_2__ll$1 a in let t_4 = t_3 b in t_4;;
  let t_2__ll$1 = fun a b x -> let t_0 = (x + a) in let t_1 = (t_0 + b) in t_1;; |}]


let%expect_test "LN_CC_3" =
to_ll {|
  let mkPair = fun x ->
    let add = fun y -> x + y in
    let mul = fun z -> x * z in
    (add, mul)
|};
[%expect {|
  let mkPair = fun x -> let t_2 = t_1__ll$1 x
                          in let add = t_2
                               in let t_5 = t_4__ll$2 x
                                    in let mul = t_5
                                         in let t_6 = add x
                                              in let t_7 = mul x
                                                   in let t_8 = alloc(t_6,
                                                        t_7) in t_8;;
  let t_1__ll$1 = fun x y -> let t_0 = (x + y) in t_0;;
  let t_4__ll$2 = fun x z -> let t_3 = (x * z) in t_3;; |}]


let%expect_test "if" =
to_ll {|
  let f =
    let ret1 = 1 in
    let ret2 = 2 in
    let greater_10 x = if x > 10 then ret1 else ret2 in
    greater_10
|};
[%expect {|
  let f = let t_0 = 1
            in let ret1 = t_0
                 in let t_1 = 2
                      in let ret2 = t_1
                           in let t_5 = t_4__ll$1 ret1
                                in let t_6 = t_5 ret2
                                     in let greater_10 = t_6
                                          in let t_7 = greater_10 ret1
                                               in let t_8 = t_7 ret2 in t_8;;
  let t_4__ll$1 = fun ret1 ret2
                    x -> let t_2 = (x > 10)
                           in let t_3 = if t_2 then ret1 else ret2 in t_3;; |}]


let%expect_test "tuple" =
to_ll {|
  let tuples =
    let a, b = 10, 20 in
    let to_tuple x = x, a, b in
    to_tuple
|};
[%expect {|
  let tuples = let t_0 = alloc(10,  20)
                 in let t_8 = t_0[0]
                      in let a = t_8
                           in let t_7 = t_0[8]
                                in let b = t_7
                                     in let t_3 = t_2__ll$1 a
                                          in let t_4 = t_3 b
                                               in let to_tuple = t_4
                                                    in let t_5 = to_tuple a
                                                         in let t_6 = t_5 b
                                                              in t_6;;
  let t_2__ll$1 = fun a b x -> let t_1 = alloc(x,  a,  b) in t_1;; |}]


let%expect_test "func" =
to_ll {|
  let f x y =
    ((fun x -> fun x -> y) x) x
|};
[%expect {|
  let f = fun x
            y -> let t_3 = t_2__ll$1 y
                   in let t_4 = t_3 x in let t_5 = t_4 x in t_5;;
  let t_0__ll$2 = fun y x -> y;;
  let t_2__ll$1 = fun y x -> let t_1 = t_0__ll$2 y in t_1;; |}]


let%expect_test "load" =
to_ll {|
  let f x =
    let g y =
      let (a, b) = x, y in
      a in g
|};
[%expect{|
  let f = fun x -> let t_4 = t_3__ll$1 x in let g = t_4 in let t_5 = g x in t_5;;

  let t_3__ll$1 = fun x
                    y -> let t_0 = alloc(x,  y)
                           in let t_2 = t_0[0]
                                in let a = t_2
                                     in let t_1 = t_0[8] in let b = t_1 in a;; |}]


let%expect_test "custom operator" =
to_ll {|
  let (@) f x = f x in
   (@) (fun x -> x) 5
|};
[%expect{|
  let t_2 = t_2__ll$2 in let t_3 = att__ll$1 t_2 5 in t_3;;
  let att__ll$1 = fun f x -> let t_0 = f x in t_0;;
  let t_2__ll$2 = fun x -> x;; |}]


let%expect_test "custom operator infix" =
to_ll {|
  let (@) f x = f x in
   (fun x -> x) @ 5
|};
[%expect{|
  let t_2 = t_2__ll$2 in let t_3 = att__ll$1 t_2 5 in t_3;;
  let att__ll$1 = fun f x -> let t_0 = f x in t_0;;
  let t_2__ll$2 = fun x -> x;; |}]


let%expect_test "custom operator 2" =
to_ll {|
  let (@) f x = f x;;
   (@) (fun x -> x) 5;;
|};
[%expect{|
  let att__ll$1 = fun f x -> let t_0 = f x in t_0;;
  let t_2 = t_2__ll$2 in let t_3 = att__ll$1 t_2 5 in t_3;;
  let t_2__ll$2 = fun x -> x;; |}]



let%expect_test "custom operator 2 infix" =
to_ll {|
  let (@) f x = f x;;
   (fun x -> x) @ 5;;
|};
[%expect{|
  let att__ll$1 = fun f x -> let t_0 = f x in t_0;;
  let t_2 = t_2__ll$2 in let t_3 = att__ll$1 t_2 5 in t_3;;
  let t_2__ll$2 = fun x -> x;; |}]


let%expect_test "override operator" =
to_ll {|
  let (+) f x = f x in
   (+) (fun x -> x) 5
|};
[%expect{|
  let t_2 = t_2__ll$2 in let t_3 = pls__ll$1 t_2 5 in t_3;;
  let pls__ll$1 = fun f x -> let t_0 = f x in t_0;;
  let t_2__ll$2 = fun x -> x;; |}]



let%expect_test "override operator 2" =
to_ll {|
  let (+) f x = f x;;
   (+) (fun x -> x) 5;;
|};
[%expect{|
  let pls__ll$1 = fun f x -> let t_0 = f x in t_0;;
  let t_2 = t_2__ll$2 in let t_3 = pls__ll$1 t_2 5 in t_3;;
  let t_2__ll$2 = fun x -> x;; |}]



let%expect_test "override operator after using it" =
to_ll {|
  let g x y = x + y in
  g 5 10;;

  let (+) f x = f x;;
   (+) (fun x -> x) 5;;
|};
[%expect{|
  let t_2 = g__ll$1 5 in let t_3 = t_2 10 in t_3;;
  let pls__ll$2 = fun f x -> let t_4 = f x in t_4;;
  let t_6 = t_6__ll$3 in let t_7 = pls__ll$2 t_6 5 in t_7;;
  let g__ll$1 = fun x y -> let t_0 = (x + y) in t_0;;
  let t_6__ll$3 = fun x -> x;; |}]
