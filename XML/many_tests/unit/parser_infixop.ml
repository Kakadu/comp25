(** Copyright 2026,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Parser
open Common.Pprinter
open Angstrom

let (let*) = Angstrom.(let*)

let parse_infix ?starts str =
  match parse_string ~consume:All (pass_ws *> pinfix_op ?starts () <* pass_ws) str with
  | Ok v -> Format.printf "%s\n" v
  | Error e -> Format.printf "%s\n" e

let parse str =
  let prog = parse str in
  match prog with
  | Ok v ->  pprint_program Format.std_formatter v
  | Error e -> Format.printf "%s\n" e


(************************** Parse built in operators **************************)

let%expect_test "parse op" =
  parse_infix "*";
  [%expect {| * |}]


let%expect_test "parse op" =
  parse_infix "+";
  [%expect {| + |}]


let%expect_test "parse op" =
  parse_infix "-";
  [%expect {| - |}]


let%expect_test "parse op" =
  parse_infix "/";
  [%expect {| / |}]


let%expect_test "parse op" =
  parse_infix "&&";
  [%expect {| && |}]


let%expect_test "parse op" =
  parse_infix "||";
  [%expect {| || |}]


let%expect_test "parse op" =
  parse_infix ">";
  [%expect {| > |}]


let%expect_test "parse op" =
  parse_infix "<";
  [%expect {| < |}]


let%expect_test "parse op" =
  parse_infix "=";
  [%expect {| = |}]


let%expect_test "don't parse op with starts" =
  parse_infix ~starts:"+" "*";
  [%expect {| : string |}]


(************************** Parse cursom operators **************************)


let%expect_test "invalid custom op" =
  parse_infix "!+";
  [%expect {| : string |}]


let%expect_test "parse custom op" =
  parse_infix "+!";
  [%expect {| +! |}]


let%expect_test "parse custom op" =
  parse_infix "**";
  [%expect {| ** |}]


let%expect_test "parse custom op" =
  parse_infix "*!";
  [%expect {| *! |}]


let%expect_test "parse custom op" =
  parse_infix "++";
  [%expect {| ++ |}]


let%expect_test "parse custom op" =
  parse_infix "<*";
  [%expect {| <* |}]


let%expect_test "parse custom op" =
  parse_infix ">>=";
  [%expect {| >>= |}]


let%expect_test "parse custom with starts" =
  parse_infix ~starts:">" ">>=";
  [%expect {| >>= |}]


let%expect_test "parse custom with starts" =
  parse_infix ~starts:">>" ">>=";
  [%expect {| >>= |}]


let%expect_test "parse custom with starts" =
  parse_infix ~starts:"**" "***+";
  [%expect {| ***+ |}]


let%expect_test "don't parse custom with starts" =
  parse_infix ~starts:"*" ">>=";
  [%expect {| : string |}]


let%expect_test "don't parse custom with starts" =
  parse_infix ~starts:"**+" "***+";
  [%expect {| : string |}]


(************************** Parse custom operators as patterns **************************)

let%expect_test "parse valid infix operator" =
  parse {|
    let (-!) x y = x - y;;
  |};
[%expect{| let (-!) = (fun x y -> x - y);; |}]


let%expect_test "redefinition" =
  parse {|
    let (+) x y = x - y;;
  |};
[%expect{| let (+) = (fun x y -> x - y);; |}]


let%expect_test "redefinition" =
  parse {|
    let (*) x y = x - y;;
    let (*) x y = x + y;;
  |};
[%expect{|
  let (*) = (fun x y -> x - y);;

  let (*) = (fun x y -> x + y);; |}]


let%expect_test "nested" =
  parse {|
    let (+) a b =
      let (-) x y = x + y in
      a - b
  |};
[%expect{| let (+) = (fun a b -> (let (-) = (fun x y -> x + y) in a - b));; |}]



let%expect_test "invalid infix operator" =
  parse {|
    let (a+) x y = x + y;;
  |};
[%expect{| : end_of_input |}]


let%expect_test "invalid infix operator" =
  parse {|
    let (~+) x = - x;;
  |};
[%expect{| : end_of_input |}]


(************************** Parse custom operators as expressions **************************)

let%expect_test "parse infix operator in paren first" =
  parse {|
    let f x y = (**) x y;;
  |};
[%expect{| let f = (fun x y -> x ** y);; |}]


let%expect_test "parse infix operator in paren first" =
  parse {|
    let f x y = (>>=) x y;;
  |};
[%expect{| let f = (fun x y -> x >>= y);; |}]


let%expect_test "parse infix operator in paren second (strange but ok)" =
  parse {|
    let f x y = x (>>=) y;;
  |};
[%expect{| let f = (fun x y -> ((x >>=) y));; |}]


let%expect_test "parse infix operator" =
  parse {|
    let f x y = x >>= y;;
  |};
[%expect{| let f = (fun x y -> x >>= y);; |}]


let%expect_test "parse infix operator in paren third (strange but ok)" =
  parse {|
    let f x y = x y (>>=);;
  |};
[%expect{| let f = (fun x y -> ((x y) >>=));; |}]


let%expect_test "now try function" =
  parse {|
    let (@) f x = f x in
    (@) (fun x -> x) 5;;
  |};
[%expect{|
  let (@) = (fun f x -> (f x)) in (fun x -> x @ 5) ;; |}]


let%expect_test "now try function infix" =
  parse {|
    let (@) f x = f x in
    (fun x -> x) @ 5;;
  |};
[%expect{|
  let (@) = (fun f x -> (f x)) in (fun x -> x @ 5) ;; |}]




(************************** Chain **************************)

let%expect_test "chain **" =
  parse {|
    let f x y = x ** y ** z;;
  |};
[%expect{| let f = (fun x y -> x ** y ** z);; |}]


let%expect_test "chain ** //" =
  parse {|
    let f x y = x ** y // z;;
  |};
[%expect{| let f = (fun x y -> x ** y // z);; |}]


let%expect_test "chain ** ++" =
  parse {|
    let f x y = x ** y ++ z;;
  |};
[%expect{| let f = (fun x y -> x ** y ++ z);; |}]



let%expect_test "chain ++ **" =
  parse {|
    let f x y = x ++ y ** z;;
  |};
[%expect{| let f = (fun x y -> x ++ y ** z);; |}]


let%expect_test "chain ++ ** paren" =
  parse {|
    let f x y = x ++ (y ** z);;
  |};
[%expect{| let f = (fun x y -> x ++ y ** z);; |}]


let%expect_test "chain ++ ** paren 2" =
  parse {|
    let f x y = (x ++ y) ** z;;
  |};
[%expect{| let f = (fun x y -> (x ++ y) ** z);; |}]



let%expect_test "chain different *" =
  parse {|
    let f x y = x * (y *! z *+++ d);;
  |};
[%expect{| let f = (fun x y -> x * (y *! z *+++ d));; |}]


let%expect_test "chain many" =
  parse {|
    let f x y = x * y ++ z *** d +++ c ++ y;;
  |};
[%expect{| let f = (fun x y -> x * y ++ z *** d +++ c ++ y);; |}]


let%expect_test "chain many with parentheses" =
  parse {|
    let f x y = x * ((y ++ z) *** d +++ c) ++ y;;
  |};
[%expect{| let f = (fun x y -> x * ((y ++ z) *** d +++ c) ++ y);; |}]
