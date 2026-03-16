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

let%expect_test "parse valid infix operator" =
  parse {|
    let f x y = x !+ y;;
  |};
[%expect{| : end_of_input |}]

