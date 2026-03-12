(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EML_lib.Frontend.Parser
open EML_lib.Frontend.Ast

let parse_test input =
  match parse input with
  | Ok ast -> Printf.printf "%s\n" (show_program ast)
  | Error fail -> Printf.printf "Ошибка: %s\n" fail
;;

let%expect_test "factorial" =
  parse_test
    {| let rec fac n =
  if n <= 1
  then 1
  else let n1 = n-1 in
       let m = fac n1 in
       n*m

let main = fac 4 |};
  [%expect
    {|
      [(SValue (Rec,
          ((PatVariable "fac"),
           (ExpLambda ((PatVariable "n"), [],
              (ExpBranch (
                 (ExpBinOper (LowestEqual, (ExpIdent "n"), (ExpConst (ConstInt 1))
                    )),
                 (ExpConst (ConstInt 1)),
                 (Some (ExpLet (NonRec,
                          ((PatVariable "n1"),
                           (ExpBinOper (Minus, (ExpIdent "n"),
                              (ExpConst (ConstInt 1))))),
                          [],
                          (ExpLet (NonRec,
                             ((PatVariable "m"),
                              (ExpApply ((ExpIdent "fac"), (ExpIdent "n1")))),
                             [],
                             (ExpBinOper (Multiply, (ExpIdent "n"), (ExpIdent "m")
                                ))
                             ))
                          )))
                 ))
              ))),
          []));
        (SValue (NonRec,
           ((PatVariable "main"),
            (ExpApply ((ExpIdent "fac"), (ExpConst (ConstInt 4))))),
           []))
        ]
|}]
;;

let%expect_test "factorial" =
  parse_test "let rec factorial n = if n < 2 then 1 else n * factorial(n - 1);;";
  [%expect
    {|
  [(SValue (Rec,
      ((PatVariable "factorial"),
       (ExpLambda ((PatVariable "n"), [],
          (ExpBranch (
             (ExpBinOper (LowerThan, (ExpIdent "n"), (ExpConst (ConstInt 2)))),
             (ExpConst (ConstInt 1)),
             (Some (ExpBinOper (Multiply, (ExpIdent "n"),
                      (ExpApply ((ExpIdent "factorial"),
                         (ExpBinOper (Minus, (ExpIdent "n"),
                            (ExpConst (ConstInt 1))))
                         ))
                      )))
             ))
          ))),
      []))
    ]
|}]
;;

let%expect_test "fibonacci" =
  parse_test "let rec fibo n = if n < 2 then 1 else fibo(n - 1) + fibo(n - 2) ;;";
  [%expect
    {|
  [(SValue (Rec,
      ((PatVariable "fibo"),
       (ExpLambda ((PatVariable "n"), [],
          (ExpBranch (
             (ExpBinOper (LowerThan, (ExpIdent "n"), (ExpConst (ConstInt 2)))),
             (ExpConst (ConstInt 1)),
             (Some (ExpBinOper (Plus,
                      (ExpApply ((ExpIdent "fibo"),
                         (ExpBinOper (Minus, (ExpIdent "n"),
                            (ExpConst (ConstInt 1))))
                         )),
                      (ExpApply ((ExpIdent "fibo"),
                         (ExpBinOper (Minus, (ExpIdent "n"),
                            (ExpConst (ConstInt 2))))
                         ))
                      )))
             ))
          ))),
      []))
    ]
|}]
;;

let%expect_test "lambda_test" =
  parse_test "let add x = fun y -> x + y;;";
  [%expect
    {|
  [(SValue (NonRec,
      ((PatVariable "add"),
       (ExpLambda ((PatVariable "x"), [],
          (ExpLambda ((PatVariable "y"), [],
             (ExpBinOper (Plus, (ExpIdent "x"), (ExpIdent "y")))))
          ))),
      []))
    ]
|}]
;;

let%expect_test "test_tuple" =
  parse_test "let x = (1, 2, true) in x;;";
  [%expect
    {|
  [(SEval
      (ExpLet (NonRec,
         ((PatVariable "x"),
          (ExpTuple ((ExpConst (ConstInt 1)), (ExpConst (ConstInt 2)),
             [(ExpConst (ConstBool true))]))),
         [], (ExpIdent "x"))))
    ]
|}]
;;

let%expect_test "test_list" =
  parse_test "let arr = [1;2;true]";
  [%expect
    {|
     [(SValue (NonRec,
         ((PatVariable "arr"),
          (ExpConstruct ("::",
             (Some (ExpTuple ((ExpConst (ConstInt 1)),
                      (ExpConstruct ("::",
                         (Some (ExpTuple ((ExpConst (ConstInt 2)),
                                  (ExpConstruct ("::",
                                     (Some (ExpTuple ((ExpConst (ConstBool true)),
                                              (ExpConstruct ("[]", None)),
                                              [])))
                                     )),
                                  [])))
                         )),
                      [])))
             ))),
         []))
       ]
|}]
;;

let%expect_test "test_one_element_in_tuple" =
  parse_test "let x = (666)";
  [%expect
    {|
     [(SValue (NonRec, ((PatVariable "x"), (ExpConst (ConstInt 666))), []))]
|}]
;;

let%expect_test "test_sum_two_args" =
  parse_test "let sum x y = x + y";
  [%expect
    {|
[(SValue (NonRec,
    ((PatVariable "sum"),
     (ExpLambda ((PatVariable "x"), [(PatVariable "y")],
        (ExpBinOper (Plus, (ExpIdent "x"), (ExpIdent "y")))))),
    []))
  ]
|}]
;;

let%expect_test "test_annotate_type_1" =
  parse_test "let sum (x:int) (y:int) = x + y";
  [%expect
    {|
[(SValue (NonRec,
    ((PatVariable "sum"),
     (ExpLambda ((PatType ((PatVariable "x"), (TyPrim "int"))),
        [(PatType ((PatVariable "y"), (TyPrim "int")))],
        (ExpBinOper (Plus, (ExpIdent "x"), (ExpIdent "y")))))),
    []))
  ]
|}]
;;

let%expect_test "test_annotate_type_2" =
  parse_test "let (a : int list) = [] ";
  [%expect
    {|
[(SValue (NonRec,
    ((PatType ((PatVariable "a"), (TyList (TyPrim "int")))),
     (ExpConstruct ("[]", None))),
    []))
  ]
|}]
;;

let%expect_test "test_minus" =
  parse_test "-1 -2 - (-1) -(3)";
  [%expect
    {|
[(SEval
    (ExpBinOper (Minus,
       (ExpBinOper (Minus,
          (ExpBinOper (Minus,
             (ExpUnarOper (Negative, (ExpConst (ConstInt 1)))),
             (ExpConst (ConstInt 2)))),
          (ExpUnarOper (Negative, (ExpConst (ConstInt 1)))))),
       (ExpConst (ConstInt 3)))))
  ]
 |}]
;;

let%expect_test "test_unit" =
  parse_test "let () = print_int 5";
  [%expect
    {|
[(SValue (NonRec,
    ((PatConstruct ("()", None)),
     (ExpApply ((ExpIdent "print_int"), (ExpConst (ConstInt 5))))),
    []))
  ]
 |}]
;;

let%expect_test "custom_infix_operator" =
  parse_test {| let ( ** ) x y = x * y in 2 ** 3 |};
  [%expect
    {|
[(SEval
    (ExpLet (NonRec,
       ((PatVariable "**"),
        (ExpLambda ((PatVariable "x"), [(PatVariable "y")],
           (ExpBinOper (Multiply, (ExpIdent "x"), (ExpIdent "y")))))),
       [],
       (ExpBinOper ((Custom "**"), (ExpConst (ConstInt 2)),
          (ExpConst (ConstInt 3))))
       )))
  ]
 |}]
;;

let%expect_test "custom_power_operator_is_right_associative" =
  parse_test {| let ( ** ) x y = x * y in 2 ** 3 ** 4 |};
  [%expect
    {|
[(SEval
    (ExpLet (NonRec,
       ((PatVariable "**"),
        (ExpLambda ((PatVariable "x"), [(PatVariable "y")],
           (ExpBinOper (Multiply, (ExpIdent "x"), (ExpIdent "y")))))),
       [],
       (ExpBinOper ((Custom "**"), (ExpConst (ConstInt 2)),
          (ExpBinOper ((Custom "**"), (ExpConst (ConstInt 3)),
             (ExpConst (ConstInt 4))))
          ))
       )))
  ]
|}]
;;
