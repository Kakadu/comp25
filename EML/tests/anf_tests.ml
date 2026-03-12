(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EML_lib.Frontend.Parser
open EML_lib.Middleend.Anf
open EML_lib.Middleend.Anf_pp
open EML_lib.Middleend.Runner
open EML_lib.Middleend.Inferencer
open EML_lib.Middleend.Resolve_builtins

let initial_scope = TypeEnv.keys TypeEnv.initial_env

let parse_and_anf input =
  match parse input with
  | Ok ast ->
    let ast' = resolve_program ast initial_scope in
    (match anf_program ast' with
     | Ok anf_ast -> Printf.printf "%s\n" (show_anf_program anf_ast)
     | Error e -> Printf.printf "ANF error: %s\n" e)
  | Error e -> Printf.printf "Parsing error: %s\n" e
;;

let parse_and_anf_pp input =
  match parse input with
  | Ok ast ->
    let ast' = resolve_program ast initial_scope in
    (match anf_program ast' with
     | Ok anf_ast -> Printf.printf "%s\n" (anf_to_string anf_ast)
     | Error e -> Printf.printf "ANF error: %s\n" e)
  | Error e -> Printf.printf "Parsing error: %s\n" e
;;

let anf_roundtrip_typecheck ~env program_str : (unit, string) Result.t =
  let ( >>= ) = Result.bind in
  parse program_str
  |> Result.map_error (fun s -> "Parse error: " ^ s)
  >>= fun ast ->
  run ast env
  |> Result.map_error (fun e ->
    Format.asprintf "Middleend: %a" EML_lib.Middleend.Runner.pp_error e)
  >>= fun (anf_ast, _env_after) ->
  let printed = anf_to_string anf_ast in
  parse printed
  |> Result.map_error (fun s -> "ANF round-trip parse error: " ^ s)
  >>= fun ast2 ->
  ResultMonad.run (infer_structure env ast2)
  |> Result.map_error (fun e ->
    Format.asprintf
      "ANF round-trip typecheck failed: %a"
      EML_lib.Middleend.Inferencer.pp_error
      e)
  |> Result.map (fun _ -> ())
;;

let%expect_test "001.ml" =
  parse_and_anf "let recfac n = if n<=1 then 1 else n * fac (n-1)";
  [%expect
    {|
[(AnfValue (NonRec,
    ("recfac", 1,
     (AnfExpr
        (ComplexLambda ([(PatVariable "n")],
           (AnfLet (NonRec, "anf_t0",
              (ComplexBinOper (LowestEqual, (ImmediateVar "n"),
                 (ImmediateConst (ConstInt 1)))),
              (AnfExpr
                 (ComplexBranch ((ImmediateVar "anf_t0"),
                    (AnfExpr (ComplexImmediate (ImmediateConst (ConstInt 1)))),
                    (AnfLet (NonRec, "anf_t1",
                       (ComplexBinOper (Minus, (ImmediateVar "n"),
                          (ImmediateConst (ConstInt 1)))),
                       (AnfLet (NonRec, "anf_t2",
                          (ComplexApp ((ImmediateVar "fac"),
                             (ImmediateVar "anf_t1"), [])),
                          (AnfExpr
                             (ComplexBinOper (Multiply, (ImmediateVar "n"),
                                (ImmediateVar "anf_t2"))))
                          ))
                       ))
                    )))
              ))
           )))),
    []))
  ]|}]
;;

let%expect_test "003occurs.ml" =
  parse_and_anf "let fix f = (fun x -> f (fun f -> x x f))  (fun x -> f (fun f -> x x f))";
  [%expect
    {|
[(AnfValue (NonRec,
    ("fix", 1,
     (AnfExpr
        (ComplexLambda ([(PatVariable "f")],
           (AnfLet (NonRec, "anf_t3",
              (ComplexLambda ([(PatVariable "x")],
                 (AnfLet (NonRec, "anf_t1",
                    (ComplexLambda ([(PatVariable "f")],
                       (AnfExpr
                          (ComplexApp ((ImmediateVar "x"),
                             (ImmediateVar "x"), [(ImmediateVar "f")])))
                       )),
                    (AnfExpr
                       (ComplexApp ((ImmediateVar "f"),
                          (ImmediateVar "anf_t1"), [])))
                    ))
                 )),
              (AnfLet (NonRec, "anf_t7",
                 (ComplexLambda ([(PatVariable "x")],
                    (AnfLet (NonRec, "anf_t5",
                       (ComplexLambda ([(PatVariable "f")],
                          (AnfExpr
                             (ComplexApp ((ImmediateVar "x"),
                                (ImmediateVar "x"), [(ImmediateVar "f")])))
                          )),
                       (AnfExpr
                          (ComplexApp ((ImmediateVar "f"),
                             (ImmediateVar "anf_t5"), [])))
                       ))
                    )),
                 (AnfExpr
                    (ComplexApp ((ImmediateVar "anf_t3"),
                       (ImmediateVar "anf_t7"), [])))
                 ))
              ))
           )))),
    []))
  ]|}]
;;

let%expect_test "004let_poly.ml" =
  parse_and_anf "let temp =\n  (fun f -> (f 1, f true)) (fun x -> x)";
  [%expect
    {|
[(AnfValue (NonRec,
    ("temp", 0,
     (AnfLet (NonRec, "anf_t3",
        (ComplexLambda ([(PatVariable "f")],
           (AnfLet (NonRec, "anf_t0",
              (ComplexApp ((ImmediateVar "f"), (ImmediateConst (ConstInt 1)),
                 [])),
              (AnfLet (NonRec, "anf_t1",
                 (ComplexApp ((ImmediateVar "f"),
                    (ImmediateConst (ConstBool true)), [])),
                 (AnfExpr
                    (ComplexTuple ((ImmediateVar "anf_t0"),
                       (ImmediateVar "anf_t1"), [])))
                 ))
              ))
           )),
        (AnfLet (NonRec, "anf_t4",
           (ComplexLambda ([(PatVariable "x")],
              (AnfExpr (ComplexImmediate (ImmediateVar "x"))))),
           (AnfExpr
              (ComplexApp ((ImmediateVar "anf_t3"), (ImmediateVar "anf_t4"),
                 [])))
           ))
        ))),
    []))
  ]|}]
;;

let%expect_test "002if.ml" =
  parse_and_anf "let main = if true then 1 else false";
  [%expect
    {|
  [(AnfValue (NonRec,
      ("main", 0,
       (AnfExpr
          (ComplexBranch ((ImmediateConst (ConstBool true)),
             (AnfExpr (ComplexImmediate (ImmediateConst (ConstInt 1)))),
             (AnfExpr (ComplexImmediate (ImmediateConst (ConstBool false)))))))),
      []))
    ]|}]
;;

let%expect_test "pretty_printer_test1" =
  parse_and_anf_pp
    "let rec fac n = if n <= 1 then 1 else n * fac (n - 1)\n  let main = fac 4";
  [%expect
    {|
      let rec fac = fun n -> let anf_t0 = (n <= 1) in
      if anf_t0 then 1 else let anf_t1 = (n - 1) in let anf_t2 = fac anf_t1 in
      (n * anf_t2)

      let main = fac 4 |}]
;;

let%expect_test "pretty_printer_test2" =
  parse_and_anf_pp
    "let rec fibo = fun n -> if n < 1 then 1 else fibo (n-1) + fibo (n-2)\n\
    \  let main = fibo 10";
  [%expect
    {|
      let rec fibo = fun n -> let anf_t0 = (n < 1) in
      if anf_t0 then 1 else let anf_t1 = (n - 1) in let anf_t2 = fibo anf_t1 in
      let anf_t3 = (n - 2) in let anf_t4 = fibo anf_t3 in
      (anf_t2 + anf_t4)

      let main = fibo 10|}]
;;

let%expect_test "anf_match_list_lowering_nil_cons_order" =
  parse_and_anf
    {| let rec h xs =
  match xs with
  | [] -> 0
  | hd::tl -> hd |};
  [%expect
    {|
[(AnfValue (Rec,
    ("h", 1,
     (AnfExpr
        (ComplexLambda ([(PatVariable "xs")],
           (AnfLet (NonRec, "anf_t0", (ComplexImmediate (ImmediateVar "xs")),
              (AnfLet (NonRec, "anf_t1",
                 (ComplexBinOper (Equal, (ImmediateVar "anf_t0"),
                    (ImmediateConst (ConstInt 0)))),
                 (AnfExpr
                    (ComplexBranch ((ImmediateVar "anf_t1"),
                       (AnfExpr
                          (ComplexImmediate (ImmediateConst (ConstInt 0)))),
                       (AnfLet (NonRec, "hd",
                          (ComplexField ((ImmediateVar "anf_t0"), 0)),
                          (AnfLet (NonRec, "tl",
                             (ComplexField ((ImmediateVar "anf_t0"), 1)),
                             (AnfExpr (ComplexImmediate (ImmediateVar "hd")))
                             ))
                          ))
                       )))
                 ))
              ))
           )))),
    []))
  ]|}]
;;

let%expect_test "anf_match_list_lowering_cons_nil_order" =
  parse_and_anf
    {| let rec h xs =
  match xs with
  | hd::tl -> hd
  | [] -> 0 |};
  [%expect
    {|
[(AnfValue (Rec,
    ("h", 1,
     (AnfExpr
        (ComplexLambda ([(PatVariable "xs")],
           (AnfLet (NonRec, "anf_t0", (ComplexImmediate (ImmediateVar "xs")),
              (AnfLet (NonRec, "anf_t1",
                 (ComplexBinOper (Equal, (ImmediateVar "anf_t0"),
                    (ImmediateConst (ConstInt 0)))),
                 (AnfExpr
                    (ComplexBranch ((ImmediateVar "anf_t1"),
                       (AnfExpr
                          (ComplexImmediate (ImmediateConst (ConstInt 0)))),
                       (AnfLet (NonRec, "hd",
                          (ComplexField ((ImmediateVar "anf_t0"), 0)),
                          (AnfLet (NonRec, "tl",
                             (ComplexField ((ImmediateVar "anf_t0"), 1)),
                             (AnfExpr (ComplexImmediate (ImmediateVar "hd")))
                             ))
                          ))
                       )))
                 ))
              ))
           )))),
    []))
  ]|}]
;;

let%expect_test "anf_roundtrip_types_fac" =
  let env = TypeEnv.initial_env in
  (match
     anf_roundtrip_typecheck
       ~env
       "let rec fac n = if n <= 1 then 1 else n * fac (n - 1)\nlet main = fac 4"
   with
   | Ok () -> Printf.printf "OK: types preserved after ANF round-trip\n"
   | Error e -> Printf.printf "FAIL: %s\n" e);
  [%expect {| OK: types preserved after ANF round-trip |}]
;;

let%expect_test "anf_roundtrip_types_fib" =
  let env = TypeEnv.initial_env in
  (match
     anf_roundtrip_typecheck
       ~env
       "let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)\nlet main = fib 5"
   with
   | Ok () -> Printf.printf "OK: types preserved after ANF round-trip\n"
   | Error e -> Printf.printf "FAIL: %s\n" e);
  [%expect {| OK: types preserved after ANF round-trip |}]
;;

let%expect_test "anf_roundtrip_types_partial" =
  let env = TypeEnv.initial_env in
  (match
     anf_roundtrip_typecheck ~env "let add x y = x + y\nlet main = let f = add 1 in f 2"
   with
   | Ok () -> Printf.printf "OK: types preserved after ANF round-trip\n"
   | Error e -> Printf.printf "FAIL: %s\n" e);
  [%expect {| OK: types preserved after ANF round-trip |}]
;;

let%expect_test "custom_infix_operator_lowers_to_app" =
  parse_and_anf "let ( =^.^= ) x y = (x * 10) + y";
  [%expect
    {|
[(AnfValue (NonRec,
    ("=^.^=", 2,
     (AnfExpr
        (ComplexLambda ([(PatVariable "x")],
           (AnfExpr
              (ComplexLambda ([(PatVariable "y")],
                 (AnfLet (NonRec, "anf_t0",
                    (ComplexBinOper (Multiply, (ImmediateVar "x"),
                       (ImmediateConst (ConstInt 10)))),
                    (AnfExpr
                       (ComplexBinOper (Plus, (ImmediateVar "anf_t0"),
                          (ImmediateVar "y"))))
                    ))
                 )))
           )))),
    []))
  ]|}]
;;
