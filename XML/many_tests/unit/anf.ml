(** Copyright 2026,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Middleend.Anf
open Middleend.Pprinter
open Common.Parser

let to_anf_prog str =
    let prog = parse_str str in
    let aprog = anf_program_res prog in
    match aprog with
    | Ok aprog ->
  print_anf_program Format.std_formatter aprog
  | Error e -> Format.fprintf Format.std_formatter "%s" (pp_anf_error e)

  let pp_error e =
    Format.fprintf Format.std_formatter "%s" (pp_anf_error e)

(************************** Expressions **************************)


let%expect_test "char" =
  to_anf_prog {| 'a' |};
  [%expect{| a;; |}]


let%expect_test "int" =
  to_anf_prog {| 1 |};
  [%expect{| 1;; |}]


let%expect_test "str" =
  to_anf_prog {| "Kakadu" |};
  [%expect{| Kakadu;; |}]


let%expect_test "tuple 2" =
  to_anf_prog {| (1, 2) |};
  [%expect{| let t_0 = alloc(1,  2) in t_0;; |}]


let%expect_test "tuple 3" =
  to_anf_prog {| (1, 2, 3) |};
  [%expect{| let t_0 = alloc(1,  2,  3) in t_0;; |}]


let%expect_test "tuples in tuple" =
  to_anf_prog {| ((1, 2), (3, 4)) |};
  [%expect{|
    let t_0 = alloc(1,  2)
      in let t_1 = alloc(3,  4) in let t_2 = alloc(t_0,  t_1) in t_2;; |}]


let%expect_test "if a then b" =
  to_anf_prog {| if a then b |};
  [%expect{| unsupported expression in ANF normaliser |}]


let%expect_test "if a then b else c" =
  to_anf_prog {| if a then b else c |};
  [%expect{| let t_0 = if a then b else c in t_0;; |}]


let%expect_test "apply f x" =
  to_anf_prog {| f x |};
  [%expect{| let t_0 = f x in t_0;; |}]


  let%expect_test "apply f (let a = x in a)" =
  to_anf_prog {| f (let a = x in a) |};
  [%expect{| let t_0 = x in let a = t_0 in let t_1 = f a in t_1;; |}]


let%expect_test "apply f x y" =
  to_anf_prog {| f x y |};
  [%expect{| let t_0 = f x in let t_1 = t_0 y in t_1;; |}]


let%expect_test "apply 1 + 2" =
  to_anf_prog {| 1 + 2 |};
  [%expect{| let t_0 = (1 + 2) in t_0;; |}]


let%expect_test "fun x -> x" =
  to_anf_prog {| fun x -> x |};
  [%expect{| let t_0 = fun x -> x in t_0;; |}]


let%expect_test "apply (fun x -> x) 10" =
  to_anf_prog {| (fun x -> x) 10 |};
  [%expect{| let t_0 = fun x -> x in let t_1 = t_0 10 in t_1;; |}]


let%expect_test "let a = 5 in a + 10" =
  to_anf_prog {| let a = 5 in a + 10 |};
  [%expect{| let t_0 = 5 in let a = t_0 in let t_1 = (a + 10) in t_1;; |}]


let%expect_test "factorial" =
  to_anf_prog {| let rec fac n = if n <= 0 then 1 else n * (fac (n-1)) in fac |};
  [%expect{|
    let rec t_5 = fun n -> let t_0 = (n <= 0)
                             in let t_4 = if t_0 then 1 else let t_1 = (n - 1)
                                                               in let t_2 = fac t_1
                                                                    in let t_3 = (n * t_2)
                                                                        in t_3
                                  in t_4 in let fac = t_5 in fac;; |}]

(************************** Structure item **************************)

let%expect_test "factorial" =
  to_anf_prog {| let rec fac n = if n <= 0 then 1 else n * (fac (n-1)) |};
  [%expect{|
    let rec fac = let t_5 = fun n -> let t_0 = (n <= 0)
                                       in let t_4 = if t_0 then 1 else let t_1 = (n - 1)
                                                                        in
                                                                        let t_2 = fac t_1
                                                                        in
                                                                        let t_3 = (n * t_2)
                                                                        in t_3
                                            in t_4 in t_5;; |}]

(**************************  Error **************************)

let%expect_test "Only_simple_var_params" =
pp_error `Only_simple_var_params;
[%expect {| Only simple variable patterns are allowed in function parameters |}]

let%expect_test "Func_no_params" =
pp_error `Func_no_params;
[%expect {| Function with no parameters found |}]


let%expect_test "Let_and_not_supported" =
pp_error `Let_and_not_supported;
[%expect {| let ... and ... is not supported in ANF yet |}]


let%expect_test "Unsupported_let_pattern" =
pp_error (`Unsupported_let_pattern "pattern");
[%expect {| Unsupported pattern in let-binding: pattern |}]


let%expect_test "Unsupported_let_pattern" =
pp_error (`Unsupported_let_pattern "wrong_pattern");
[%expect {| Unsupported pattern in let-binding: wrong_pattern |}]


let%expect_test "Unsupported_expr_in_normaliser" =
pp_error (`Unsupported_expr_in_normaliser);
[%expect {| unsupported expression in ANF normaliser |}]

let%expect_test "Mutual_rec_not_supported" =
pp_error `Mutual_rec_not_supported;
[%expect {| Mutually recursive let ... and ... bindings are not supported yet. |}]


let%expect_test "Unsupported_toplevel_let" =
pp_error `Unsupported_toplevel_let;
[%expect {| Unsupported pattern in a top-level let-binding. Only simple variables are allowed. |}]


let%expect_test "Unsupported_toplevel_item" =
pp_error `Unsupported_toplevel_item;
[%expect {| Unsupported top-level structure item. |}]

