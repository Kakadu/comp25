(** Copyright 2025-2026, Rodion Suvorov, Dmitriy Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Ast.Expression
open Ast.Structure
open Middle.Anf

let test_anf (prog : program) =
  let anf_prog = anf_transform prog in
  print_endline (show_aprogram anf_prog)
;;

(*
   let rec fac n = if n <= 1 then 1 else n * fac (n - 1)

let main =
  let () = print_int (fac 4) in
  0
*)
let%expect_test "anf_fac" =
  let expr =
    [ Str_value
        ( Recursive
        , ( { pat = Pat_var "fac"
            ; expr =
                Exp_fun
                  ( (Pat_var "n", [])
                  , Exp_if
                      ( Exp_apply
                          ( Exp_ident "<="
                          , Exp_tuple (Exp_ident "n", Exp_constant (Const_integer 1), [])
                          )
                      , Exp_constant (Const_integer 1)
                      , Some
                          (Exp_apply
                             ( Exp_ident "*"
                             , Exp_tuple
                                 ( Exp_ident "n"
                                 , Exp_apply
                                     ( Exp_ident "fac"
                                     , Exp_apply
                                         ( Exp_ident "-"
                                         , Exp_tuple
                                             ( Exp_ident "n"
                                             , Exp_constant (Const_integer 1)
                                             , [] ) ) )
                                 , [] ) )) ) )
            }
          , [] ) )
    ; Str_value
        ( Nonrecursive
        , ( { pat = Pat_var "main"
            ; expr =
                Exp_let
                  ( Nonrecursive
                  , ( { pat = Pat_construct ("()", None)
                      ; expr =
                          Exp_apply
                            ( Exp_ident "print_int"
                            , Exp_apply (Exp_ident "fac", Exp_constant (Const_integer 4))
                            )
                      }
                    , [] )
                  , Exp_constant (Const_integer 0) )
            }
          , [] ) )
    ]
  in
  test_anf expr;
  [%expect
    {|
    [(AStr_value (Recursive, "fac",
        (ACE
           (CFun ("n",
              (ALet (Nonrecursive, "t_0", (CBinop (Le, (ImmId "n"), (ImmNum 1))),
                 (ACE
                    (CIte ((ImmId "t_0"), (ACE (CImm (ImmNum 1))),
                       (ALet (Nonrecursive, "t_1",
                          (CBinop (Sub, (ImmId "n"), (ImmNum 1))),
                          (ALet (Nonrecursive, "t_2",
                             (CApp ((ImmId "fac"), [(ImmId "t_1")])),
                             (ACE (CBinop (Mul, (ImmId "n"), (ImmId "t_2"))))))
                          ))
                       )))
                 ))
              )))
        ));
      (AStr_value (Nonrecursive, "main",
         (ALet (Nonrecursive, "t_4", (CApp ((ImmId "fac"), [(ImmNum 4)])),
            (ALet (Nonrecursive, "t_5",
               (CApp ((ImmId "print_int"), [(ImmId "t_4")])),
               (ALet (Nonrecursive, "()", (CImm (ImmId "t_5")),
                  (ACE (CImm (ImmNum 0)))))
               ))
            ))
         ))
      ] |}]
;;

(*
   let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

let main =
  let () = print_int (fib 4) in
  0
*)
let%expect_test "anf_fib" =
  let expr =
    [ Str_value
        ( Recursive
        , ( { pat = Pat_var "fib"
            ; expr =
                Exp_fun
                  ( (Pat_var "n", [])
                  , Exp_if
                      ( Exp_apply
                          ( Exp_ident "<"
                          , Exp_tuple (Exp_ident "n", Exp_constant (Const_integer 2), [])
                          )
                      , Exp_ident "n"
                      , Some
                          (Exp_apply
                             ( Exp_ident "+"
                             , Exp_tuple
                                 ( Exp_apply
                                     ( Exp_ident "fib"
                                     , Exp_apply
                                         ( Exp_ident "-"
                                         , Exp_tuple
                                             ( Exp_ident "n"
                                             , Exp_constant (Const_integer 1)
                                             , [] ) ) )
                                 , Exp_apply
                                     ( Exp_ident "fib"
                                     , Exp_apply
                                         ( Exp_ident "-"
                                         , Exp_tuple
                                             ( Exp_ident "n"
                                             , Exp_constant (Const_integer 2)
                                             , [] ) ) )
                                 , [] ) )) ) )
            }
          , [] ) )
    ; Str_value
        ( Nonrecursive
        , ( { pat = Pat_var "main"
            ; expr =
                Exp_let
                  ( Nonrecursive
                  , ( { pat = Pat_construct ("()", None)
                      ; expr =
                          Exp_apply
                            ( Exp_ident "print_int"
                            , Exp_apply (Exp_ident "fib", Exp_constant (Const_integer 4))
                            )
                      }
                    , [] )
                  , Exp_constant (Const_integer 0) )
            }
          , [] ) )
    ]
  in
  test_anf expr;
  [%expect
    {|
    [(AStr_value (Recursive, "fib",
        (ACE
           (CFun ("n",
              (ALet (Nonrecursive, "t_0", (CBinop (Lt, (ImmId "n"), (ImmNum 2))),
                 (ACE
                    (CIte ((ImmId "t_0"), (ACE (CImm (ImmId "n"))),
                       (ALet (Nonrecursive, "t_1",
                          (CBinop (Sub, (ImmId "n"), (ImmNum 1))),
                          (ALet (Nonrecursive, "t_2",
                             (CApp ((ImmId "fib"), [(ImmId "t_1")])),
                             (ALet (Nonrecursive, "t_3",
                                (CBinop (Sub, (ImmId "n"), (ImmNum 2))),
                                (ALet (Nonrecursive, "t_4",
                                   (CApp ((ImmId "fib"), [(ImmId "t_3")])),
                                   (ACE
                                      (CBinop (Add, (ImmId "t_2"), (ImmId "t_4")
                                         )))
                                   ))
                                ))
                             ))
                          ))
                       )))
                 ))
              )))
        ));
      (AStr_value (Nonrecursive, "main",
         (ALet (Nonrecursive, "t_6", (CApp ((ImmId "fib"), [(ImmNum 4)])),
            (ALet (Nonrecursive, "t_7",
               (CApp ((ImmId "print_int"), [(ImmId "t_6")])),
               (ALet (Nonrecursive, "()", (CImm (ImmId "t_7")),
                  (ACE (CImm (ImmNum 0)))))
               ))
            ))
         ))
      ] |}]
;;

(*
   let large x = if 0<>x then print_int 0 else print_int 1
  let main =
     let x = if (if (if 0
                     then 0 else (let t42 = print_int 42 in 1))
                 then 0 else 1)
             then 0 else 1 in
     large x
*)
let%expect_test "anf_third_test" =
  let expr =
    [ Str_value
        ( Nonrecursive
        , ( { pat = Pat_var "large"
            ; expr =
                Exp_fun
                  ( (Pat_var "x", [])
                  , Exp_if
                      ( Exp_apply
                          ( Exp_ident "<>"
                          , Exp_tuple (Exp_constant (Const_integer 0), Exp_ident "x", [])
                          )
                      , Exp_apply (Exp_ident "print_int", Exp_constant (Const_integer 0))
                      , Some
                          (Exp_apply
                             (Exp_ident "print_int", Exp_constant (Const_integer 1))) ) )
            }
          , [] ) )
    ; Str_value
        ( Nonrecursive
        , ( { pat = Pat_var "main"
            ; expr =
                Exp_let
                  ( Nonrecursive
                  , ( { pat = Pat_var "x"
                      ; expr =
                          Exp_if
                            ( Exp_if
                                ( Exp_if
                                    ( Exp_constant (Const_integer 0)
                                    , Exp_constant (Const_integer 0)
                                    , Some
                                        (Exp_let
                                           ( Nonrecursive
                                           , ( { pat = Pat_var "t42"
                                               ; expr =
                                                   Exp_apply
                                                     ( Exp_ident "print_int"
                                                     , Exp_constant (Const_integer 42) )
                                               }
                                             , [] )
                                           , Exp_constant (Const_integer 1) )) )
                                , Exp_constant (Const_integer 0)
                                , Some (Exp_constant (Const_integer 1)) )
                            , Exp_constant (Const_integer 0)
                            , Some (Exp_constant (Const_integer 1)) )
                      }
                    , [] )
                  , Exp_apply (Exp_ident "large", Exp_ident "x") )
            }
          , [] ) )
    ]
  in
  test_anf expr;
  [%expect
    {|
    [(AStr_value (Nonrecursive, "large",
        (ACE
           (CFun ("x",
              (ALet (Nonrecursive, "t_0",
                 (CBinop (Neq, (ImmNum 0), (ImmId "x"))),
                 (ACE
                    (CIte ((ImmId "t_0"),
                       (ACE (CApp ((ImmId "print_int"), [(ImmNum 0)]))),
                       (ACE (CApp ((ImmId "print_int"), [(ImmNum 1)]))))))
                 ))
              )))
        ));
      (AStr_value (Nonrecursive, "main",
         (ACE
            (CIte ((ImmNum 0),
               (ACE
                  (CIte ((ImmNum 0),
                     (ACE
                        (CIte ((ImmNum 0),
                           (ALet (Nonrecursive, "x", (CImm (ImmNum 0)),
                              (ACE (CApp ((ImmId "large"), [(ImmId "x")]))))),
                           (ALet (Nonrecursive, "x", (CImm (ImmNum 1)),
                              (ACE (CApp ((ImmId "large"), [(ImmId "x")])))))
                           ))),
                     (ACE
                        (CIte ((ImmNum 1),
                           (ALet (Nonrecursive, "x", (CImm (ImmNum 0)),
                              (ACE (CApp ((ImmId "large"), [(ImmId "x")]))))),
                           (ALet (Nonrecursive, "x", (CImm (ImmNum 1)),
                              (ACE (CApp ((ImmId "large"), [(ImmId "x")])))))
                           )))
                     ))),
               (ALet (Nonrecursive, "t_7",
                  (CApp ((ImmId "print_int"), [(ImmNum 42)])),
                  (ALet (Nonrecursive, "t42", (CImm (ImmId "t_7")),
                     (ACE
                        (CIte ((ImmNum 1),
                           (ACE
                              (CIte ((ImmNum 0),
                                 (ALet (Nonrecursive, "x", (CImm (ImmNum 0)),
                                    (ACE (CApp ((ImmId "large"), [(ImmId "x")])))
                                    )),
                                 (ALet (Nonrecursive, "x", (CImm (ImmNum 1)),
                                    (ACE (CApp ((ImmId "large"), [(ImmId "x")])))
                                    ))
                                 ))),
                           (ACE
                              (CIte ((ImmNum 1),
                                 (ALet (Nonrecursive, "x", (CImm (ImmNum 0)),
                                    (ACE (CApp ((ImmId "large"), [(ImmId "x")])))
                                    )),
                                 (ALet (Nonrecursive, "x", (CImm (ImmNum 1)),
                                    (ACE (CApp ((ImmId "large"), [(ImmId "x")])))
                                    ))
                                 )))
                           )))
                     ))
                  ))
               )))
         ))
      ] |}]
;;
