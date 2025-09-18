Copyright 2025-2026, Friend-zva, RodionovMaxim05
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../bin/akaML.exe -dparsetree <<EOF
  > let rec fac n = if n <= 1 then 1 else n * fac (n - 1)
  > 
  > let main =
  >   let () = print_int (fac 4) in
  >   0
  > ;;
  [(Struct_value (Recursive,
      { pat = (Pat_var "fac");
        exp =
        (Exp_fun ((Pat_var "n"), [],
           (Exp_ifthenelse (
              (Exp_apply
                 ((Exp_apply ((Exp_ident "<="), (Exp_ident "n"))),
                  (Exp_constant (Const_integer 1)))),
              (Exp_constant (Const_integer 1)),
              (Some (Exp_apply
                       ((Exp_apply ((Exp_ident "*"), (Exp_ident "n"))),
                        (Exp_apply
                           ((Exp_ident "fac"),
                            (Exp_apply
                               ((Exp_apply ((Exp_ident "-"), (Exp_ident "n"))),
                                (Exp_constant (Const_integer 1)))))))))
              ))
           ))
        },
      []));
    (Struct_value (Nonrecursive,
       { pat = (Pat_var "main");
         exp =
         (Exp_let (Nonrecursive,
            { pat = (Pat_construct ("()", None));
              exp =
              (Exp_apply
                 ((Exp_ident "print_int"),
                  (Exp_apply
                     ((Exp_ident "fac"), (Exp_constant (Const_integer 4))))))
              },
            [], (Exp_constant (Const_integer 0))))
         },
       []))
    ]

