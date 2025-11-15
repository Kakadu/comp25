Copyright 2025-2026, Friend-zva, RodionovMaxim05
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../bin/akaML.exe -dparsetree <<EOF
  > let rec fac n =
  >   if n <= 1
  >   then 1
  >   else (let n1 = n-1 in
  >      let m = fac n1 in
  >      n*m)
  > 
  > let main = fac 4
  [(Struct_value (Recursive,
      { pat = (Pat_var "fac");
        exp =
        (Exp_fun ((Pat_var "n"), [],
           (Exp_ifthenelse (
              (Exp_apply
                 ((Exp_apply ((Exp_ident "<="), (Exp_ident "n"))),
                  (Exp_constant (Const_integer 1)))),
              (Exp_constant (Const_integer 1)),
              (Some (Exp_let (Nonrecursive,
                       { pat = (Pat_var "n1");
                         exp =
                         (Exp_apply
                            ((Exp_apply ((Exp_ident "-"), (Exp_ident "n"))),
                             (Exp_constant (Const_integer 1))))
                         },
                       [],
                       (Exp_let (Nonrecursive,
                          { pat = (Pat_var "m");
                            exp =
                            (Exp_apply ((Exp_ident "fac"), (Exp_ident "n1"))) },
                          [],
                          (Exp_apply
                             ((Exp_apply ((Exp_ident "*"), (Exp_ident "n"))),
                              (Exp_ident "m")))
                          ))
                       )))
              ))
           ))
        },
      []));
    (Struct_value (Nonrecursive,
       { pat = (Pat_var "main");
         exp =
         (Exp_apply ((Exp_ident "fac"), (Exp_constant (Const_integer 4)))) },
       []))
    ]
