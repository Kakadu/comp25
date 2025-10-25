Copyright 2025, Ksenia Kotelnikova, Sofya Kozyreva, Vyacheslav Kochergin
SPDX-License-Identifier: LGPL-3.0-or-later

  $ dune exec ../../../repl/repl.exe -- -dumpanf -fromfile large_print.ml
  let large =
  fun x -> 
    let neq_2 = 0 <> x in
      if neq_2
        then let res_of_app_0 = print_int 0 in
               res_of_app_0
        else let res_of_app_1 = print_int 1 in
               res_of_app_1
  let main =
  let eq_7 = 0 = 1 in
    if eq_7
      then let eq_5 = 0 = 1 in
             if eq_5
               then let eq_4 = 0 = 1 in
                      if eq_4
                        then let x = 0 in
                               let res_of_app_3 = large x in
                                 res_of_app_3
                        else let x = 1 in
                               let res_of_app_3 = large x in
                                 res_of_app_3
               else let eq_4 = 1 = 1 in
                      if eq_4
                        then let x = 0 in
                               let res_of_app_3 = large x in
                                 res_of_app_3
                        else let x = 1 in
                               let res_of_app_3 = large x in
                                 res_of_app_3
      else let res_of_app_6 = print_int 42 in
             let t42 = res_of_app_6 in
               let eq_5 = 1 = 1 in
                 if eq_5
                   then let eq_4 = 0 = 1 in
                          if eq_4
                            then let x = 0 in
                                   let res_of_app_3 = large x in
                                     res_of_app_3
                            else let x = 1 in
                                   let res_of_app_3 = large x in
                                     res_of_app_3
                   else let eq_4 = 1 = 1 in
                          if eq_4
                            then let x = 0 in
                                   let res_of_app_3 = large x in
                                     res_of_app_3
                            else let x = 1 in
                                   let res_of_app_3 = large x in
                                     res_of_app_3
