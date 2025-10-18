Copyright 2025, Ksenia Kotelnikova, Sofya Kozyreva, Vyacheslav Kochergin
SPDX-License-Identifier: LGPL-3.0-or-later

  $ dune exec ../../../repl/repl.exe -- -dumpanf -fromfile large_print.ml
  let large =
  fun x -> 
    let neq_13 = 0 <> x in
      if neq_13
        then let res_of_app_15 = print_int 0 in
               res_of_app_15
        else let res_of_app_14 = print_int 1 in
               res_of_app_14
  let main =
  let eq_3 = 0 = 1 in
    if eq_3
      then let eq_2 = 0 = 1 in
             if eq_2
               then let eq_1 = 0 = 1 in
                      if eq_1
                        then let x = 0 in
                               let res_of_app_12 = large x in
                                 res_of_app_12
                        else let x = 1 in
                               let res_of_app_11 = large x in
                                 res_of_app_11
               else let eq_1 = 1 = 1 in
                      if eq_1
                        then let x = 0 in
                               let res_of_app_10 = large x in
                                 res_of_app_10
                        else let x = 1 in
                               let res_of_app_9 = large x in
                                 res_of_app_9
      else let res_of_app_4 = print_int 42 in
             let t42 = res_of_app_4 in
               let eq_2 = 1 = 1 in
                 if eq_2
                   then let eq_1 = 0 = 1 in
                          if eq_1
                            then let x = 0 in
                                   let res_of_app_8 = large x in
                                     res_of_app_8
                            else let x = 1 in
                                   let res_of_app_7 = large x in
                                     res_of_app_7
                   else let eq_1 = 1 = 1 in
                          if eq_1
                            then let x = 0 in
                                   let res_of_app_6 = large x in
                                     res_of_app_6
                            else let x = 1 in
                                   let res_of_app_5 = large x in
                                     res_of_app_5
