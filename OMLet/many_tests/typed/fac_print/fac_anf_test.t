Copyright 2025, Ksenia Kotelnikova, Sofya Kozyreva, Vyacheslav Kochergin
SPDX-License-Identifier: LGPL-3.0-or-later

  $ dune exec ../../../repl/repl.exe -- -dumpanf -fromfile fac_print.ml
  let rec fac =
  fun n -> 
    let lte_3 = n <= 1 in
      if lte_3
        then 1
        else let res_of_minus_5 = n - 1 in
               let res_of_app_6 = fac res_of_minus_5 in
                 let res_of_mul_4 = n * res_of_app_6 in
                   res_of_mul_4
  let main =
  let res_of_app_1 = fac 4 in
    let res_of_app_2 = print_int res_of_app_1 in
      res_of_app_2
