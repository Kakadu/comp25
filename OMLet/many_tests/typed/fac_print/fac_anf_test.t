Copyright 2025, Ksenia Kotelnikova, Sofya Kozyreva, Vyacheslav Kochergin
SPDX-License-Identifier: LGPL-3.0-or-later

  $ dune exec ../../../repl/repl.exe -- -dumpanf -fromfile fac_print.ml
  let rec fac =
  fun n -> 
    let lte_3 = n <= 1 in
      if lte_3
        then 1
        else let res_of_minus_1 = n - 1 in
               let res_of_app_2 = fac res_of_minus_1 in
                 let res_of_mul_0 = n * res_of_app_2 in
                   res_of_mul_0
  let main =
  let res_of_app_4 = fac 4 in
    let res_of_app_5 = print_int res_of_app_4 in
      res_of_app_5
