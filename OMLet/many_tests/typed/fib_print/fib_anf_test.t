Copyright 2025, Ksenia Kotelnikova, Sofya Kozyreva, Vyacheslav Kochergin
SPDX-License-Identifier: LGPL-3.0-or-later

  $ dune exec ../../../repl/repl.exe -- -dumpanf -fromfile fib_print.ml
  let rec fib =
  fun n -> 
    let lt_3 = n < 2 in
      if lt_3
        then n
        else let res_of_minus_5 = n - 1 in
               let res_of_app_6 = fib res_of_minus_5 in
                 let res_of_minus_7 = n - 2 in
                   let res_of_app_8 = fib res_of_minus_7 in
                     let res_of_plus_4 = res_of_app_6 + res_of_app_8 in
                       res_of_plus_4
  let main =
  let res_of_app_1 = fib 4 in
    let res_of_app_2 = print_int res_of_app_1 in
      res_of_app_2
