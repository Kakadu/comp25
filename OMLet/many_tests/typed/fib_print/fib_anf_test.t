Copyright 2025, Ksenia Kotelnikova, Sofya Kozyreva, Vyacheslav Kochergin
SPDX-License-Identifier: LGPL-3.0-or-later

  $ dune exec ../../../repl/repl.exe -- -dumpanf -fromfile fib_print.ml
  let rec fib =
  fun n -> 
    let lt_5 = n < 2 in
      if lt_5
        then n
        else let res_of_minus_1 = n - 1 in
               let res_of_app_2 = fib res_of_minus_1 in
                 let res_of_minus_3 = n - 2 in
                   let res_of_app_4 = fib res_of_minus_3 in
                     let res_of_plus_0 = res_of_app_2 + res_of_app_4 in
                       res_of_plus_0
  let main =
  let res_of_app_6 = fib 4 in
    let res_of_app_7 = print_int res_of_app_6 in
      res_of_app_7
