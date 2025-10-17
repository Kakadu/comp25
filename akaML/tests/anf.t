Copyright 2025-2026, Friend-zva, RodionovMaxim05
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../bin/akaML.exe -anf <<EOF
  > let rec fac n = if n = 0 then 1 else n * fac (n - 1)
  > 
  > let main = print_int (fac 4)
  let rec fac =
    fun n ->
      (let temp0 = n = 0 in
      let temp4 =
        if temp0 then 1
        else (let temp1 = n - 1 in
          let temp2 = fac temp1 in
          let temp3 = n * temp2 in
          temp3) in
      let temp5 = temp4 in
      temp5);;
  let main = let temp6 = fac 4 in
             let temp7 = print_int temp6 in
             temp7;;

  $ ../bin/akaML.exe -anf <<EOF
  > let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)
  > 
  > let main = print_int (fib 6)
  let rec fib =
    fun n ->
      (let temp0 = n < 2 in
      let temp6 =
        if temp0 then n
        else (let temp1 = n - 1 in
          let temp2 = fib temp1 in
          let temp3 = n - 2 in
          let temp4 = fib temp3 in
          let temp5 = temp2 + temp4 in
          temp5) in
      let temp7 = temp6 in
      temp7);;
  let main = let temp8 = fib 6 in
             let temp9 = print_int temp8 in
             temp9;;
