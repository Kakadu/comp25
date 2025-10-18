Copyright 2025-2026, Friend-zva, RodionovMaxim05
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../bin/akaML.exe -anf <<EOF
  > let rec fac n = if n = 0 then 1 else n * fac (n - 1)
  > 
  > let main = print_int (fac 4)
  let rec fac =
    fun n ->
      (let temp0 = n = 0 in
      if temp0 then 1
      else (let temp1 = n - 1 in
        let temp2 = fac temp1 in
        n * temp2));;
  let main = let temp6 = fac 4 in
             print_int temp6;;

  $ ../bin/akaML.exe -anf <<EOF
  > let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)
  > 
  > let main = print_int (fib 6)
  let rec fib =
    fun n ->
      (let temp0 = n < 2 in
      if temp0 then n
      else (let temp1 = n - 1 in
        let temp2 = fib temp1 in
        let temp3 = n - 2 in
        let temp4 = fib temp3 in
        temp2 + temp4));;
  let main = let temp8 = fib 6 in
             print_int temp8;;

  $ ../bin/akaML.exe -anf <<EOF
  > let large x = if 0<>x then print_int 0 else print_int 1
  > let main =
  >   let x = if (if (if 0 = 1
  >                   then 0 = 1 else (let t42 = print_int 42 in 1 = 1))
  >               then 0 = 1 else 1 = 1)
  >           then 0 else 1 in
  >   large x
  let large =
    fun x -> (let temp0 = 0 <> x in
             if temp0 then print_int 0 else print_int 1);;
  let main =
    let temp5 = 0 = 1 in
    let temp9 = if temp5 then 0 = 1 else (let t42 = print_int 42 in
                                      1 = 1) in
    let temp12 = if temp9 then 0 = 1 else 1 = 1 in
    let x = if temp12 then 0 else 1 in
    large x;;
