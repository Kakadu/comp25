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

  $ ../bin/akaML.exe -anf -fromfile manytests/typed/010faccps_ll.ml
  let id = fun x -> x;;
  let fresh_1 = fun n -> (fun k -> (fun p -> (let temp1 = p * n in
                                             k temp1)));;
  let rec fac_cps =
    fun n ->
      (fun k ->
         (let temp4 = n = 1 in
         if temp4 then k 1
         else (let temp6 = n - 1 in
           let temp7 = fresh_1 n k in
           fac_cps temp6 temp7)));;
  let main = let temp11 = fac_cps 4 id in
             let temp12 = print_int temp11 in
             0;;

  $ ../bin/akaML.exe -anf -fromfile manytests/typed/010fibcps_ll.ml
  let id = fun x -> x;;
  let fresh_2 =
    fun p1 -> (fun k -> (fun p2 -> (let temp1 = p1 + p2 in
                                   k temp1)));;
  let fresh_1 =
    fun n ->
      (fun k ->
         (fun fib ->
            (fun p1 ->
               (let temp4 = n - 2 in
               let temp5 = fresh_2 p1 k in
               fib temp4 temp5))));;
  let rec fib =
    fun n ->
      (fun k ->
         (let temp8 = n < 2 in
         if temp8 then k n
         else (let temp10 = n - 1 in
           let temp11 = fresh_1 n k fib in
           fib temp10 temp11)));;
  let main = let temp15 = fib 6 id in
             let z = print_int temp15 in
             0;;

  $ ../bin/akaML.exe -anf -fromfile manytests/typed/004manyargs.ml
  let wrap = fun f -> (let temp0 = 1 = 1 in
                      if temp0 then f else f);;
  let test3 =
    fun a ->
      (fun b ->
         (fun c ->
            (let a = print_int a in
            let b = print_int b in
            let c = print_int c in
            0)));;
  let test10 =
    fun a ->
      (fun b ->
         (fun c ->
            (fun d ->
               (fun e ->
                  (fun f ->
                     (fun g ->
                        (fun h ->
                           (fun i ->
                              (fun j ->
                                 (let temp7 = a + b in
                                 let temp8 = temp7 + c in
                                 let temp9 = temp8 + d in
                                 let temp10 = temp9 + e in
                                 let temp11 = temp10 + f in
                                 let temp12 = temp11 + g in
                                 let temp13 = temp12 + h in
                                 let temp14 = temp13 + i in
                                 temp14 + j))))))))));;
  let main =
    let rez =
      wrap test10 1 10 100 1000 10000 100000 1000000 10000000 100000000
        1000000000 in
    let temp18 = print_int rez in
    let temp2 = wrap test3 1 10 100 in
    0;;

  $ ../bin/akaML.exe -anf -fromfile manytests/typed/012faccps.ml
  let ll_0 = fun k -> (fun n -> (fun a -> (let temp0 = a * n in
                                          k temp0)));;
  let rec fac =
    fun n ->
      (fun k ->
         (let temp3 = n < 2 in
         if temp3 then k 1
         else (let temp5 = n - 1 in
           let temp6 = ll_0 k n in
           fac temp5 temp6)));;
  let ll_1 = fun x -> x;;
  let main = let temp11 = fac 6 ll_1 in
             print_int temp11;;

  $ ../bin/akaML.exe -anf -fromfile manytests/typed/012fibcps.ml
  let ll_1 = fun a -> (fun k -> (fun b -> (let temp0 = a + b in
                                          k temp0)));;
  let ll_0 =
    fun k ->
      (fun n ->
         (fun a ->
            (let temp3 = n - 2 in
            let temp4 = ll_1 a k in
            fib temp3 temp4)));;
  let rec fib =
    fun n ->
      (fun k ->
         (let temp7 = n < 2 in
         if temp7 then k n
         else (let temp9 = n - 1 in
           let temp10 = ll_0 k n in
           fib temp9 temp10)));;
  let ll_2 = fun x -> x;;
  let main = let temp15 = fib 6 ll_2 in
             print_int temp15;;
