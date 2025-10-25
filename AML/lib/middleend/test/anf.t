(** Copyright 2025-2026, Rodion Suvorov, Dmitriy Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
  $ cat >fac.ml <<EOF
  > let rec fac n =
  >   if n <= 1
  >   then 1
  >   else (let n1 = n-1 in
  >      let m = fac n1 in
  >      n*m)
  > 
  > let main = print_int (fac 4)
  > EOF
  $ ../../../bin/AML.exe --dump-anf fac.ml
  let rec fac =
    fun n ->
      let t_0 = n <= 1 in
      if t_0 then
        1
      else
        let t_1 = n - 1 in
        let n1 = t_1 in
        let t_2 = fac n1 in
        let m = t_2 in
        n * m
  
  let main =
    let t_4 = fac 4 in
    print_int t_4

  $ cat >fib.ml <<EOF
  > let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)
  > let main = let () = print_int (fib 4) in 0
  > EOF
  $ ../../../bin/AML.exe --dump-anf fib.ml
  let rec fib =
    fun n ->
      let t_0 = n < 2 in
      if t_0 then
        n
      else
        let t_1 = n - 1 in
        let t_2 = fib t_1 in
        let t_3 = n - 2 in
        let t_4 = fib t_3 in
        t_2 + t_4
  
  let main =
    let t_6 = fib 4 in
    let t_7 = print_int t_6 in
    let () = t_7 in
    0
  $ cat >ite.ml <<EOF
  > let large x = if 0<>x then print_int 0 else print_int 1
  >   let main =
  >      let x = if (if (if 0=1
  >                      then 0 else (let t42 = print_int 42 in 1))=1
  >                  then 0 else 1)=1
  >              then 0 else 1 in
  >      large x
  > EOF
  $ ../../../bin/AML.exe --dump-anf ite.ml
  let large =
    fun x ->
      let t_0 = 0 <> x in
      if t_0 then
        print_int 0
      else
        print_int 1
  
  let main =
    let t_3 = 0 = 1 in
    if t_3 then
      let t_4 = 0 = 1 in
      if t_4 then
        let t_5 = 0 = 1 in
        if t_5 then
          let x = 0 in
          large x
        else
          let x = 1 in
          large x
      else
        let t_8 = 1 = 1 in
        if t_8 then
          let x = 0 in
          large x
        else
          let x = 1 in
          large x
    else
      let t_11 = print_int 42 in
      let t42 = t_11 in
      let t_12 = 1 = 1 in
      if t_12 then
        let t_13 = 0 = 1 in
        if t_13 then
          let x = 0 in
          large x
        else
          let x = 1 in
          large x
      else
        let t_16 = 1 = 1 in
        if t_16 then
          let x = 0 in
          large x
        else
          let x = 1 in
          large x

  $ cat >faccps_ll.ml <<EOF
  > let id x = x
  > let fresh_1 n k p = k (p * n)
  > 
  > let rec fac_cps n k =
  >   if n = 1
  >   then k 1
  >  else fac_cps (n-1) (fresh_1 n k)
  > 
  > let main =
  >   let () = print_int (fac_cps 4 id) in
  >   0
  > EOF
  $ ../../../bin/AML.exe --dump-anf faccps_ll.ml
  let id =
    fun x ->
      x
  
  let fresh_1 =
    fun n ->
      fun k ->
        fun p ->
          let t_0 = p * n in
          k t_0
  
  let rec fac_cps =
    fun n ->
      fun k ->
        let t_2 = n = 1 in
        if t_2 then
          k 1
        else
          let t_4 = n - 1 in
          let t_5 = fresh_1 n k in
          fac_cps t_4 t_5
  
  let main =
    let t_7 = fac_cps 4 id in
    let t_8 = print_int t_7 in
    let () = t_8 in
    0
