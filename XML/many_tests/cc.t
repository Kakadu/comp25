  $ dune exec ./../bin/XML.exe -- --cc <<EOF
  > let main =
  > let c = 2 in
  > let add x = x + c in
  > let mul x = x * c in
  > print_int (add 3 + mul 4)
  let main = (let c = 2 in (let add = ((fun c x -> x + c) c) in (let mul = ((fun c x -> x * c) c) in (print_int ((add c) 3) + ((mul c) 4)))));;
  
  
  $ dune exec ./../bin/XML.exe -- --cc <<EOF
  > let rec fac n = if n = 0 then 1 else n * fac (n - 1)
  > 
  > let main = print_int (fac 4)
  let rec fac = (fun n -> (if n = 0
    then 1
    else n * (fac n - 1)));;
  
  let main = (print_int (fac 4));;
  
  

  $ ../bin/XML.exe --cc -fromfile manytests/typed/010fibcps_ll.ml
  let id = (fun x -> x);;
  
  let fresh_2 = (fun p1 k p2 -> (k p1 + p2));;
  
  let fresh_1 = (fun n k fib p1 -> ((fib n - 2) ((fresh_2 p1) k)));;
  
  let rec fib = (fun n k -> (if n < 2
    then (k n)
    else ((fib n - 1) ((fresh_1 n) k) fib)));;
  
  let main = (let z = (print_int ((fib 6) id)) in 0);;
  
  

  $ ../bin/XML.exe --cc -fromfile manytests/typed/004manyargs.ml
  let wrap = (fun f -> (if 1 = 1
    then f
    else f));;
  
  let test3 = (fun a b c -> (let a = (print_int a) in (let b = (print_int b) in (let c = (print_int c) in 0))));;
  
  let test10 = (fun a b c d e f g h i j -> a + b + c + d + e + f + g + h + i + j);;
  
  let main = (let rez = ((((((((((wrap test10) 1) 10) 100) 1000) 10000) 100000) 1000000) 10000000) 100000000) 1000000000 in (let (()) = (print_int rez) in (let temp2 = (((wrap test3) 1) 10) 100 in 0)));;
  
  

