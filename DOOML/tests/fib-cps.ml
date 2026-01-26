let rec fib_cps =
  fun n k ->
  if n <= 1
  then k 1
  else fib_cps (n - 1) (fun fib1 -> fib_cps (n - 2) (fun fib2 -> k (fib1 + fib2)))
;;

let f = fun n -> fib_cps n (fun x -> x)

let main =
  let _ = print_int (f 1) in
  let _ = print_int (f 2) in
  let _ = print_int (f 5) in
  print_int (f 8)
;;
