let rec fib n k =
  if n < 2 then k n else fib (n - 1) (fun a -> fib (n - 2) (fun b -> k (a + b)))
;;

let main =
  let _ = print_int (fib 6 (fun x -> x)) in
  let _ = print_gc_status () in
  let _ = print_int (fib 6 (fun x -> x)) in
  let _ = print_gc_status () in
  let _ = collect () in
  print_gc_status ()
;;
