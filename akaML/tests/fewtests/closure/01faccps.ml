let rec fac n k = if n < 2 then k 1 else fac (n - 1) (fun a -> k (a * n))

let main =
  let subfac = fac 4 (fun x -> x) in
  let _ = print_gc_status () in
  let _ = collect () in
  let _ = print_gc_status () in
  print_int subfac
;;
