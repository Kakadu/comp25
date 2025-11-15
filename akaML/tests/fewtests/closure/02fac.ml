let rec fac n = if n < 2 then 1 else n * fac (n - 1)

let main =
  let multiplier = 2 in
  let subfac k =
    let result = fac 4 in
    k (result * multiplier)
  in
  let _ = print_gc_status () in
  let _ = collect () in
  let _ = print_gc_status () in
  let _ = print_int (subfac (fun x -> x)) in
  let _ = collect () in
  print_gc_status ()
;;
