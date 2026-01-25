let rec f = fun n -> if n = 1 then 1 else f (n - 1) * n

let main =
  fun () ->
  let _ = print_int (f 1) in
  let _ = print_int (f 2) in
  let _ = print_int (f 5) in
  print_int (f 8)
;;
