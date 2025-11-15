let main =
  let x = 10 in
  let y = 20 in
  let adder = fun a -> x + y + a in
  let _ = print_gc_status () in
  let _ = collect () in
  let _ = print_gc_status () in
  let _ = adder 5 in
  let _ = collect () in
  print_gc_status ()
;;
