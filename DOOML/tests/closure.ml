let rec add = fun a b c -> a + b + c

let main =
  fun () ->
  let f = add 1 in
  let _ = print_int (f 2 3) in
  let _ = print_int (f 3 4) in
  let g = f 10 in
  let _ = print_int (g 2) in
  let _ = print_int (g 3) in
  print_int (g 10)
;;
