let plus a b c d e = a + b + c + d + e

let main =
  let clos1 = plus 1 2 in
  let _ = print_gc_status () in
  let clos2 = clos1 3 in
  let _ = print_gc_status () in
  let _ = collect () in
  let _ = print_gc_status () in
  let clos3 = clos2 4 5 in
  let _ = collect () in
  let _ = print_gc_status () in
  print_int clos3
;;
