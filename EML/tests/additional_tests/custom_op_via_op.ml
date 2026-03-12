let ( ** ) x y = x * y
let ( +++ ) x y = (x ** y) + 1

let main =
  let () = print_int (3 +++ 4) in
  0
;;
