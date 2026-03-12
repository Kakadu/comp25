let ( ~> ) x f = f x
let succ x = x + 1

let main =
  let () = print_int (10 ~>succ) in
  0
;;
