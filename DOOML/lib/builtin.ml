type t =
  { name : string
  ; arity : int
  }

let all =
  [ { name = "*"; arity = 2 }
  ; { name = "/"; arity = 2 }
  ; { name = "+"; arity = 2 }
  ; { name = "-"; arity = 2 }
  ; { name = "<"; arity = 2 }
  ; { name = "<="; arity = 2 }
  ; { name = "=="; arity = 2 }
  ; { name = "="; arity = 2 }
  ; { name = "<>"; arity = 2 }
  ; { name = ">"; arity = 2 }
  ; { name = ">="; arity = 2 }
  ; { name = "&&"; arity = 2 }
  ; { name = "||"; arity = 2 }
  ; { name = "print_int"; arity = 1 }
  ; { name = "tuple_nth"; arity = 2 }
  ; { name = "collect"; arity = 1 }
  ]
;;
