(** Copyright 2025-2026, Georgiy Belyanin, Ignat Sergeev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

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
  ; { name = "get_heap_start"; arity = 1 }
  ; { name = "get_heap_fin"; arity = 1 }
  ; { name = "print_gc_status"; arity = 1 }
  ]
;;
