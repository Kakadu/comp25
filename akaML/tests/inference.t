Copyright 2025-2026, Friend-zva, RodionovMaxim05
SPDX-License-Identifier: LGPL-3.0-or-later

====================== do_not_type ======================
  $ ../bin/akaML.exe -inference -fromfile manytests/do_not_type/001.ml
  Inferencer error: Undefined variable 'fac'

  $ ../bin/akaML.exe -inference -fromfile manytests/do_not_type/002if.ml
  Inferencer error: Unification failed on int and bool

  $ ../bin/akaML.exe -inference -fromfile manytests/do_not_type/003occurs.ml
  Inferencer error: Occurs check failed: the type variable 'ty1 occurs inside 'ty1 -> 'ty3

  $ ../bin/akaML.exe -inference -fromfile manytests/do_not_type/004let_poly.ml
  Inferencer error: Unification failed on int and bool

  $ ../bin/akaML.exe -inference -fromfile manytests/do_not_type/015tuples.ml
  Inferencer error: Only variables are allowed as left-hand side of `let rec'

  $ ../bin/akaML.exe -inference -fromfile manytests/do_not_type/099.ml
  Inferencer error: Only variables are allowed as left-hand side of `let rec'

====================== typed ======================
  $ ../bin/akaML.exe -inference -fromfile manytests/typed/001fac.ml
  val fac : int -> int
  val main : int

  $ ../bin/akaML.exe -inference -fromfile manytests/typed/002fac.ml
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int

  $ ../bin/akaML.exe -inference -fromfile manytests/typed/003fib.ml
  val fib_acc : int -> int -> int -> int
  val fib : int -> int
  val main : int

  $ ../bin/akaML.exe -inference -fromfile manytests/typed/004manyargs.ml
  val wrap : 'a -> 'a
  val test3 : int -> int -> int -> int
  val test10 : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val main : int

  $ ../bin/akaML.exe -inference -fromfile manytests/typed/005fix.ml
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val fac : (int -> int) -> int -> int
  val main : int

  $ ../bin/akaML.exe -inference -fromfile manytests/typed/006partial.ml
  val foo : int -> int
  val main : int

  $ ../bin/akaML.exe -inference -fromfile manytests/typed/006partial2.ml
  val foo : int -> int -> int -> int
  val main : int

  $ ../bin/akaML.exe -inference -fromfile manytests/typed/006partial3.ml
  val foo : int -> int -> int -> unit
  val main : int

  $ ../bin/akaML.exe -inference -fromfile manytests/typed/007order.ml
  val _start : unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main : unit

  $ ../bin/akaML.exe -inference -fromfile manytests/typed/008ascription.ml
  val addi : ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
  val main : int

  $ ../bin/akaML.exe -inference -fromfile manytests/typed/009let_poly.ml
  val temp : int * bool

  $ ../bin/akaML.exe -inference -fromfile manytests/typed/010fac_anf.ml
  val fac : int -> int
  val main : int

  $ ../bin/akaML.exe -inference -fromfile manytests/typed/010faccps_ll.ml
  val id : 'a -> 'a
  val fresh_1 : int -> (int -> 'a) -> int -> 'a
  val fac_cps : int -> (int -> 'a) -> 'a
  val main : int

  $ ../bin/akaML.exe -inference -fromfile manytests/typed/010fibcps_ll.ml
  val id : 'a -> 'a
  val fresh_2 : int -> (int -> 'a) -> int -> 'a
  val fresh_1 : int -> (int -> 'a) -> (int -> (int -> 'a) -> 'b) -> int -> 'b
  val fib : int -> (int -> 'a) -> 'a
  val main : int

  $ ../bin/akaML.exe -inference -fromfile manytests/typed/010sukharev.ml
  Inferencer error: Unification failed on int * int * int and 'ty18 * 'ty19

  $ ../bin/akaML.exe -inference -fromfile manytests/typed/011mapcps.ml
  val map : ('c -> 'a) -> 'c list -> ('a list -> 'b) -> 'b
  val iter : ('a -> 'b) -> 'a list -> unit
  val main : unit

  $ ../bin/akaML.exe -inference -fromfile manytests/typed/012fibcps.ml
  val fib : int -> (int -> 'a) -> 'a
  val main : unit

  $ ../bin/akaML.exe -inference -fromfile manytests/typed/013foldfoldr.ml
  val id : 'a -> 'a
  val fold_right : ('b -> 'a -> 'a) -> 'a -> 'b list -> 'a
  val foldl : ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b
  val main : unit

  $ ../bin/akaML.exe -inference -fromfile manytests/typed/015tuples.ml
  val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
  val map : ('b -> 'a) -> 'b * 'b -> 'a * 'a
  val fixpoly : (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) * (('a -> 'b) * ('a -> 'b) -> 'a -> 'b) -> ('a -> 'b) * ('a -> 'b)
  val feven : 'a * (int -> int) -> int -> int
  val fodd : (int -> int) * 'a -> int -> int
  val tie : (int -> int) * (int -> int)
  val meven : int -> int
  val modd : int -> int
  val main : int

  $ ../bin/akaML.exe -inference -fromfile manytests/typed/016lists.ml
  val length : 'a list -> int
  val length_tail : 'a list -> int
  val map : ('a -> 'b) -> 'a list -> 'b list
  val append : 'a list -> 'a list -> 'a list
  val concat : 'a list list -> 'a list
  val iter : ('a -> unit) -> 'a list -> unit
  val cartesian : 'a list -> 'b list -> ('a * 'b) list
  val main : int
