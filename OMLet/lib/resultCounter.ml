(** Copyright 2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Sofya Kozyreva <k81sofia@gmail.com>, Vyacheslav Kochergin <vyacheslav.kochergin1@gmail.com> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module type ResultCounterMonadType = sig
  type ('a, 'e) t

  val return : 'a -> ('a, 'e) t
  val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val fail : 'e -> ('a, 'e) t
  val read : (int, 'e) t
  val write : int -> (unit, 'e) t
  val run : ('a, 'e) t -> int -> ('a * int, 'e) result

  module Syntax : sig
    val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  end
end

module ResultCounterMonad : ResultCounterMonadType = struct
  type ('a, 'e) t = int -> ('a * int, 'e) result

  let return x c = Result.Ok (x, c)

  let ( >>= ) x f c =
    match x c with
    | Ok (x, c') -> f x c'
    | Result.Error e -> Result.Error e
  ;;

  let fail e _ = Result.Error e
  let read c = Result.Ok (c, c)
  let write c _ = Result.Ok ((), c)
  let run m = m

  module Syntax = struct
    let ( let* ) = ( >>= )
  end
end
