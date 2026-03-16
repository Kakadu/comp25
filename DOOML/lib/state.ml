(** Copyright 2025-2026, Georgiy Belyanin, Ignat Sergeev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module M (S : sig
    type t
  end) =
struct
  type state = S.t
  type 'a t = state -> 'a * state

  let return v st = v, st

  let ( >>= ) m f st =
    let v, st' = m st in
    f v st'
  ;;

  let ( let* ) = ( >>= )
  let get st = st, st
  let put st v = v, st
  let run m = m
end
