[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Gleb Nasretdinov, Ilhom Kombaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end

module State (S : sig
    type state
  end) =
struct
  type state = S.state
  type 'a t = state -> state * 'a

  let return v st = st, v

  let ( >>= ) m f st =
    let s1, v = m st in
    f v s1
  ;;

  let ( >>| ) m f = m >>= fun x -> return (f x)
  let ( let* ) = ( >>= )
  let ( let+ ) = ( >>| )
  let get s = s, s
  let put s v = s, v

  let modify f =
    let* s = get in
    put (f s) >>| fun _ -> ()
  ;;

  let run m = m
end

module StateR (S : sig
    type state
    type error
  end) =
struct
  type state = S.state
  type error = S.error
  type 'a t = state -> state * ('a, error) result

  let return v st = st, Ok v

  let ( >>= ) m f st =
    let s1, r = m st in
    match r with
    | Error e -> s1, Error e
    | Ok v -> f v s1
  ;;

  let ( >>| ) m f = m >>= fun x -> return (f x)
  let ( let* ) = ( >>= )
  let ( let+ ) = ( >>| )
  let get s = s, Ok s
  let put s v = s, Ok v

  let modify f =
    let* s = get in
    put (f s) >>| fun _ -> ()
  ;;

  let fail e st = st, Error e
  let run m = m
end

module Counter = struct
  include State (struct
      type state = int
    end)

  let make_fresh : state t =
    let* st = get in
    put (st + 1) >>| fun _ -> st
  ;;
end

module CounterR = struct
  include StateR (struct
      type state = int
      type error = string
    end)

  let make_fresh : state t =
    let* st = get in
    put (st + 1) >>| fun _ -> st
  ;;
end
