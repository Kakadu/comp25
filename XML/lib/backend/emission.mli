(** Copyright 2024,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Emission : sig
  val code : (Machine.instr * string) Base.Queue.t
  val emit : ?comm:string -> ((Machine.instr -> unit) -> 'a) -> 'a
  val flush_queue : Format.formatter -> unit
  val emit_tagged_binop : string -> Machine.reg -> Machine.reg -> Machine.reg -> unit
  val emit_prologue : string -> int -> unit
  val emit_epilogue : int -> unit
end
