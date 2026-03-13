(** Copyright 2026,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Machine

(** [optimize instrs] performs optimizations on the code [instrs] *)
val optimize : (instr * string) Queue.t -> (instr * string) Queue.t
