(** Copyright 2024,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Target : sig
  val word_size : int
  val arg_regs : string array
  val temp_regs : string array
end
