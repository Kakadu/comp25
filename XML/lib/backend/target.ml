(** Copyright 2024,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Target = struct
  let word_size = 8
  let arg_regs = [| "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6"; "a7" |]
  let temp_regs = [| "t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6" |]
end
