(** Copyright 2026,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Format
open Machine
open Target

let rec elim_sd_ld = function
  | (Sd (r1, o1), _) :: (Ld (r2, o2), _) :: rest
    when equal_reg r1 r2 && equal_offset o1 o2 -> elim_sd_ld rest
  | x :: rest -> x :: elim_sd_ld rest
  | [] -> []
;;

let peephole code : (instr * string) Queue.t =
  let code = Queue.to_list code in
  let optimized = elim_sd_ld code in
  Queue.of_list optimized
;;

let optimize code : (instr * string) Queue.t =
  let code = peephole code in
  code
;;
