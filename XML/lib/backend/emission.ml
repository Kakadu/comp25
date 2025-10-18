(** Copyright 2024, Mikhail Gavrilenko, Danila Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base.Format
open Base
open Machine
open Target

module Emission = struct
  let code : (instr * string) Queue.t = Queue.create ()
  let emit ?(comm = "") push_instr = push_instr (fun i -> Queue.enqueue code (i, comm))

  let flush_queue ppf =
    while not (Queue.is_empty code) do
      let i, comm = Queue.dequeue_exn code in
      (match i with
       | Label _ -> fprintf ppf "%a" pp_instr i
       | _ -> fprintf ppf "  %a" pp_instr i);
      if String.(comm <> "") then fprintf ppf " # %s" comm;
      fprintf ppf "\n"
    done
  ;;

  let emit_bin_op op rd r1 r2 =
    match op with
    | "+" -> emit add rd r1 r2
    | "-" -> emit sub rd r1 r2
    | "*" -> emit mul rd r1 r2
    | "=" ->
      emit xor rd r1 r2;
      emit seqz rd rd
    | "<" -> emit slt rd r1 r2
    | ">" -> emit slt rd r2 r1
    | "<=" ->
      emit slt rd r2 r1;
      emit xori rd rd 1
    | ">=" ->
      emit slt rd r1 r2;
      emit xori rd rd 1
    | "<>" ->
      let temp = T 2 in
      emit xor temp r1 r2;
      (* temp = 0 if r1 == r2, non-zero otherwise *)
      emit snez rd temp (* dst = 1 if temp != 0, else 0 *)
    | _ -> failwith ("Unknown binary operator: " ^ op)
  ;;

  (*миша я переписал через емит чтобы у нас вся оработка шла черз один модуль*)
  (*re: horosho ;)*)
  let emit_prologue name stack_size =
    (* name: *)
    emit label name;
    (* addi sp, sp, -stack_size *)
    emit addi SP SP (-stack_size);
    (* sd ra, (sp + stack_size - word) *)
    emit sd RA (SP, stack_size - Target.word_size);
    (* sd fp(S0), (sp + stack_size - 2*word) *)
    emit sd (S 0) (SP, stack_size - (2 * Target.word_size));
    (* fp := sp + stack_size - 2*word *)
    emit addi (S 0) SP (stack_size - (2 * Target.word_size))
  ;;

  let emit_epilogue stack_size =
    let (_ : 'a) = stack_size in
    (* should be used in future *)
    emit addi SP (S 0) (2 * Target.word_size);
    (* sp = fp + 2*word *)
    emit ld RA (S 0, Target.word_size);
    (* ra = [fp+word] *)
    emit ld (S 0) (S 0, 0);
    (* fp = [fp+0] *)
    emit ret
  ;;
end
