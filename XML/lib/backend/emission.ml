(** Copyright 2024,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

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

  let emit_tagged_binop op dst r1 r2 =
    match op with
    | "+" ->
      (* (2a+1)+(2b+1)-1 = 2(a+b)+1 *)
      emit add dst r1 r2;
      emit addi dst dst (-1)
    | "-" ->
      (* (2a+1)-(2b+1)+1 = 2(a-b)+1 *)
      emit sub dst r1 r2;
      emit addi dst dst 1
    | "*" ->
      (* a = x >> 1, b = y >> 1 *)
      emit srai (T 2) r1 1;
      emit srai (T 3) r2 1;
      emit mul dst (T 2) (T 3);
      (* a*b *)
      (* (a*b) << 1 | 1  — тегаем обратно *)
      emit add dst dst dst;
      emit addi dst dst 1
    | "=" ->
      emit xor (T 2) r1 r2;
      emit seqz dst (T 2)
    | "<>" ->
      emit xor (T 2) r1 r2;
      emit snez dst (T 2)
    | "<" -> emit slt dst r1 r2
    | ">" -> emit slt dst r2 r1
    | "<=" ->
      emit slt (T 2) r2 r1;
      emit seqz dst (T 2)
    | ">=" ->
      emit slt (T 2) r1 r2;
      emit seqz dst (T 2)
    | _ -> invalid_arg ("Unknown binary operator: " ^ op)
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
