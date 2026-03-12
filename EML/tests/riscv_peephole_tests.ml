(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EML_lib
open Backend.Ricsv.Architecture
open Riscv_backend

let print_instrs instructions =
  let rendered =
    List.map (fun instruction -> Format.asprintf "%a" pp_instr instruction) instructions
  in
  print_endline (String.concat "\n" rendered)
;;

let run_peephole input = input |> Backend.Ricsv.Peephole.optimize |> print_instrs

let%expect_test "optimizes repeated stack load pattern from task description" =
  let input =
    [ Li (T 0, 1)
    ; Ld (T 1, (SP, 64))
    ; Add (T 0, T 1, T 0)
    ; Sd (T 1, (SP, 64))
    ; Li (T 0, 2)
    ; Ld (T 1, (SP, 64))
    ; Mul (T 0, T 1, T 0)
    ; Sd (T 1, (SP, 64))
    ]
  in
  run_peephole input;
  [%expect
    {|
li t0, 1
ld t1, 64(sp)
slli t0, t1, 1
|}]
;;
