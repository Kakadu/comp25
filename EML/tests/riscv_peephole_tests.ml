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

let%expect_test "removes redundant load and forwards store to load" =
  let input =
    [ Ld (T 0, (SP, 64)); Ld (T 1, (SP, 64)); Sd (T 1, (SP, 64)); Ld (A 0, (SP, 64)) ]
  in
  run_peephole input;
  [%expect
    {|
ld t0, 64(sp)
mv t1, t0
sd t1, 64(sp)
mv a0, t1
|}]
;;

let%expect_test "folds addi chain and removes dead overwrite" =
  let input =
    [ Addi (SP, SP, -16); Addi (SP, SP, 8); Li (T 0, 1); Li (T 0, 2); Addi (T 1, T 1, 0) ]
  in
  run_peephole input;
  [%expect
    {|
addi sp, sp, -8
li t0, 2
|}]
;;

let%expect_test "drops jump to the immediately following label" =
  let input = [ J "l1"; Label "l1"; Mv (A 0, A 0); Ret ] in
  run_peephole input;
  [%expect
    {|
ret
|}]
;;

let%expect_test "collapses double copy before binary op" =
  let input = [ Mv (T 0, A 0); Mv (T 1, A 0); Add (A 0, T 0, T 1) ] in
  run_peephole input;
  [%expect
    {|
add a0, a0, a0
|}]
;;

let%expect_test "propagates single mv into following consumer" =
  let input = [ Mv (T 0, A 0); Li (T 1, 1); Slt (A 0, T 0, T 1) ] in
  run_peephole input;
  [%expect
    {|
li t1, 1
slt a0, a0, t1
|}]
;;

let%expect_test "rewrites li plus add into addi" =
  let input = [ Li (T 1, 1); Add (A 0, T 0, T 1) ] in
  run_peephole input;
  [%expect
    {|
addi a0, t0, 1
|}]
;;

let%expect_test "folds li plus add when destination is constant register" =
  let input = [ Li (T 0, 1); Add (T 0, T 1, T 0) ] in
  run_peephole input;
  [%expect
    {|
addi t0, t1, 1
|}]
;;

let%expect_test "rewrites mul by power of two into slli" =
  let input = [ Li (T 0, 4); Mul (A 0, T 1, T 0) ] in
  run_peephole input;
  [%expect
    {|
slli a0, t1, 2
|}]
;;

let%expect_test "keeps load cache barriers on call" =
  let input =
    [ Ld (T 0, (SP, 64)); Call "foo"; Ld (T 1, (SP, 64)); Add (A 0, T 0, T 1) ]
  in
  run_peephole input;
  [%expect
    {|
ld t0, 64(sp)
call foo
ld t1, 64(sp)
add a0, t0, t1
|}]
;;

let%expect_test "forwards store to following load on same slot" =
  let input = [ Sd (A 0, (fp, -16)); Ld (T 0, (fp, -16)); Add (A 0, T 0, A 1) ] in
  run_peephole input;
  [%expect
    {|
sd a0, -16(fp)
add a0, a0, a1
|}]
;;

let%expect_test "folds constant beq into jump" =
  let input = [ Li (T 0, 1); Li (T 1, 1); Beq (T 0, T 1, "else_1") ] in
  run_peephole input;
  [%expect
    {|
li t0, 1
li t1, 1
j else_1
|}]
;;

let%expect_test "removes dead store before ret in same block" =
  let input = [ Sd (A 0, (fp, -8)); Add (A 0, A 0, A 1); Ret ] in
  run_peephole input;
  [%expect
    {|
add a0, a0, a1
ret
|}]
;;

let%expect_test "keeps store before call barrier" =
  let input = [ Sd (A 0, (fp, -8)); Call "foo"; Ret ] in
  run_peephole input;
  [%expect
    {|
sd a0, -8(fp)
call foo
ret
|}]
;;

let%expect_test "removes store that restores unchanged loaded slot value" =
  let input = [ Ld (T 1, (sp, 64)); Add (T 0, T 1, T 0); Sd (T 1, (sp, 64)) ] in
  run_peephole input;
  [%expect
    {|
ld t1, 64(sp)
add t0, t1, t0
|}]
;;

let%expect_test "drops overwritten store before next store to same slot" =
  let input = [ Sd (A 0, (fp, -8)); Add (A 0, A 0, A 1); Sd (T 0, (fp, -8)); Ret ] in
  run_peephole input;
  [%expect
    {|
add a0, a0, a1
ret
|}]
;;
