(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EML_lib
open Frontend.Parser
open Middleend.Anf
open Middleend.Inferencer
open Middleend.Resolve_builtins
open Backend.Ricsv.Architecture
open Riscv_backend

let print_instrs instructions =
  let rendered =
    List.map (fun instruction -> Format.asprintf "%a" pp_instr instruction) instructions
  in
  print_endline (String.concat "\n" rendered)
;;

let compile_riscv ?(enable_peephole = true) src =
  match parse src with
  | Error e -> "Parse error: " ^ e
  | Ok ast ->
    let scope = TypeEnv.keys TypeEnv.initial_env in
    let ast' = resolve_program ast scope in
    (match anf_program ast' with
     | Error e -> "ANF error: " ^ e
     | Ok anf ->
       let buf = Buffer.create 1024 in
       let ppf = Format.formatter_of_buffer buf in
       (match Backend.Ricsv.Runner.gen_program ~enable_peephole ppf anf with
        | Ok () ->
          Format.pp_print_flush ppf ();
          Buffer.contents buf
        | Error e -> "Codegen error: " ^ e))
;;

let show_diff ~input ~output value =
  print_endline "=== Without peepholes ===";
  input value;
  print_endline "";
  print_endline "=== With peepholes ===";
  output value
;;

let show_codogen_diff src =
  show_diff
    ~input:(fun source -> print_endline (compile_riscv ~enable_peephole:false source))
    ~output:(fun source -> print_endline (compile_riscv ~enable_peephole:true source))
    src
;;

let show_instr_diff instrs =
  show_diff
    ~input:print_instrs
    ~output:(fun instructions ->
      instructions |> Backend.Ricsv.Peephole.optimize |> print_instrs)
    instrs
;;

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
  show_instr_diff input;
  [%expect
    {|
=== Without peepholes ===
li t0, 1
ld t1, 64(sp)
add t0, t1, t0
sd t1, 64(sp)
li t0, 2
ld t1, 64(sp)
mul t0, t1, t0
sd t1, 64(sp)

=== With peepholes ===
li t0, 1
ld t1, 64(sp)
slli t0, t1, 1
|}]
;;

let%expect_test "removes redundant load and forwards store to load" =
  let input =
    [ Ld (T 0, (SP, 64)); Ld (T 1, (SP, 64)); Sd (T 1, (SP, 64)); Ld (A 0, (SP, 64)) ]
  in
  show_instr_diff input;
  [%expect
    {|
=== Without peepholes ===
ld t0, 64(sp)
ld t1, 64(sp)
sd t1, 64(sp)
ld a0, 64(sp)

=== With peepholes ===
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
  show_instr_diff input;
  [%expect
    {|
=== Without peepholes ===
addi sp, sp, -16
addi sp, sp, 8
li t0, 1
li t0, 2
addi t1, t1, 0

=== With peepholes ===
addi sp, sp, -8
li t0, 2
|}]
;;

let%expect_test "drops jump to the immediately following label" =
  let input = [ J "l1"; Label "l1"; Mv (A 0, A 0); Ret ] in
  show_instr_diff input;
  [%expect
    {|
=== Without peepholes ===
j l1
l1:
mv a0, a0
ret

=== With peepholes ===
ret
|}]
;;

let%expect_test "collapses double copy before binary op" =
  let input = [ Mv (T 0, A 0); Mv (T 1, A 0); Add (A 0, T 0, T 1) ] in
  show_instr_diff input;
  [%expect
    {|
=== Without peepholes ===
mv t0, a0
mv t1, a0
add a0, t0, t1

=== With peepholes ===
add a0, a0, a0
|}]
;;

let%expect_test "propagates single mv into following consumer" =
  let input = [ Mv (T 0, A 0); Li (T 1, 1); Slt (A 0, T 0, T 1) ] in
  show_instr_diff input;
  [%expect
    {|
=== Without peepholes ===
mv t0, a0
li t1, 1
slt a0, t0, t1

=== With peepholes ===
li t1, 1
slt a0, a0, t1
|}]
;;

let%expect_test "rewrites li plus add into addi" =
  let input = [ Li (T 1, 1); Add (A 0, T 0, T 1) ] in
  show_instr_diff input;
  [%expect
    {|
=== Without peepholes ===
li t1, 1
add a0, t0, t1

=== With peepholes ===
addi a0, t0, 1
|}]
;;

let%expect_test "folds li plus add when destination is constant register" =
  let input = [ Li (T 0, 1); Add (T 0, T 1, T 0) ] in
  show_instr_diff input;
  [%expect
    {|
=== Without peepholes ===
li t0, 1
add t0, t1, t0

=== With peepholes ===
addi t0, t1, 1
|}]
;;

let%expect_test "rewrites mul by power of two into slli" =
  let input = [ Li (T 0, 4); Mul (A 0, T 1, T 0) ] in
  show_instr_diff input;
  [%expect
    {|
=== Without peepholes ===
li t0, 4
mul a0, t1, t0

=== With peepholes ===
slli a0, t1, 2
|}]
;;

let%expect_test "keeps load cache barriers on call" =
  let input =
    [ Ld (T 0, (SP, 64)); Call "foo"; Ld (T 1, (SP, 64)); Add (A 0, T 0, T 1) ]
  in
  show_instr_diff input;
  [%expect
    {|
=== Without peepholes ===
ld t0, 64(sp)
call foo
ld t1, 64(sp)
add a0, t0, t1

=== With peepholes ===
ld t0, 64(sp)
call foo
ld t1, 64(sp)
add a0, t0, t1
|}]
;;

let%expect_test "forwards store to following load on same slot" =
  let input = [ Sd (A 0, (fp, -16)); Ld (T 0, (fp, -16)); Add (A 0, T 0, A 1) ] in
  show_instr_diff input;
  [%expect
    {|
=== Without peepholes ===
sd a0, -16(fp)
ld t0, -16(fp)
add a0, t0, a1

=== With peepholes ===
sd a0, -16(fp)
add a0, a0, a1
|}]
;;

let%expect_test "folds constant beq into jump" =
  let input = [ Li (T 0, 1); Li (T 1, 1); Beq (T 0, T 1, "else_1") ] in
  show_instr_diff input;
  [%expect
    {|
=== Without peepholes ===
li t0, 1
li t1, 1
beq t0, t1, else_1

=== With peepholes ===
li t0, 1
li t1, 1
j else_1
|}]
;;

let%expect_test "removes dead store before ret in same block" =
  let input = [ Sd (A 0, (fp, -8)); Add (A 0, A 0, A 1); Ret ] in
  show_instr_diff input;
  [%expect
    {|
=== Without peepholes ===
sd a0, -8(fp)
add a0, a0, a1
ret

=== With peepholes ===
add a0, a0, a1
ret
|}]
;;

let%expect_test "keeps store before call barrier" =
  let input = [ Sd (A 0, (fp, -8)); Call "foo"; Ret ] in
  show_instr_diff input;
  [%expect
    {|
=== Without peepholes ===
sd a0, -8(fp)
call foo
ret

=== With peepholes ===
sd a0, -8(fp)
call foo
ret
|}]
;;

let%expect_test "removes store that restores unchanged loaded slot value" =
  let input = [ Ld (T 1, (sp, 64)); Add (T 0, T 1, T 0); Sd (T 1, (sp, 64)) ] in
  show_instr_diff input;
  [%expect
    {|
=== Without peepholes ===
ld t1, 64(sp)
add t0, t1, t0
sd t1, 64(sp)

=== With peepholes ===
ld t1, 64(sp)
add t0, t1, t0
|}]
;;

let%expect_test "drops overwritten store before next store to same slot" =
  let input = [ Sd (A 0, (fp, -8)); Add (A 0, A 0, A 1); Sd (T 0, (fp, -8)); Ret ] in
  show_instr_diff input;
  [%expect
    {|
=== Without peepholes ===
sd a0, -8(fp)
add a0, a0, a1
sd t0, -8(fp)
ret

=== With peepholes ===
add a0, a0, a1
ret
|}]
;;

let%expect_test "shows code with and without peephole 1" =
  let src =
    {|
      let f x =
        let y = x < 0 in
        y + 1
      let main = f 1
    |}
  in
  show_codogen_diff src;
  [%expect
    {|
    === Without peepholes ===
    .section .text
      .globl f
      .type f, @function
    f:
      addi sp, sp, -24
      sd ra, 16(sp)
      sd fp, 8(sp)
      addi fp, sp, 8
      sd a0, -8(fp)
      ld t0, -8(fp)
      li t1, 1
      slt a0, t0, t1
      add a0, a0, a0
      addi a0, a0, 1
      sd a0, -16(fp)
      ld t0, -16(fp)
      li t1, 3
      add a0, t0, t1
      addi a0, a0, -1
      addi sp, fp, 16
      ld ra, 8(fp)
      ld fp, 0(fp)
      ret

      .globl main
      .type main, @function
    main:
      addi sp, sp, -200
      sd ra, 192(sp)
      sd fp, 184(sp)
      addi fp, sp, 184
      li a0, 3
      call f
      addi sp, fp, 16
      ld ra, 8(fp)
      ld fp, 0(fp)
      li a0, 0
      ret


    === With peepholes ===
    .section .text
      .globl f
      .type f, @function
    f:
      addi sp, sp, -24
      sd ra, 16(sp)
      sd fp, 8(sp)
      addi fp, sp, 8
      sd a0, -8(fp)
      li t1, 1
      slt a0, a0, t1
      add a0, a0, a0
      addi a0, a0, 1
      sd a0, -16(fp)
      addi a0, a0, 2
      addi sp, fp, 16
      ld ra, 8(fp)
      ld fp, 0(fp)
      ret

      .globl main
      .type main, @function
    main:
      addi sp, sp, -200
      sd ra, 192(sp)
      sd fp, 184(sp)
      addi fp, sp, 184
      li a0, 3
      call f
      addi sp, fp, 16
      ld ra, 8(fp)
      ld fp, 0(fp)
      li a0, 0
      ret |}]
;;

let%expect_test "shows code with and without peephole 2" =
  let src =
    {|
      let f y =
        let x = 1 + y in
        let z = 2 * y in
        x + z
      let main = f 10
    |}
  in
  show_codogen_diff src;
  [%expect
    {|
    === Without peepholes ===
    .section .text
      .globl f
      .type f, @function
    f:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd fp, 16(sp)
      addi fp, sp, 16
      sd a0, -8(fp)
      li t0, 3
      ld t1, -8(fp)
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -16(fp)
      li t0, 5
      ld t1, -8(fp)
      srli t0, t0, 1
      addi t1, t1, -1
      mul a0, t0, t1
      addi a0, a0, 1
      sd a0, -24(fp)
      ld t0, -16(fp)
      ld t1, -24(fp)
      add a0, t0, t1
      addi a0, a0, -1
      addi sp, fp, 16
      ld ra, 8(fp)
      ld fp, 0(fp)
      ret

      .globl main
      .type main, @function
    main:
      addi sp, sp, -200
      sd ra, 192(sp)
      sd fp, 184(sp)
      addi fp, sp, 184
      li a0, 21
      call f
      addi sp, fp, 16
      ld ra, 8(fp)
      ld fp, 0(fp)
      li a0, 0
      ret


    === With peepholes ===
    .section .text
      .globl f
      .type f, @function
    f:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd fp, 16(sp)
      addi fp, sp, 16
      sd a0, -8(fp)
      li t0, 3
      mv t1, a0
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -16(fp)
      li t0, 5
      srli t0, t0, 1
      addi t1, t1, -1
      mul a0, t0, t1
      addi a0, a0, 1
      sd a0, -24(fp)
      ld t0, -16(fp)
      add a0, t0, a0
      addi a0, a0, -1
      addi sp, fp, 16
      ld ra, 8(fp)
      ld fp, 0(fp)
      ret

      .globl main
      .type main, @function
    main:
      addi sp, sp, -200
      sd ra, 192(sp)
      sd fp, 184(sp)
      addi fp, sp, 184
      li a0, 21
      call f
      addi sp, fp, 16
      ld ra, 8(fp)
      ld fp, 0(fp)
      li a0, 0
      ret |}]
;;

let%expect_test "shows code with and without peephole 3" =
  let src =
    {|
      let g x =
        let a = x + 1 in
        let b = a + 1 in
        b + 1
      let main = g 1
    |}
  in
  show_codogen_diff src;
  [%expect
    {|
    === Without peepholes ===
    .section .text
      .globl g
      .type g, @function
    g:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd fp, 16(sp)
      addi fp, sp, 16
      sd a0, -8(fp)
      ld t0, -8(fp)
      li t1, 3
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -16(fp)
      ld t0, -16(fp)
      li t1, 3
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -24(fp)
      ld t0, -24(fp)
      li t1, 3
      add a0, t0, t1
      addi a0, a0, -1
      addi sp, fp, 16
      ld ra, 8(fp)
      ld fp, 0(fp)
      ret

      .globl main
      .type main, @function
    main:
      addi sp, sp, -200
      sd ra, 192(sp)
      sd fp, 184(sp)
      addi fp, sp, 184
      li a0, 3
      call g
      addi sp, fp, 16
      ld ra, 8(fp)
      ld fp, 0(fp)
      li a0, 0
      ret


    === With peepholes ===
    .section .text
      .globl g
      .type g, @function
    g:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd fp, 16(sp)
      addi fp, sp, 16
      sd a0, -8(fp)
      addi a0, a0, 2
      sd a0, -16(fp)
      addi a0, a0, 2
      sd a0, -24(fp)
      addi a0, a0, 2
      addi sp, fp, 16
      ld ra, 8(fp)
      ld fp, 0(fp)
      ret

      .globl main
      .type main, @function
    main:
      addi sp, sp, -200
      sd ra, 192(sp)
      sd fp, 184(sp)
      addi fp, sp, 184
      li a0, 3
      call g
      addi sp, fp, 16
      ld ra, 8(fp)
      ld fp, 0(fp)
      li a0, 0
      ret |}]
;;
