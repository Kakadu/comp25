(** Copyright 2025-2026, Rodion Suvorov, Dmitriy Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parser
open Codegen
open Inferencer.Infer

let run str =
  match parse_str str with
  | str ->
    (match run_infer_program str env_with_things with
     | Ok _ -> Format.printf "%a\n%!" codegen str
     | Error _ -> Format.printf "Parsing error\n")
;;

let%expect_test "binary operations" =
  run
    {|
  let f =
    let x = 52 + 52 in
    let y = 52 - 52 in
    let z = 52 * 52 in
    let w = 52 <= 52 in
    x
  ;;
  |};
  [%expect
    {|
      .text
      .globl f
      .type f, @function
    f:
      addi sp, sp, -48
      sd ra, 40(sp)
      sd s0, 32(sp)
      addi s0, sp, 48
      li t0, 52
      li t1, 52
      add t0, t0, t1
      sd t0, -24(s0)
      li t0, 52
      li t1, 52
      sub t0, t0, t1
      sd t0, -32(s0)
      li t0, 52
      li t1, 52
      mul t0, t0, t1
      sd t0, -40(s0)
      li t0, 52
      li t1, 52
      slt t0, t1, t0
      xori t0, t0, 1
      sd t0, -48(s0)
      ld a0, -24(s0)
    f_end:
      ld ra, 40(sp)
      ld s0, 32(sp)
      addi sp, sp, 48
      ret |}]
;;

let%expect_test "some branches" =
  run
    {|
  let f =
    let x = 5 in
    let y = 2 in
    let z = 3 in
    let w = 4 in
    if x <= y then z else w
  ;;
  |};
  [%expect
    {|
      .text
      .globl f
      .type f, @function
    f:
      addi sp, sp, -48
      sd ra, 40(sp)
      sd s0, 32(sp)
      addi s0, sp, 48
      li t0, 5
      sd t0, -24(s0)
      li t0, 2
      sd t0, -32(s0)
      li t0, 3
      sd t0, -40(s0)
      li t0, 4
      sd t0, -48(s0)
      ld t0, -24(s0)
      ld t1, -32(s0)
      slt t0, t1, t0
      xori t0, t0, 1
      beq t0, x0, .Lelse_0
      ld a0, -40(s0)
      j .Lendif_1
    .Lelse_0:
      ld a0, -48(s0)
    .Lendif_1:
    f_end:
      ld ra, 40(sp)
      ld s0, 32(sp)
      addi sp, sp, 48
      ret |}]
;;
