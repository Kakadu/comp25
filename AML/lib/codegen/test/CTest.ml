(** Copyright 2025-2026, Rodion Suvorov, Dmitriy Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parser
open Codegen
open Inferencer.Infer
open Middle.Anf

let run str =
  match parse_str str with
  | program ->
    (match run_infer_program program env_with_things with
     | Ok _ ->
       (match anf_transform program with
        | Ok aprogram -> Format.printf "%a\n%!" codegen aprogram
        | Error msg -> Format.eprintf "ANF transform error: %s\n%!" msg)
     | Error _ -> Format.eprintf "Parsing error\n%!")
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
      addi sp, sp, -80
      sd ra, 72(sp)
      sd s0, 64(sp)
      addi s0, sp, 80
      li t0, 52
      li t1, 52
      add t0, t0, t1
      sd t0, -24(s0)
      ld t0, -24(s0)
      sd t0, -32(s0)
      li t0, 52
      li t1, 52
      sub t0, t0, t1
      sd t0, -40(s0)
      ld t0, -40(s0)
      sd t0, -48(s0)
      li t0, 52
      li t1, 52
      mul t0, t0, t1
      sd t0, -56(s0)
      ld t0, -56(s0)
      sd t0, -64(s0)
      li t0, 52
      li t1, 52
      slt t0, t1, t0
      xori t0, t0, 1
      sd t0, -72(s0)
      ld t0, -72(s0)
      sd t0, -80(s0)
      ld a0, -32(s0)
    f_end:
      ld ra, 72(sp)
      ld s0, 64(sp)
      addi sp, sp, 80
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
      addi sp, sp, -56
      sd ra, 48(sp)
      sd s0, 40(sp)
      addi s0, sp, 56
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
      sd t0, -56(s0)
      ld t0, -56(s0)
      beq t0, x0, .Lelse_0
      ld a0, -40(s0)
      j .Lendif_1
    .Lelse_0:
      ld a0, -48(s0)
    .Lendif_1:
    f_end:
      ld ra, 48(sp)
      ld s0, 40(sp)
      addi sp, sp, 56
      ret |}]
;;
