[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Parser

let run str =
  match parse str with
  | Ok ast -> Format.printf "%a\n%!" RiscV.Codegen.gen_structure ast
  | Error _ -> Format.printf "Parsing error\n"
;;

let%expect_test "codegen bin op" =
  run
    {|
  let foo =
    let a = 1 + 2 in
    let b = 3 - 4 in
    let c = 5 * 6 in
    let d = 7 <= 8 in
    let e = 9 >= 10 in
    let f = 11 = 12 in
    let g = 13 <> 14 in
    a
  ;;
  |};
  [%expect
    {|
    .section .text
      .globl foo
      .type foo, @function
    foo:
      addi sp, sp, -72
      sd ra, 64(sp)
      sd s0, 56(sp)
      addi s0, sp, 56 # Prologue ends
      li t0, 1
      li t1, 2
      add  a0, t0, t1
      sd a0, -8(s0) # a
      li t0, 3
      li t1, 4
      sub a0, t0, t1
      sd a0, -16(s0) # b
      li t0, 5
      li t1, 6
      mul a0, t0, t1
      sd a0, -24(s0) # c
      li t0, 7
      li t1, 8
      slt a0, t1, t0
      xori a0, a0, 1
      sd a0, -32(s0) # d
      li t0, 9
      li t1, 10
      slt a0, t0, t1
      xori a0, a0, 1
      sd a0, -40(s0) # e
      li t0, 11
      li t1, 12
      xor a0, t0, t1
      seqz a0, a0
      sd a0, -48(s0) # f
      li t0, 13
      li t1, 14
      xor a0, t0, t1
      snez a0, a0
      sd a0, -56(s0) # g
      ld a0, -8(s0)
      addi sp, s0, 16 # Epilogue starts
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret |}]
;;
