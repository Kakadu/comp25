[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Parser

let run_default str =
  match parse str with
  | Ok ast -> Format.printf "%a\n%!" RiscV.Codegen.Default.gen_structure ast
  | Error _ -> Format.printf "Parsing error\n"
;;

let run_anf str =
  match parse str with
  | Ok ast ->
    let anf_ast = Anf.Anf_core.anf_structure ast in
    Format.printf "%a\n%!" RiscV.Codegen.Anf.gen_a_structure anf_ast
  | Error _ -> Format.printf "Parsing error\n"
;;

let%expect_test "codegen default bin op" =
  run_default
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
      ret
  |}]
;;

let%expect_test "codegen ANF bin op" =
  run_anf
    {|
  let foo = 1 + 2
  |};
  [%expect
    {|
    .section .text
      .globl foo
      .type foo, @function
    foo:
      addi sp, sp, -16
      sd ra, 8(sp)
      sd s0, 0(sp)
      addi s0, sp, 0 # Prologue ends
      li t0, 1
      li t1, 2
      add  a0, t0, t1
      addi sp, s0, 16 # Epilogue starts
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
  |}]
;;

let%expect_test "codegen default main function" =
  run_default
    {|
  let main = 
    let temp1 = fac 4 in
    temp1
  ;;
  |};
  [%expect
    {|
  .section .text
    .globl _start
    .type _start, @function
  _start:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8 # Prologue ends
    li a0, 4
    call fac
    sd a0, -8(s0) # temp1
    ld a0, -8(s0)
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    li a0, 0
    ret
  |}]
;;

let%expect_test "codegen ANF main function" =
  run_anf
    {|
  let main = fac 4
  |};
  [%expect
    (* run_default sample *)
    {|
  .section .text
    .globl main
    .type main, @function
  main:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 0 # Prologue ends
    li a0, 4
    call fac
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    li a0, 0
    ret
  |}]
;;

let%expect_test "codegen default factorial" =
  run_default
    {|
  let rec fac =
    fun n ->
    let temp1 = n = 0 in
    let temp5 =
      if temp1
      then 1
      else (
        let temp2 = n - 1 in
        let temp3 = fac temp2 in
        let temp4 = n * temp3 in
        temp4)
    in
    let temp6 = temp5 in
    temp6
  ;;
  |};
  [%expect
    {|
  .section .text
    .globl fac
    .type fac, @function
  fac:
    addi sp, sp, -64
    sd ra, 56(sp)
    sd s0, 48(sp)
    addi s0, sp, 48 # Prologue ends
    mv t0, a0
    li t1, 0
    mv a1, a0
    xor a0, t0, t1
    seqz a0, a0
    sd a0, -8(s0) # temp1
    ld t0, -8(s0)
    beq t0, zero, else_0
    li a0, 1
    j end_0
  else_0:
    mv t0, a1
    li t1, 1
    sub a0, t0, t1
    sd a0, -16(s0) # temp2
    ld a0, -16(s0)
    addi sp, sp, -8 # Saving 'live' regs
    sd a1, -24(s0)
    call fac
    sd a0, -32(s0) # temp3
    ld t0, -24(s0)
    ld t1, -32(s0)
    mul a0, t0, t1
    sd a0, -40(s0) # temp4
    ld a0, -40(s0)
  end_0:
    sd a0, -48(s0) # temp5
    ld a0, -48(s0)
    sd a0, -56(s0) # temp6
    ld a0, -56(s0)
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  |}]
;;

let%expect_test "codegen ANF factorial" =
  run_anf
    {|
  let rec fac n = if n = 0 then 1 else n * fac (n - 1)
  |};
  [%expect
    (* run_default sample *)
    {|
  .section .text
    .globl fac
    .type fac, @function
  fac:
    addi sp, sp, -40
    sd ra, 32(sp)
    sd s0, 24(sp)
    addi s0, sp, 24 # Prologue ends
    mv t0, a0
    li t1, 0
    mv a1, a0
    xor a0, t0, t1
    seqz a0, a0
    sd a0, -8(s0) # temp0
    ld t0, -8(s0)
    beq t0, zero, else_0
    li a0, 1
    j end_0
  else_0:
    mv t0, a1
    li t1, 1
    sub a0, t0, t1
    sd a0, -16(s0) # temp1
    ld a0, -16(s0)
    addi sp, sp, -8 # Saving 'live' regs
    sd a1, -24(s0)
    call fac
    sd a0, -32(s0) # temp2
    ld t0, -24(s0)
    ld t1, -32(s0)
    mul a0, t0, t1
  end_0:
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  |}]
;;
