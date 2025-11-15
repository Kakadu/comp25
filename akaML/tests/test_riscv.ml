[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Parser

let run str =
  match parse str with
  | Ok ast ->
    (match Middleend.Anf_core.anf_structure ast with
     | Error e_anf -> Format.eprintf "ANF transformation error: %s\n%!" e_anf
     | Ok anf_ast ->
       Format.printf "%a\n%!" (RiscV.Codegen.gen_a_structure ~enable_gc:false) anf_ast)
  | Error _ -> Format.printf "Parsing error\n"
;;

let%expect_test "codegen default bin op" =
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
      li t0, 3
      li t1, 5
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -8(s0) # a
      li t0, 7
      li t1, 9
      sub a0, t0, t1
      addi a0, a0, 1
      sd a0, -16(s0) # b
      li t0, 11
      li t1, 13
      srli t0, t0, 1
      addi t1, t1, -1
      mul a0, t0, t1
      addi a0, a0, 1
      sd a0, -24(s0) # c
      li t0, 15
      li t1, 17
      slt a0, t1, t0
      xori a0, a0, 1
      sd a0, -32(s0) # d
      li t0, 19
      li t1, 21
      slt a0, t0, t1
      xori a0, a0, 1
      sd a0, -40(s0) # e
      li t0, 23
      li t1, 25
      xor a0, t0, t1
      seqz a0, a0
      sd a0, -48(s0) # f
      li t0, 27
      li t1, 29
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
  run
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
      li t0, 3
      li t1, 5
      add a0, t0, t1
      addi a0, a0, -1
      addi sp, s0, 16 # Epilogue starts
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
  |}]
;;

let%expect_test "codegen default main function" =
  run
    {|
  let id x = x

  let main = 
    let temp1 = id 4 in
    temp1
  ;;
  |};
  [%expect
    {|
  .section .text
    .globl id
    .type id, @function
  id:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 0 # Prologue ends
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret

    .globl main
    .type main, @function
  main:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8 # Prologue ends
    li a0, 9
    call id
    sd a0, -8(s0) # temp1
    ld a0, -8(s0)
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    li a0, 0
    ret
  |}]
;;

let%expect_test "codegen default factorial" =
  run
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
    li t1, 1
    mv a1, a0
    xor a0, t0, t1
    seqz a0, a0
    sd a0, -8(s0) # temp1
    ld t0, -8(s0)
    beq t0, zero, else_0
    li a0, 3
    j end_0
  else_0:
    mv t0, a1
    li t1, 3
    sub a0, t0, t1
    addi a0, a0, 1
    sd a0, -16(s0) # temp2
    addi sp, sp, -8 # Saving 'live' regs
    sd a1, -24(s0)
    ld a0, -16(s0)
    call fac
    sd a0, -32(s0) # temp3
    ld t0, -24(s0)
    ld t1, -32(s0)
    srli t0, t0, 1
    addi t1, t1, -1
    mul a0, t0, t1
    addi a0, a0, 1
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
  run
    {|
  let rec fac n = if n = 0 then 1 else n * fac (n - 1)
  |};
  [%expect
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
    li t1, 1
    mv a1, a0
    xor a0, t0, t1
    seqz a0, a0
    sd a0, -8(s0) # temp0
    ld t0, -8(s0)
    beq t0, zero, else_0
    li a0, 3
    j end_0
  else_0:
    mv t0, a1
    li t1, 3
    sub a0, t0, t1
    addi a0, a0, 1
    sd a0, -16(s0) # temp1
    addi sp, sp, -8 # Saving 'live' regs
    sd a1, -24(s0)
    ld a0, -16(s0)
    call fac
    sd a0, -32(s0) # temp2
    ld t0, -24(s0)
    ld t1, -32(s0)
    srli t0, t0, 1
    addi t1, t1, -1
    mul a0, t0, t1
    addi a0, a0, 1
  end_0:
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  |}]
;;

let%expect_test "codegen constant" =
  run
    {|
  let a = 1
  let main = print_int a
  |};
  [%expect
    {|
    .section .text
      .globl a
      .type a, @function
    a:
      addi sp, sp, -16
      sd ra, 8(sp)
      sd s0, 0(sp)
      addi s0, sp, 0 # Prologue ends
      li a0, 3
      addi sp, s0, 16 # Epilogue starts
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret

      .globl main
      .type main, @function
    main:
      addi sp, sp, -16
      sd ra, 8(sp)
      sd s0, 0(sp)
      addi s0, sp, 0 # Prologue ends
      addi sp, sp, -8 # Saving 'dangerous' args
      call a
      sd a0, -8(s0)
      ld a0, -8(s0)
      call print_int
      addi sp, s0, 16 # Epilogue starts
      ld ra, 8(s0)
      ld s0, 0(s0)
      li a0, 0
      ret
  |}]
;;

let%expect_test "codegen closure fn with 10 arg" =
  run
    {|
  let plus a b c d e f h i j k = a + b + c + d + e + f + h + i + j + k

  let main =
    let clos1 = plus 1 2 3 4 5 6 7 in
    let clos2 = clos1 8 in
    let clos3 = clos2 9 10 in
    print_int clos3
  ;;
  |};
  [%expect
    {|
    .section .text
      .globl plus
      .type plus, @function
    plus:
      addi sp, sp, -80
      sd ra, 72(sp)
      sd s0, 64(sp)
      addi s0, sp, 64 # Prologue ends
      mv t0, a0
      mv t1, a1
      sd a0, -8(s0)
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -16(s0) # temp0
      ld t0, -16(s0)
      mv t1, a2
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -24(s0) # temp1
      ld t0, -24(s0)
      mv t1, a3
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -32(s0) # temp2
      ld t0, -32(s0)
      mv t1, a4
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -40(s0) # temp3
      ld t0, -40(s0)
      mv t1, a5
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -48(s0) # temp4
      ld t0, -48(s0)
      mv t1, a6
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -56(s0) # temp5
      ld t0, -56(s0)
      mv t1, a7
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -64(s0) # temp6
      ld t0, -64(s0)
      ld t1, 16(s0)
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -72(s0) # temp7
      ld t0, -72(s0)
      ld t1, 24(s0)
      add a0, t0, t1
      addi a0, a0, -1
      addi sp, s0, 16 # Epilogue starts
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret

      .globl main
      .type main, @function
    main:
      addi sp, sp, -40
      sd ra, 32(sp)
      sd s0, 24(sp)
      addi s0, sp, 24 # Prologue ends
      la a0, plus
      li a1, 10
      call alloc_closure
      li a1, 7
      li a2, 3
      li a3, 5
      li a4, 7
      li a5, 9
      li a6, 11
      li a7, 13
      addi sp, sp, -8 # Stack space for variadic args
      li t0, 15
      sd t0, 0(sp)
      call applyN
      addi sp, sp, 8 # Restore stack after applyN
      sd a0, -8(s0) # clos1
      ld a0, -8(s0)
      li a1, 1
      li a2, 17
      call applyN
      sd a0, -16(s0) # clos2
      ld a0, -16(s0)
      li a1, 2
      li a2, 19
      li a3, 21
      call applyN
      sd a0, -24(s0) # clos3
      ld a0, -24(s0)
      call print_int
      addi sp, s0, 16 # Epilogue starts
      ld ra, 8(s0)
      ld s0, 0(s0)
      li a0, 0
      ret
  |}]
;;

let%expect_test "codegen fn with 10 arg" =
  run
    {|
  let plus a b c d e f h i j k = a + b + c + d + e + f + h + i + j + k

  let main =
    let res = plus 1 2 3 4 5 6 7 8 9 10 in
    print_int res
  ;;
  |};
  [%expect
    {|
    .section .text
      .globl plus
      .type plus, @function
    plus:
      addi sp, sp, -80
      sd ra, 72(sp)
      sd s0, 64(sp)
      addi s0, sp, 64 # Prologue ends
      mv t0, a0
      mv t1, a1
      sd a0, -8(s0)
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -16(s0) # temp0
      ld t0, -16(s0)
      mv t1, a2
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -24(s0) # temp1
      ld t0, -24(s0)
      mv t1, a3
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -32(s0) # temp2
      ld t0, -32(s0)
      mv t1, a4
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -40(s0) # temp3
      ld t0, -40(s0)
      mv t1, a5
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -48(s0) # temp4
      ld t0, -48(s0)
      mv t1, a6
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -56(s0) # temp5
      ld t0, -56(s0)
      mv t1, a7
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -64(s0) # temp6
      ld t0, -64(s0)
      ld t1, 16(s0)
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -72(s0) # temp7
      ld t0, -72(s0)
      ld t1, 24(s0)
      add a0, t0, t1
      addi a0, a0, -1
      addi sp, s0, 16 # Epilogue starts
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret

      .globl main
      .type main, @function
    main:
      addi sp, sp, -24
      sd ra, 16(sp)
      sd s0, 8(sp)
      addi s0, sp, 8 # Prologue ends
      li a0, 3
      li a1, 5
      li a2, 7
      li a3, 9
      li a4, 11
      li a5, 13
      li a6, 15
      li a7, 17
      addi sp, sp, -16 # Stack space for variadic args
      li t0, 19
      sd t0, 0(sp)
      li t0, 21
      sd t0, 8(sp)
      call plus
      addi sp, sp, 16 # Restore stack after call
      sd a0, -8(s0) # res
      ld a0, -8(s0)
      call print_int
      addi sp, s0, 16 # Epilogue starts
      ld ra, 8(s0)
      ld s0, 0(s0)
      li a0, 0
      ret
  |}]
;;
