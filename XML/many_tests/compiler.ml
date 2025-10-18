(** Copyright 2024, Mikhail Gavrilenko, Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Parser
open Format

let to_asm ast =
  let anf_ast = Middleend.Anf.anf_program ast in
  let buf = Buffer.create 1024 in
  let ppf = formatter_of_buffer buf in
  Backend.Codegen.gen_program ppf anf_ast;
  pp_print_flush ppf ();
  Buffer.contents buf

(*--- аст для арифметичских операций*)
let%expect_test "add_codegen" =
  let ast = parse_str {|
let main =
  let () = print_int (1 + 2) in
  0
;; |} in
  let asm = to_asm ast in
  print_endline asm;
  [%expect {|
    .section .text
    .global main
    .type main, @function
    main:
      addi sp, sp, -24
      sd ra, 16(sp)
      sd s0, 8(sp)
      addi s0, sp, 8
      li a0, 3
      call print_int
      mv t0, a0
      sd t0, -8(s0)
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    |}]
;;

let%expect_test "sub_codegen" =
  let ast = parse_str {|
let main =
  let () = print_int (5 - 3) in
  0
;; |} in
  let asm = to_asm ast in
  print_endline asm;
  [%expect {|
    .section .text
    .global main
    .type main, @function
    main:
      addi sp, sp, -24
      sd ra, 16(sp)
      sd s0, 8(sp)
      addi s0, sp, 8
      li a0, 2
      call print_int
      mv t0, a0
      sd t0, -8(s0)
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    |}]
;;

let%expect_test "mul_codegen" =
  let ast = parse_str {|
let main =
  let () = print_int (6 * 7) in
  0
;; |} in
  let asm = to_asm ast in
  print_endline asm;
  [%expect {|
    .section .text
    .global main
    .type main, @function
    main:
      addi sp, sp, -24
      sd ra, 16(sp)
      sd s0, 8(sp)
      addi s0, sp, 8
      li a0, 42
      call print_int
      mv t0, a0
      sd t0, -8(s0)
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    |}]
;;

(* AST для ветвления и проверки условий*)
let%expect_test "if_lt_true_codegen" =
  let ast = parse_str {|
let main =
  let () = print_int (if 2 < 3 then 11 else 22) in
  0
;; |} in
  let asm = to_asm ast in
  print_endline asm;
  [%expect {|
    .section .text
    .global main
    .type main, @function
    main:
      addi sp, sp, -40
      sd ra, 32(sp)
      sd s0, 24(sp)
      addi s0, sp, 24
      li t0, 2
      li t1, 3
      slt t0, t0, t1
      sd t0, -8(s0)
      ld t0, -8(s0)
      beq t0, zero, else_0
      li t0, 11
      j endif_1
    else_0:
      li t0, 22
    endif_1:
      sd t0, -16(s0)
      ld a0, -16(s0)
      call print_int
      mv t0, a0
      sd t0, -24(s0)
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    |}]
;;

let%expect_test "if_lt_false_codegen" =
  let ast = parse_str {|
let main =
  let () = print_int (if 5 < 4 then 1 else 0) in
  0
;; |} in
  let asm = to_asm ast in
  print_endline asm;
  [%expect {|
    .section .text
    .global main
    .type main, @function
    main:
      addi sp, sp, -40
      sd ra, 32(sp)
      sd s0, 24(sp)
      addi s0, sp, 24
      li t0, 5
      li t1, 4
      slt t0, t0, t1
      sd t0, -8(s0)
      ld t0, -8(s0)
      beq t0, zero, else_0
      li t0, 1
      j endif_1
    else_0:
      li t0, 0
    endif_1:
      sd t0, -16(s0)
      ld a0, -16(s0)
      call print_int
      mv t0, a0
      sd t0, -24(s0)
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    |}]
;;

let%expect_test "if_gt_true_codegen" =
  let ast = parse_str {|
let main =
  let () = print_int (if 4 > 3 then 7 else 9) in
  0
;; |} in
  let asm = to_asm ast in
  print_endline asm;
  [%expect {|
    .section .text
    .global main
    .type main, @function
    main:
      addi sp, sp, -40
      sd ra, 32(sp)
      sd s0, 24(sp)
      addi s0, sp, 24
      li t0, 4
      li t1, 3
      slt t0, t1, t0
      sd t0, -8(s0)
      ld t0, -8(s0)
      beq t0, zero, else_0
      li t0, 7
      j endif_1
    else_0:
      li t0, 9
    endif_1:
      sd t0, -16(s0)
      ld a0, -16(s0)
      call print_int
      mv t0, a0
      sd t0, -24(s0)
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    |}]
;;

let%expect_test "if_le_true_codegen" =
  let ast = parse_str {|
let main =
  let () = print_int (if 3 <= 3 then 10 else 20) in
  0
;; |} in
  let asm = to_asm ast in
  print_endline asm;
  [%expect {|
    .section .text
    .global main
    .type main, @function
    main:
      addi sp, sp, -40
      sd ra, 32(sp)
      sd s0, 24(sp)
      addi s0, sp, 24
      li t0, 3
      li t1, 3
      slt t0, t1, t0
      xori t0, t0, 1
      sd t0, -8(s0)
      ld t0, -8(s0)
      beq t0, zero, else_0
      li t0, 10
      j endif_1
    else_0:
      li t0, 20
    endif_1:
      sd t0, -16(s0)
      ld a0, -16(s0)
      call print_int
      mv t0, a0
      sd t0, -24(s0)
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    |}]
;;

let%expect_test "if_ge_true_codegen" =
  let ast = parse_str {|
let main =
  let () = print_int (if 5 >= 2 then 8 else 9) in
  0
;; |} in
  let asm = to_asm ast in
  print_endline asm;
  [%expect {|
    .section .text
    .global main
    .type main, @function
    main:
      addi sp, sp, -40
      sd ra, 32(sp)
      sd s0, 24(sp)
      addi s0, sp, 24
      li t0, 5
      li t1, 2
      slt t0, t0, t1
      xori t0, t0, 1
      sd t0, -8(s0)
      ld t0, -8(s0)
      beq t0, zero, else_0
      li t0, 8
      j endif_1
    else_0:
      li t0, 9
    endif_1:
      sd t0, -16(s0)
      ld a0, -16(s0)
      call print_int
      mv t0, a0
      sd t0, -24(s0)
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    |}]
;;

let%expect_test "if_eq_false_codegen" =
  let ast = parse_str {|
let main =
  let () = print_int (if 5 = 6 then 1 else 2) in
  0
;; |} in
  let asm = to_asm ast in
  print_endline asm;
  [%expect {|
    .section .text
    .global main
    .type main, @function
    main:
      addi sp, sp, -40
      sd ra, 32(sp)
      sd s0, 24(sp)
      addi s0, sp, 24
      li t0, 5
      li t1, 6
      xor t0, t0, t1
      seqz t0, t0
      sd t0, -8(s0)
      ld t0, -8(s0)
      beq t0, zero, else_0
      li t0, 1
      j endif_1
    else_0:
      li t0, 2
    endif_1:
      sd t0, -16(s0)
      ld a0, -16(s0)
      call print_int
      mv t0, a0
      sd t0, -24(s0)
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    |}]
;;

(*--- Ast для вызова функции*)

let%expect_test "simple_call_codegen" =
  let ast = parse_str {|
let inc x = x + 1

let main =
  let () = print_int (inc 41) in
  0
;; |} in
  let asm = to_asm ast in
  print_endline asm;
  [%expect {|
    .section .text
    .global main
    .type main, @function
    inc:
      addi sp, sp, -24
      sd ra, 16(sp)
      sd s0, 8(sp)
      addi s0, sp, 8
      mv t0, a0
      li t1, 1
      add t0, t0, t1
      sd t0, -8(s0)
      ld a0, -8(s0)
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li a0, 41
      call inc
      mv t0, a0
      sd t0, -8(s0)
      ld a0, -8(s0)
      call print_int
      mv t0, a0
      sd t0, -16(s0)
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    |}]
;;

let%expect_test "sum_recursion_codegen" =
  let ast = parse_str {|
let rec sum n = if n <= 1 then n else n + sum (n - 1)

let main =
  let () = print_int (sum 5) in
  0
;; |} in
  let asm = to_asm ast in
  print_endline asm;
  [%expect {|
    .section .text
    .global main
    .type main, @function
    sum:
      addi sp, sp, -48
      sd ra, 40(sp)
      sd s0, 32(sp)
      addi s0, sp, 32
      mv t0, a0
      li t1, 1
      slt t0, t1, t0
      xori t0, t0, 1
      sd t0, -8(s0)
      ld t0, -8(s0)
      beq t0, zero, else_0
      mv t0, a0
      j endif_1
    else_0:
      mv t0, a0
      li t1, 1
      sub t0, t0, t1
      sd t0, -16(s0)
      addi sp, sp, -8
      sd a0, 0(sp)
      ld a0, -16(s0)
      call sum
      mv t0, a0
      ld a0, 0(sp)
      addi sp, sp, 8
      sd t0, -24(s0)
      mv t0, a0
      ld t1, -24(s0)
      add t0, t0, t1
      sd t0, -32(s0)
      ld t0, -32(s0)
    endif_1:
      sd t0, -16(s0)
      ld a0, -16(s0)
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li a0, 5
      call sum
      mv t0, a0
      sd t0, -8(s0)
      ld a0, -8(s0)
      call print_int
      mv t0, a0
      sd t0, -16(s0)
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    |}]
;;

(* --- AST для factorial + main --- *)

let%expect_test "factorial_codegen" =
  let ast_factorial = parse_str "let rec fac n = if n <= 1 then 1 else n * fac (n - 1)

let main =
  let () = print_int (fac 2) in
  0
;;" in
  let asm = to_asm ast_factorial in
  print_endline asm;
  [%expect {|
    .section .text
    .global main
    .type main, @function
    fac:
      addi sp, sp, -48
      sd ra, 40(sp)
      sd s0, 32(sp)
      addi s0, sp, 32
      mv t0, a0
      li t1, 1
      slt t0, t1, t0
      xori t0, t0, 1
      sd t0, -8(s0)
      ld t0, -8(s0)
      beq t0, zero, else_0
      li t0, 1
      j endif_1
    else_0:
      mv t0, a0
      li t1, 1
      sub t0, t0, t1
      sd t0, -16(s0)
      addi sp, sp, -8
      sd a0, 0(sp)
      ld a0, -16(s0)
      call fac
      mv t0, a0
      ld a0, 0(sp)
      addi sp, sp, 8
      sd t0, -24(s0)
      mv t0, a0
      ld t1, -24(s0)
      mul t0, t0, t1
      sd t0, -32(s0)
      ld t0, -32(s0)
    endif_1:
      sd t0, -16(s0)
      ld a0, -16(s0)
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li a0, 2
      call fac
      mv t0, a0
      sd t0, -8(s0)
      ld a0, -8(s0)
      call print_int
      mv t0, a0
      sd t0, -16(s0)
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    |}]

let%expect_test "factorial_basic_codegen" =
  let ast_factorial = parse_str "let rec fac n = if n <= 1 then 1 else n * fac (n - 1)
;;" in
  let asm = to_asm ast_factorial in
  print_endline asm;
  [%expect {|
    fac:
      addi sp, sp, -48
      sd ra, 40(sp)
      sd s0, 32(sp)
      addi s0, sp, 32
      mv t0, a0
      li t1, 1
      slt t0, t1, t0
      xori t0, t0, 1
      sd t0, -8(s0)
      ld t0, -8(s0)
      beq t0, zero, else_0
      li t0, 1
      j endif_1
    else_0:
      mv t0, a0
      li t1, 1
      sub t0, t0, t1
      sd t0, -16(s0)
      addi sp, sp, -8
      sd a0, 0(sp)
      ld a0, -16(s0)
      call fac
      mv t0, a0
      ld a0, 0(sp)
      addi sp, sp, 8
      sd t0, -24(s0)
      mv t0, a0
      ld t1, -24(s0)
      mul t0, t0, t1
      sd t0, -32(s0)
      ld t0, -32(s0)
    endif_1:
      sd t0, -16(s0)
      ld a0, -16(s0)
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    |}]


let%expect_test "ifs" =
  let ast_if = parse_str "
  let large x = if 0<>x then print_int 0 else print_int 1
  let main =
  let x = if (if (if 0
  then 0 else (let t42 = print_int 42 in 1))
  then 0 else 1)
  then 0 else 1 in
  large x
  ;;" in
  let asm = to_asm ast_if in
  print_endline asm;
  [%expect {|
    .section .text
    .global main
    .type main, @function
    large:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 0
      mv t1, a0
      xor t2, t0, t1
      snez t0, t2
      sd t0, -8(s0)
      ld t0, -8(s0)
      beq t0, zero, else_0
      addi sp, sp, -8
      sd a0, 0(sp)
      li a0, 0
      call print_int
      mv t0, a0
      ld a0, 0(sp)
      addi sp, sp, 8
      sd t0, -16(s0)
      ld t0, -16(s0)
      j endif_1
    else_0:
      addi sp, sp, -8
      sd a0, 0(sp)
      li a0, 1
      call print_int
      mv t0, a0
      ld a0, 0(sp)
      addi sp, sp, 8
      sd t0, -16(s0)
      ld t0, -16(s0)
    endif_1:
      sd t0, -16(s0)
      ld a0, -16(s0)
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    main:
      addi sp, sp, -48
      sd ra, 40(sp)
      sd s0, 32(sp)
      addi s0, sp, 32
      li t0, 0
      beq t0, zero, else_2
      li t0, 0
      j endif_3
    else_2:
      li a0, 42
      call print_int
      mv t0, a0
      sd t0, -8(s0)
      li t0, 1
    endif_3:
      sd t0, -8(s0)
      ld t0, -8(s0)
      beq t0, zero, else_4
      li t0, 0
      j endif_5
    else_4:
      li t0, 1
    endif_5:
      sd t0, -16(s0)
      ld t0, -16(s0)
      beq t0, zero, else_6
      li t0, 0
      j endif_7
    else_6:
      li t0, 1
    endif_7:
      sd t0, -24(s0)
      ld a0, -24(s0)
      call large
      mv t0, a0
      sd t0, -32(s0)
      ld a0, -32(s0)
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    |}]

let%expect_test "ifs" =
  let ast_factorial = parse_str "
  let large x = if 0<>x then print_int 0 else print_int 1
  let main =
  let x = if (if (if 0
  then 0 else (let t42 = print_int 42 in 1))
  then 0 else 1)
  then 0 else 1 in
  large x
  ;;" in
  let asm = to_asm ast_factorial in
  print_endline asm;
  [%expect {|
    .section .text
    .global main
    .type main, @function
    large:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li t0, 0
      mv t1, a0
      xor t2, t0, t1
      snez t0, t2
      sd t0, -8(s0)
      ld t0, -8(s0)
      beq t0, zero, else_0
      addi sp, sp, -8
      sd a0, 0(sp)
      li a0, 0
      call print_int
      mv t0, a0
      ld a0, 0(sp)
      addi sp, sp, 8
      sd t0, -16(s0)
      ld t0, -16(s0)
      j endif_1
    else_0:
      addi sp, sp, -8
      sd a0, 0(sp)
      li a0, 1
      call print_int
      mv t0, a0
      ld a0, 0(sp)
      addi sp, sp, 8
      sd t0, -16(s0)
      ld t0, -16(s0)
    endif_1:
      sd t0, -16(s0)
      ld a0, -16(s0)
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    main:
      addi sp, sp, -48
      sd ra, 40(sp)
      sd s0, 32(sp)
      addi s0, sp, 32
      li t0, 0
      beq t0, zero, else_2
      li t0, 0
      j endif_3
    else_2:
      li a0, 42
      call print_int
      mv t0, a0
      sd t0, -8(s0)
      li t0, 1
    endif_3:
      sd t0, -8(s0)
      ld t0, -8(s0)
      beq t0, zero, else_4
      li t0, 0
      j endif_5
    else_4:
      li t0, 1
    endif_5:
      sd t0, -16(s0)
      ld t0, -16(s0)
      beq t0, zero, else_6
      li t0, 0
      j endif_7
    else_6:
      li t0, 1
    endif_7:
      sd t0, -24(s0)
      ld a0, -24(s0)
      call large
      mv t0, a0
      sd t0, -32(s0)
      ld a0, -32(s0)
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    |}]

let%expect_test "fib" =
  let ast_factorial = parse_str "let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)

let main =
  let () = print_int (fib 2) in
  0
  ;;" in
  let asm = to_asm ast_factorial in
  print_endline asm;
  [%expect {|
    .section .text
    .global main
    .type main, @function
    fib:
      addi sp, sp, -64
      sd ra, 56(sp)
      sd s0, 48(sp)
      addi s0, sp, 48
      mv t0, a0
      li t1, 1
      slt t0, t1, t0
      xori t0, t0, 1
      sd t0, -8(s0)
      ld t0, -8(s0)
      beq t0, zero, else_0
      mv t0, a0
      j endif_1
    else_0:
      mv t0, a0
      li t1, 1
      sub t0, t0, t1
      sd t0, -16(s0)
      addi sp, sp, -8
      sd a0, 0(sp)
      ld a0, -16(s0)
      call fib
      mv t0, a0
      ld a0, 0(sp)
      addi sp, sp, 8
      sd t0, -24(s0)
      mv t0, a0
      li t1, 2
      sub t0, t0, t1
      sd t0, -32(s0)
      addi sp, sp, -8
      sd a0, 0(sp)
      ld a0, -32(s0)
      call fib
      mv t0, a0
      ld a0, 0(sp)
      addi sp, sp, 8
      sd t0, -40(s0)
      ld t0, -24(s0)
      ld t1, -40(s0)
      add t0, t0, t1
      sd t0, -48(s0)
      ld t0, -48(s0)
    endif_1:
      sd t0, -16(s0)
      ld a0, -16(s0)
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    main:
      addi sp, sp, -32
      sd ra, 24(sp)
      sd s0, 16(sp)
      addi s0, sp, 16
      li a0, 2
      call fib
      mv t0, a0
      sd t0, -8(s0)
      ld a0, -8(s0)
      call print_int
      mv t0, a0
      sd t0, -16(s0)
      li a0, 0
      addi sp, s0, 16
      ld ra, 8(s0)
      ld s0, 0(s0)
      ret
    |}]
