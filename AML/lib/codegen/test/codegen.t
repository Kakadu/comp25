(** Copyright 2025-2026, Rodion Suvorov, Dmitriy Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
  $ cat >fac.ml <<EOF
  > let rec fac n =
  >   if n <= 1
  >   then 1
  >   else (let n1 = n-1 in
  >      let m = fac n1 in
  >      n*m)
  > 
  > let main = print_int (fac 4)
  > EOF
  $ ../../../bin/AML.exe fac.ml fac.s
  Generated: fac.s
  $ cat fac.s
    .text
    .globl fac
    .type fac, @function
  fac:
    addi sp, sp, -56
    sd ra, 48(sp)
    sd s0, 40(sp)
    addi s0, sp, 56
    addi t0, a0, 0
    li t1, 1
    slt t0, t1, t0
    xori t0, t0, 1
    sd t0, -24(s0)
    ld t0, -24(s0)
    beq t0, x0, .Lelse_0
    li a0, 1
    j .Lendif_1
  .Lelse_0:
    addi t0, a0, 0
    li t1, 1
    sub t0, t0, t1
    sd t0, -32(s0)
    ld t0, -32(s0)
    sd t0, -40(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld a0, -40(s0)
    call fac
    addi t0, a0, 0
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -48(s0)
    ld t0, -48(s0)
    sd t0, -56(s0)
    addi t0, a0, 0
    ld t1, -56(s0)
    mul a0, t0, t1
  .Lendif_1:
  fac_end:
    ld ra, 48(sp)
    ld s0, 40(sp)
    addi sp, sp, 56
    ret
    .globl main
    .type main, @function
  main:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 24
    li a0, 4
    call fac
    addi t0, a0, 0
    sd t0, -24(s0)
    ld a0, -24(s0)
    call print_int
  main_end:
    ld ra, 16(sp)
    ld s0, 8(sp)
    addi sp, sp, 24
    li a0, 0
    li a7, 93
    ecall
  $ riscv64-linux-gnu-as -march=rv64gc fac.s -o fac.o
  $ riscv64-linux-gnu-gcc -static fac.o -L../../../runtime -l:libruntime.a -o fac.elf -Wl,--no-warnings
  $ qemu-riscv64 ./fac.elf
  24

  $ cat >fib.ml <<EOF
  > let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)
  > let main = let () = print_int (fib 4) in 0
  > EOF
  $ ../../../bin/AML.exe fib.ml fib.s
  Generated: fib.s
  $ cat fib.s
    .text
    .globl fib
    .type fib, @function
  fib:
    addi sp, sp, -56
    sd ra, 48(sp)
    sd s0, 40(sp)
    addi s0, sp, 56
    addi t0, a0, 0
    li t1, 2
    slt t0, t0, t1
    sd t0, -24(s0)
    ld t0, -24(s0)
    beq t0, x0, .Lelse_0
    addi a0, a0, 0
    j .Lendif_1
  .Lelse_0:
    addi t0, a0, 0
    li t1, 1
    sub t0, t0, t1
    sd t0, -32(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld a0, -32(s0)
    call fib
    addi t0, a0, 0
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -40(s0)
    addi t0, a0, 0
    li t1, 2
    sub t0, t0, t1
    sd t0, -48(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld a0, -48(s0)
    call fib
    addi t0, a0, 0
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -56(s0)
    ld t0, -40(s0)
    ld t1, -56(s0)
    add a0, t0, t1
  .Lendif_1:
  fib_end:
    ld ra, 48(sp)
    ld s0, 40(sp)
    addi sp, sp, 56
    ret
    .globl main
    .type main, @function
  main:
    addi sp, sp, -40
    sd ra, 32(sp)
    sd s0, 24(sp)
    addi s0, sp, 40
    li a0, 4
    call fib
    addi t0, a0, 0
    sd t0, -24(s0)
    ld a0, -24(s0)
    call print_int
    addi t0, a0, 0
    sd t0, -32(s0)
    ld t0, -32(s0)
    sd t0, -40(s0)
    li a0, 0
  main_end:
    ld ra, 32(sp)
    ld s0, 24(sp)
    addi sp, sp, 40
    li a0, 0
    li a7, 93
    ecall
  $ riscv64-linux-gnu-as -march=rv64gc fib.s -o fib.o
  $ riscv64-linux-gnu-gcc -static fib.o -L../../../runtime -l:libruntime.a -o fib.elf -Wl,--no-warnings
  $ qemu-riscv64 ./fib.elf
  3

  $ cat >ite.ml <<EOF
  > let large x = if 0<>x then print_int 0 else print_int 1
  >   let main =
  >      let x = if (if (if 0=1
  >                      then 0 else (let t42 = print_int 42 in 1))=1
  >                  then 0 else 1)=1
  >              then 0 else 1 in
  >      large x
  > EOF
  $ ../../../bin/AML.exe ite.ml ite.s
  Generated: ite.s
  $ cat ite.s
    .text
    .globl large
    .type large, @function
  large:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 24
    li t0, 0
    addi t1, a0, 0
    sub t2, t0, t1
    slt t0, x0, t2
    slt t3, t2, x0
    add t0, t0, t3
    sd t0, -24(s0)
    ld t0, -24(s0)
    beq t0, x0, .Lelse_0
    addi sp, sp, -8
    sd a0, 0(sp)
    li a0, 0
    call print_int
    ld a0, 0(sp)
    addi sp, sp, 8
    j .Lendif_1
  .Lelse_0:
    addi sp, sp, -8
    sd a0, 0(sp)
    li a0, 1
    call print_int
    ld a0, 0(sp)
    addi sp, sp, 8
  .Lendif_1:
  large_end:
    ld ra, 16(sp)
    ld s0, 8(sp)
    addi sp, sp, 24
    ret
    .globl main
    .type main, @function
  main:
    addi sp, sp, -64
    sd ra, 56(sp)
    sd s0, 48(sp)
    addi s0, sp, 64
    li t0, 0
    li t1, 1
    sub t2, t0, t1
    slt t0, x0, t2
    slt t3, t2, x0
    add t0, t0, t3
    xori t0, t0, 1
    sd t0, -24(s0)
    ld t0, -24(s0)
    beq t0, x0, .Lelse_2
    li t0, 0
    li t1, 1
    sub t2, t0, t1
    slt t0, x0, t2
    slt t3, t2, x0
    add t0, t0, t3
    xori t0, t0, 1
    sd t0, -32(s0)
    ld t0, -32(s0)
    beq t0, x0, .Lelse_4
    li t0, 0
    li t1, 1
    sub t2, t0, t1
    slt t0, x0, t2
    slt t3, t2, x0
    add t0, t0, t3
    xori t0, t0, 1
    sd t0, -40(s0)
    ld t0, -40(s0)
    beq t0, x0, .Lelse_6
    li t0, 0
    sd t0, -48(s0)
    ld a0, -48(s0)
    call large
    j .Lendif_7
  .Lelse_6:
    li t0, 1
    sd t0, -56(s0)
    ld a0, -56(s0)
    call large
  .Lendif_7:
    j .Lendif_5
  .Lelse_4:
    li t0, 1
    li t1, 1
    sub t2, t0, t1
    slt t0, x0, t2
    slt t3, t2, x0
    add t0, t0, t3
    xori t0, t0, 1
    sd t0, -64(s0)
    ld t0, -64(s0)
    beq t0, x0, .Lelse_8
    li t0, 0
    sd t0, -72(s0)
    ld a0, -72(s0)
    call large
    j .Lendif_9
  .Lelse_8:
    li t0, 1
    sd t0, -80(s0)
    ld a0, -80(s0)
    call large
  .Lendif_9:
  .Lendif_5:
    j .Lendif_3
  .Lelse_2:
    li a0, 42
    call print_int
    addi t0, a0, 0
    sd t0, -88(s0)
    ld t0, -88(s0)
    sd t0, -96(s0)
    li t0, 1
    li t1, 1
    sub t2, t0, t1
    slt t0, x0, t2
    slt t3, t2, x0
    add t0, t0, t3
    xori t0, t0, 1
    sd t0, -104(s0)
    ld t0, -104(s0)
    beq t0, x0, .Lelse_10
    li t0, 0
    li t1, 1
    sub t2, t0, t1
    slt t0, x0, t2
    slt t3, t2, x0
    add t0, t0, t3
    xori t0, t0, 1
    sd t0, -112(s0)
    ld t0, -112(s0)
    beq t0, x0, .Lelse_12
    li t0, 0
    sd t0, -120(s0)
    ld a0, -120(s0)
    call large
    j .Lendif_13
  .Lelse_12:
    li t0, 1
    sd t0, -128(s0)
    ld a0, -128(s0)
    call large
  .Lendif_13:
    j .Lendif_11
  .Lelse_10:
    li t0, 1
    li t1, 1
    sub t2, t0, t1
    slt t0, x0, t2
    slt t3, t2, x0
    add t0, t0, t3
    xori t0, t0, 1
    sd t0, -136(s0)
    ld t0, -136(s0)
    beq t0, x0, .Lelse_14
    li t0, 0
    sd t0, -144(s0)
    ld a0, -144(s0)
    call large
    j .Lendif_15
  .Lelse_14:
    li t0, 1
    sd t0, -152(s0)
    ld a0, -152(s0)
    call large
  .Lendif_15:
  .Lendif_11:
  .Lendif_3:
  main_end:
    ld ra, 56(sp)
    ld s0, 48(sp)
    addi sp, sp, 64
    li a0, 0
    li a7, 93
    ecall
  $ riscv64-linux-gnu-as -march=rv64gc ite.s -o ite.o
  $ riscv64-linux-gnu-gcc -static ite.o -L../../../runtime -l:libruntime.a -o ite.elf -Wl,--no-warnings
  $ qemu-riscv64 ./ite.elf
  420
