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
  > let main = fac 4
  > EOF
  $ ../../../bin/AML.exe fac.ml fac.s
  Generated: fac.s
  $ cat fac.s
    .text
    .globl fac
    .type fac, @function
  fac:
    addi sp, sp, -32
    sd ra, 24(sp)
    sd s0, 16(sp)
    addi s0, sp, 32
    addi t0, a0, 0
    li t1, 1
    slt t0, t1, t0
    xori t0, t0, 1
    beq t0, x0, .Lelse_0
    li a0, 1
    j .Lendif_1
  .Lelse_0:
    addi t0, a0, 0
    li t1, 1
    sub t0, t0, t1
    sd t0, -24(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld a0, -24(s0)
    jal ra, fac
    addi t0, a0, 0
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -32(s0)
    addi t0, a0, 0
    ld t1, -32(s0)
    mul a0, t0, t1
  .Lendif_1:
  fac_end:
    ld ra, 24(sp)
    ld s0, 16(sp)
    addi sp, sp, 32
    ret
    .globl _start
    .type _start, @function
  _start:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 16
    li a0, 4
    jal ra, fac
  main_end:
    ld ra, 8(sp)
    ld s0, 0(sp)
    addi sp, sp, 16
    li a7, 93
    ecall
  $ riscv64-linux-gnu-as -march=rv64gc fac.s -o fac.o
  $ riscv64-linux-gnu-ld fac.o -o fac.elf
  $ qemu-riscv64 fac.elf
  [24]

