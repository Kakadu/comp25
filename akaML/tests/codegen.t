Copyright 2025-2026, Friend-zva, RodionovMaxim05
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../bin/akaML.exe -o factorial.s <<EOF
  > let rec fac n =
  >   if n <= 1
  >   then 1
  >   else (let n1 = n-1 in
  >      let m = fac n1 in
  >      n*m)
  > 
  > let main = fac 4

  $ cat factorial.s
  .section .text
    .globl fac
    .type fac, @function
  fac:
    addi sp, sp, -32
    sd ra, 24(sp)
    sd s0, 16(sp)
    addi s0, sp, 16 # Prologue ends
    mv t0, a0
    li t1, 1
    slt t0, t1, t0
    xori t0, t0, 1
    beq t0, zero, else_0
    li a0, 1
    j end_0
  else_0:
    mv t0, a0
    li t1, 1
    mv a1, a0
    sub a0, t0, t1
    sd a0, -8(s0) # n1
    ld a0, -8(s0)
    addi sp, sp, -8 # Saving 'live' regs
    sd a1, -16(s0)
    call fac
    sd a0, -24(s0) # m
    ld t0, -16(s0)
    ld t1, -24(s0)
    mul a0, t0, t1
  end_0:
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  
    .globl _start
    .type _start, @function
  _start:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 0 # Prologue ends
    li a0, 4
    call fac
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    li a7, 93
    ecall
  
  $ riscv64-linux-gnu-as -march=rv64gc factorial.s -o temp.o
  $ riscv64-linux-gnu-ld temp.o -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  [24]
