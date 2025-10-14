Copyright 2025-2026, Friend-zva, RodionovMaxim05
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../bin/akaML.exe -o factorial.s <<EOF
  > let rec fac n = if n = 0 then 1 else n * fac (n - 1)
  > 
  > let main = fac 4

  $ cat factorial.s
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
  
    .globl _start
    .type _start, @function
  _start:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8 # Prologue ends
    li a0, 4
    call fac
    sd a0, -8(s0) # temp7
    ld a0, -8(s0)
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    li a7, 93
    ecall
  
  $ riscv64-linux-gnu-as -march=rv64gc factorial.s -o temp.o
  $ riscv64-linux-gnu-ld temp.o -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  [24]
