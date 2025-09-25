Copyright 2025-2026, Friend-zva, RodionovMaxim05
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../bin/akaML.exe <<EOF
  > let rec fac n =
  >   if n <= 1
  >   then 1
  >   else (let n1 = n-1 in
  >      let m = fac n1 in
  >      n*m)
  > 
  > let main = fac 4
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
    beq t0, zero, else_1
    li a0, 1
    j end_2
  else_1:
    mv t0, a0
    li t1, 1
    mv a1, a0
    sub a0, t0, t1
    sd a0, -8(s0) # n1
    ld a0, -8(s0)
    addi sp, sp, -8 # spill live regs
    sd a1, 0(sp)
    call fac
    sd a0, -16(s0) # m
    ld t0, 0(sp)
    ld t1, -16(s0)
    mul a0, t0, t1
  end_2:
    addi sp, s0, 16
    ld ra, 8(s0) # Epilogue starts
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
    addi sp, s0, 16
    ld ra, 8(s0) # Epilogue starts
    ld s0, 0(s0)
    li a7, 93
    ecall
  
