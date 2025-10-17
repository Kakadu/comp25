Copyright 2025-2026, Friend-zva, RodionovMaxim05
SPDX-License-Identifier: LGPL-3.0-or-later

====================== Factorial ======================
  $ ../bin/akaML.exe -o factorial.s <<EOF
  > let rec fac n = if n = 0 then 1 else n * fac (n - 1)
  > 
  > let main = print_int (fac 4)

  $ cat factorial.s
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
  
    .globl main
    .type main, @function
  main:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8 # Prologue ends
    li a0, 4
    call fac
    sd a0, -8(s0) # temp6
    ld a0, -8(s0)
    call print_int
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    li a0, 0
    ret
  

  $ riscv64-linux-gnu-as -march=rv64gc factorial.s -o temp.o
  $ riscv64-linux-gnu-gcc ../lib/runtime/runtime.c -c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  24

====================== Fibonacci ======================
  $ ../bin/akaML.exe -o fibonacci.s <<EOF
  > let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)
  > 
  > let main = print_int (fib 6)

  $ cat fibonacci.s
  .section .text
    .globl fib
    .type fib, @function
  fib:
    addi sp, sp, -56
    sd ra, 48(sp)
    sd s0, 40(sp)
    addi s0, sp, 40 # Prologue ends
    mv t0, a0
    li t1, 1
    mv a1, a0
    slt a0, t1, t0
    xori a0, a0, 1
    sd a0, -8(s0) # temp0
    ld t0, -8(s0)
    beq t0, zero, else_0
    mv a0, a1
    j end_0
  else_0:
    mv t0, a1
    li t1, 1
    sub a0, t0, t1
    sd a0, -16(s0) # temp1
    ld a0, -16(s0)
    addi sp, sp, -8 # Saving 'live' regs
    sd a1, -24(s0)
    call fib
    sd a0, -32(s0) # temp2
    ld t0, -24(s0)
    li t1, 2
    sub a0, t0, t1
    sd a0, -40(s0) # temp3
    ld a0, -40(s0)
    call fib
    sd a0, -48(s0) # temp4
    ld t0, -32(s0)
    ld t1, -48(s0)
    add  a0, t0, t1
  end_0:
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
    li a0, 6
    call fib
    sd a0, -8(s0) # temp8
    ld a0, -8(s0)
    call print_int
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    li a0, 0
    ret
  

  $ riscv64-linux-gnu-as -march=rv64gc fibonacci.s -o temp.o
  $ riscv64-linux-gnu-gcc ../lib/runtime/runtime.c -c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  8
