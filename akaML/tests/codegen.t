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
  
    .globl main
    .type main, @function
  main:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8 # Prologue ends
    li a0, 9
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
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_runtime.a -o file.exe
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
    li t1, 3
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
    li t1, 3
    sub a0, t0, t1
    addi a0, a0, 1
    sd a0, -16(s0) # temp1
    addi sp, sp, -8 # Saving 'live' regs
    sd a1, -24(s0)
    ld a0, -16(s0)
    call fib
    sd a0, -32(s0) # temp2
    ld t0, -24(s0)
    li t1, 5
    sub a0, t0, t1
    addi a0, a0, 1
    sd a0, -40(s0) # temp3
    ld a0, -40(s0)
    call fib
    sd a0, -48(s0) # temp4
    ld t0, -32(s0)
    ld t1, -48(s0)
    add a0, t0, t1
    addi a0, a0, -1
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
    li a0, 13
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
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_runtime.a -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  8

====================== Ififif ======================
  $ ../bin/akaML.exe -o ififif.s <<EOF
  > let large x = if 0<>x then print_int 0 else print_int 1
  > let main =
  >   let x = if (if (if 0 = 1
  >                   then 0 = 1 else (let t42 = print_int 42 in 1 = 1))
  >               then 0 else 1) = 1
  >           then 0 else 1 in
  >   large x

  $ cat ififif.s
  .section .text
    .globl large
    .type large, @function
  large:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8 # Prologue ends
    li t0, 1
    mv t1, a0
    mv a1, a0
    xor a0, t0, t1
    snez a0, a0
    sd a0, -8(s0) # temp0
    ld t0, -8(s0)
    beq t0, zero, else_0
    addi sp, sp, -8 # Saving 'live' regs
    sd a1, -16(s0)
    li a0, 1
    call print_int
    j end_0
  else_0:
    addi sp, sp, -8 # Saving 'live' regs
    sd a1, -16(s0)
    li a0, 3
    call print_int
  end_0:
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  
    .globl main
    .type main, @function
  main:
    addi sp, sp, -64
    sd ra, 56(sp)
    sd s0, 48(sp)
    addi s0, sp, 48 # Prologue ends
    li t0, 1
    li t1, 3
    xor a0, t0, t1
    seqz a0, a0
    sd a0, -8(s0) # temp5
    ld t0, -8(s0)
    beq t0, zero, else_1
    li t0, 1
    li t1, 3
    xor a0, t0, t1
    seqz a0, a0
    j end_1
  else_1:
    li a0, 85
    call print_int
    sd a0, -16(s0) # t42
    li t0, 3
    li t1, 3
    xor a0, t0, t1
    seqz a0, a0
  end_1:
    sd a0, -24(s0) # temp9
    ld t0, -24(s0)
    beq t0, zero, else_2
    li a0, 1
    j end_2
  else_2:
    li a0, 3
  end_2:
    sd a0, -32(s0) # temp10
    ld t0, -32(s0)
    li t1, 3
    xor a0, t0, t1
    seqz a0, a0
    sd a0, -40(s0) # temp11
    ld t0, -40(s0)
    beq t0, zero, else_3
    li a0, 1
    j end_3
  else_3:
    li a0, 3
  end_3:
    sd a0, -48(s0) # x
    ld a0, -48(s0)
    call large
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    li a0, 0
    ret
  

  $ riscv64-linux-gnu-as -march=rv64gc ififif.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_runtime.a -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  420

====================== Simple closure ======================
  $ ../bin/akaML.exe -o closure.s <<EOF
  > let plus x y = x + y
  > 
  > let main =
  >   let temp = plus 1 in
  >   let temp2 = temp 3 in 
  >   print_int temp2
  > ;;

  $ cat closure.s
  .section .text
    .globl plus
    .type plus, @function
  plus:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd s0, 0(sp)
    addi s0, sp, 0 # Prologue ends
    mv t0, a0
    mv t1, a1
    mv a2, a0
    add a0, t0, t1
    addi a0, a0, -1
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  
    .globl main
    .type main, @function
  main:
    addi sp, sp, -32
    sd ra, 24(sp)
    sd s0, 16(sp)
    addi s0, sp, 16 # Prologue ends
    la a0, plus
    li a1, 2
    call alloc_closure
    li a1, 1
    li a2, 3
    call applyN
    sd a0, -8(s0) # temp
    ld a0, -8(s0)
    li a1, 1
    li a2, 7
    call applyN
    sd a0, -16(s0) # temp2
    ld a0, -16(s0)
    call print_int
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    li a0, 0
    ret
  

  $ riscv64-linux-gnu-as -march=rv64gc closure.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_runtime.a -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  4

====================== CPS Factorial ======================
  $ ../bin/akaML.exe -fromfile manytests/typed/010faccps_ll.ml -o 010faccps_ll.s

  $ cat 010faccps_ll.s
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
  
    .globl fresh_1
    .type fresh_1, @function
  fresh_1:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8 # Prologue ends
    mv t0, a2
    mv t1, a0
    mv a3, a0
    srli t0, t0, 1
    addi t1, t1, -1
    mul a0, t0, t1
    addi a0, a0, 1
    sd a0, -8(s0) # temp1
    addi sp, sp, -24 # Saving 'live' regs
    sd a1, -16(s0)
    sd a3, -24(s0)
    sd a2, -32(s0)
    ld a0, -16(s0)
    li a1, 1
    ld a2, -8(s0)
    call applyN
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  
    .globl fac_cps
    .type fac_cps, @function
  fac_cps:
    addi sp, sp, -40
    sd ra, 32(sp)
    sd s0, 24(sp)
    addi s0, sp, 24 # Prologue ends
    mv t0, a0
    li t1, 3
    mv a2, a0
    xor a0, t0, t1
    seqz a0, a0
    sd a0, -8(s0) # temp4
    ld t0, -8(s0)
    beq t0, zero, else_0
    addi sp, sp, -16 # Saving 'live' regs
    sd a1, -16(s0)
    sd a2, -24(s0)
    ld a0, -16(s0)
    li a1, 1
    li a2, 3
    call applyN
    j end_0
  else_0:
    mv t0, a2
    li t1, 3
    sub a0, t0, t1
    addi a0, a0, 1
    sd a0, -16(s0) # temp6
    addi sp, sp, -16 # Saving 'live' regs
    sd a1, -24(s0)
    sd a2, -32(s0)
    la a0, fresh_1
    li a1, 3
    call alloc_closure
    li a1, 2
    ld a2, -32(s0)
    ld a3, -24(s0)
    call applyN
    sd a0, -40(s0) # temp7
    ld a0, -16(s0)
    ld a1, -40(s0)
    call fac_cps
  end_0:
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  
    .globl main
    .type main, @function
  main:
    addi sp, sp, -32
    sd ra, 24(sp)
    sd s0, 16(sp)
    addi s0, sp, 16 # Prologue ends
    addi sp, sp, -8 # Saving 'dangerous' args
    la a0, id
    li a1, 1
    call alloc_closure
    sd a0, -8(s0)
    li a0, 9
    ld a1, -8(s0)
    call fac_cps
    sd a0, -16(s0) # temp11
    ld a0, -16(s0)
    call print_int
    sd a0, -24(s0) # temp12
    li a0, 1
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    li a0, 0
    ret
  

====================== CPS Fibonacci ======================
  $ ../bin/akaML.exe -fromfile manytests/typed/010fibcps_ll.ml -o 010fibcps_ll.s

  $ cat 010fibcps_ll.s
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
  
    .globl fresh_2
    .type fresh_2, @function
  fresh_2:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8 # Prologue ends
    mv t0, a0
    mv t1, a2
    mv a3, a0
    add a0, t0, t1
    addi a0, a0, -1
    sd a0, -8(s0) # temp1
    addi sp, sp, -24 # Saving 'live' regs
    sd a1, -16(s0)
    sd a3, -24(s0)
    sd a2, -32(s0)
    ld a0, -16(s0)
    li a1, 1
    ld a2, -8(s0)
    call applyN
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  
    .globl fresh_1
    .type fresh_1, @function
  fresh_1:
    addi sp, sp, -32
    sd ra, 24(sp)
    sd s0, 16(sp)
    addi s0, sp, 16 # Prologue ends
    mv t0, a0
    li t1, 5
    mv a4, a0
    sub a0, t0, t1
    addi a0, a0, 1
    sd a0, -8(s0) # temp4
    addi sp, sp, -32 # Saving 'live' regs
    sd a2, -16(s0)
    sd a1, -24(s0)
    sd a4, -32(s0)
    sd a3, -40(s0)
    la a0, fresh_2
    li a1, 3
    call alloc_closure
    li a1, 2
    ld a2, -40(s0)
    ld a3, -24(s0)
    call applyN
    sd a0, -48(s0) # temp5
    ld a0, -8(s0)
    ld a1, -48(s0)
    call fib
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  
    .globl fib
    .type fib, @function
  fib:
    addi sp, sp, -40
    sd ra, 32(sp)
    sd s0, 24(sp)
    addi s0, sp, 24 # Prologue ends
    mv t0, a0
    li t1, 5
    mv a2, a0
    slt a0, t0, t1
    sd a0, -8(s0) # temp8
    ld t0, -8(s0)
    beq t0, zero, else_0
    addi sp, sp, -16 # Saving 'live' regs
    sd a1, -16(s0)
    sd a2, -24(s0)
    ld a0, -16(s0)
    li a1, 1
    ld a2, -24(s0)
    call applyN
    j end_0
  else_0:
    mv t0, a2
    li t1, 3
    sub a0, t0, t1
    addi a0, a0, 1
    sd a0, -16(s0) # temp10
    addi sp, sp, -16 # Saving 'live' regs
    sd a1, -24(s0)
    sd a2, -32(s0)
    addi sp, sp, -8 # Saving 'dangerous' args
    la a0, fib
    li a1, 2
    call alloc_closure
    sd a0, -40(s0)
    la a0, fresh_1
    li a1, 4
    call alloc_closure
    li a1, 3
    ld a2, -32(s0)
    ld a3, -24(s0)
    ld a4, -40(s0)
    call applyN
    sd a0, -48(s0) # temp11
    ld a0, -16(s0)
    ld a1, -48(s0)
    call fib
  end_0:
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  
    .globl main
    .type main, @function
  main:
    addi sp, sp, -32
    sd ra, 24(sp)
    sd s0, 16(sp)
    addi s0, sp, 16 # Prologue ends
    addi sp, sp, -8 # Saving 'dangerous' args
    la a0, id
    li a1, 1
    call alloc_closure
    sd a0, -8(s0)
    li a0, 13
    ld a1, -8(s0)
    call fib
    sd a0, -16(s0) # temp15
    ld a0, -16(s0)
    call print_int
    sd a0, -24(s0) # z
    li a0, 1
    addi sp, s0, 16 # Epilogue starts
    ld ra, 8(s0)
    ld s0, 0(s0)
    li a0, 0
    ret
  
====================== Plus with 10 args ======================
  $ ../bin/akaML.exe -o plus_with_10_args.s <<EOF
  > let plus a b c d e f h i j k = a + b + c + d + e + f + h + i + j + k
  > 
  > let main =
  >   let res = plus 1 2 3 4 5 6 7 8 9 10 in
  >   print_int res
  > ;;

  $ riscv64-linux-gnu-as -march=rv64gc plus_with_10_args.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_runtime.a -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  55

====================== Closure plus with 10 args ======================
  $ ../bin/akaML.exe -o closure_plus_with_10_args.s <<EOF
  > let plus a b c d e f h i j k = a + b + c + d + e + f + h + i + j + k
  > 
  > let main =
  >  let clos1 = plus 1 2 3 4 5 6 7 in
  >  let clos2 = clos1 8 in
  >  let clos3 = clos2 9 10 in
  >  print_int clos3
  > ;;

  $ riscv64-linux-gnu-as -march=rv64gc closure_plus_with_10_args.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_runtime.a -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  55
