(** Copyright 2025-2026, Rodion Suvorov, Dmitriy Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
=================== manytests ===================
  $ ../../../bin/AML.exe ./manytests/typed/010faccps_ll.ml faccps.s
  Generated: faccps.s
  $ cat faccps.s
    .text
    .globl id
    .type id, @function
  id:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 24
    sd a0, -24(s0)
    ld a0, -24(s0)
  id_end:
    ld ra, 16(sp)
    ld s0, 8(sp)
    addi sp, sp, 24
    ret
    
    .globl fresh_1
    .type fresh_1, @function
  fresh_1:
    addi sp, sp, -48
    sd ra, 40(sp)
    sd s0, 32(sp)
    addi s0, sp, 48
    sd a0, -24(s0)
    sd a1, -32(s0)
    sd a2, -40(s0)
    ld t0, -40(s0)
    ld t1, -24(s0)
    mul t0, t0, t1
    sd t0, -48(s0)
    addi sp, sp, -8
    ld t0, -48(s0)
    sd t0, 0(sp)
    ld a0, -32(s0)
    li a1, 1
    addi a2, sp, 0
    call closure_apply
    addi sp, sp, 8
  fresh_1_end:
    ld ra, 40(sp)
    ld s0, 32(sp)
    addi sp, sp, 48
    ret
    
    .globl fac_cps
    .type fac_cps, @function
  fac_cps:
    addi sp, sp, -56
    sd ra, 48(sp)
    sd s0, 40(sp)
    addi s0, sp, 56
    sd a0, -24(s0)
    sd a1, -32(s0)
    ld t0, -24(s0)
    li t1, 1
    sub t2, t0, t1
    slt t0, x0, t2
    slt t3, t2, x0
    add t0, t0, t3
    xori t0, t0, 1
    sd t0, -40(s0)
    ld t0, -40(s0)
    beq t0, x0, .Lelse_0
    addi sp, sp, -8
    li t0, 1
    sd t0, 0(sp)
    ld a0, -32(s0)
    li a1, 1
    addi a2, sp, 0
    call closure_apply
    addi sp, sp, 8
    j .Lendif_1
  .Lelse_0:
    ld t0, -24(s0)
    li t1, 1
    sub t0, t0, t1
    sd t0, -48(s0)
    addi sp, sp, -16
    ld t0, -24(s0)
    sd t0, 0(sp)
    ld t0, -32(s0)
    sd t0, 8(sp)
    la a0, fresh_1
    li a1, 3
    call closure_alloc
    li a1, 2
    addi a2, sp, 0
    call closure_apply
    addi sp, sp, 16
    addi t0, a0, 0
    sd t0, -56(s0)
    addi sp, sp, -16
    addi t3, sp, 0
    ld t0, -48(s0)
    sd t0, 0(t3)
    ld t0, -56(s0)
    sd t0, 8(t3)
    ld a0, 0(t3)
    ld a1, 8(t3)
    call fac_cps
    addi t0, a0, 0
    addi a0, t0, 0
    addi sp, sp, 16
  .Lendif_1:
  fac_cps_end:
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
    addi sp, sp, -16
    addi t3, sp, 0
    li t0, 4
    sd t0, 0(t3)
    la a0, id
    li a1, 1
    call closure_alloc
    addi t0, a0, 0
    sd t0, 8(t3)
    ld a0, 0(t3)
    ld a1, 8(t3)
    call fac_cps
    addi t0, a0, 0
    addi sp, sp, 16
    sd t0, -24(s0)
    addi sp, sp, -8
    addi t3, sp, 0
    ld t0, -24(s0)
    sd t0, 0(t3)
    ld a0, 0(t3)
    call print_int
    addi t0, a0, 0
    addi sp, sp, 8
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
  $ riscv64-linux-gnu-as -march=rv64gc faccps.s -o faccps.o
  $ riscv64-linux-gnu-gcc -static faccps.o -L../../../runtime -l:libruntime.a -o faccps.elf -Wl,--no-warnings
  $ qemu-riscv64 ./faccps.elf
  24

  $ ../../../bin/AML.exe ./manytests/typed/010fibcps_ll.ml fibcps_ll.s
  Generated: fibcps_ll.s
  $ cat fibcps_ll.s
    .text
    .globl id
    .type id, @function
  id:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 24
    sd a0, -24(s0)
    ld a0, -24(s0)
  id_end:
    ld ra, 16(sp)
    ld s0, 8(sp)
    addi sp, sp, 24
    ret
    
    .globl fresh_2
    .type fresh_2, @function
  fresh_2:
    addi sp, sp, -48
    sd ra, 40(sp)
    sd s0, 32(sp)
    addi s0, sp, 48
    sd a0, -24(s0)
    sd a1, -32(s0)
    sd a2, -40(s0)
    ld t0, -24(s0)
    ld t1, -40(s0)
    add t0, t0, t1
    sd t0, -48(s0)
    addi sp, sp, -8
    ld t0, -48(s0)
    sd t0, 0(sp)
    ld a0, -32(s0)
    li a1, 1
    addi a2, sp, 0
    call closure_apply
    addi sp, sp, 8
  fresh_2_end:
    ld ra, 40(sp)
    ld s0, 32(sp)
    addi sp, sp, 48
    ret
    
    .globl fresh_1
    .type fresh_1, @function
  fresh_1:
    addi sp, sp, -64
    sd ra, 56(sp)
    sd s0, 48(sp)
    addi s0, sp, 64
    sd a0, -24(s0)
    sd a1, -32(s0)
    sd a2, -40(s0)
    sd a3, -48(s0)
    ld t0, -24(s0)
    li t1, 2
    sub t0, t0, t1
    sd t0, -56(s0)
    addi sp, sp, -16
    ld t0, -48(s0)
    sd t0, 0(sp)
    ld t0, -32(s0)
    sd t0, 8(sp)
    la a0, fresh_2
    li a1, 3
    call closure_alloc
    li a1, 2
    addi a2, sp, 0
    call closure_apply
    addi sp, sp, 16
    addi t0, a0, 0
    sd t0, -64(s0)
    addi sp, sp, -16
    ld t0, -56(s0)
    sd t0, 0(sp)
    ld t0, -64(s0)
    sd t0, 8(sp)
    ld a0, -40(s0)
    li a1, 2
    addi a2, sp, 0
    call closure_apply
    addi sp, sp, 16
  fresh_1_end:
    ld ra, 56(sp)
    ld s0, 48(sp)
    addi sp, sp, 64
    ret
    
    .globl fib
    .type fib, @function
  fib:
    addi sp, sp, -56
    sd ra, 48(sp)
    sd s0, 40(sp)
    addi s0, sp, 56
    sd a0, -24(s0)
    sd a1, -32(s0)
    ld t0, -24(s0)
    li t1, 2
    slt t0, t0, t1
    sd t0, -40(s0)
    ld t0, -40(s0)
    beq t0, x0, .Lelse_0
    addi sp, sp, -8
    ld t0, -24(s0)
    sd t0, 0(sp)
    ld a0, -32(s0)
    li a1, 1
    addi a2, sp, 0
    call closure_apply
    addi sp, sp, 8
    j .Lendif_1
  .Lelse_0:
    ld t0, -24(s0)
    li t1, 1
    sub t0, t0, t1
    sd t0, -48(s0)
    addi sp, sp, -24
    ld t0, -24(s0)
    sd t0, 0(sp)
    ld t0, -32(s0)
    sd t0, 8(sp)
    la a0, fib
    li a1, 2
    call closure_alloc
    addi t0, a0, 0
    sd t0, 16(sp)
    addi sp, sp, -8
    sd x0, 0(sp)
    la a0, fresh_1
    li a1, 4
    call closure_alloc
    li a1, 3
    addi a2, sp, 8
    call closure_apply
    addi sp, sp, 8
    addi sp, sp, 24
    addi t0, a0, 0
    sd t0, -56(s0)
    addi sp, sp, -16
    addi t3, sp, 0
    ld t0, -48(s0)
    sd t0, 0(t3)
    ld t0, -56(s0)
    sd t0, 8(t3)
    ld a0, 0(t3)
    ld a1, 8(t3)
    call fib
    addi t0, a0, 0
    addi a0, t0, 0
    addi sp, sp, 16
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
    addi sp, sp, -16
    addi t3, sp, 0
    li t0, 6
    sd t0, 0(t3)
    la a0, id
    li a1, 1
    call closure_alloc
    addi t0, a0, 0
    sd t0, 8(t3)
    ld a0, 0(t3)
    ld a1, 8(t3)
    call fib
    addi t0, a0, 0
    addi sp, sp, 16
    sd t0, -24(s0)
    addi sp, sp, -8
    addi t3, sp, 0
    ld t0, -24(s0)
    sd t0, 0(t3)
    ld a0, 0(t3)
    call print_int
    addi t0, a0, 0
    addi sp, sp, 8
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
  $ riscv64-linux-gnu-as -march=rv64gc fibcps_ll.s -o fibcps_ll.o
  $ riscv64-linux-gnu-gcc -static fibcps_ll.o -L../../../runtime -l:libruntime.a -o fibcps_ll.elf -Wl,--no-warnings
  $ qemu-riscv64 ./fibcps_ll.elf
  8

=================== without partial ===================
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
    addi sp, sp, -64
    sd ra, 56(sp)
    sd s0, 48(sp)
    addi s0, sp, 64
    sd a0, -24(s0)
    ld t0, -24(s0)
    li t1, 2
    slt t0, t0, t1
    sd t0, -32(s0)
    ld t0, -32(s0)
    beq t0, x0, .Lelse_0
    ld a0, -24(s0)
    j .Lendif_1
  .Lelse_0:
    ld t0, -24(s0)
    li t1, 1
    sub t0, t0, t1
    sd t0, -40(s0)
    addi sp, sp, -8
    addi t3, sp, 0
    ld t0, -40(s0)
    sd t0, 0(t3)
    ld a0, 0(t3)
    call fib
    addi t0, a0, 0
    addi sp, sp, 8
    sd t0, -48(s0)
    ld t0, -24(s0)
    li t1, 2
    sub t0, t0, t1
    sd t0, -56(s0)
    addi sp, sp, -8
    addi t3, sp, 0
    ld t0, -56(s0)
    sd t0, 0(t3)
    ld a0, 0(t3)
    call fib
    addi t0, a0, 0
    addi sp, sp, 8
    sd t0, -64(s0)
    ld t0, -48(s0)
    ld t1, -64(s0)
    add a0, t0, t1
  .Lendif_1:
  fib_end:
    ld ra, 56(sp)
    ld s0, 48(sp)
    addi sp, sp, 64
    ret
    
    .globl main
    .type main, @function
  main:
    addi sp, sp, -40
    sd ra, 32(sp)
    sd s0, 24(sp)
    addi s0, sp, 40
    addi sp, sp, -8
    addi t3, sp, 0
    li t0, 4
    sd t0, 0(t3)
    ld a0, 0(t3)
    call fib
    addi t0, a0, 0
    addi sp, sp, 8
    sd t0, -24(s0)
    addi sp, sp, -8
    addi t3, sp, 0
    ld t0, -24(s0)
    sd t0, 0(t3)
    ld a0, 0(t3)
    call print_int
    addi t0, a0, 0
    addi sp, sp, 8
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
    addi sp, sp, -32
    sd ra, 24(sp)
    sd s0, 16(sp)
    addi s0, sp, 32
    sd a0, -24(s0)
    li t0, 0
    ld t1, -24(s0)
    sub t2, t0, t1
    slt t0, x0, t2
    slt t3, t2, x0
    add t0, t0, t3
    sd t0, -32(s0)
    ld t0, -32(s0)
    beq t0, x0, .Lelse_0
    addi sp, sp, -8
    addi t3, sp, 0
    li t0, 0
    sd t0, 0(t3)
    ld a0, 0(t3)
    call print_int
    addi t0, a0, 0
    addi a0, t0, 0
    addi sp, sp, 8
    j .Lendif_1
  .Lelse_0:
    addi sp, sp, -8
    addi t3, sp, 0
    li t0, 1
    sd t0, 0(t3)
    ld a0, 0(t3)
    call print_int
    addi t0, a0, 0
    addi a0, t0, 0
    addi sp, sp, 8
  .Lendif_1:
  large_end:
    ld ra, 24(sp)
    ld s0, 16(sp)
    addi sp, sp, 32
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
    addi sp, sp, -8
    addi t3, sp, 0
    ld t0, -48(s0)
    sd t0, 0(t3)
    ld a0, 0(t3)
    call large
    addi t0, a0, 0
    addi a0, t0, 0
    addi sp, sp, 8
    j .Lendif_7
  .Lelse_6:
    li t0, 1
    sd t0, -56(s0)
    addi sp, sp, -8
    addi t3, sp, 0
    ld t0, -56(s0)
    sd t0, 0(t3)
    ld a0, 0(t3)
    call large
    addi t0, a0, 0
    addi a0, t0, 0
    addi sp, sp, 8
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
    addi sp, sp, -8
    addi t3, sp, 0
    ld t0, -72(s0)
    sd t0, 0(t3)
    ld a0, 0(t3)
    call large
    addi t0, a0, 0
    addi a0, t0, 0
    addi sp, sp, 8
    j .Lendif_9
  .Lelse_8:
    li t0, 1
    sd t0, -80(s0)
    addi sp, sp, -8
    addi t3, sp, 0
    ld t0, -80(s0)
    sd t0, 0(t3)
    ld a0, 0(t3)
    call large
    addi t0, a0, 0
    addi a0, t0, 0
    addi sp, sp, 8
  .Lendif_9:
  .Lendif_5:
    j .Lendif_3
  .Lelse_2:
    addi sp, sp, -8
    addi t3, sp, 0
    li t0, 42
    sd t0, 0(t3)
    ld a0, 0(t3)
    call print_int
    addi t0, a0, 0
    addi sp, sp, 8
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
    addi sp, sp, -8
    addi t3, sp, 0
    ld t0, -120(s0)
    sd t0, 0(t3)
    ld a0, 0(t3)
    call large
    addi t0, a0, 0
    addi a0, t0, 0
    addi sp, sp, 8
    j .Lendif_13
  .Lelse_12:
    li t0, 1
    sd t0, -128(s0)
    addi sp, sp, -8
    addi t3, sp, 0
    ld t0, -128(s0)
    sd t0, 0(t3)
    ld a0, 0(t3)
    call large
    addi t0, a0, 0
    addi a0, t0, 0
    addi sp, sp, 8
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
    addi sp, sp, -8
    addi t3, sp, 0
    ld t0, -144(s0)
    sd t0, 0(t3)
    ld a0, 0(t3)
    call large
    addi t0, a0, 0
    addi a0, t0, 0
    addi sp, sp, 8
    j .Lendif_15
  .Lelse_14:
    li t0, 1
    sd t0, -152(s0)
    addi sp, sp, -8
    addi t3, sp, 0
    ld t0, -152(s0)
    sd t0, 0(t3)
    ld a0, 0(t3)
    call large
    addi t0, a0, 0
    addi a0, t0, 0
    addi sp, sp, 8
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

=================== custom ===================
  $ cat >many_args.ml <<EOF
  > let f a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 = a0+a1+a2+a3+a4+a5+a6+a7+a8+a9+a10
  > let main = print_int (f 0 1 2 3 4 5 6 7 8 9 10)
  > EOF
  $ ../../../bin/AML.exe many_args.ml many_args.s
  Generated: many_args.s
  $ cat many_args.s
    .text
    .globl f
    .type f, @function
  f:
    addi sp, sp, -152
    sd ra, 144(sp)
    sd s0, 136(sp)
    addi s0, sp, 152
    sd a0, -24(s0)
    sd a1, -32(s0)
    sd a2, -40(s0)
    sd a3, -48(s0)
    sd a4, -56(s0)
    sd a5, -64(s0)
    sd a6, -72(s0)
    sd a7, -80(s0)
    ld t0, -24(s0)
    ld t1, -32(s0)
    add t0, t0, t1
    sd t0, -88(s0)
    ld t0, -88(s0)
    ld t1, -40(s0)
    add t0, t0, t1
    sd t0, -96(s0)
    ld t0, -96(s0)
    ld t1, -48(s0)
    add t0, t0, t1
    sd t0, -104(s0)
    ld t0, -104(s0)
    ld t1, -56(s0)
    add t0, t0, t1
    sd t0, -112(s0)
    ld t0, -112(s0)
    ld t1, -64(s0)
    add t0, t0, t1
    sd t0, -120(s0)
    ld t0, -120(s0)
    ld t1, -72(s0)
    add t0, t0, t1
    sd t0, -128(s0)
    ld t0, -128(s0)
    ld t1, -80(s0)
    add t0, t0, t1
    sd t0, -136(s0)
    ld t0, -136(s0)
    ld t1, 0(s0)
    add t0, t0, t1
    sd t0, -144(s0)
    ld t0, -144(s0)
    ld t1, 8(s0)
    add t0, t0, t1
    sd t0, -152(s0)
    ld t0, -152(s0)
    ld t1, 16(s0)
    add a0, t0, t1
  f_end:
    ld ra, 144(sp)
    ld s0, 136(sp)
    addi sp, sp, 152
    ret
    
    .globl main
    .type main, @function
  main:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 24
    addi sp, sp, -64
    addi t3, sp, 0
    li t0, 0
    sd t0, 0(t3)
    li t0, 1
    sd t0, 8(t3)
    li t0, 2
    sd t0, 16(t3)
    li t0, 3
    sd t0, 24(t3)
    li t0, 4
    sd t0, 32(t3)
    li t0, 5
    sd t0, 40(t3)
    li t0, 6
    sd t0, 48(t3)
    li t0, 7
    sd t0, 56(t3)
    li t0, 10
    addi sp, sp, -8
    sd t0, 0(sp)
    li t0, 9
    addi sp, sp, -8
    sd t0, 0(sp)
    li t0, 8
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(t3)
    ld a1, 8(t3)
    ld a2, 16(t3)
    ld a3, 24(t3)
    ld a4, 32(t3)
    ld a5, 40(t3)
    ld a6, 48(t3)
    ld a7, 56(t3)
    call f
    addi t0, a0, 0
    addi sp, sp, 24
    addi sp, sp, 64
    sd t0, -24(s0)
    addi sp, sp, -8
    addi t3, sp, 0
    ld t0, -24(s0)
    sd t0, 0(t3)
    ld a0, 0(t3)
    call print_int
    addi t0, a0, 0
    addi a0, t0, 0
    addi sp, sp, 8
  main_end:
    ld ra, 16(sp)
    ld s0, 8(sp)
    addi sp, sp, 24
    li a0, 0
    li a7, 93
    ecall

  $ riscv64-linux-gnu-as -march=rv64gc many_args.s -o many_args.o
  $ riscv64-linux-gnu-gcc -static many_args.o -L../../../runtime -l:libruntime.a -o many_args.elf -Wl,--no-warnings
  $ qemu-riscv64 ./many_args.elf
  55
