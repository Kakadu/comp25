this is a copy of codegen.t, but with peephole optimizations turned off

  $ dune exec ./../bin/XML.exe -- -o factorial.s -no-opt-peep <<EOF
  > let rec fac n = if n = 0 then 1 else n * fac (n - 1)
  > 
  > let main = print_int (fac 4)

  $ cat factorial.s
  .section .text
  .global main
  .type main, @function
  fac:
    addi sp, sp, -88
    sd ra, 80(sp)
    sd s0, 72(sp)
    addi s0, sp, 72
    mv t0, a0
    li t1, 1
    xor t2, t0, t1
    seqz t0, t2
    sd t0, -8(s0)
    ld t0, -8(s0)
    beq t0, zero, else_0
    li t0, 3
    j endif_1
  else_0:
    mv t0, a0
    li t1, 3
    sub t0, t0, t1
    addi t0, t0, 1
    sd t0, -16(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    ld t0, -16(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call fac
    mv t0, a0
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -24(s0)
    mv t0, a0
    ld t1, -24(s0)
    srai t2, t0, 1
    srai t3, t1, 1
    mul t0, t2, t3
    add t0, t0, t0
    addi t0, t0, 1
    sd t0, -32(s0)
    ld t0, -32(s0)
  endif_1:
    sd t0, -40(s0)
    ld a0, -40(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -72
    sd ra, 64(sp)
    sd s0, 56(sp)
    addi s0, sp, 56
    li a0, 5120
    call rt_init
    li t0, 9
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call fac
    mv t0, a0
    sd t0, -8(s0)
    ld t0, -8(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call print_int
    mv t0, a0
    sd t0, -16(s0)
    ld a0, -16(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  $ riscv64-linux-gnu-as -march=rv64gc factorial.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  24

====================== Fibonacci ======================
  $ dune exec -- ../bin/XML.exe -o fibonacci.s -no-opt-peep <<EOF
  > let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)
  > 
  > let main = print_int (fib 6)

  $ cat fibonacci.s
  .section .text
  .global main
  .type main, @function
  fib:
    addi sp, sp, -104
    sd ra, 96(sp)
    sd s0, 88(sp)
    addi s0, sp, 88
    mv t0, a0
    li t1, 3
    slt t2, t1, t0
    seqz t0, t2
    sd t0, -8(s0)
    ld t0, -8(s0)
    beq t0, zero, else_0
    mv t0, a0
    j endif_1
  else_0:
    mv t0, a0
    li t1, 3
    sub t0, t0, t1
    addi t0, t0, 1
    sd t0, -16(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    ld t0, -16(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call fib
    mv t0, a0
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -24(s0)
    mv t0, a0
    li t1, 5
    sub t0, t0, t1
    addi t0, t0, 1
    sd t0, -32(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    ld t0, -32(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call fib
    mv t0, a0
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -40(s0)
    ld t0, -24(s0)
    ld t1, -40(s0)
    add t0, t0, t1
    addi t0, t0, -1
    sd t0, -48(s0)
    ld t0, -48(s0)
  endif_1:
    sd t0, -56(s0)
    ld a0, -56(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -72
    sd ra, 64(sp)
    sd s0, 56(sp)
    addi s0, sp, 56
    li a0, 5120
    call rt_init
    li t0, 13
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call fib
    mv t0, a0
    sd t0, -8(s0)
    ld t0, -8(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call print_int
    mv t0, a0
    sd t0, -16(s0)
    ld a0, -16(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  $ riscv64-linux-gnu-as -march=rv64gc fibonacci.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  8

====================== Ififif ======================
  $ dune exec -- ../bin/XML.exe -o ififif.s -no-opt-peep <<EOF
  > let large x = if 0<>x then print_int 0 else print_int 1
  > let main =
  >   let x = if (if (if 0 = 1
  >                   then 0 = 1 else (let t42 = print_int 42 in 1 = 1))
  >               then 0 else 1) = 1
  >           then 0 else 1 in
  >   large x

  $ cat ififif.s
  .section .text
  .global main
  .type main, @function
  large:
    addi sp, sp, -72
    sd ra, 64(sp)
    sd s0, 56(sp)
    addi s0, sp, 56
    li t0, 1
    mv t1, a0
    xor t2, t0, t1
    snez t0, t2
    sd t0, -8(s0)
    ld t0, -8(s0)
    beq t0, zero, else_0
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    li t0, 1
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call print_int
    mv t0, a0
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -16(s0)
    ld t0, -16(s0)
    j endif_1
  else_0:
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    li t0, 3
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call print_int
    mv t0, a0
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -16(s0)
    ld t0, -16(s0)
  endif_1:
    sd t0, -24(s0)
    ld a0, -24(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -112
    sd ra, 104(sp)
    sd s0, 96(sp)
    addi s0, sp, 96
    li a0, 5120
    call rt_init
    li t0, 1
    li t1, 3
    xor t2, t0, t1
    seqz t0, t2
    sd t0, -8(s0)
    ld t0, -8(s0)
    beq t0, zero, else_2
    li t0, 1
    li t1, 3
    xor t2, t0, t1
    seqz t0, t2
    sd t0, -16(s0)
    ld t0, -16(s0)
    j endif_3
  else_2:
    li t0, 85
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call print_int
    mv t0, a0
    sd t0, -16(s0)
    ld t0, -16(s0)
    sd t0, -24(s0)
    li t0, 3
    li t1, 3
    xor t2, t0, t1
    seqz t0, t2
    sd t0, -32(s0)
    ld t0, -32(s0)
  endif_3:
    sd t0, -40(s0)
    ld t0, -40(s0)
    beq t0, zero, else_4
    li t0, 1
    j endif_5
  else_4:
    li t0, 3
  endif_5:
    sd t0, -48(s0)
    ld t0, -48(s0)
    li t1, 3
    xor t2, t0, t1
    seqz t0, t2
    sd t0, -56(s0)
    ld t0, -56(s0)
    beq t0, zero, else_6
    li t0, 1
    j endif_7
  else_6:
    li t0, 3
  endif_7:
    sd t0, -64(s0)
    ld t0, -64(s0)
    sd t0, -72(s0)
    ld t0, -72(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call large
    mv t0, a0
    sd t0, -80(s0)
    ld a0, -80(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret

  $ riscv64-linux-gnu-as -march=rv64gc ififif.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  42
  0


====================== Simple Closure ======================
  $ dune exec -- ../bin/XML.exe -o closure.s -no-opt-peep <<EOF
  > let simplesum x y = x + y
  > let partialapp_sum = simplesum 5
  > let main = print_int (partialapp_sum 5)
  $ cat closure.s
  .section .text
  .global main
  .type main, @function
  simplesum:
    addi sp, sp, -64
    sd ra, 56(sp)
    sd s0, 48(sp)
    addi s0, sp, 48
    mv t0, a0
    mv t1, a1
    add t0, t0, t1
    addi t0, t0, -1
    sd t0, -8(s0)
    ld a0, -8(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  partialapp_sum:
    addi sp, sp, -64
    sd ra, 56(sp)
    sd s0, 48(sp)
    addi s0, sp, 48
    addi sp, sp, -8
    li t1, 11
    sd t1, 0(sp)
    la a0, simplesum
    li a1, 2
    call alloc_closure
    mv t0, a0
    ld t1, 0(sp)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    addi sp, sp, 8
    sd t0, -8(s0)
    ld a0, -8(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -72
    sd ra, 64(sp)
    sd s0, 56(sp)
    addi s0, sp, 56
    li a0, 5120
    call rt_init
    call partialapp_sum
    mv t0, a0
    li t1, 11
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -8(s0)
    ld t0, -8(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call print_int
    mv t0, a0
    sd t0, -16(s0)
    ld a0, -16(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret

  $ riscv64-linux-gnu-as -march=rv64gc closure.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  10



====================== CPS Factorial ======================
  $ dune exec -- ../bin/XML.exe -fromfile manytests/typed/010faccps_ll.ml -o 010faccps_ll.s -no-opt-peep

  $ cat 010faccps_ll.s
  .section .text
  .global main
  .type main, @function
  id:
    addi sp, sp, -56
    sd ra, 48(sp)
    sd s0, 40(sp)
    addi s0, sp, 40
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  fresh_1:
    addi sp, sp, -72
    sd ra, 64(sp)
    sd s0, 56(sp)
    addi s0, sp, 56
    mv t0, a2
    mv t1, a0
    srai t2, t0, 1
    srai t3, t1, 1
    mul t0, t2, t3
    add t0, t0, t0
    addi t0, t0, 1
    sd t0, -8(s0)
    addi sp, sp, -8
    sd a2, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    mv t0, a1
    ld t1, -8(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a2, 0(sp)
    addi sp, sp, 8
    sd t0, -16(s0)
    ld a0, -16(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  fac_cps:
    addi sp, sp, -104
    sd ra, 96(sp)
    sd s0, 88(sp)
    addi s0, sp, 88
    mv t0, a0
    li t1, 3
    xor t2, t0, t1
    seqz t0, t2
    sd t0, -8(s0)
    ld t0, -8(s0)
    beq t0, zero, else_0
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    mv t0, a1
    li t1, 3
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -16(s0)
    ld t0, -16(s0)
    j endif_1
  else_0:
    mv t0, a0
    li t1, 3
    sub t0, t0, t1
    addi t0, t0, 1
    sd t0, -16(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    ld t1, -16(s0)
    sd t1, 0(sp)
    la a0, fac_cps
    li a1, 2
    call alloc_closure
    mv t0, a0
    ld t1, 0(sp)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -24(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    mv t1, a0
    sd t1, 0(sp)
    la a0, fresh_1
    li a1, 3
    call alloc_closure
    mv t0, a0
    ld t1, 0(sp)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -32(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    ld t0, -32(s0)
    mv t1, a1
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -40(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    ld t0, -24(s0)
    ld t1, -40(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -48(s0)
    ld t0, -48(s0)
  endif_1:
    sd t0, -56(s0)
    ld a0, -56(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -80
    sd ra, 72(sp)
    sd s0, 64(sp)
    addi s0, sp, 64
    li a0, 5120
    call rt_init
    addi sp, sp, -8
    li t1, 9
    sd t1, 0(sp)
    la a0, fac_cps
    li a1, 2
    call alloc_closure
    mv t0, a0
    ld t1, 0(sp)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    addi sp, sp, 8
    sd t0, -8(s0)
    ld t0, -8(s0)
    la a0, id
    li a1, 1
    call alloc_closure
    mv t1, a0
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -16(s0)
    ld t0, -16(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call print_int
    mv t0, a0
    sd t0, -24(s0)
    li a0, 1
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  $ riscv64-linux-gnu-as -march=rv64gc 010faccps_ll.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  24
  [1]

====================== CPS Fibbo ======================
  $ dune exec -- ../bin/XML.exe -fromfile manytests/typed/010fibcps_ll.ml -o 010fibcps_ll.s -no-opt-peep

  $ cat 010fibcps_ll.s
  .section .text
  .global main
  .type main, @function
  id:
    addi sp, sp, -56
    sd ra, 48(sp)
    sd s0, 40(sp)
    addi s0, sp, 40
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  fresh_2:
    addi sp, sp, -72
    sd ra, 64(sp)
    sd s0, 56(sp)
    addi s0, sp, 56
    mv t0, a0
    mv t1, a2
    add t0, t0, t1
    addi t0, t0, -1
    sd t0, -8(s0)
    addi sp, sp, -8
    sd a2, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    mv t0, a1
    ld t1, -8(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a2, 0(sp)
    addi sp, sp, 8
    sd t0, -16(s0)
    ld a0, -16(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  fresh_1:
    addi sp, sp, -96
    sd ra, 88(sp)
    sd s0, 80(sp)
    addi s0, sp, 80
    mv t0, a0
    li t1, 5
    sub t0, t0, t1
    addi t0, t0, 1
    sd t0, -8(s0)
    addi sp, sp, -8
    sd a3, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    sd a2, 0(sp)
    mv t0, a2
    ld t1, -8(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a2, 0(sp)
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a3, 0(sp)
    addi sp, sp, 8
    sd t0, -16(s0)
    addi sp, sp, -8
    sd a3, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    sd a2, 0(sp)
    addi sp, sp, -8
    mv t1, a3
    sd t1, 0(sp)
    la a0, fresh_2
    li a1, 3
    call alloc_closure
    mv t0, a0
    ld t1, 0(sp)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    addi sp, sp, 8
    ld a2, 0(sp)
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a3, 0(sp)
    addi sp, sp, 8
    sd t0, -24(s0)
    addi sp, sp, -8
    sd a3, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    sd a2, 0(sp)
    ld t0, -24(s0)
    mv t1, a1
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a2, 0(sp)
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a3, 0(sp)
    addi sp, sp, 8
    sd t0, -32(s0)
    addi sp, sp, -8
    sd a3, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    sd a2, 0(sp)
    ld t0, -16(s0)
    ld t1, -32(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a2, 0(sp)
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a3, 0(sp)
    addi sp, sp, 8
    sd t0, -40(s0)
    ld a0, -40(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  fib:
    addi sp, sp, -112
    sd ra, 104(sp)
    sd s0, 96(sp)
    addi s0, sp, 96
    mv t0, a0
    li t1, 5
    slt t0, t0, t1
    sd t0, -8(s0)
    ld t0, -8(s0)
    beq t0, zero, else_0
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    mv t0, a1
    mv t1, a0
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -16(s0)
    ld t0, -16(s0)
    j endif_1
  else_0:
    mv t0, a0
    li t1, 3
    sub t0, t0, t1
    addi t0, t0, 1
    sd t0, -16(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    ld t1, -16(s0)
    sd t1, 0(sp)
    la a0, fib
    li a1, 2
    call alloc_closure
    mv t0, a0
    ld t1, 0(sp)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -24(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    mv t1, a0
    sd t1, 0(sp)
    la a0, fresh_1
    li a1, 4
    call alloc_closure
    mv t0, a0
    ld t1, 0(sp)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -32(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    ld t0, -32(s0)
    mv t1, a1
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -40(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    ld t0, -40(s0)
    la a0, fib
    li a1, 2
    call alloc_closure
    mv t1, a0
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -48(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    ld t0, -24(s0)
    ld t1, -48(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -56(s0)
    ld t0, -56(s0)
  endif_1:
    sd t0, -64(s0)
    ld a0, -64(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -88
    sd ra, 80(sp)
    sd s0, 72(sp)
    addi s0, sp, 72
    li a0, 5120
    call rt_init
    addi sp, sp, -8
    li t1, 13
    sd t1, 0(sp)
    la a0, fib
    li a1, 2
    call alloc_closure
    mv t0, a0
    ld t1, 0(sp)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    addi sp, sp, 8
    sd t0, -8(s0)
    ld t0, -8(s0)
    la a0, id
    li a1, 1
    call alloc_closure
    mv t1, a0
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -16(s0)
    ld t0, -16(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call print_int
    mv t0, a0
    sd t0, -24(s0)
    ld t0, -24(s0)
    sd t0, -32(s0)
    li a0, 1
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  $ riscv64-linux-gnu-as -march=rv64gc 010fibcps_ll.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  8
  [1]

  $ dune exec -- ../bin/XML.exe -fromfile manytests/typed/004manyargs.ml -o 004manyargs.s -no-opt-peep

  $ cat 004manyargs.s
  .section .text
  .global main
  .type main, @function
  wrap:
    addi sp, sp, -72
    sd ra, 64(sp)
    sd s0, 56(sp)
    addi s0, sp, 56
    li t0, 3
    li t1, 3
    xor t2, t0, t1
    seqz t0, t2
    sd t0, -8(s0)
    ld t0, -8(s0)
    beq t0, zero, else_0
    mv t0, a0
    j endif_1
  else_0:
    mv t0, a0
  endif_1:
    sd t0, -16(s0)
    ld a0, -16(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  test3:
    addi sp, sp, -104
    sd ra, 96(sp)
    sd s0, 88(sp)
    addi s0, sp, 88
    addi sp, sp, -8
    sd a2, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    mv t0, a0
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call print_int
    mv t0, a0
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a2, 0(sp)
    addi sp, sp, 8
    sd t0, -8(s0)
    ld t0, -8(s0)
    sd t0, -16(s0)
    addi sp, sp, -8
    sd a2, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    mv t0, a1
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call print_int
    mv t0, a0
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a2, 0(sp)
    addi sp, sp, 8
    sd t0, -24(s0)
    ld t0, -24(s0)
    sd t0, -32(s0)
    addi sp, sp, -8
    sd a2, 0(sp)
    addi sp, sp, -8
    mv t0, a2
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call print_int
    mv t0, a0
    addi sp, sp, 8
    ld a2, 0(sp)
    addi sp, sp, 8
    sd t0, -40(s0)
    ld t0, -40(s0)
    sd t0, -48(s0)
    li a0, 1
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  test10:
    addi sp, sp, -128
    sd ra, 120(sp)
    sd s0, 112(sp)
    addi s0, sp, 112
    mv t0, a0
    mv t1, a1
    add t0, t0, t1
    addi t0, t0, -1
    sd t0, -8(s0)
    ld t0, -8(s0)
    mv t1, a2
    add t0, t0, t1
    addi t0, t0, -1
    sd t0, -16(s0)
    ld t0, -16(s0)
    mv t1, a3
    add t0, t0, t1
    addi t0, t0, -1
    sd t0, -24(s0)
    ld t0, -24(s0)
    mv t1, a4
    add t0, t0, t1
    addi t0, t0, -1
    sd t0, -32(s0)
    ld t0, -32(s0)
    mv t1, a5
    add t0, t0, t1
    addi t0, t0, -1
    sd t0, -40(s0)
    ld t0, -40(s0)
    mv t1, a6
    add t0, t0, t1
    addi t0, t0, -1
    sd t0, -48(s0)
    ld t0, -48(s0)
    mv t1, a7
    add t0, t0, t1
    addi t0, t0, -1
    sd t0, -56(s0)
    ld t0, -56(s0)
    ld t1, 16(s0)
    add t0, t0, t1
    addi t0, t0, -1
    sd t0, -64(s0)
    ld t0, -64(s0)
    ld t1, 24(s0)
    add t0, t0, t1
    addi t0, t0, -1
    sd t0, -72(s0)
    ld a0, -72(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -200
    sd ra, 192(sp)
    sd s0, 184(sp)
    addi s0, sp, 184
    li a0, 5120
    call rt_init
    la a0, test10
    li a1, 10
    call alloc_closure
    mv t0, a0
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call wrap
    mv t0, a0
    sd t0, -8(s0)
    ld t0, -8(s0)
    li t1, 3
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -16(s0)
    ld t0, -16(s0)
    li t1, 21
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -24(s0)
    ld t0, -24(s0)
    li t1, 201
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -32(s0)
    ld t0, -32(s0)
    li t1, 2001
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -40(s0)
    ld t0, -40(s0)
    li t1, 20001
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -48(s0)
    ld t0, -48(s0)
    li t1, 200001
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -56(s0)
    ld t0, -56(s0)
    li t1, 2000001
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -64(s0)
    ld t0, -64(s0)
    li t1, 20000001
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -72(s0)
    ld t0, -72(s0)
    li t1, 200000001
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -80(s0)
    ld t0, -80(s0)
    li t1, 2000000001
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -88(s0)
    ld t0, -88(s0)
    sd t0, -96(s0)
    ld t0, -96(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call print_int
    mv t0, a0
    sd t0, -104(s0)
    la a0, test3
    li a1, 3
    call alloc_closure
    mv t0, a0
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call wrap
    mv t0, a0
    sd t0, -112(s0)
    ld t0, -112(s0)
    li t1, 3
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -120(s0)
    ld t0, -120(s0)
    li t1, 21
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -128(s0)
    ld t0, -128(s0)
    li t1, 201
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -136(s0)
    ld t0, -136(s0)
    sd t0, -144(s0)
    li a0, 1
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret

  $ riscv64-linux-gnu-as -march=rv64gc 004manyargs.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  1111111111
  1
  10
  100
  [1]

  $ dune exec -- ../bin/XML.exe -o tuple_return.s -no-opt-peep <<EOF
  > let make_pair x y = (x, y)
  > 
  > let main =
  >   let p = make_pair 10 20 in
  >   let (a, b) = p in
  >   print_int (a + b)
  $ cat tuple_return.s
  .section .text
  .global main
  .type main, @function
  make_pair:
    addi sp, sp, -64
    sd ra, 56(sp)
    sd s0, 48(sp)
    addi s0, sp, 48
    addi sp, sp, -8
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    li a0, 2
    call create_tuple
    addi sp, sp, 8
    sd a0, 16(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    mv t1, a0
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 0(t2)
    mv t1, a1
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 8(t2)
    ld t0, 0(sp)
    addi sp, sp, 8
    sd t0, -8(s0)
    ld a0, -8(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -136
    sd ra, 128(sp)
    sd s0, 120(sp)
    addi s0, sp, 120
    li a0, 5120
    call rt_init
    addi sp, sp, -8
    li t1, 21
    sd t1, 0(sp)
    la a0, make_pair
    li a1, 2
    call alloc_closure
    mv t0, a0
    ld t1, 0(sp)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    addi sp, sp, 8
    sd t0, -8(s0)
    ld t0, -8(s0)
    li t1, 41
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -16(s0)
    ld t0, -16(s0)
    sd t0, -24(s0)
    ld t0, -24(s0)
    sd t0, -32(s0)
    ld a0, -32(s0)
    li a1, 0
    call field
    mv t0, a0
    sd t0, -40(s0)
    ld t0, -40(s0)
    sd t0, -48(s0)
    ld a0, -32(s0)
    li a1, 1
    call field
    mv t0, a0
    sd t0, -56(s0)
    ld t0, -56(s0)
    sd t0, -64(s0)
    ld t0, -48(s0)
    ld t1, -64(s0)
    add t0, t0, t1
    addi t0, t0, -1
    sd t0, -72(s0)
    ld t0, -72(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call print_int
    mv t0, a0
    sd t0, -80(s0)
    ld a0, -80(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret

  $ riscv64-linux-gnu-as -march=rv64gc tuple_return.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  30

  $ dune exec -- ../bin/XML.exe -o tuple_swap.s -no-opt-peep <<EOF
  > let swap p =
  >   let (a, b) = p in
  >   (b, a)
  > 
  > let main =
  >   let p1 = (1, 2) in
  >   let p2 = swap p1 in
  >   let (x, y) = p2 in
  >   print_int x
  $ cat tuple_swap.s
  .section .text
  .global main
  .type main, @function
  swap:
    addi sp, sp, -104
    sd ra, 96(sp)
    sd s0, 88(sp)
    addi s0, sp, 88
    mv t0, a0
    sd t0, -8(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    ld a0, -8(s0)
    li a1, 0
    call field
    addi sp, sp, 8
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -16(s0)
    ld t0, -16(s0)
    sd t0, -24(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    ld a0, -8(s0)
    li a1, 1
    call field
    addi sp, sp, 8
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -32(s0)
    ld t0, -32(s0)
    sd t0, -40(s0)
    addi sp, sp, -8
    addi sp, sp, -8
    sd a0, 0(sp)
    li a0, 2
    call create_tuple
    sd a0, 8(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    ld t1, -40(s0)
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 0(t2)
    ld t1, -24(s0)
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 8(t2)
    ld t0, 0(sp)
    addi sp, sp, 8
    sd t0, -48(s0)
    ld a0, -48(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -136
    sd ra, 128(sp)
    sd s0, 120(sp)
    addi s0, sp, 120
    li a0, 5120
    call rt_init
    addi sp, sp, -8
    addi sp, sp, -8
    li a0, 2
    call create_tuple
    addi sp, sp, 8
    sd a0, 0(sp)
    li t1, 3
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 0(t2)
    li t1, 5
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 8(t2)
    ld t0, 0(sp)
    addi sp, sp, 8
    sd t0, -8(s0)
    ld t0, -8(s0)
    sd t0, -16(s0)
    ld t0, -16(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call swap
    mv t0, a0
    sd t0, -24(s0)
    ld t0, -24(s0)
    sd t0, -32(s0)
    ld t0, -32(s0)
    sd t0, -40(s0)
    ld a0, -40(s0)
    li a1, 0
    call field
    mv t0, a0
    sd t0, -48(s0)
    ld t0, -48(s0)
    sd t0, -56(s0)
    ld a0, -40(s0)
    li a1, 1
    call field
    mv t0, a0
    sd t0, -64(s0)
    ld t0, -64(s0)
    sd t0, -72(s0)
    ld t0, -56(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call print_int
    mv t0, a0
    sd t0, -80(s0)
    ld a0, -80(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret

  $ riscv64-linux-gnu-as -march=rv64gc tuple_swap.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  2

  $ dune exec -- ../bin/XML.exe -o tuple_order.s -no-opt-peep <<EOF
  > let f n =
  >   n
  > 
  > let main =
  >   let t = (f 10, f 20) in
  >   let (a, b) = t in
  >   print_int (a + b)
  $ cat tuple_order.s
  .section .text
  .global main
  .type main, @function
  f:
    addi sp, sp, -56
    sd ra, 48(sp)
    sd s0, 40(sp)
    addi s0, sp, 40
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -144
    sd ra, 136(sp)
    sd s0, 128(sp)
    addi s0, sp, 128
    li a0, 5120
    call rt_init
    li t0, 21
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call f
    mv t0, a0
    sd t0, -8(s0)
    li t0, 41
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call f
    mv t0, a0
    sd t0, -16(s0)
    addi sp, sp, -8
    addi sp, sp, -8
    li a0, 2
    call create_tuple
    addi sp, sp, 8
    sd a0, 0(sp)
    ld t1, -8(s0)
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 0(t2)
    ld t1, -16(s0)
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 8(t2)
    ld t0, 0(sp)
    addi sp, sp, 8
    sd t0, -24(s0)
    ld t0, -24(s0)
    sd t0, -32(s0)
    ld t0, -32(s0)
    sd t0, -40(s0)
    ld a0, -40(s0)
    li a1, 0
    call field
    mv t0, a0
    sd t0, -48(s0)
    ld t0, -48(s0)
    sd t0, -56(s0)
    ld a0, -40(s0)
    li a1, 1
    call field
    mv t0, a0
    sd t0, -64(s0)
    ld t0, -64(s0)
    sd t0, -72(s0)
    ld t0, -56(s0)
    ld t1, -72(s0)
    add t0, t0, t1
    addi t0, t0, -1
    sd t0, -80(s0)
    ld t0, -80(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call print_int
    mv t0, a0
    sd t0, -88(s0)
    ld a0, -88(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret

  $ riscv64-linux-gnu-as -march=rv64gc tuple_order.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  30

  $ dune exec -- ../bin/XML.exe -o tuple_linked_list.s -notypes -no-opt-peep <<EOF
  > let rec sum_list lst =
  >   if lst = 0 then 0 else
  >   let (head, tail) = lst in
  >   head + sum_list tail
  > 
  > let main =
  >   let lst = (10, (20, (30, 0))) in
  >   print_int (sum_list lst)
  $ cat tuple_linked_list.s
  .section .text
  .global main
  .type main, @function
  sum_list:
    addi sp, sp, -120
    sd ra, 112(sp)
    sd s0, 104(sp)
    addi s0, sp, 104
    mv t0, a0
    li t1, 1
    xor t2, t0, t1
    seqz t0, t2
    sd t0, -8(s0)
    ld t0, -8(s0)
    beq t0, zero, else_0
    li t0, 1
    j endif_1
  else_0:
    mv t0, a0
    sd t0, -16(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    ld a0, -16(s0)
    li a1, 0
    call field
    addi sp, sp, 8
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -24(s0)
    ld t0, -24(s0)
    sd t0, -32(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    ld a0, -16(s0)
    li a1, 1
    call field
    addi sp, sp, 8
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -40(s0)
    ld t0, -40(s0)
    sd t0, -48(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    ld t0, -48(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call sum_list
    mv t0, a0
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -56(s0)
    ld t0, -32(s0)
    ld t1, -56(s0)
    add t0, t0, t1
    addi t0, t0, -1
    sd t0, -64(s0)
    ld t0, -64(s0)
  endif_1:
    sd t0, -72(s0)
    ld a0, -72(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -104
    sd ra, 96(sp)
    sd s0, 88(sp)
    addi s0, sp, 88
    li a0, 5120
    call rt_init
    addi sp, sp, -8
    addi sp, sp, -8
    li a0, 2
    call create_tuple
    addi sp, sp, 8
    sd a0, 0(sp)
    li t1, 61
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 0(t2)
    li t1, 1
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 8(t2)
    ld t0, 0(sp)
    addi sp, sp, 8
    sd t0, -8(s0)
    addi sp, sp, -8
    addi sp, sp, -8
    li a0, 2
    call create_tuple
    addi sp, sp, 8
    sd a0, 0(sp)
    li t1, 41
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 0(t2)
    ld t1, -8(s0)
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 8(t2)
    ld t0, 0(sp)
    addi sp, sp, 8
    sd t0, -16(s0)
    addi sp, sp, -8
    addi sp, sp, -8
    li a0, 2
    call create_tuple
    addi sp, sp, 8
    sd a0, 0(sp)
    li t1, 21
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 0(t2)
    ld t1, -16(s0)
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 8(t2)
    ld t0, 0(sp)
    addi sp, sp, 8
    sd t0, -24(s0)
    ld t0, -24(s0)
    sd t0, -32(s0)
    ld t0, -32(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call sum_list
    mv t0, a0
    sd t0, -40(s0)
    ld t0, -40(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call print_int
    mv t0, a0
    sd t0, -48(s0)
    ld a0, -48(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret

  $ riscv64-linux-gnu-as -march=rv64gc tuple_linked_list.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  60

  $ dune exec -- ../bin/XML.exe -o tuple_large.s -no-opt-peep <<EOF
  > let main =
  >   let t = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10) in
  >   let (a, b, c, d, e, f, g, h, i, j) = t in
  >   print_int j
  $ cat tuple_large.s
  .section .text
  .global main
  .type main, @function
  main:
    addi sp, sp, -248
    sd ra, 240(sp)
    sd s0, 232(sp)
    addi s0, sp, 232
    li a0, 5120
    call rt_init
    addi sp, sp, -8
    addi sp, sp, -8
    li a0, 10
    call create_tuple
    addi sp, sp, 8
    sd a0, 0(sp)
    li t1, 3
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 0(t2)
    li t1, 5
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 8(t2)
    li t1, 7
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 16(t2)
    li t1, 9
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 24(t2)
    li t1, 11
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 32(t2)
    li t1, 13
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 40(t2)
    li t1, 15
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 48(t2)
    li t1, 17
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 56(t2)
    li t1, 19
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 64(t2)
    li t1, 21
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 72(t2)
    ld t0, 0(sp)
    addi sp, sp, 8
    sd t0, -8(s0)
    ld t0, -8(s0)
    sd t0, -16(s0)
    ld t0, -16(s0)
    sd t0, -24(s0)
    ld a0, -24(s0)
    li a1, 0
    call field
    mv t0, a0
    sd t0, -32(s0)
    ld t0, -32(s0)
    sd t0, -40(s0)
    ld a0, -24(s0)
    li a1, 1
    call field
    mv t0, a0
    sd t0, -48(s0)
    ld t0, -48(s0)
    sd t0, -56(s0)
    ld a0, -24(s0)
    li a1, 2
    call field
    mv t0, a0
    sd t0, -64(s0)
    ld t0, -64(s0)
    sd t0, -72(s0)
    ld a0, -24(s0)
    li a1, 3
    call field
    mv t0, a0
    sd t0, -80(s0)
    ld t0, -80(s0)
    sd t0, -88(s0)
    ld a0, -24(s0)
    li a1, 4
    call field
    mv t0, a0
    sd t0, -96(s0)
    ld t0, -96(s0)
    sd t0, -104(s0)
    ld a0, -24(s0)
    li a1, 5
    call field
    mv t0, a0
    sd t0, -112(s0)
    ld t0, -112(s0)
    sd t0, -120(s0)
    ld a0, -24(s0)
    li a1, 6
    call field
    mv t0, a0
    sd t0, -128(s0)
    ld t0, -128(s0)
    sd t0, -136(s0)
    ld a0, -24(s0)
    li a1, 7
    call field
    mv t0, a0
    sd t0, -144(s0)
    ld t0, -144(s0)
    sd t0, -152(s0)
    ld a0, -24(s0)
    li a1, 8
    call field
    mv t0, a0
    sd t0, -160(s0)
    ld t0, -160(s0)
    sd t0, -168(s0)
    ld a0, -24(s0)
    li a1, 9
    call field
    mv t0, a0
    sd t0, -176(s0)
    ld t0, -176(s0)
    sd t0, -184(s0)
    ld t0, -184(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call print_int
    mv t0, a0
    sd t0, -192(s0)
    ld a0, -192(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret

  $ riscv64-linux-gnu-as -march=rv64gc tuple_large.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  10

  $ dune exec -- ../bin/XML.exe -o tuple_basic.s -no-opt-peep <<EOF
  > let main =
  >   let t = (10, 20) in
  >   let (a, b) = t in
  >   print_int (a + b)
  $ cat tuple_basic.s
  .section .text
  .global main
  .type main, @function
  main:
    addi sp, sp, -128
    sd ra, 120(sp)
    sd s0, 112(sp)
    addi s0, sp, 112
    li a0, 5120
    call rt_init
    addi sp, sp, -8
    addi sp, sp, -8
    li a0, 2
    call create_tuple
    addi sp, sp, 8
    sd a0, 0(sp)
    li t1, 21
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 0(t2)
    li t1, 41
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 8(t2)
    ld t0, 0(sp)
    addi sp, sp, 8
    sd t0, -8(s0)
    ld t0, -8(s0)
    sd t0, -16(s0)
    ld t0, -16(s0)
    sd t0, -24(s0)
    ld a0, -24(s0)
    li a1, 0
    call field
    mv t0, a0
    sd t0, -32(s0)
    ld t0, -32(s0)
    sd t0, -40(s0)
    ld a0, -24(s0)
    li a1, 1
    call field
    mv t0, a0
    sd t0, -48(s0)
    ld t0, -48(s0)
    sd t0, -56(s0)
    ld t0, -40(s0)
    ld t1, -56(s0)
    add t0, t0, t1
    addi t0, t0, -1
    sd t0, -64(s0)
    ld t0, -64(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call print_int
    mv t0, a0
    sd t0, -72(s0)
    ld a0, -72(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret

  $ riscv64-linux-gnu-as -march=rv64gc tuple_basic.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  30

  $ dune exec -- ../bin/XML.exe -o tuple_nested.s -no-opt-peep <<EOF
  > let main =
  >   let complex = (100, (20, 3)) in
  >   let (a, (b, c)) = complex in
  >   print_int (a + b + c)
  $ cat tuple_nested.s
  .section .text
  .global main
  .type main, @function
  main:
    addi sp, sp, -168
    sd ra, 160(sp)
    sd s0, 152(sp)
    addi s0, sp, 152
    li a0, 5120
    call rt_init
    addi sp, sp, -8
    addi sp, sp, -8
    li a0, 2
    call create_tuple
    addi sp, sp, 8
    sd a0, 0(sp)
    li t1, 41
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 0(t2)
    li t1, 7
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 8(t2)
    ld t0, 0(sp)
    addi sp, sp, 8
    sd t0, -8(s0)
    addi sp, sp, -8
    addi sp, sp, -8
    li a0, 2
    call create_tuple
    addi sp, sp, 8
    sd a0, 0(sp)
    li t1, 201
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 0(t2)
    ld t1, -8(s0)
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 8(t2)
    ld t0, 0(sp)
    addi sp, sp, 8
    sd t0, -16(s0)
    ld t0, -16(s0)
    sd t0, -24(s0)
    ld t0, -24(s0)
    sd t0, -32(s0)
    ld a0, -32(s0)
    li a1, 0
    call field
    mv t0, a0
    sd t0, -40(s0)
    ld t0, -40(s0)
    sd t0, -48(s0)
    ld a0, -32(s0)
    li a1, 1
    call field
    mv t0, a0
    sd t0, -56(s0)
    ld a0, -56(s0)
    li a1, 0
    call field
    mv t0, a0
    sd t0, -64(s0)
    ld t0, -64(s0)
    sd t0, -72(s0)
    ld a0, -56(s0)
    li a1, 1
    call field
    mv t0, a0
    sd t0, -80(s0)
    ld t0, -80(s0)
    sd t0, -88(s0)
    ld t0, -48(s0)
    ld t1, -72(s0)
    add t0, t0, t1
    addi t0, t0, -1
    sd t0, -96(s0)
    ld t0, -96(s0)
    ld t1, -88(s0)
    add t0, t0, t1
    addi t0, t0, -1
    sd t0, -104(s0)
    ld t0, -104(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call print_int
    mv t0, a0
    sd t0, -112(s0)
    ld a0, -112(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret

  $ riscv64-linux-gnu-as -march=rv64gc tuple_nested.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  123

  $ dune exec -- ../bin/XML.exe -o tuple_arg.s -no-opt-peep <<EOF
  > let sum_pair p =
  >   let (x, y) = p in
  >   x + y
  > 
  > let main =
  >   let p = (40, 2) in
  >   print_int (sum_pair p)
  $ cat tuple_arg.s
  .section .text
  .global main
  .type main, @function
  sum_pair:
    addi sp, sp, -104
    sd ra, 96(sp)
    sd s0, 88(sp)
    addi s0, sp, 88
    mv t0, a0
    sd t0, -8(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    ld a0, -8(s0)
    li a1, 0
    call field
    addi sp, sp, 8
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -16(s0)
    ld t0, -16(s0)
    sd t0, -24(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    ld a0, -8(s0)
    li a1, 1
    call field
    addi sp, sp, 8
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -32(s0)
    ld t0, -32(s0)
    sd t0, -40(s0)
    ld t0, -24(s0)
    ld t1, -40(s0)
    add t0, t0, t1
    addi t0, t0, -1
    sd t0, -48(s0)
    ld a0, -48(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -88
    sd ra, 80(sp)
    sd s0, 72(sp)
    addi s0, sp, 72
    li a0, 5120
    call rt_init
    addi sp, sp, -8
    addi sp, sp, -8
    li a0, 2
    call create_tuple
    addi sp, sp, 8
    sd a0, 0(sp)
    li t1, 81
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 0(t2)
    li t1, 5
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 8(t2)
    ld t0, 0(sp)
    addi sp, sp, 8
    sd t0, -8(s0)
    ld t0, -8(s0)
    sd t0, -16(s0)
    ld t0, -16(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call sum_pair
    mv t0, a0
    sd t0, -24(s0)
    ld t0, -24(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call print_int
    mv t0, a0
    sd t0, -32(s0)
    ld a0, -32(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret

  $ riscv64-linux-gnu-as -march=rv64gc tuple_arg.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  42

  $ dune exec -- ../bin/XML.exe -o tuple_gc_stress.s -notypes -no-opt-peep <<EOF
  > let rec make_list n acc =
  >   if n = 0 then acc else
  >   make_list (n - 1) (n, acc)
  > 
  > let main =
  >   let _ = print_gc_status in
  >   let result = make_list 10000 0 in
  >   let (head, tail) = result in
  >   let _ = print_gc_status in
  >   print_int head
  $ cat tuple_gc_stress.s
  .section .text
  .global main
  .type main, @function
  make_list:
    addi sp, sp, -96
    sd ra, 88(sp)
    sd s0, 80(sp)
    addi s0, sp, 80
    mv t0, a0
    li t1, 1
    xor t2, t0, t1
    seqz t0, t2
    sd t0, -8(s0)
    ld t0, -8(s0)
    beq t0, zero, else_0
    mv t0, a1
    j endif_1
  else_0:
    mv t0, a0
    li t1, 3
    sub t0, t0, t1
    addi t0, t0, 1
    sd t0, -16(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    ld t1, -16(s0)
    sd t1, 0(sp)
    la a0, make_list
    li a1, 2
    call alloc_closure
    mv t0, a0
    ld t1, 0(sp)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -24(s0)
    addi sp, sp, -8
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    li a0, 2
    call create_tuple
    addi sp, sp, 8
    sd a0, 16(sp)
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    mv t1, a0
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 0(t2)
    mv t1, a1
    ld t2, 0(sp)
    addi t2, t2, 16
    sd t1, 8(t2)
    ld t0, 0(sp)
    addi sp, sp, 8
    sd t0, -32(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    ld t0, -24(s0)
    ld t1, -32(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -40(s0)
    ld t0, -40(s0)
  endif_1:
    sd t0, -48(s0)
    ld a0, -48(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -144
    sd ra, 136(sp)
    sd s0, 128(sp)
    addi s0, sp, 128
    li a0, 5120
    call rt_init
    call print_gc_status
    mv t0, a0
    sd t0, -8(s0)
    addi sp, sp, -8
    li t1, 20001
    sd t1, 0(sp)
    la a0, make_list
    li a1, 2
    call alloc_closure
    mv t0, a0
    ld t1, 0(sp)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    addi sp, sp, 8
    sd t0, -16(s0)
    ld t0, -16(s0)
    li t1, 1
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -24(s0)
    ld t0, -24(s0)
    sd t0, -32(s0)
    ld t0, -32(s0)
    sd t0, -40(s0)
    ld a0, -40(s0)
    li a1, 0
    call field
    mv t0, a0
    sd t0, -48(s0)
    ld t0, -48(s0)
    sd t0, -56(s0)
    ld a0, -40(s0)
    li a1, 1
    call field
    mv t0, a0
    sd t0, -64(s0)
    ld t0, -64(s0)
    sd t0, -72(s0)
    call print_gc_status
    mv t0, a0
    sd t0, -80(s0)
    ld t0, -56(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    addi sp, sp, 8
    call print_int
    mv t0, a0
    sd t0, -88(s0)
    ld a0, -88(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  $ riscv64-linux-gnu-as -march=rv64gc tuple_gc_stress.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  === GC Status ===
  Current allocated: 0
  Free        space: 524288
  Heap         size: 524288
  Current      bank: 0
  Total   allocated: 0
  GC    collections: 0
  GC    allocations: 0
  =================
  === GC Status ===
  Current allocated: 231552
  Free        space: 292736
  Heap         size: 524288
  Current      bank: 0
  Total   allocated: 1280096
  GC    collections: 2
  GC    allocations: 30002
  =================
  1

