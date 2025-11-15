  $ dune exec ./../bin/XML.exe -- -o factorial.s <<EOF
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
    ld t0, -16(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
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
    call fac
    mv t0, a0
    addi sp, sp, 8
    sd t0, -8(s0)
    ld t0, -8(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    call print_int
    mv t0, a0
    addi sp, sp, 8
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
  $ ../bin/XML.exe -o fibonacci.s <<EOF
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
    ld t0, -16(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
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
    ld t0, -32(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
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
    call fib
    mv t0, a0
    addi sp, sp, 8
    sd t0, -8(s0)
    ld t0, -8(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    call print_int
    mv t0, a0
    addi sp, sp, 8
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
  $ ../bin/XML.exe -o ififif.s <<EOF
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
    li t0, 1
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
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
    li t0, 3
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
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
    addi sp, sp, -104
    sd ra, 96(sp)
    sd s0, 88(sp)
    addi s0, sp, 88
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
    call print_int
    mv t0, a0
    addi sp, sp, 8
    sd t0, -16(s0)
    li t0, 3
    li t1, 3
    xor t2, t0, t1
    seqz t0, t2
    sd t0, -24(s0)
    ld t0, -24(s0)
  endif_3:
    sd t0, -32(s0)
    ld t0, -32(s0)
    beq t0, zero, else_4
    li t0, 1
    j endif_5
  else_4:
    li t0, 3
  endif_5:
    sd t0, -40(s0)
    ld t0, -40(s0)
    li t1, 3
    xor t2, t0, t1
    seqz t0, t2
    sd t0, -48(s0)
    ld t0, -48(s0)
    beq t0, zero, else_6
    li t0, 1
    j endif_7
  else_6:
    li t0, 3
  endif_7:
    sd t0, -56(s0)
    ld t0, -56(s0)
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    call large
    mv t0, a0
    addi sp, sp, 8
    sd t0, -64(s0)
    ld a0, -64(s0)
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
  $ ../bin/XML.exe -o closure.s <<EOF
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
    call print_int
    mv t0, a0
    addi sp, sp, 8
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
  $ ../bin/XML.exe -fromfile manytests/typed/010faccps_ll.ml -o 010faccps_ll.s

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
    mv t0, a1
    ld t1, -8(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
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
    call print_int
    mv t0, a0
    addi sp, sp, 8
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
  $ ../bin/XML.exe -fromfile manytests/typed/010fibcps_ll.ml -o 010fibcps_ll.s

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
    mv t0, a1
    ld t1, -8(s0)
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
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
    addi sp, sp, -80
    sd ra, 72(sp)
    sd s0, 64(sp)
    addi s0, sp, 64
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
    call print_int
    mv t0, a0
    addi sp, sp, 8
    sd t0, -24(s0)
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

  $ ../bin/XML.exe -fromfile manytests/typed/004manyargs.ml -o 004manyargs.s

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
    addi sp, sp, -80
    sd ra, 72(sp)
    sd s0, 64(sp)
    addi s0, sp, 64
    addi sp, sp, -8
    sd a2, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    addi sp, sp, -8
    sd a0, 0(sp)
    mv t0, a0
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
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
    addi sp, sp, -8
    sd a2, 0(sp)
    addi sp, sp, -8
    sd a1, 0(sp)
    mv t0, a1
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    call print_int
    mv t0, a0
    addi sp, sp, 8
    ld a1, 0(sp)
    addi sp, sp, 8
    ld a2, 0(sp)
    addi sp, sp, 8
    sd t0, -16(s0)
    addi sp, sp, -8
    sd a2, 0(sp)
    mv t0, a2
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    call print_int
    mv t0, a0
    addi sp, sp, 8
    ld a2, 0(sp)
    addi sp, sp, 8
    sd t0, -24(s0)
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
    addi sp, sp, -184
    sd ra, 176(sp)
    sd s0, 168(sp)
    addi s0, sp, 168
    li a0, 5120
    call rt_init
    la a0, test10
    li a1, 10
    call alloc_closure
    mv t0, a0
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    call wrap
    mv t0, a0
    addi sp, sp, 8
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
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    call print_int
    mv t0, a0
    addi sp, sp, 8
    sd t0, -96(s0)
    la a0, test3
    li a1, 3
    call alloc_closure
    mv t0, a0
    addi sp, sp, -8
    sd t0, 0(sp)
    ld a0, 0(sp)
    call wrap
    mv t0, a0
    addi sp, sp, 8
    sd t0, -104(s0)
    ld t0, -104(s0)
    li t1, 3
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -112(s0)
    ld t0, -112(s0)
    li t1, 21
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -120(s0)
    ld t0, -120(s0)
    li t1, 201
    mv a0, t0
    mv a1, t1
    call apply1
    mv t0, a0
    sd t0, -128(s0)
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
