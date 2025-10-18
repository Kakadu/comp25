  $ dune exec ./../bin/XML.exe -- -o factorial.s <<EOF
  > let rec fac n = if n = 0 then 1 else n * fac (n - 1)
  > 
  > let main = print_int (fac 4)

  $ cat factorial.s
  .section .text
  .global main
  .type main, @function
  fac:
    addi sp, sp, -48
    sd ra, 40(sp)
    sd s0, 32(sp)
    addi s0, sp, 32
    mv t0, a0
    li t1, 0
    xor t0, t0, t1
    seqz t0, t0
    sd t0, -8(s0)
    ld t0, -8(s0)
    beq t0, zero, else_0
    li t0, 1
    j endif_1
  else_0:
    mv t0, a0
    li t1, 1
    sub t0, t0, t1
    sd t0, -16(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld a0, -16(s0)
    call fac
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -24(s0)
    mv t0, a0
    ld t1, -24(s0)
    mul t0, t0, t1
    sd t0, -32(s0)
    ld t0, -32(s0)
  endif_1:
    sd t0, -16(s0)
    ld a0, -16(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -32
    sd ra, 24(sp)
    sd s0, 16(sp)
    addi s0, sp, 16
    li a0, 4
    call fac
    mv t0, a0
    sd t0, -8(s0)
    ld a0, -8(s0)
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
  $ ../bin/XML.exe -o fibonacci.s <<EOF
  > let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)
  > 
  > let main = print_int (fib 6)

  $ cat fibonacci.s
  .section .text
  .global main
  .type main, @function
  fib:
    addi sp, sp, -64
    sd ra, 56(sp)
    sd s0, 48(sp)
    addi s0, sp, 48
    mv t0, a0
    li t1, 1
    slt t0, t1, t0
    xori t0, t0, 1
    sd t0, -8(s0)
    ld t0, -8(s0)
    beq t0, zero, else_0
    mv t0, a0
    j endif_1
  else_0:
    mv t0, a0
    li t1, 1
    sub t0, t0, t1
    sd t0, -16(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld a0, -16(s0)
    call fib
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -24(s0)
    mv t0, a0
    li t1, 2
    sub t0, t0, t1
    sd t0, -32(s0)
    addi sp, sp, -8
    sd a0, 0(sp)
    ld a0, -32(s0)
    call fib
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -40(s0)
    ld t0, -24(s0)
    ld t1, -40(s0)
    add t0, t0, t1
    sd t0, -48(s0)
    ld t0, -48(s0)
  endif_1:
    sd t0, -16(s0)
    ld a0, -16(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -32
    sd ra, 24(sp)
    sd s0, 16(sp)
    addi s0, sp, 16
    li a0, 6
    call fib
    mv t0, a0
    sd t0, -8(s0)
    ld a0, -8(s0)
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
    addi sp, sp, -32
    sd ra, 24(sp)
    sd s0, 16(sp)
    addi s0, sp, 16
    li t0, 0
    mv t1, a0
    xor t2, t0, t1
    snez t0, t2
    sd t0, -8(s0)
    ld t0, -8(s0)
    beq t0, zero, else_0
    addi sp, sp, -8
    sd a0, 0(sp)
    li a0, 0
    call print_int
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -16(s0)
    ld t0, -16(s0)
    j endif_1
  else_0:
    addi sp, sp, -8
    sd a0, 0(sp)
    li a0, 1
    call print_int
    mv t0, a0
    ld a0, 0(sp)
    addi sp, sp, 8
    sd t0, -16(s0)
    ld t0, -16(s0)
  endif_1:
    sd t0, -16(s0)
    ld a0, -16(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  main:
    addi sp, sp, -64
    sd ra, 56(sp)
    sd s0, 48(sp)
    addi s0, sp, 48
    li t0, 0
    li t1, 1
    xor t0, t0, t1
    seqz t0, t0
    sd t0, -8(s0)
    ld t0, -8(s0)
    beq t0, zero, else_2
    li t0, 0
    li t1, 1
    xor t0, t0, t1
    seqz t0, t0
    sd t0, -16(s0)
    ld t0, -16(s0)
    j endif_3
  else_2:
    li a0, 42
    call print_int
    mv t0, a0
    sd t0, -16(s0)
    li t0, 1
    li t1, 1
    xor t0, t0, t1
    seqz t0, t0
    sd t0, -24(s0)
    ld t0, -24(s0)
  endif_3:
    sd t0, -16(s0)
    ld t0, -16(s0)
    beq t0, zero, else_4
    li t0, 0
    j endif_5
  else_4:
    li t0, 1
  endif_5:
    sd t0, -24(s0)
    ld t0, -24(s0)
    li t1, 1
    xor t0, t0, t1
    seqz t0, t0
    sd t0, -32(s0)
    ld t0, -32(s0)
    beq t0, zero, else_6
    li t0, 0
    j endif_7
  else_6:
    li t0, 1
  endif_7:
    sd t0, -40(s0)
    ld a0, -40(s0)
    call large
    mv t0, a0
    sd t0, -48(s0)
    ld a0, -48(s0)
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret

  $ riscv64-linux-gnu-as -march=rv64gc ififif.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  420
