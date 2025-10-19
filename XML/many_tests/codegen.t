  $ ../bin/XML.exe -o factorial.s <<EOF
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
    addi s0, sp, 24 
    mv t0, a0
    li t1, 0
    mv a1, a0
    xor a0, t0, t1
    seqz a0, a0
    sd a0, -8(s0) 
    ld t0, -8(s0)
    beq t0, zero, else_0
    li a0, 1
    j end_0
  else_0:
    mv t0, a1
    li t1, 1
    sub a0, t0, t1
    sd a0, -16(s0) 
    ld a0, -16(s0)
    addi sp, sp, -8 
    sd a1, -24(s0)
    call fac
    sd a0, -32(s0) 
    ld t0, -24(s0)
    ld t1, -32(s0)
    mul a0, t0, t1
  end_0:
    addi sp, s0, 16 
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  
    .globl main
    .type main, @function
  main:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8 
    li a0, 4
    call fac
    sd a0, -8(s0) 
    ld a0, -8(s0)
    call print_int
    addi sp, s0, 16 
    ld ra, 8(s0)
    ld s0, 0(s0)
    li a0, 0
    ret
  
  $ riscv64-linux-gnu-as -march=rv64gc factorial.s -o temp.o
  $ riscv64-linux-gnu-gcc -c bin/runtime.c -o runtime.o
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
    .globl fib
    .type fib, @function
  fib:
    addi sp, sp, -56
    sd ra, 48(sp)
    sd s0, 40(sp)
    addi s0, sp, 40 
    mv t0, a0
    li t1, 1
    mv a1, a0
    slt a0, t1, t0
    xori a0, a0, 1
    sd a0, -8(s0) 
    ld t0, -8(s0)
    beq t0, zero, else_0
    mv a0, a1
    j end_0
  else_0:
    mv t0, a1
    li t1, 1
    sub a0, t0, t1
    sd a0, -16(s0) 
    ld a0, -16(s0)
    addi sp, sp, -8 
    sd a1, -24(s0)
    call fib
    sd a0, -32(s0) 
    ld t0, -24(s0)
    li t1, 2
    sub a0, t0, t1
    sd a0, -40(s0) 
    ld a0, -40(s0)
    call fib
    sd a0, -48(s0) 
    ld t0, -32(s0)
    ld t1, -48(s0)
    add  a0, t0, t1
  end_0:
    addi sp, s0, 16 
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  
    .globl main
    .type main, @function
  main:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8 
    li a0, 6
    call fib
    sd a0, -8(s0) # temp8
    ld a0, -8(s0)
    call print_int
    addi sp, s0, 16 
    ld ra, 8(s0)
    ld s0, 0(s0)
    li a0, 0
    ret
  
  $ riscv64-linux-gnu-as -march=rv64gc fibonacci.s -o temp.o
  $ riscv64-linux-gnu-gcc -c bin/runtime.c -o runtime.o
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
    .globl large
    .type large, @function
  large:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd s0, 8(sp)
    addi s0, sp, 8
    li t0, 0
    mv t1, a0
    mv a1, a0
    xor a0, t0, t1
    snez a0, a0
    sd a0, -8(s0) 
    ld t0, -8(s0)
    beq t0, zero, else_0
    li a0, 0
    addi sp, sp, -8
    sd a1, -16(s0)
    call print_int
    j end_0
  else_0:
    li a0, 1
    call print_int
  end_0:
    addi sp, s0, 16 
    ld ra, 8(s0)
    ld s0, 0(s0)
    ret
  
    .globl main
    .type main, @function
  main:
    addi sp, sp, -64
    sd ra, 56(sp)
    sd s0, 48(sp)
    addi s0, sp, 48 
    li t0, 0
    li t1, 1
    xor a0, t0, t1
    seqz a0, a0
    sd a0, -8(s0) 
    ld t0, -8(s0)
    beq t0, zero, else_1
    li t0, 0
    li t1, 1
    xor a0, t0, t1
    seqz a0, a0
    j end_1
  else_1:
    li a0, 42
    call print_int
    sd a0, -16(s0) # t42
    li t0, 1
    li t1, 1
    xor a0, t0, t1
    seqz a0, a0
  end_1:
    sd a0, -24(s0) 
    ld t0, -24(s0)
    beq t0, zero, else_2
    li a0, 0
    j end_2
  else_2:
    li a0, 1
  end_2:
    sd a0, -32(s0) 0
    ld t0, -32(s0)
    li t1, 1
    xor a0, t0, t1
    seqz a0, a0
    sd a0, -40(s0) 1
    ld t0, -40(s0)
    beq t0, zero, else_3
    li a0, 0
    j end_3
  else_3:
    li a0, 1
  end_3:
    sd a0, -48(s0) # x
    ld a0, -48(s0)
    call large
    addi sp, s0, 16
    ld ra, 8(s0)
    ld s0, 0(s0)
    li a0, 0
    ret
  

  $ riscv64-linux-gnu-as -march=rv64gc ififif.s -o temp.o
  $ riscv64-linux-gnu-gcc -c bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  420
