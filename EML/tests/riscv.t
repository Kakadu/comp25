Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev
SPDX-License-Identifier: LGPL-3.0-or-later

  $ EML -o fact.s <<EOF
  > let rec fac x = if x = 0 then 1 else x * fac (x - 1)
  > 
  > let main = print_int (fac 4)

  $ riscv64-linux-gnu-as -march=rv64gc fact.s -o fact.o
  $ riscv64-linux-gnu-gcc fact.o ../lib/runtime/rv64_runtime.a -o fact.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./fact.exe
  24

  $ EML -o fibo.s <<EOF
  > let rec fib x = if x <= 1 then x else fib (x - 1) + fib (x - 2)
  > 
  > let main = print_int (fib 6)

  $ riscv64-linux-gnu-as -march=rv64gc fibo.s -o fibo.o
  $ riscv64-linux-gnu-gcc fibo.o ../lib/runtime/rv64_runtime.a -o fibo.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./fibo.exe
  8
