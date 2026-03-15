  $ dune exec Dooml-Llvm -- closure.ml closure.o
  $ riscv64-linux-gnu-gcc -static -o closure.out closure.o -L../lib/ -l:riscv64-gc-runtime.a
  $ qemu-riscv64 closure.out
  6
  8
  13
  14
  21
  [3]

  $ dune exec Dooml-Llvm -- fac.ml fac.o
  $ riscv64-linux-gnu-gcc -static -o fac.out fac.o -L../lib/ -l:riscv64-gc-runtime.a
  $ qemu-riscv64 fac.out
  1
  2
  120
  40320
  [6]

  $ dune exec Dooml-Llvm -- fib-cps.ml fib-cps.o
  $ riscv64-linux-gnu-gcc -static -o fib-cps.out fib-cps.o -L../lib/ -l:riscv64-gc-runtime.a
  $ qemu-riscv64 fib-cps.out
  1
  2
  8
  34
  [3]
