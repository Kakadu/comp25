  $ dune exec Dooml-Riscv -- closure.ml > closure.S
  $ riscv64-linux-gnu-gcc -static -o closure.out closure.S -L../lib/ -l:riscv64-runtime.a
  $ qemu-riscv64 closure.out
  6
  8
  13
  14
  21

  $ dune exec Dooml-Riscv -- fac.ml > fac.S
  $ riscv64-linux-gnu-gcc -static -o fac.out fac.S -L../lib/ -l:riscv64-runtime.a
  $ qemu-riscv64 fac.out
  1
  2
  120
  40320

  $ dune exec Dooml-Riscv -- fib-cps.ml > fib-cps.S
  $ riscv64-linux-gnu-gcc -static -o fib-cps.out fib-cps.S -L../lib/ -l:riscv64-runtime.a
  $ qemu-riscv64 fib-cps.out
  1
  2
  8
  34

