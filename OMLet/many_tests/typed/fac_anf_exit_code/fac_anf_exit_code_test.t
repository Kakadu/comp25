Copyright 2025, Ksenia Kotelnikova, Sofya Kozyreva, Vyacheslav Kochergin
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../../../repl/repl.exe -fromfile fac_anf_exit_code.ml | riscv64-linux-gnu-as -march=rv64gc -o temp.o -
  $ riscv64-linux-gnu-gcc ../../../lib/runtime.c -c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -nostartfiles -o factorial.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu factorial.exe
  [24]
