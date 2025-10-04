Copyright 2025, Sofya Kozyreva, Maksim Shipilov
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ./compile.exe | riscv64-linux-gnu-as -march=rv64gc -o temp.o -
  $ riscv64-linux-gnu-ld temp.o -o factorial.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu factorial.exe
  [24]
