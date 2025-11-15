Copyright 2025-2026, Friend-zva, RodionovMaxim05
SPDX-License-Identifier: LGPL-3.0-or-later

====================== without gc ======================
  $ ../bin/akaML.exe -fromfile manytests/typed/001fac.ml -o 001fac.s
  $ riscv64-linux-gnu-as -march=rv64gc 001fac.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_runtime.a -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  24

  $ ../bin/akaML.exe -fromfile manytests/typed/004manyargs.ml -o 004manyargs.s
  $ riscv64-linux-gnu-as -march=rv64gc 004manyargs.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_runtime.a -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  1111111111110100

  $ ../bin/akaML.exe -fromfile manytests/typed/010faccps_ll.ml -o 010faccps_ll.s
  $ riscv64-linux-gnu-as -march=rv64gc 010faccps_ll.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_runtime.a -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  24

  $ ../bin/akaML.exe -fromfile manytests/typed/010fibcps_ll.ml -o 010fibcps_ll.s
  $ riscv64-linux-gnu-as -march=rv64gc 010fibcps_ll.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_runtime.a -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  8

  $ ../bin/akaML.exe -fromfile manytests/typed/012faccps.ml -o 012faccps.s
  $ riscv64-linux-gnu-as -march=rv64gc 012faccps.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_runtime.a -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  720

  $ ../bin/akaML.exe -fromfile manytests/typed/012fibcps.ml -o 012fibcps.s
  $ riscv64-linux-gnu-as -march=rv64gc 012fibcps.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_runtime.a -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  8

====================== gc ======================
  $ ../bin/akaML.exe -gc -fromfile manytests/typed/001fac.ml -o 001fac.s
  $ riscv64-linux-gnu-as -march=rv64gc 001fac.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_gc_runtime.a -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  24

  $ ../bin/akaML.exe -gc -fromfile manytests/typed/004manyargs.ml -o 004manyargs.s
  $ riscv64-linux-gnu-as -march=rv64gc 004manyargs.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_gc_runtime.a -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  1111111111110100

  $ ../bin/akaML.exe -gc -fromfile manytests/typed/010faccps_ll.ml -o 010faccps_ll.s
  $ riscv64-linux-gnu-as -march=rv64gc 010faccps_ll.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_gc_runtime.a -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  24

  $ ../bin/akaML.exe -gc -fromfile manytests/typed/010fibcps_ll.ml -o 010fibcps_ll.s
  $ riscv64-linux-gnu-as -march=rv64gc 010fibcps_ll.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_gc_runtime.a -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  8

  $ ../bin/akaML.exe -gc -fromfile manytests/typed/012faccps.ml -o 012faccps.s
  $ riscv64-linux-gnu-as -march=rv64gc 012faccps.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_gc_runtime.a -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  720

  $ ../bin/akaML.exe -gc -fromfile manytests/typed/012fibcps.ml -o 012fibcps.s
  $ riscv64-linux-gnu-as -march=rv64gc 012fibcps.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_gc_runtime.a -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  8

  $ ../bin/akaML.exe -gc -fromfile fewtests/closure/01faccps.ml -o 01faccps.s
  $ riscv64-linux-gnu-as -march=rv64gc 01faccps.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_gc_runtime.a -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  === GC Status ===
  Current allocated: 199
  Free        space: 1601
  Heap         size: 1800
  Current      bank: 0
  Total   allocated: 199
  GC    collections: 0
  GC    allocations: 14
  =================
  === GC Status ===
  Current allocated: 5
  Free        space: 1795
  Heap         size: 1800
  Current      bank: 1
  Total   allocated: 199
  GC    collections: 1
  GC    allocations: 14
  =================
  24

  $ ../bin/akaML.exe -gc -fromfile fewtests/closure/02fac.ml -o 02fac.s
  $ riscv64-linux-gnu-as -march=rv64gc 02fac.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_gc_runtime.a -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  === GC Status ===
  Current allocated: 6
  Free        space: 1794
  Heap         size: 1800
  Current      bank: 0
  Total   allocated: 6
  GC    collections: 0
  GC    allocations: 1
  =================
  === GC Status ===
  Current allocated: 6
  Free        space: 1794
  Heap         size: 1800
  Current      bank: 1
  Total   allocated: 6
  GC    collections: 1
  GC    allocations: 1
  =================
  48=== GC Status ===
  Current allocated: 11
  Free        space: 1789
  Heap         size: 1800
  Current      bank: 0
  Total   allocated: 16
  GC    collections: 2
  GC    allocations: 4
  =================

  $ ../bin/akaML.exe -gc -fromfile fewtests/closure/03adder.ml -o 03adder.s
  $ riscv64-linux-gnu-as -march=rv64gc 03adder.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_gc_runtime.a -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  === GC Status ===
  Current allocated: 7
  Free        space: 1793
  Heap         size: 1800
  Current      bank: 0
  Total   allocated: 7
  GC    collections: 0
  GC    allocations: 1
  =================
  === GC Status ===
  Current allocated: 7
  Free        space: 1793
  Heap         size: 1800
  Current      bank: 1
  Total   allocated: 7
  GC    collections: 1
  GC    allocations: 1
  =================
  === GC Status ===
  Current allocated: 7
  Free        space: 1793
  Heap         size: 1800
  Current      bank: 0
  Total   allocated: 11
  GC    collections: 2
  GC    allocations: 2
  =================

  $ ../bin/akaML.exe -gc -fromfile fewtests/closure/04plusss.ml -o 04plusss.s
  $ riscv64-linux-gnu-as -march=rv64gc 04plusss.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_gc_runtime.a -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  === GC Status ===
  Current allocated: 80
  Free        space: 1720
  Heap         size: 1800
  Current      bank: 0
  Total   allocated: 80
  GC    collections: 0
  GC    allocations: 3
  =================
  === GC Status ===
  Current allocated: 151
  Free        space: 1649
  Heap         size: 1800
  Current      bank: 0
  Total   allocated: 151
  GC    collections: 0
  GC    allocations: 5
  =================
  === GC Status ===
  Current allocated: 130
  Free        space: 1670
  Heap         size: 1800
  Current      bank: 1
  Total   allocated: 151
  GC    collections: 1
  GC    allocations: 5
  =================
  === GC Status ===
  Current allocated: 130
  Free        space: 1670
  Heap         size: 1800
  Current      bank: 0
  Total   allocated: 157
  GC    collections: 2
  GC    allocations: 6
  =================
  15

  $ ../bin/akaML.exe -gc -fromfile fewtests/closure/05fibcps.ml -o 05fibcps.s
  $ riscv64-linux-gnu-as -march=rv64gc 05fibcps.s -o temp.o
  $ riscv64-linux-gnu-gcc temp.o ../lib/runtime/rv64_gc_runtime.a -o file.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./file.exe
  8=== GC Status ===
  Current allocated: 1543
  Free        space: 257
  Heap         size: 1800
  Current      bank: 0
  Total   allocated: 1543
  GC    collections: 0
  GC    allocations: 98
  =================
  8=== GC Status ===
  Current allocated: 1617
  Free        space: 183
  Heap         size: 1800
  Current      bank: 1
  Total   allocated: 3086
  GC    collections: 1
  GC    allocations: 196
  =================
  === GC Status ===
  Current allocated: 10
  Free        space: 1790
  Heap         size: 1800
  Current      bank: 0
  Total   allocated: 3086
  GC    collections: 2
  GC    allocations: 196
  =================
