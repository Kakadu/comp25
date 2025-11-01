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
