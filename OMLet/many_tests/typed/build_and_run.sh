#!/bin/bash

SRC="$1"
REPL="../../../repl/repl.exe"
RUNTIME="../../../lib/runtime.c"

"$REPL" -fromfile "$SRC" | riscv64-linux-gnu-as -march=rv64gc -o temp.o -
riscv64-linux-gnu-gcc "$RUNTIME" -c -o runtime.o
riscv64-linux-gnu-gcc temp.o runtime.o -nostartfiles -o factorial.exe
qemu-riscv64 -L /usr/riscv64-linux-gnu factorial.exe

EXIT_CODE=$?

rm -f temp.o runtime.o factorial.exe

exit $EXIT_CODE
