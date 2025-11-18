#!/usr/bin/env bash
set -euo pipefail

SRC="$1"
GC_FLAG="${2:-}"      # optional second argument
HEAP_SIZE="${3:-}"    # optional third argument (only relevant for GC)

CFLAGS=""
if [ "${GC_FLAG}" = "--gc" ]; then
#   echo "[build_and_run] GC enabled"
  CFLAGS="-DENABLE_GC"
  
  # optionally override HEAP_INIT_SIZE
  if [ -n "$HEAP_SIZE" ]; then
    CFLAGS="$CFLAGS -DHEAP_SIZE=$HEAP_SIZE"
    # echo "[build_and_run] HEAP_INIT_SIZE=$HEAP_SIZE"
  fi
else
#   echo "[build_and_run] GC disabled"
  CFLAGS=""
fi

COMPILER="../../../bin/omlet.exe"
RUNTIME="../../../lib/runtime.c"
GC="../../../lib/gc.c"
CALLF="../../../lib/callf.s"

# Assemble code from COMPILER
"$COMPILER" -fromfile "$SRC" | riscv64-linux-gnu-as -march=rv64gc -o temp.o -

# Compile runtime pieces
riscv64-linux-gnu-gcc $CFLAGS "$RUNTIME" -c -o runtime.o
riscv64-linux-gnu-gcc $CFLAGS "$GC" -c -o gc.o
riscv64-linux-gnu-gcc $CFLAGS "$CALLF" -c -o callf.o

# Link everything
riscv64-linux-gnu-gcc temp.o runtime.o gc.o callf.o -nostartfiles -o binary.exe

# Run under QEMU
qemu-riscv64 -L /usr/riscv64-linux-gnu binary.exe
EXIT_CODE=$?

# Clean up build artifacts
rm -f temp.o runtime.o gc.o callf.o binary.exe

exit $EXIT_CODE
