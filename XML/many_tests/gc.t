  $ dune exec ./../bin/XML.exe -- -o gc_smoke.s <<EOF
  > let main =
  >   let _ = print_gc_status in
  >   let _ = collect in
  >   print_gc_status
  $ riscv64-linux-gnu-as -march=rv64gc gc_smoke.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  === GC Status ===
  Current allocated: 0
  Free        space: 524288
  Heap         size: 524288
  Current      bank: 0
  Total   allocated: 0
  GC    collections: 0
  GC    allocations: 0
  =================
  === GC Status ===
  Current allocated: 0
  Free        space: 524288
  Heap         size: 524288
  Current      bank: 1
  Total   allocated: 0
  GC    collections: 1
  GC    allocations: 0
  =================

  $ dune exec ./../bin/XML.exe -- -o lots_of_garbage.s <<EOF
  > let rec make_garbage n =
  >   if n = 0 then 0 else
  >     let _ = alloc_block 1 in
  >     make_garbage (n - 1)
  > let main =
  >   let _ = print_gc_status in
  >   let f = (fun x -> x) in
  >   let _ = make_garbage 2000 in
  >   let _ = print_gc_status in
  >   let _ = collect in
  >   let _ = print_gc_status in
  >   print_int (f 8)

  $ riscv64-linux-gnu-as -march=rv64gc lots_of_garbage.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  === GC Status ===
  Current allocated: 0
  Free        space: 524288
  Heap         size: 524288
  Current      bank: 0
  Total   allocated: 0
  GC    collections: 0
  GC    allocations: 0
  =================
  === GC Status ===
  Current allocated: 80000
  Free        space: 444288
  Heap         size: 524288
  Current      bank: 0
  Total   allocated: 80000
  GC    collections: 0
  GC    allocations: 2000
  =================
  === GC Status ===
  Current allocated: 0
  Free        space: 524288
  Heap         size: 524288
  Current      bank: 1
  Total   allocated: 80000
  GC    collections: 1
  GC    allocations: 2000
  =================
  8



  $ dune exec ./../bin/XML.exe -- -o keep_block_across_gc.s <<EOF
  > let rec spam n =
  >   if n = 0 then 0 else
  >     let _ = (fun z -> z) in
  >     spam (n - 1)
  > 
  > let make n =
  >   let b = alloc_block n in
  >   (fun x -> let _u = b in x)
  > 
  > let main =
  >   let f = make 10 in
  >   let _ = spam 4000 in
  >   let _ = collect in
  >   print_int (f 7)
  $ riscv64-linux-gnu-as -march=rv64gc keep_block_across_gc.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  7


  $ dune exec ./../bin/XML.exe -- -o gc_oom_block.s <<EOF
  > let main =
  >   let _ = alloc_block 10000000 in
  >   print_int 0

  $ riscv64-linux-gnu-as -march=rv64gc gc_oom_block.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe || true
  GC: out of memory
  Aborted (core dumped)
