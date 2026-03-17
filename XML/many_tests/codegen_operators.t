llvm and riscv tests to check if operators are compiled correctly
\
  $ clang-18 --target=riscv64-linux-gnu --sysroot=/usr/riscv64-unknown-linux-gnu -c ./../bin/runtime.c -o runtime.o

====================== Llvm ======================

====================== Custom operators used as functions ======================

  $ dune exec ../bin/XML_llvm.exe -- -o oper_custom1.ll <<EOF
  > let (@) f x = f x in
  > print_int ((@) (fun x -> x) 5)

  $ llc-18 oper_custom1.ll -o temp.s
  $ clang-18  --target=riscv64-linux-gnu -static temp.s runtime.o -o temp.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./temp.exe
  5


  $ dune exec ../bin/XML_llvm.exe -- -o oper_custom3.ll <<EOF
  > let (@) f x = f x;;
  > 
  > print_int ((@) (fun x -> x) 5)

  $ llc-18 oper_custom3.ll -o temp.s
  $ clang-18  --target=riscv64-linux-gnu -static temp.s runtime.o -o temp.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./temp.exe
  5

====================== Custom operators used infixly ======================


  $ dune exec ../bin/XML_llvm.exe -- -o oper_custom2.ll <<EOF
  > let (@) f x = f x in
  > print_int ((fun x -> x) @ 5)

  $ llc-18 oper_custom2.ll -o temp.s
  $ clang-18  --target=riscv64-linux-gnu -static temp.s runtime.o -o temp.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./temp.exe
  5


  $ dune exec ../bin/XML_llvm.exe -- -o oper_custom4.ll <<EOF
  > let (@) f x = f x;;
  > 
  > print_int ((fun x -> x) @ 5)

  $ llc-18 oper_custom4.ll -o temp.s
  $ clang-18  --target=riscv64-linux-gnu -static temp.s runtime.o -o temp.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./temp.exe
  5

====================== Renaming ======================

  $ dune exec ../bin/XML_llvm.exe -- -o renaming1.ll <<EOF
  > let (+) x y = x - y
  > let main =
  >  print_int (10 + 5)

  $ llc-18 renaming1.ll -o temp.s
  $ clang-18  --target=riscv64-linux-gnu -static temp.s runtime.o -o temp.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./temp.exe
  5


  $ dune exec ../bin/XML_llvm.exe -- -o renaming2.ll <<EOF
  > let g x y = x + y
  > let (+) x y = x - y
  > let main =
  >  let _ = print_int (g 10 5) in
  >  print_int (10 + 5)

  $ llc-18 renaming2.ll -o temp.s
  $ clang-18  --target=riscv64-linux-gnu -static temp.s runtime.o -o temp.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./temp.exe
  15
  5

====================== Associativity ======================

  $ dune exec ../bin/XML_llvm.exe -- -o associativity.ll <<EOF
  > let (+>) x y = x + y;;
  > let (***) x y = x * y;;
  > let (=&) x y = x - y;;
  > let main =
  >   print_int (2 +> 2 *** 2 =& 3)

  $ llc-18 associativity.ll -o temp.s
  $ clang-18  --target=riscv64-linux-gnu -static temp.s runtime.o -o temp.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./temp.exe
  3


====================== RISC-V ======================

====================== Custom operators used as functions ======================

  $ dune exec ../bin/XML.exe -- -o oper_custom1.s <<EOF
  > let (@) f x = f x in
  > print_int ((@) (fun x -> x) 5)

  $ riscv64-linux-gnu-as -march=rv64gc oper_custom1.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  5



  $ dune exec ../bin/XML.exe -- -o oper_custom3.s <<EOF
  > let (@) f x = f x;;
  > 
  > print_int ((@) (fun x -> x) 5)

  $ riscv64-linux-gnu-as -march=rv64gc oper_custom3.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  5


====================== Custom operators used infixly ======================

  $ dune exec ../bin/XML.exe -- -o oper_custom2.s <<EOF
  > let (@) f x = f x in
  > print_int ((fun x -> x) @ 5)


  $ riscv64-linux-gnu-as -march=rv64gc oper_custom2.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  5



  $ dune exec ../bin/XML.exe -- -o oper_custom4.s <<EOF
  > let (@) f x = f x;;
  > 
  > print_int ((fun x -> x) @ 5)

  $ riscv64-linux-gnu-as -march=rv64gc oper_custom4.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  5

====================== Renaming ======================

  $ dune exec ../bin/XML.exe -- -o renaming1.s <<EOF
  > let (+) x y = x - y
  > let main =
  >  print_int (10 + 5)

  $ riscv64-linux-gnu-as -march=rv64gc renaming1.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  5



  $ dune exec ../bin/XML.exe -- -o renaming2.s <<EOF
  > let g x y = x + y
  > let (+) x y = x - y
  > let main =
  >  let _ = print_int (g 10 5) in
  >  print_int (10 + 5)

  $ riscv64-linux-gnu-as -march=rv64gc renaming2.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  15
  5

====================== Associativity ======================

  $ dune exec ../bin/XML.exe -- -o associativity.s <<EOF
  > let (+>) x y = x + y;;
  > let (***) x y = x * y;;
  > let (=&) x y = x - y;;
  > let main =
  >   print_int (2 +> 2 *** 2 =& 3)

  $ riscv64-linux-gnu-as -march=rv64gc associativity.s -o temp.o
  $ riscv64-linux-gnu-gcc -c ../bin/runtime.c -o runtime.o
  $ riscv64-linux-gnu-gcc temp.o runtime.o -o prog.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./prog.exe
  3
