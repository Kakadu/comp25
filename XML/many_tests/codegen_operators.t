  $ clang-18 --target=riscv64-linux-gnu --sysroot=/usr/riscv64-unknown-linux-gnu -c ./../bin/runtime.c -o runtime.o

====================== Llvm ======================

====================== Custom operators ======================

  $ dune exec ../bin/XML_llvm.exe -- -o oper_custom1.ll <<EOF
  > let (@) f x = f x in
  > print_int ((@) (fun x -> x) 5)
  Fatal error: exception Invalid_argument("Unsupported binary operator: @")
  Raised at Stdlib.invalid_arg in file "stdlib.ml", line 30, characters 20-45
  Called from Backend__Codegen_llvm.gen_anf_expr in file "lib/backend/codegen_llvm.ml", line 291, characters 19-54
  Called from Backend__Codegen_llvm.gen_astructure_item in file "lib/backend/codegen_llvm.ml", line 344, characters 21-47
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Backend__Codegen_llvm.gen_program_ir in file "lib/backend/codegen_llvm.ml", line 389, characters 4-119
  Called from Dune__exe__XML_llvm.to_llvm_ir in file "bin/XML_llvm.ml", line 39, characters 2-55
  [2]

  $ cat oper_custom1.ll
  cat: oper_custom1.ll: No such file or directory
  [1]

  $ llc-18 oper_custom1.ll -o temp.s
  llc-18: error: llc-18: oper_custom1.ll: error: Could not open input file: No such file or directory
  [1]
  $ clang-18  --target=riscv64-linux-gnu -static temp.s runtime.o -o temp.exe
  clang-18: error: no such file or directory: 'temp.s'
  [1]
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./temp.exe
  [1]



  $ dune exec ../bin/XML_llvm.exe -- -o oper_custom2.ll <<EOF
  > let (@) f x = f x in
  > print_int ((fun x -> x) @ 5)
  Fatal error: exception Invalid_argument("Unsupported binary operator: @")
  Raised at Stdlib.invalid_arg in file "stdlib.ml", line 30, characters 20-45
  Called from Backend__Codegen_llvm.gen_anf_expr in file "lib/backend/codegen_llvm.ml", line 291, characters 19-54
  Called from Backend__Codegen_llvm.gen_astructure_item in file "lib/backend/codegen_llvm.ml", line 344, characters 21-47
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Backend__Codegen_llvm.gen_program_ir in file "lib/backend/codegen_llvm.ml", line 389, characters 4-119
  Called from Dune__exe__XML_llvm.to_llvm_ir in file "bin/XML_llvm.ml", line 39, characters 2-55
  [2]

  $ cat oper_custom2.ll
  cat: oper_custom2.ll: No such file or directory
  [1]

  $ llc-18 oper_custom2.ll -o temp.s
  llc-18: error: llc-18: oper_custom2.ll: error: Could not open input file: No such file or directory
  [1]
  $ clang-18  --target=riscv64-linux-gnu -static temp.s runtime.o -o temp.exe
  clang-18: error: no such file or directory: 'temp.s'
  [1]
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./temp.exe
  [1]



  $ dune exec ../bin/XML_llvm.exe -- -o oper_custom3.ll <<EOF
  > let (@) f x = f x;;
  > 
  > print_int ((@) (fun x -> x) 5)
  Fatal error: exception Invalid_argument("Unsupported binary operator: @")
  Raised at Stdlib.invalid_arg in file "stdlib.ml", line 30, characters 20-45
  Called from Backend__Codegen_llvm.gen_anf_expr in file "lib/backend/codegen_llvm.ml", line 291, characters 19-54
  Called from Backend__Codegen_llvm.gen_astructure_item in file "lib/backend/codegen_llvm.ml", line 344, characters 21-47
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Backend__Codegen_llvm.gen_program_ir in file "lib/backend/codegen_llvm.ml", line 389, characters 4-119
  Called from Dune__exe__XML_llvm.to_llvm_ir in file "bin/XML_llvm.ml", line 39, characters 2-55
  [2]

  $ cat oper_custom3.ll
  cat: oper_custom3.ll: No such file or directory
  [1]

  $ llc-18 oper_custom3.ll -o temp.s
  llc-18: error: llc-18: oper_custom3.ll: error: Could not open input file: No such file or directory
  [1]
  $ clang-18  --target=riscv64-linux-gnu -static temp.s runtime.o -o temp.exe
  clang-18: error: no such file or directory: 'temp.s'
  [1]
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./temp.exe
  [1]


  $ dune exec ../bin/XML_llvm.exe -- -o oper_custom4.ll <<EOF
  > let (@) f x = f x;;
  > 
  > print_int ((fun x -> x) @ 5)
  Fatal error: exception Invalid_argument("Unsupported binary operator: @")
  Raised at Stdlib.invalid_arg in file "stdlib.ml", line 30, characters 20-45
  Called from Backend__Codegen_llvm.gen_anf_expr in file "lib/backend/codegen_llvm.ml", line 291, characters 19-54
  Called from Backend__Codegen_llvm.gen_astructure_item in file "lib/backend/codegen_llvm.ml", line 344, characters 21-47
  Called from Stdlib__List.fold_left in file "list.ml", line 121, characters 24-34
  Called from Backend__Codegen_llvm.gen_program_ir in file "lib/backend/codegen_llvm.ml", line 389, characters 4-119
  Called from Dune__exe__XML_llvm.to_llvm_ir in file "bin/XML_llvm.ml", line 39, characters 2-55
  [2]

  $ cat oper_custom4.ll
  cat: oper_custom4.ll: No such file or directory
  [1]

  $ llc-18 oper_custom4.ll -o temp.s
  llc-18: error: llc-18: oper_custom4.ll: error: Could not open input file: No such file or directory
  [1]
  $ clang-18  --target=riscv64-linux-gnu -static temp.s runtime.o -o temp.exe
  clang-18: error: no such file or directory: 'temp.s'
  [1]
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./temp.exe
  [1]
