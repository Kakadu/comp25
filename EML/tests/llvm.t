Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev
SPDX-License-Identifier: LGPL-3.0-or-later

  $ make compile_llvm many_tests/typed/001fac.ml
  24

  $ make compile_llvm many_tests/typed/002fac.ml
  24

  $ make compile_llvm many_tests/typed/003fib.ml
  3
  3

  $ make compile_llvm many_tests/typed/004manyargs.ml
  1111111111
  1
  10
  100

  $ make compile_llvm many_tests/typed/005fix.ml
  720

  $ make compile_llvm many_tests/typed/006partial.ml
  1122

  $ make compile_llvm many_tests/typed/006partial2.ml
  1
  2
  3
  7

  $ make compile_llvm many_tests/typed/006partial3.ml
  4
  8
  9

  $ make compile_llvm many_tests/typed/007order.ml
  1
  2
  4
  -1
  103
  -555555
  10000

  $ make compile_llvm many_tests/typed/008ascription.ml
  8

  $ make compile_llvm many_tests/typed/009let_poly.ml

  $ make compile_llvm many_tests/typed/010fac_anf.ml

  $ make compile_llvm many_tests/typed/010faccps_ll.ml
  24

  $ make compile_llvm many_tests/typed/010fibcps_ll.ml
  8

  $ make compile_llvm many_tests/typed/011mapcps.ml
  2
  3
  4

  $ make compile_llvm many_tests/typed/012faccps.ml
  720

  $ make compile_llvm many_tests/typed/012fibcps.ml
  8

  $ make compile_llvm many_tests/typed/013foldfoldr.ml
  6

  $ make compile_llvm many_tests/typed/015tuples.ml
  1
  1
  1
  1

  $ make compile_llvm additional_tests/mangling_test.ml
  24

  $ make compile_llvm additional_tests/custom_op_left_associativity.ml
  -6

  $ make compile_llvm additional_tests/custom_op_right_associativity.ml
  9

  $ make compile_llvm additional_tests/custom_op_shadowing.ml
  9

  $ make compile_llvm additional_tests/custom_op_via_op.ml
  13

  $ make compile_llvm additional_tests/custom_op_pipe.ml
  11
