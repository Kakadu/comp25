(** Copyright 2025-2026, Mikhail Gavrilenko,Danila Rudnev-Stepanyan, Daniel Vlasenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Backend.Codegen_llvm
open Middleend.Anf
open Middleend.Ll
open Common.Parser

let codegen_prog_str str =
  let prog = parse_str str in
  let anf = anf_program prog in
  let ll = lambda_lift_program anf in
  print_string (gen_program_ir ll "riscv64-unknown-linux-gnu" None)

let%expect_test "num" =
  codegen_prog_str {| 5 |};
  [%expect {|
    ; ModuleID = 'main'
    source_filename = "main"
    target triple = "riscv64-unknown-linux-gnu"

    declare void @print_int(i64)

    declare i64 @alloc_block(i64)

    declare i64 @alloc_closure(i64, i64)

    declare i64 @apply1(i64, i64)

    declare void @print_gc_status()

    declare void @collect()

    declare i64 @create_tuple(i64)

    declare i64 @create_tuple_init(i64, i64)

    declare i64 @field(i64, i64)

    declare void @rt_init(i64)

    define i64 @main() {
    entry:
      call void @rt_init(i64 5120)
      call void @collect()
      ret i64 0
    } |}]

let%expect_test "comp binop" =
  codegen_prog_str {| 1 + 2 |};
  [%expect{|
    ; ModuleID = 'main'
    source_filename = "main"
    target triple = "riscv64-unknown-linux-gnu"

    declare void @print_int(i64)

    declare i64 @alloc_block(i64)

    declare i64 @alloc_closure(i64, i64)

    declare i64 @apply1(i64, i64)

    declare void @print_gc_status()

    declare void @collect()

    declare i64 @create_tuple(i64)

    declare i64 @create_tuple_init(i64, i64)

    declare i64 @field(i64, i64)

    declare void @rt_init(i64)

    define i64 @main() {
    entry:
      %t_0 = alloca i64, align 8
      call void @rt_init(i64 5120)
      store i64 7, ptr %t_0, align 8
      %t_01 = load i64, ptr %t_0, align 8
      call void @collect()
      ret i64 0
    } |}]

let%expect_test "print_int 5" =
  codegen_prog_str {| print_int 5 |};
  [%expect {|
    ; ModuleID = 'main'
    source_filename = "main"
    target triple = "riscv64-unknown-linux-gnu"

    declare void @print_int(i64)

    declare i64 @alloc_block(i64)

    declare i64 @alloc_closure(i64, i64)

    declare i64 @apply1(i64, i64)

    declare void @print_gc_status()

    declare void @collect()

    declare i64 @create_tuple(i64)

    declare i64 @create_tuple_init(i64, i64)

    declare i64 @field(i64, i64)

    declare void @rt_init(i64)

    define i64 @main() {
    entry:
      %t_0 = alloca i64, align 8
      call void @rt_init(i64 5120)
      call void @print_int(i64 11)
      store i64 0, ptr %t_0, align 8
      %t_01 = load i64, ptr %t_0, align 8
      call void @collect()
      ret i64 0
    } |}]

let%expect_test "if 1 then 1 else 2 " =
  codegen_prog_str {| if 1 then 1 else 2 |};
  [%expect{|
    ; ModuleID = 'main'
    source_filename = "main"
    target triple = "riscv64-unknown-linux-gnu"

    declare void @print_int(i64)

    declare i64 @alloc_block(i64)

    declare i64 @alloc_closure(i64, i64)

    declare i64 @apply1(i64, i64)

    declare void @print_gc_status()

    declare void @collect()

    declare i64 @create_tuple(i64)

    declare i64 @create_tuple_init(i64, i64)

    declare i64 @field(i64, i64)

    declare void @rt_init(i64)

    define i64 @main() {
    entry:
      %t_0 = alloca i64, align 8
      call void @rt_init(i64 5120)
      br i1 true, label %then, label %else

    then:                                             ; preds = %entry
      br label %ifcont

    else:                                             ; preds = %entry
      br label %ifcont

    ifcont:                                           ; preds = %else, %then
      %iftmp = phi i64 [ 3, %then ], [ 5, %else ]
      store i64 %iftmp, ptr %t_0, align 8
      %t_01 = load i64, ptr %t_0, align 8
      call void @collect()
      ret i64 0
    } |}]

let%expect_test "tuple" =
  codegen_prog_str {| (1, 2, 3, 4) |};
  [%expect {|
    ; ModuleID = 'main'
    source_filename = "main"
    target triple = "riscv64-unknown-linux-gnu"

    declare void @print_int(i64)

    declare i64 @alloc_block(i64)

    declare i64 @alloc_closure(i64, i64)

    declare i64 @apply1(i64, i64)

    declare void @print_gc_status()

    declare void @collect()

    declare i64 @create_tuple(i64)

    declare i64 @create_tuple_init(i64, i64)

    declare i64 @field(i64, i64)

    declare void @rt_init(i64)

    define i64 @main() {
    entry:
      %t_0 = alloca i64, align 8
      call void @rt_init(i64 5120)
      %tuple_vals_alloca = alloca i64, i64 4, align 8
      %ptr_to_elem = getelementptr i64, ptr %tuple_vals_alloca, i64 0
      store i64 3, ptr %ptr_to_elem, align 8
      %ptr_to_elem1 = getelementptr i64, ptr %tuple_vals_alloca, i64 1
      store i64 5, ptr %ptr_to_elem1, align 8
      %ptr_to_elem2 = getelementptr i64, ptr %tuple_vals_alloca, i64 2
      store i64 7, ptr %ptr_to_elem2, align 8
      %ptr_to_elem3 = getelementptr i64, ptr %tuple_vals_alloca, i64 3
      store i64 9, ptr %ptr_to_elem3, align 8
      %alloca_as_i64 = ptrtoint ptr %tuple_vals_alloca to i64
      %tuple_tmp = call i64 @create_tuple_init(i64 4, i64 %alloca_as_i64)
      store i64 %tuple_tmp, ptr %t_0, align 8
      %t_04 = load i64, ptr %t_0, align 8
      call void @collect()
      ret i64 0
    } |}]

let%expect_test "load" =
  codegen_prog_str {| let a, b = (1, 2) in a, b |};
  [%expect {|
    ; ModuleID = 'main'
    source_filename = "main"
    target triple = "riscv64-unknown-linux-gnu"

    declare void @print_int(i64)

    declare i64 @alloc_block(i64)

    declare i64 @alloc_closure(i64, i64)

    declare i64 @apply1(i64, i64)

    declare void @print_gc_status()

    declare void @collect()

    declare i64 @create_tuple(i64)

    declare i64 @create_tuple_init(i64, i64)

    declare i64 @field(i64, i64)

    declare void @rt_init(i64)

    define i64 @main() {
    entry:
      %t_1 = alloca i64, align 8
      %b = alloca i64, align 8
      %t_2 = alloca i64, align 8
      %a = alloca i64, align 8
      %t_3 = alloca i64, align 8
      %t_0 = alloca i64, align 8
      call void @rt_init(i64 5120)
      %tuple_vals_alloca = alloca i64, i64 2, align 8
      %ptr_to_elem = getelementptr i64, ptr %tuple_vals_alloca, i64 0
      store i64 3, ptr %ptr_to_elem, align 8
      %ptr_to_elem1 = getelementptr i64, ptr %tuple_vals_alloca, i64 1
      store i64 5, ptr %ptr_to_elem1, align 8
      %alloca_as_i64 = ptrtoint ptr %tuple_vals_alloca to i64
      %tuple_tmp = call i64 @create_tuple_init(i64 2, i64 %alloca_as_i64)
      store i64 %tuple_tmp, ptr %t_0, align 8
      %t_02 = load i64, ptr %t_0, align 8
      %load_tmp = call i64 @field(i64 %t_02, i64 0)
      store i64 %load_tmp, ptr %t_3, align 8
      %t_33 = load i64, ptr %t_3, align 8
      store i64 %t_33, ptr %a, align 8
      %t_04 = load i64, ptr %t_0, align 8
      %load_tmp5 = call i64 @field(i64 %t_04, i64 1)
      store i64 %load_tmp5, ptr %t_2, align 8
      %t_26 = load i64, ptr %t_2, align 8
      store i64 %t_26, ptr %b, align 8
      %a7 = load i64, ptr %a, align 8
      %b8 = load i64, ptr %b, align 8
      %tuple_vals_alloca9 = alloca i64, i64 2, align 8
      %ptr_to_elem10 = getelementptr i64, ptr %tuple_vals_alloca9, i64 0
      store i64 %a7, ptr %ptr_to_elem10, align 8
      %ptr_to_elem11 = getelementptr i64, ptr %tuple_vals_alloca9, i64 1
      store i64 %b8, ptr %ptr_to_elem11, align 8
      %alloca_as_i6412 = ptrtoint ptr %tuple_vals_alloca9 to i64
      %tuple_tmp13 = call i64 @create_tuple_init(i64 2, i64 %alloca_as_i6412)
      store i64 %tuple_tmp13, ptr %t_1, align 8
      %t_114 = load i64, ptr %t_1, align 8
      call void @collect()
      ret i64 0
    } |}]

let%expect_test "new function " =
  codegen_prog_str {| let a = fun x -> x |};
  [%expect {|
    ; ModuleID = 'main'
    source_filename = "main"
    target triple = "riscv64-unknown-linux-gnu"

    declare void @print_int(i64)

    declare i64 @alloc_block(i64)

    declare i64 @alloc_closure(i64, i64)

    declare i64 @apply1(i64, i64)

    declare void @print_gc_status()

    declare void @collect()

    declare i64 @create_tuple(i64)

    declare i64 @create_tuple_init(i64, i64)

    declare i64 @field(i64, i64)

    declare void @rt_init(i64)

    define i64 @a(i64 %x) {
    entry:
      %x1 = alloca i64, align 8
      store i64 %x, ptr %x1, align 8
      %x2 = load i64, ptr %x1, align 8
      ret i64 %x2
    }

    define i64 @main() {
    entry:
      call void @rt_init(i64 5120)
      call void @collect()
      ret i64 0
    } |}]

let%expect_test "call function " =
  codegen_prog_str {|
    let a = fun x -> x;;
  
    let main = a 5;;
  |};
  [%expect {|
    ; ModuleID = 'main'
    source_filename = "main"
    target triple = "riscv64-unknown-linux-gnu"

    declare void @print_int(i64)

    declare i64 @alloc_block(i64)

    declare i64 @alloc_closure(i64, i64)

    declare i64 @apply1(i64, i64)

    declare void @print_gc_status()

    declare void @collect()

    declare i64 @create_tuple(i64)

    declare i64 @create_tuple_init(i64, i64)

    declare i64 @field(i64, i64)

    declare void @rt_init(i64)

    define i64 @a(i64 %x) {
    entry:
      %x1 = alloca i64, align 8
      store i64 %x, ptr %x1, align 8
      %x2 = load i64, ptr %x1, align 8
      ret i64 %x2
    }

    define i64 @main() {
    entry:
      %main = alloca i64, align 8
      %t_1 = alloca i64, align 8
      call void @rt_init(i64 5120)
      %calltmp = call i64 @a(i64 11)
      store i64 %calltmp, ptr %t_1, align 8
      %t_11 = load i64, ptr %t_1, align 8
      store i64 %t_11, ptr %main, align 8
      call void @collect()
      ret i64 0
    } |}]


let%expect_test "partial " =
  codegen_prog_str {|
    let a = fun x -> fun y -> x;;
  
    let main = a 5;;
  |};
  [%expect {|
    ; ModuleID = 'main'
    source_filename = "main"
    target triple = "riscv64-unknown-linux-gnu"

    declare void @print_int(i64)

    declare i64 @alloc_block(i64)

    declare i64 @alloc_closure(i64, i64)

    declare i64 @apply1(i64, i64)

    declare void @print_gc_status()

    declare void @collect()

    declare i64 @create_tuple(i64)

    declare i64 @create_tuple_init(i64, i64)

    declare i64 @field(i64, i64)

    declare void @rt_init(i64)

    define i64 @a(i64 %x, i64 %y) {
    entry:
      %y2 = alloca i64, align 8
      %x1 = alloca i64, align 8
      store i64 %x, ptr %x1, align 8
      store i64 %y, ptr %y2, align 8
      %x3 = load i64, ptr %x1, align 8
      ret i64 %x3
    }

    define i64 @main() {
    entry:
      %main = alloca i64, align 8
      %t_1 = alloca i64, align 8
      call void @rt_init(i64 5120)
      %closure_tmp = call i64 @alloc_closure(i64 ptrtoint (ptr @a to i64), i64 2)
      %apptmp = call i64 @apply1(i64 %closure_tmp, i64 11)
      store i64 %apptmp, ptr %t_1, align 8
      %t_11 = load i64, ptr %t_1, align 8
      store i64 %t_11, ptr %main, align 8
      call void @collect()
      ret i64 0
    } |}]


let%expect_test "user main" =
  codegen_prog_str {|
  let main x = x
  
  let a = print_int (main 5)
  |};
  [%expect{|
    ; ModuleID = 'main'
    source_filename = "main"
    target triple = "riscv64-unknown-linux-gnu"

    declare void @print_int(i64)

    declare i64 @alloc_block(i64)

    declare i64 @alloc_closure(i64, i64)

    declare i64 @apply1(i64, i64)

    declare void @print_gc_status()

    declare void @collect()

    declare i64 @create_tuple(i64)

    declare i64 @create_tuple_init(i64, i64)

    declare i64 @field(i64, i64)

    declare void @rt_init(i64)

    define i64 @__user_main(i64 %x) {
    entry:
      %x1 = alloca i64, align 8
      store i64 %x, ptr %x1, align 8
      %x2 = load i64, ptr %x1, align 8
      ret i64 %x2
    }

    define i64 @main() {
    entry:
      %a = alloca i64, align 8
      %t_2 = alloca i64, align 8
      %t_1 = alloca i64, align 8
      call void @rt_init(i64 5120)
      %calltmp = call i64 @__user_main(i64 11)
      store i64 %calltmp, ptr %t_1, align 8
      %t_11 = load i64, ptr %t_1, align 8
      call void @print_int(i64 %t_11)
      store i64 0, ptr %t_2, align 8
      %t_22 = load i64, ptr %t_2, align 8
      store i64 %t_22, ptr %a, align 8
      call void @collect()
      ret i64 0
    } |}]


let%expect_test "use reserved name" =
  codegen_prog_str {|
  let some_fun x y = x + y
  let closure_tmp x y = x + y
  
  let a = print_int (some_fun 5 10)
  let b = print_int (closure_tmp 5 10)
  let closure_tmp = 5
  let c = print_int closure_tmp
  |};
  [%expect{|
    ; ModuleID = 'main'
    source_filename = "main"
    target triple = "riscv64-unknown-linux-gnu"

    declare void @print_int(i64)

    declare i64 @alloc_block(i64)

    declare i64 @alloc_closure(i64, i64)

    declare i64 @apply1(i64, i64)

    declare void @print_gc_status()

    declare void @collect()

    declare i64 @create_tuple(i64)

    declare i64 @create_tuple_init(i64, i64)

    declare i64 @field(i64, i64)

    declare void @rt_init(i64)

    define i64 @some_fun(i64 %x, i64 %y) {
    entry:
      %t_0 = alloca i64, align 8
      %y2 = alloca i64, align 8
      %x1 = alloca i64, align 8
      store i64 %x, ptr %x1, align 8
      store i64 %y, ptr %y2, align 8
      %x3 = load i64, ptr %x1, align 8
      %y4 = load i64, ptr %y2, align 8
      %addtmp1 = add i64 %x3, %y4
      %addtmp2 = sub i64 %addtmp1, 1
      store i64 %addtmp2, ptr %t_0, align 8
      %t_05 = load i64, ptr %t_0, align 8
      ret i64 %t_05
    }

    define i64 @closure_tmp(i64 %x, i64 %y) {
    entry:
      %t_2 = alloca i64, align 8
      %y2 = alloca i64, align 8
      %x1 = alloca i64, align 8
      store i64 %x, ptr %x1, align 8
      store i64 %y, ptr %y2, align 8
      %x3 = load i64, ptr %x1, align 8
      %y4 = load i64, ptr %y2, align 8
      %addtmp1 = add i64 %x3, %y4
      %addtmp2 = sub i64 %addtmp1, 1
      store i64 %addtmp2, ptr %t_2, align 8
      %t_25 = load i64, ptr %t_2, align 8
      ret i64 %t_25
    }

    define i64 @main() {
    entry:
      %c = alloca i64, align 8
      %t_10 = alloca i64, align 8
      %closure_tmp9 = alloca i64, align 8
      %b = alloca i64, align 8
      %t_9 = alloca i64, align 8
      %t_8 = alloca i64, align 8
      %t_7 = alloca i64, align 8
      %a = alloca i64, align 8
      %t_6 = alloca i64, align 8
      %t_5 = alloca i64, align 8
      %t_4 = alloca i64, align 8
      call void @rt_init(i64 5120)
      %closure_tmp = call i64 @alloc_closure(i64 ptrtoint (ptr @some_fun to i64), i64 2)
      %apptmp = call i64 @apply1(i64 %closure_tmp, i64 11)
      store i64 %apptmp, ptr %t_4, align 8
      %t_4_val = load i64, ptr %t_4, align 8
      %apptmp1 = call i64 @apply1(i64 %t_4_val, i64 21)
      store i64 %apptmp1, ptr %t_5, align 8
      %t_52 = load i64, ptr %t_5, align 8
      call void @print_int(i64 %t_52)
      store i64 0, ptr %t_6, align 8
      %t_63 = load i64, ptr %t_6, align 8
      store i64 %t_63, ptr %a, align 8
      %closure_tmp4 = call i64 @alloc_closure(i64 ptrtoint (ptr @closure_tmp to i64), i64 2)
      %apptmp5 = call i64 @apply1(i64 %closure_tmp4, i64 11)
      store i64 %apptmp5, ptr %t_7, align 8
      %t_7_val = load i64, ptr %t_7, align 8
      %apptmp6 = call i64 @apply1(i64 %t_7_val, i64 21)
      store i64 %apptmp6, ptr %t_8, align 8
      %t_87 = load i64, ptr %t_8, align 8
      call void @print_int(i64 %t_87)
      store i64 0, ptr %t_9, align 8
      %t_98 = load i64, ptr %t_9, align 8
      store i64 %t_98, ptr %b, align 8
      store i64 11, ptr %closure_tmp9, align 8
      %closure_tmp10 = load i64, ptr %closure_tmp9, align 8
      call void @print_int(i64 %closure_tmp10)
      store i64 0, ptr %t_10, align 8
      %t_1011 = load i64, ptr %t_10, align 8
      store i64 %t_1011, ptr %c, align 8
      call void @collect()
      ret i64 0
    } |}]
