(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EML_lib
open Frontend.Parser
open Middleend.Anf
open Middleend.Inferencer
open Middleend.Resolve_builtins

let initial_scope = TypeEnv.keys TypeEnv.initial_env

let compile_llvm src : string =
  match parse src with
  | Error e -> "Parse error: " ^ e
  | Ok ast ->
    let ast' = resolve_program ast initial_scope in
    (match anf_program ast' with
     | Error e -> "ANF error: " ^ e
     | Ok anf ->
       let buf = Buffer.create 4096 in
       let ppf = Format.formatter_of_buffer buf in
       (match Backend.Llvm_ir.Runner.gen_program ~enable_gc:false ppf anf with
        | Ok () ->
          Format.pp_print_flush ppf ();
          Buffer.contents buf
        | Error e -> "Codegen error: " ^ e))
;;

let compile_llvm_show src = Format.printf "%s" (compile_llvm src)

let%expect_test "unary_minus" =
  compile_llvm_show "let x = -5";
  [%expect
    {|
; ModuleID = 'EML'
source_filename = "EML"

declare ptr @eml_applyN(ptr, i64, ptr)

declare ptr @create_tuple(i64, ptr)

declare ptr @alloc_closure(ptr, i64)

declare ptr @field(ptr, i64)

declare ptr @llvm_call_indirect(ptr, ptr, i64)

declare void @print_int(i64)

declare void @init_gc()

declare void @destroy_gc()

declare void @set_ptr_stack(ptr)

declare i64 @get_heap_start()

declare i64 @get_heap_final()

declare ptr @collect()

declare ptr @print_gc_status()

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
declare ptr @llvm.frameaddress.p0(i32 immarg) #0

define ptr @x() {
entry:
  ret ptr inttoptr (i64 -9 to ptr)
}

define ptr @main() {
entry:
  ret ptr inttoptr (i64 1 to ptr)
}

attributes #0 = { nocallback nofree nosync nounwind willreturn memory(none) }

|}]
;;

let%expect_test "unary_not" =
  compile_llvm_show "let x = not true";
  [%expect
    {|
; ModuleID = 'EML'
source_filename = "EML"

declare ptr @eml_applyN(ptr, i64, ptr)

declare ptr @create_tuple(i64, ptr)

declare ptr @alloc_closure(ptr, i64)

declare ptr @field(ptr, i64)

declare ptr @llvm_call_indirect(ptr, ptr, i64)

declare void @print_int(i64)

declare void @init_gc()

declare void @destroy_gc()

declare void @set_ptr_stack(ptr)

declare i64 @get_heap_start()

declare i64 @get_heap_final()

declare ptr @collect()

declare ptr @print_gc_status()

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
declare ptr @llvm.frameaddress.p0(i32 immarg) #0

define ptr @x() {
entry:
  ret ptr inttoptr (i64 2 to ptr)
}

define ptr @main() {
entry:
  ret ptr inttoptr (i64 1 to ptr)
}

attributes #0 = { nocallback nofree nosync nounwind willreturn memory(none) }

|}]
;;

let%expect_test "unit_main" =
  compile_llvm_show "let main = ()";
  [%expect
    {|
; ModuleID = 'EML'
source_filename = "EML"

declare ptr @eml_applyN(ptr, i64, ptr)

declare ptr @create_tuple(i64, ptr)

declare ptr @alloc_closure(ptr, i64)

declare ptr @field(ptr, i64)

declare ptr @llvm_call_indirect(ptr, ptr, i64)

declare void @print_int(i64)

declare void @init_gc()

declare void @destroy_gc()

declare void @set_ptr_stack(ptr)

declare i64 @get_heap_start()

declare i64 @get_heap_final()

declare ptr @collect()

declare ptr @print_gc_status()

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
declare ptr @llvm.frameaddress.p0(i32 immarg) #0

define ptr @main() {
entry:
  ret ptr inttoptr (i64 1 to ptr)
}

attributes #0 = { nocallback nofree nosync nounwind willreturn memory(none) }

|}]
;;

let%expect_test "mul_only" =
  compile_llvm_show "let main = 7 * 8";
  [%expect
    {|
; ModuleID = 'EML'
source_filename = "EML"

declare ptr @eml_applyN(ptr, i64, ptr)

declare ptr @create_tuple(i64, ptr)

declare ptr @alloc_closure(ptr, i64)

declare ptr @field(ptr, i64)

declare ptr @llvm_call_indirect(ptr, ptr, i64)

declare void @print_int(i64)

declare void @init_gc()

declare void @destroy_gc()

declare void @set_ptr_stack(ptr)

declare i64 @get_heap_start()

declare i64 @get_heap_final()

declare ptr @collect()

declare ptr @print_gc_status()

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
declare ptr @llvm.frameaddress.p0(i32 immarg) #0

define ptr @main() {
entry:
  ret ptr inttoptr (i64 113 to ptr)
}

attributes #0 = { nocallback nofree nosync nounwind willreturn memory(none) }

|}]
;;

let%expect_test "double_fn" =
  compile_llvm_show
    {|
  let double x = x + x
  let main = double 21
  |};
  [%expect
    {|
    ; ModuleID = 'EML'
    source_filename = "EML"

    declare ptr @eml_applyN(ptr, i64, ptr)

    declare ptr @create_tuple(i64, ptr)

    declare ptr @alloc_closure(ptr, i64)

    declare ptr @field(ptr, i64)

    declare ptr @llvm_call_indirect(ptr, ptr, i64)

    declare void @print_int(i64)

    declare void @init_gc()

    declare void @destroy_gc()

    declare void @set_ptr_stack(ptr)

    declare i64 @get_heap_start()

    declare i64 @get_heap_final()

    declare ptr @collect()

    declare ptr @print_gc_status()

    ; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
    declare ptr @llvm.frameaddress.p0(i32 immarg) #0

    define ptr @double(ptr %x) {
    entry:
      %raw_int = ptrtoint ptr %x to i64
      %minus1 = sub i64 %raw_int, 1
      %untagged = sdiv i64 %minus1, 2
      %raw_int1 = ptrtoint ptr %x to i64
      %minus12 = sub i64 %raw_int1, 1
      %untagged3 = sdiv i64 %minus12, 2
      %add = add i64 %untagged, %untagged3
      %twice = mul i64 %add, 2
      %tagged = add i64 %twice, 1
      %result_int = inttoptr i64 %tagged to ptr
      ret ptr %result_int
    }

    define ptr @main() {
    entry:
      %direct_double = call ptr @double(ptr inttoptr (i64 43 to ptr))
      ret ptr %direct_double
    }

    attributes #0 = { nocallback nofree nosync nounwind willreturn memory(none) } |}]
;;

let%expect_test "abs_fn" =
  compile_llvm_show
    {|
  let abs x = if x < 0 then -x else x
  let main = abs 7
  |};
  [%expect
    {|
    ; ModuleID = 'EML'
    source_filename = "EML"

    declare ptr @eml_applyN(ptr, i64, ptr)

    declare ptr @create_tuple(i64, ptr)

    declare ptr @alloc_closure(ptr, i64)

    declare ptr @field(ptr, i64)

    declare ptr @llvm_call_indirect(ptr, ptr, i64)

    declare void @print_int(i64)

    declare void @init_gc()

    declare void @destroy_gc()

    declare void @set_ptr_stack(ptr)

    declare i64 @get_heap_start()

    declare i64 @get_heap_final()

    declare ptr @collect()

    declare ptr @print_gc_status()

    ; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
    declare ptr @llvm.frameaddress.p0(i32 immarg) #0

    define ptr @abs(ptr %x) {
    entry:
      %raw_int = ptrtoint ptr %x to i64
      %minus1 = sub i64 %raw_int, 1
      %untagged = sdiv i64 %minus1, 2
      %icmp_slt = icmp slt i64 %untagged, 0
      %tagged_bool = select i1 %icmp_slt, i64 4, i64 2
      %result_bool = inttoptr i64 %tagged_bool to ptr
      %raw_bool = ptrtoint ptr %result_bool to i64
      %is_true = icmp eq i64 %raw_bool, 4
      br i1 %is_true, label %then_0, label %else_0

    then_0:                                           ; preds = %entry
      %raw_int1 = ptrtoint ptr %x to i64
      %minus12 = sub i64 %raw_int1, 1
      %untagged3 = sdiv i64 %minus12, 2
      %neg = sub i64 0, %untagged3
      %twice = mul i64 %neg, 2
      %tagged = add i64 %twice, 1
      %result_int = inttoptr i64 %tagged to ptr
      br label %merge_0

    else_0:                                           ; preds = %entry
      br label %merge_0

    merge_0:                                          ; preds = %else_0, %then_0
      %ite_result = phi ptr [ %result_int, %then_0 ], [ %x, %else_0 ]
      ret ptr %ite_result
    }

    define ptr @main() {
    entry:
      %direct_abs = call ptr @abs(ptr inttoptr (i64 15 to ptr))
      ret ptr %direct_abs
    }

    attributes #0 = { nocallback nofree nosync nounwind willreturn memory(none) } |}]
;;

let%expect_test "nested_calls" =
  compile_llvm_show
    {|
  let sq x = x * x
  let sum_of_squares a b = sq a + sq b
  let main = sum_of_squares 3 4
  |};
  [%expect
    {|
    ; ModuleID = 'EML'
    source_filename = "EML"

    declare ptr @eml_applyN(ptr, i64, ptr)

    declare ptr @create_tuple(i64, ptr)

    declare ptr @alloc_closure(ptr, i64)

    declare ptr @field(ptr, i64)

    declare ptr @llvm_call_indirect(ptr, ptr, i64)

    declare void @print_int(i64)

    declare void @init_gc()

    declare void @destroy_gc()

    declare void @set_ptr_stack(ptr)

    declare i64 @get_heap_start()

    declare i64 @get_heap_final()

    declare ptr @collect()

    declare ptr @print_gc_status()

    ; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
    declare ptr @llvm.frameaddress.p0(i32 immarg) #0

    define ptr @sq(ptr %x) {
    entry:
      %raw_int = ptrtoint ptr %x to i64
      %minus1 = sub i64 %raw_int, 1
      %untagged = sdiv i64 %minus1, 2
      %raw_int1 = ptrtoint ptr %x to i64
      %minus12 = sub i64 %raw_int1, 1
      %untagged3 = sdiv i64 %minus12, 2
      %mul = mul i64 %untagged, %untagged3
      %twice = mul i64 %mul, 2
      %tagged = add i64 %twice, 1
      %result_int = inttoptr i64 %tagged to ptr
      ret ptr %result_int
    }

    define ptr @sum_of_squares(ptr %a, ptr %b) {
    entry:
      %direct_sq = call ptr @sq(ptr %a)
      %direct_sq1 = call ptr @sq(ptr %b)
      %raw_int = ptrtoint ptr %direct_sq to i64
      %minus1 = sub i64 %raw_int, 1
      %untagged = sdiv i64 %minus1, 2
      %raw_int2 = ptrtoint ptr %direct_sq1 to i64
      %minus13 = sub i64 %raw_int2, 1
      %untagged4 = sdiv i64 %minus13, 2
      %add = add i64 %untagged, %untagged4
      %twice = mul i64 %add, 2
      %tagged = add i64 %twice, 1
      %result_int = inttoptr i64 %tagged to ptr
      ret ptr %result_int
    }

    define ptr @main() {
    entry:
      %direct_sum_of_squares = call ptr @sum_of_squares(ptr inttoptr (i64 7 to ptr), ptr inttoptr (i64 9 to ptr))
      ret ptr %direct_sum_of_squares
    }

    attributes #0 = { nocallback nofree nosync nounwind willreturn memory(none) } |}]
;;

let%expect_test "fibonacci" =
  compile_llvm_show
    {|
  let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2)
  let main = fib 6
  |};
  [%expect
    {|
    ; ModuleID = 'EML'
    source_filename = "EML"

    declare ptr @eml_applyN(ptr, i64, ptr)

    declare ptr @create_tuple(i64, ptr)

    declare ptr @alloc_closure(ptr, i64)

    declare ptr @field(ptr, i64)

    declare ptr @llvm_call_indirect(ptr, ptr, i64)

    declare void @print_int(i64)

    declare void @init_gc()

    declare void @destroy_gc()

    declare void @set_ptr_stack(ptr)

    declare i64 @get_heap_start()

    declare i64 @get_heap_final()

    declare ptr @collect()

    declare ptr @print_gc_status()

    ; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
    declare ptr @llvm.frameaddress.p0(i32 immarg) #0

    define ptr @fib(ptr %n) {
    entry:
      %raw_int = ptrtoint ptr %n to i64
      %minus1 = sub i64 %raw_int, 1
      %untagged = sdiv i64 %minus1, 2
      %icmp_slt = icmp slt i64 %untagged, 2
      %tagged_bool = select i1 %icmp_slt, i64 4, i64 2
      %result_bool = inttoptr i64 %tagged_bool to ptr
      %raw_bool = ptrtoint ptr %result_bool to i64
      %is_true = icmp eq i64 %raw_bool, 4
      br i1 %is_true, label %then_0, label %else_0

    then_0:                                           ; preds = %entry
      br label %merge_0

    else_0:                                           ; preds = %entry
      %raw_int1 = ptrtoint ptr %n to i64
      %minus12 = sub i64 %raw_int1, 1
      %untagged3 = sdiv i64 %minus12, 2
      %sub = sub i64 %untagged3, 1
      %twice = mul i64 %sub, 2
      %tagged = add i64 %twice, 1
      %result_int = inttoptr i64 %tagged to ptr
      %direct_fib = call ptr @fib(ptr %result_int)
      %raw_int4 = ptrtoint ptr %n to i64
      %minus15 = sub i64 %raw_int4, 1
      %untagged6 = sdiv i64 %minus15, 2
      %sub7 = sub i64 %untagged6, 2
      %twice8 = mul i64 %sub7, 2
      %tagged9 = add i64 %twice8, 1
      %result_int10 = inttoptr i64 %tagged9 to ptr
      %direct_fib11 = call ptr @fib(ptr %result_int10)
      %raw_int12 = ptrtoint ptr %direct_fib to i64
      %minus113 = sub i64 %raw_int12, 1
      %untagged14 = sdiv i64 %minus113, 2
      %raw_int15 = ptrtoint ptr %direct_fib11 to i64
      %minus116 = sub i64 %raw_int15, 1
      %untagged17 = sdiv i64 %minus116, 2
      %add = add i64 %untagged14, %untagged17
      %twice18 = mul i64 %add, 2
      %tagged19 = add i64 %twice18, 1
      %result_int20 = inttoptr i64 %tagged19 to ptr
      br label %merge_0

    merge_0:                                          ; preds = %else_0, %then_0
      %ite_result = phi ptr [ inttoptr (i64 3 to ptr), %then_0 ], [ %result_int20, %else_0 ]
      ret ptr %ite_result
    }

    define ptr @main() {
    entry:
      %direct_fib = call ptr @fib(ptr inttoptr (i64 13 to ptr))
      ret ptr %direct_fib
    }

    attributes #0 = { nocallback nofree nosync nounwind willreturn memory(none) } |}]
;;

let%expect_test "is_positive" =
  compile_llvm_show
    {|
  let is_positive n = n > 0
  let main = is_positive 42
  |};
  [%expect
    {|
    ; ModuleID = 'EML'
    source_filename = "EML"

    declare ptr @eml_applyN(ptr, i64, ptr)

    declare ptr @create_tuple(i64, ptr)

    declare ptr @alloc_closure(ptr, i64)

    declare ptr @field(ptr, i64)

    declare ptr @llvm_call_indirect(ptr, ptr, i64)

    declare void @print_int(i64)

    declare void @init_gc()

    declare void @destroy_gc()

    declare void @set_ptr_stack(ptr)

    declare i64 @get_heap_start()

    declare i64 @get_heap_final()

    declare ptr @collect()

    declare ptr @print_gc_status()

    ; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
    declare ptr @llvm.frameaddress.p0(i32 immarg) #0

    define ptr @is_positive(ptr %n) {
    entry:
      %raw_int = ptrtoint ptr %n to i64
      %minus1 = sub i64 %raw_int, 1
      %untagged = sdiv i64 %minus1, 2
      %icmp_sgt = icmp sgt i64 %untagged, 0
      %tagged_bool = select i1 %icmp_sgt, i64 4, i64 2
      %result_bool = inttoptr i64 %tagged_bool to ptr
      ret ptr %result_bool
    }

    define ptr @main() {
    entry:
      %direct_is_positive = call ptr @is_positive(ptr inttoptr (i64 85 to ptr))
      ret ptr %direct_is_positive
    }

    attributes #0 = { nocallback nofree nosync nounwind willreturn memory(none) } |}]
;;

let%expect_test "mul3" =
  compile_llvm_show
    {|
  let mul3 a b c = a * b * c
  let main = mul3 2 3 4
  |};
  [%expect
    {|
    ; ModuleID = 'EML'
    source_filename = "EML"

    declare ptr @eml_applyN(ptr, i64, ptr)

    declare ptr @create_tuple(i64, ptr)

    declare ptr @alloc_closure(ptr, i64)

    declare ptr @field(ptr, i64)

    declare ptr @llvm_call_indirect(ptr, ptr, i64)

    declare void @print_int(i64)

    declare void @init_gc()

    declare void @destroy_gc()

    declare void @set_ptr_stack(ptr)

    declare i64 @get_heap_start()

    declare i64 @get_heap_final()

    declare ptr @collect()

    declare ptr @print_gc_status()

    ; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
    declare ptr @llvm.frameaddress.p0(i32 immarg) #0

    define ptr @mul3(ptr %a, ptr %b, ptr %c) {
    entry:
      %raw_int = ptrtoint ptr %a to i64
      %minus1 = sub i64 %raw_int, 1
      %untagged = sdiv i64 %minus1, 2
      %raw_int1 = ptrtoint ptr %b to i64
      %minus12 = sub i64 %raw_int1, 1
      %untagged3 = sdiv i64 %minus12, 2
      %mul = mul i64 %untagged, %untagged3
      %twice = mul i64 %mul, 2
      %tagged = add i64 %twice, 1
      %result_int = inttoptr i64 %tagged to ptr
      %raw_int4 = ptrtoint ptr %result_int to i64
      %minus15 = sub i64 %raw_int4, 1
      %untagged6 = sdiv i64 %minus15, 2
      %raw_int7 = ptrtoint ptr %c to i64
      %minus18 = sub i64 %raw_int7, 1
      %untagged9 = sdiv i64 %minus18, 2
      %mul10 = mul i64 %untagged6, %untagged9
      %twice11 = mul i64 %mul10, 2
      %tagged12 = add i64 %twice11, 1
      %result_int13 = inttoptr i64 %tagged12 to ptr
      ret ptr %result_int13
    }

    define ptr @main() {
    entry:
      %direct_mul3 = call ptr @mul3(ptr inttoptr (i64 5 to ptr), ptr inttoptr (i64 7 to ptr), ptr inttoptr (i64 9 to ptr))
      ret ptr %direct_mul3
    }

    attributes #0 = { nocallback nofree nosync nounwind willreturn memory(none) } |}]
;;

let%expect_test "test1" =
  compile_llvm_show
    {|
   let large x = if 0<>x then print_int 0 else print_int 1
   let main =
     let x = if (if (if 0
                     then 0 else (let t42 = print_int 42 in 1))
                 then 0 else 1)
             then 0 else 1 in
     large x
  |};
  [%expect
    {|
    ; ModuleID = 'EML'
    source_filename = "EML"

    declare ptr @eml_applyN(ptr, i64, ptr)

    declare ptr @create_tuple(i64, ptr)

    declare ptr @alloc_closure(ptr, i64)

    declare ptr @field(ptr, i64)

    declare ptr @llvm_call_indirect(ptr, ptr, i64)

    declare void @print_int(i64)

    declare void @init_gc()

    declare void @destroy_gc()

    declare void @set_ptr_stack(ptr)

    declare i64 @get_heap_start()

    declare i64 @get_heap_final()

    declare ptr @collect()

    declare ptr @print_gc_status()

    ; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
    declare ptr @llvm.frameaddress.p0(i32 immarg) #0

    define ptr @large(ptr %x) {
    entry:
      %raw_int = ptrtoint ptr %x to i64
      %minus1 = sub i64 %raw_int, 1
      %untagged = sdiv i64 %minus1, 2
      %icmp_ne = icmp ne i64 0, %untagged
      %tagged_bool = select i1 %icmp_ne, i64 4, i64 2
      %result_bool = inttoptr i64 %tagged_bool to ptr
      %raw_bool = ptrtoint ptr %result_bool to i64
      %is_true = icmp eq i64 %raw_bool, 4
      br i1 %is_true, label %then_0, label %else_0

    then_0:                                           ; preds = %entry
      call void @print_int(i64 1)
      br label %merge_0

    else_0:                                           ; preds = %entry
      call void @print_int(i64 3)
      br label %merge_0

    merge_0:                                          ; preds = %else_0, %then_0
      %ite_result = phi ptr [ inttoptr (i64 1 to ptr), %then_0 ], [ inttoptr (i64 1 to ptr), %else_0 ]
      ret ptr %ite_result
    }

    define ptr @main() {
    entry:
      br i1 false, label %then_1, label %else_1

    then_1:                                           ; preds = %entry
      br label %merge_1

    else_1:                                           ; preds = %entry
      call void @print_int(i64 85)
      br label %merge_1

    merge_1:                                          ; preds = %else_1, %then_1
      %ite_result = phi ptr [ inttoptr (i64 1 to ptr), %then_1 ], [ inttoptr (i64 3 to ptr), %else_1 ]
      %raw_bool = ptrtoint ptr %ite_result to i64
      %is_true = icmp eq i64 %raw_bool, 4
      br i1 %is_true, label %then_2, label %else_2

    then_2:                                           ; preds = %merge_1
      br label %merge_2

    else_2:                                           ; preds = %merge_1
      br label %merge_2

    merge_2:                                          ; preds = %else_2, %then_2
      %ite_result1 = phi ptr [ inttoptr (i64 1 to ptr), %then_2 ], [ inttoptr (i64 3 to ptr), %else_2 ]
      %raw_bool2 = ptrtoint ptr %ite_result1 to i64
      %is_true3 = icmp eq i64 %raw_bool2, 4
      br i1 %is_true3, label %then_3, label %else_3

    then_3:                                           ; preds = %merge_2
      br label %merge_3

    else_3:                                           ; preds = %merge_2
      br label %merge_3

    merge_3:                                          ; preds = %else_3, %then_3
      %ite_result4 = phi ptr [ inttoptr (i64 1 to ptr), %then_3 ], [ inttoptr (i64 3 to ptr), %else_3 ]
      %direct_large = call ptr @large(ptr %ite_result4)
      ret ptr %direct_large
    }

    attributes #0 = { nocallback nofree nosync nounwind willreturn memory(none) } |}]
;;

let%expect_test "codegen closure fn with 10 arg" =
  compile_llvm_show
    {|
  let add a b c d e f g = a + b + c + d + e + f + g

  let main =
    let temp1 = add 1 1 1 1 in
    let temp2 = temp1 1 1 in
    let temp3 = temp2 1 1 in
    print_int temp3
  ;;
  |};
  [%expect
    {|
    ; ModuleID = 'EML'
    source_filename = "EML"

    declare ptr @eml_applyN(ptr, i64, ptr)

    declare ptr @create_tuple(i64, ptr)

    declare ptr @alloc_closure(ptr, i64)

    declare ptr @field(ptr, i64)

    declare ptr @llvm_call_indirect(ptr, ptr, i64)

    declare void @print_int(i64)

    declare void @init_gc()

    declare void @destroy_gc()

    declare void @set_ptr_stack(ptr)

    declare i64 @get_heap_start()

    declare i64 @get_heap_final()

    declare ptr @collect()

    declare ptr @print_gc_status()

    ; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
    declare ptr @llvm.frameaddress.p0(i32 immarg) #0

    define ptr @add(ptr %a, ptr %b, ptr %c, ptr %d, ptr %e, ptr %f, ptr %g) {
    entry:
      %raw_int = ptrtoint ptr %a to i64
      %minus1 = sub i64 %raw_int, 1
      %untagged = sdiv i64 %minus1, 2
      %raw_int1 = ptrtoint ptr %b to i64
      %minus12 = sub i64 %raw_int1, 1
      %untagged3 = sdiv i64 %minus12, 2
      %add = add i64 %untagged, %untagged3
      %twice = mul i64 %add, 2
      %tagged = add i64 %twice, 1
      %result_int = inttoptr i64 %tagged to ptr
      %raw_int4 = ptrtoint ptr %result_int to i64
      %minus15 = sub i64 %raw_int4, 1
      %untagged6 = sdiv i64 %minus15, 2
      %raw_int7 = ptrtoint ptr %c to i64
      %minus18 = sub i64 %raw_int7, 1
      %untagged9 = sdiv i64 %minus18, 2
      %add10 = add i64 %untagged6, %untagged9
      %twice11 = mul i64 %add10, 2
      %tagged12 = add i64 %twice11, 1
      %result_int13 = inttoptr i64 %tagged12 to ptr
      %raw_int14 = ptrtoint ptr %result_int13 to i64
      %minus115 = sub i64 %raw_int14, 1
      %untagged16 = sdiv i64 %minus115, 2
      %raw_int17 = ptrtoint ptr %d to i64
      %minus118 = sub i64 %raw_int17, 1
      %untagged19 = sdiv i64 %minus118, 2
      %add20 = add i64 %untagged16, %untagged19
      %twice21 = mul i64 %add20, 2
      %tagged22 = add i64 %twice21, 1
      %result_int23 = inttoptr i64 %tagged22 to ptr
      %raw_int24 = ptrtoint ptr %result_int23 to i64
      %minus125 = sub i64 %raw_int24, 1
      %untagged26 = sdiv i64 %minus125, 2
      %raw_int27 = ptrtoint ptr %e to i64
      %minus128 = sub i64 %raw_int27, 1
      %untagged29 = sdiv i64 %minus128, 2
      %add30 = add i64 %untagged26, %untagged29
      %twice31 = mul i64 %add30, 2
      %tagged32 = add i64 %twice31, 1
      %result_int33 = inttoptr i64 %tagged32 to ptr
      %raw_int34 = ptrtoint ptr %result_int33 to i64
      %minus135 = sub i64 %raw_int34, 1
      %untagged36 = sdiv i64 %minus135, 2
      %raw_int37 = ptrtoint ptr %f to i64
      %minus138 = sub i64 %raw_int37, 1
      %untagged39 = sdiv i64 %minus138, 2
      %add40 = add i64 %untagged36, %untagged39
      %twice41 = mul i64 %add40, 2
      %tagged42 = add i64 %twice41, 1
      %result_int43 = inttoptr i64 %tagged42 to ptr
      %raw_int44 = ptrtoint ptr %result_int43 to i64
      %minus145 = sub i64 %raw_int44, 1
      %untagged46 = sdiv i64 %minus145, 2
      %raw_int47 = ptrtoint ptr %g to i64
      %minus148 = sub i64 %raw_int47, 1
      %untagged49 = sdiv i64 %minus148, 2
      %add50 = add i64 %untagged46, %untagged49
      %twice51 = mul i64 %add50, 2
      %tagged52 = add i64 %twice51, 1
      %result_int53 = inttoptr i64 %tagged52 to ptr
      ret ptr %result_int53
    }

    define ptr @main() {
    entry:
      %boxed_alloc_closure = call ptr @alloc_closure(ptr @add, i64 7)
      br label %apply_step_0

    merge_0:                                          ; preds = %apply_step_3
      %apply_result = phi ptr [ %apply_step_310, %apply_step_3 ]
      br label %apply_step_011

    apply_step_0:                                     ; preds = %entry
      %apply_one = alloca [1 x ptr], align 8
      %one_elem = getelementptr [1 x ptr], ptr %apply_one, i32 0, i32 0
      store ptr inttoptr (i64 3 to ptr), ptr %one_elem, align 8
      %apply_step_01 = call ptr @eml_applyN(ptr %boxed_alloc_closure, i64 1, ptr %one_elem)
      br label %apply_step_1

    apply_step_1:                                     ; preds = %apply_step_0
      %cur_1 = phi ptr [ %apply_step_01, %apply_step_0 ]
      %apply_one2 = alloca [1 x ptr], align 8
      %one_elem3 = getelementptr [1 x ptr], ptr %apply_one2, i32 0, i32 0
      store ptr inttoptr (i64 3 to ptr), ptr %one_elem3, align 8
      %apply_step_14 = call ptr @eml_applyN(ptr %cur_1, i64 1, ptr %one_elem3)
      br label %apply_step_2

    apply_step_2:                                     ; preds = %apply_step_1
      %cur_2 = phi ptr [ %apply_step_14, %apply_step_1 ]
      %apply_one5 = alloca [1 x ptr], align 8
      %one_elem6 = getelementptr [1 x ptr], ptr %apply_one5, i32 0, i32 0
      store ptr inttoptr (i64 3 to ptr), ptr %one_elem6, align 8
      %apply_step_27 = call ptr @eml_applyN(ptr %cur_2, i64 1, ptr %one_elem6)
      br label %apply_step_3

    apply_step_3:                                     ; preds = %apply_step_2
      %cur_3 = phi ptr [ %apply_step_27, %apply_step_2 ]
      %apply_one8 = alloca [1 x ptr], align 8
      %one_elem9 = getelementptr [1 x ptr], ptr %apply_one8, i32 0, i32 0
      store ptr inttoptr (i64 3 to ptr), ptr %one_elem9, align 8
      %apply_step_310 = call ptr @eml_applyN(ptr %cur_3, i64 1, ptr %one_elem9)
      br label %merge_0

    merge_1:                                          ; preds = %apply_step_112
      %apply_result20 = phi ptr [ %apply_step_119, %apply_step_112 ]
      br label %apply_step_021

    apply_step_011:                                   ; preds = %merge_0
      %apply_one13 = alloca [1 x ptr], align 8
      %one_elem14 = getelementptr [1 x ptr], ptr %apply_one13, i32 0, i32 0
      store ptr inttoptr (i64 3 to ptr), ptr %one_elem14, align 8
      %apply_step_015 = call ptr @eml_applyN(ptr %apply_result, i64 1, ptr %one_elem14)
      br label %apply_step_112

    apply_step_112:                                   ; preds = %apply_step_011
      %cur_116 = phi ptr [ %apply_step_015, %apply_step_011 ]
      %apply_one17 = alloca [1 x ptr], align 8
      %one_elem18 = getelementptr [1 x ptr], ptr %apply_one17, i32 0, i32 0
      store ptr inttoptr (i64 3 to ptr), ptr %one_elem18, align 8
      %apply_step_119 = call ptr @eml_applyN(ptr %cur_116, i64 1, ptr %one_elem18)
      br label %merge_1

    merge_2:                                          ; preds = %apply_step_122
      %apply_result30 = phi ptr [ %apply_step_129, %apply_step_122 ]
      %print_int_arg = ptrtoint ptr %apply_result30 to i64
      call void @print_int(i64 %print_int_arg)
      ret ptr inttoptr (i64 1 to ptr)

    apply_step_021:                                   ; preds = %merge_1
      %apply_one23 = alloca [1 x ptr], align 8
      %one_elem24 = getelementptr [1 x ptr], ptr %apply_one23, i32 0, i32 0
      store ptr inttoptr (i64 3 to ptr), ptr %one_elem24, align 8
      %apply_step_025 = call ptr @eml_applyN(ptr %apply_result20, i64 1, ptr %one_elem24)
      br label %apply_step_122

    apply_step_122:                                   ; preds = %apply_step_021
      %cur_126 = phi ptr [ %apply_step_025, %apply_step_021 ]
      %apply_one27 = alloca [1 x ptr], align 8
      %one_elem28 = getelementptr [1 x ptr], ptr %apply_one27, i32 0, i32 0
      store ptr inttoptr (i64 3 to ptr), ptr %one_elem28, align 8
      %apply_step_129 = call ptr @eml_applyN(ptr %cur_126, i64 1, ptr %one_elem28)
      br label %merge_2
    }

    attributes #0 = { nocallback nofree nosync nounwind willreturn memory(none) } |}]
;;

let%expect_test "custom op cat" =
  compile_llvm_show {|let ( =^.^= ) x y = x - y|};
  [%expect
    {|
    ; ModuleID = 'EML'
    source_filename = "EML"

    declare ptr @eml_applyN(ptr, i64, ptr)

    declare ptr @create_tuple(i64, ptr)

    declare ptr @alloc_closure(ptr, i64)

    declare ptr @field(ptr, i64)

    declare ptr @llvm_call_indirect(ptr, ptr, i64)

    declare void @print_int(i64)

    declare void @init_gc()

    declare void @destroy_gc()

    declare void @set_ptr_stack(ptr)

    declare i64 @get_heap_start()

    declare i64 @get_heap_final()

    declare ptr @collect()

    declare ptr @print_gc_status()

    ; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
    declare ptr @llvm.frameaddress.p0(i32 immarg) #0

    define ptr @op__eq_hat_dot_hat_eq(ptr %x, ptr %y) {
    entry:
      %raw_int = ptrtoint ptr %x to i64
      %minus1 = sub i64 %raw_int, 1
      %untagged = sdiv i64 %minus1, 2
      %raw_int1 = ptrtoint ptr %y to i64
      %minus12 = sub i64 %raw_int1, 1
      %untagged3 = sdiv i64 %minus12, 2
      %sub = sub i64 %untagged, %untagged3
      %twice = mul i64 %sub, 2
      %tagged = add i64 %twice, 1
      %result_int = inttoptr i64 %tagged to ptr
      ret ptr %result_int
    }

    define ptr @main() {
    entry:
      ret ptr inttoptr (i64 1 to ptr)
    }

    attributes #0 = { nocallback nofree nosync nounwind willreturn memory(none) }
  |}]
;;

let%expect_test "custom op pipe" =
  compile_llvm_show {|let ( ~> ) x f = f x|};
  [%expect
    {|
    ; ModuleID = 'EML'
    source_filename = "EML"

    declare ptr @eml_applyN(ptr, i64, ptr)

    declare ptr @create_tuple(i64, ptr)

    declare ptr @alloc_closure(ptr, i64)

    declare ptr @field(ptr, i64)

    declare ptr @llvm_call_indirect(ptr, ptr, i64)

    declare void @print_int(i64)

    declare void @init_gc()

    declare void @destroy_gc()

    declare void @set_ptr_stack(ptr)

    declare i64 @get_heap_start()

    declare i64 @get_heap_final()

    declare ptr @collect()

    declare ptr @print_gc_status()

    ; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
    declare ptr @llvm.frameaddress.p0(i32 immarg) #0

    define ptr @op__tilde_gt(ptr %x, ptr %f) {
    entry:
      br label %apply_step_0

    merge_0:                                          ; preds = %apply_step_0
      %apply_result = phi ptr [ %apply_step_01, %apply_step_0 ]
      ret ptr %apply_result

    apply_step_0:                                     ; preds = %entry
      %apply_one = alloca [1 x ptr], align 8
      %one_elem = getelementptr [1 x ptr], ptr %apply_one, i32 0, i32 0
      store ptr %x, ptr %one_elem, align 8
      %apply_step_01 = call ptr @eml_applyN(ptr %f, i64 1, ptr %one_elem)
      br label %merge_0
    }

    define ptr @main() {
    entry:
      ret ptr inttoptr (i64 1 to ptr)
    }

    attributes #0 = { nocallback nofree nosync nounwind willreturn memory(none) }
  |}]
;;
