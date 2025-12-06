open Llvm_wrapper

let spf = Format.asprintf
let failf fmt = Format.kasprintf failwith fmt

let add_builtins (llvm : (module S)) = 
    let open (val llvm) in
    declare_func "print_int" void_type [| i64_type |] |> ignore;
    ()

let emit_ir _ _ =
  let context = Llvm.global_context () in
  let builder = Llvm.builder context in
  let () = assert (Llvm_executionengine.initialize ()) in
  let the_module = Llvm.create_module context "main" in
  Llvm.set_target_triple "x86_64-pc-linux-gnu" the_module;
  let _the_execution_engine = Llvm_executionengine.create the_module in
  let module Llvm_wrapper = (val Llvm_wrapper.make context builder the_module) in
  let llvm = (module Llvm_wrapper : S) in
  add_builtins llvm
;;
