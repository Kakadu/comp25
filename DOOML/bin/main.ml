[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open DOOML
module Map = Base.Map.Poly

let failf fmt = Format.kasprintf failwith fmt

let parse input =
  let code = In_channel.with_open_text input In_channel.input_all in
  match Fe.parse code with
  | Error msg -> Error msg
  | Ok ast_list -> Ok (ast_list |> Cc.cc |> Ll.ll |> Anf.anf)
;;

let () =
  match Array.to_list Sys.argv with
  | [ _exe; input; output ] ->
    let module_ = (match parse input with
     | Error msg -> failf "%s" msg
     | Ok anf_list -> Codegen.emit_ir anf_list) in
    (* Codegen.optimize_ir module_; *)
    Llvm.print_module output module_
  | _ -> exit 1
;;
