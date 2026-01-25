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
  | Ok ast_list ->
    Ok
      (ast_list
       |> Cc.cc
       (*|> fun asts ->
   Format.printf
     "CC: %a\n\n"
     (Format.pp_print_list ~pp_sep:Format.pp_print_newline Ast.pp_top_level)
     asts;
   asts*)
       |> Ll.ll
       (*|> fun asts ->
   Format.printf
     "LL: %a\n\n"
     (Format.pp_print_list ~pp_sep:Format.pp_print_newline Ast.pp_top_level)
     asts;
   asts*)
       (*Cc.cc |> Ll.ll *)
       |> Anf.anf
          (*|> fun asts ->
   Format.printf
     "ANF %a\n\n"
     (Format.pp_print_list ~pp_sep:Format.pp_print_newline Anf.pp_decl)
     asts;
   asts
  *)
      )
;;

let () =
  match parse Sys.argv.(1) with
  | Error msg -> failf "%s" msg
  | Ok anf_list -> Format.printf "%a\n" Riscv.pp_code (Riscv.riscv anf_list)
;;
