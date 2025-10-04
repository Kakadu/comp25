(** Copyright 2025-2026, Rodion Suvorov, Dmitriy Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Codegen
open Format

let usage_msg = "Usage: AML.exe <input file> <output file>"

let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s
;;

let write_file filename content =
  let oc = open_out filename in
  output_string oc content;
  close_out oc
;;

let parse_args = function
  | [ input; output ] -> input, output
  | _ ->
    prerr_endline usage_msg;
    exit 1
;;

let compile input_file output_file =
  let src = read_file input_file in
  let program = Parser.parse_str src in
  let buf = Buffer.create 1024 in
  let fmt = formatter_of_buffer buf in
  codegen fmt program;
  pp_print_flush fmt ();
  write_file output_file (Buffer.contents buf);
  Printf.printf "Generated: %s\n" output_file
;;

let main input_file output_file =
  let input_file, output_file = parse_args [ input_file; output_file ] in
  compile input_file output_file
;;

let () = main Sys.argv.(1) Sys.argv.(2)
