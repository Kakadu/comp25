(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

(* ------------------------------- *)
(*       Command-line Options      *)
(* ------------------------------- *)

type options =
  { mutable input_file_name : string option
  ; mutable output_file_name : string option
  ; mutable show_ast : bool
  ; mutable show_anf : bool
  }

(* ------------------------------- *)
(*     Compiler Entry Points       *)
(* ------------------------------- *)

let to_asm ast : string =
  let anf_ast = Middleend.Anf.anf_program ast in
  let buf = Buffer.create 1024 in
  let ppf = formatter_of_buffer buf in
  Backend.Codegen.gen_program ppf anf_ast;
  pp_print_flush ppf ();
  Buffer.contents buf
;;

let compile_and_write options source_code =
  let ast = Common.Parser.parse_str source_code in
  if options.show_ast
  then (
    printf "%a\n" Common.Pprinter.pprint_program ast;
    exit 0);
  let anf_ast = Middleend.Anf.anf_program ast in
  if options.show_anf
  then (
    Middleend.Pprinter.print_anf_program std_formatter anf_ast;
    exit 0);
  let asm_code = to_asm ast in
  match options.output_file_name with
  | Some out_file ->
    (try
       let oc = open_out out_file in
       output_string oc asm_code;
       close_out oc
     with
     | Sys_error msg ->
       eprintf "Error: Could not write to output file '%s': %s\n" out_file msg;
       exit 1)
  | None -> print_string asm_code
;;

let read_channel_to_string ic =
  let buf = Buffer.create 1024 in
  try
    while true do
      Buffer.add_string buf (input_line ic ^ "\n")
    done;
    "" (* Недостижимо *)
  with
  | End_of_file -> Buffer.contents buf
;;

(* ------------------------------- *)
(*           Main Driver           *)
(* ------------------------------- *)

let () =
  let options =
    { input_file_name = None
    ; output_file_name = None
    ; show_ast = false
    ; show_anf = false
    }
  in
  let usage_msg =
    "MiniML Compiler\n\n"
    ^ "Usage: dune exec ./bin/compile.exe -- <options> [input_file.ml]\n"
    ^ "If no input file is provided, reads from standard input.\n\n"
    ^ "Options:"
  in
  let arg_specs =
    [ ( "-o"
      , Arg.String (fun fname -> options.output_file_name <- Some fname)
      , " <file>  Set the output file name for the assembly code" )
    ; ( "--ast"
      , Arg.Unit (fun () -> options.show_ast <- true)
      , "         Show the parsed Abstract Syntax Tree and exit" )
    ; ( "--anf"
      , Arg.Unit (fun () -> options.show_anf <- true)
      , "         Show the ANF representation and exit" )
    ]
  in
  let handle_anon_arg filename =
    match options.input_file_name with
    | None -> options.input_file_name <- Some filename
    | Some _ ->
      eprintf "Error: Only one input file is allowed.\n";
      Arg.usage arg_specs usage_msg;
      exit 1
  in
  Arg.parse arg_specs handle_anon_arg usage_msg;
  let source_code =
    match options.input_file_name with
    | Some path ->
      (try
         let ch = open_in path in
         let s = really_input_string ch (in_channel_length ch) in
         close_in ch;
         s
       with
       | Sys_error msg ->
         eprintf "Error: Could not read input file: %s\n" msg;
         exit 1)
    | None -> read_channel_to_string stdin
  in
  compile_and_write options source_code
;;
