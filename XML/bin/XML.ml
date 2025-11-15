(** Copyright 2024-2025, Mikhail Gavrilenko,Danila Rudnev-Stepanyan, Daniel Vlasenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

(* ------------------------------- *)
(*       Command-line Options      *)
(* ------------------------------- *)

type options =
  { mutable input_file_name : string option
  ; mutable from_file_name : string option
  ; mutable output_file_name : string option
  ; mutable show_ast : bool
  ; mutable show_anf : bool
  ; mutable show_cc : bool
  ; mutable show_ll : bool
  ; mutable gc_stats : bool
  }

(* ------------------------------- *)
(*     Compiler Entry Points       *)
(* ------------------------------- *)

let to_asm ~gc_stats ast : string =
  let cc_program = Middleend.Cc.cc_program ast in
  let anf_ast = Middleend.Anf.anf_program cc_program in
  let ll_anf = Middleend.Ll.lambda_lift_program anf_ast in
  let buf = Buffer.create 1024 in
  let ppf = formatter_of_buffer buf in
  Backend.Codegen.gen_program_with_gc_stats ~gc_stats ppf ll_anf;
  pp_print_flush ppf ();
  Buffer.contents buf
;;

let compile_and_write options source_code =
  let ast = Common.Parser.parse_str source_code in
  if options.show_ast
  then (
    printf "%a\n" Common.Pprinter.pprint_program ast;
    exit 0);
  let cc_ast = Middleend.Cc.cc_program ast in
  if options.show_cc
  then (
    printf "%a\n" Common.Pprinter.pprint_program cc_ast;
    exit 0);
  let anf_ast = Middleend.Anf.anf_program cc_ast in
  if options.show_anf
  then (
    Middleend.Pprinter.print_anf_program std_formatter anf_ast;
    exit 0);
  let anf_after_ll = Middleend.Ll.lambda_lift_program anf_ast in
  if options.show_ll
  then (
    Middleend.Pprinter.print_anf_program std_formatter anf_after_ll;
    exit 0);
  let asm_code = to_asm ~gc_stats:options.gc_stats ast in
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

let read_file path =
  try
    let ch = open_in path in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
  with
  | Sys_error msg ->
    eprintf "Error: Could not read input file '%s': %s\n" path msg;
    exit 1
;;

(* ------------------------------- *)
(*           Main Driver           *)
(* ------------------------------- *)

let () =
  let options =
    { input_file_name = None
    ; from_file_name = None
    ; output_file_name = None
    ; show_ast = false
    ; show_anf = false
    ; show_cc = false
    ; show_ll = false
    ; gc_stats = false
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
    ; ( "--cc"
      , Arg.Unit (fun () -> options.show_cc <- true)
      , "         Show the representation after applying CC and exit" )
    ; ( "-fromfile"
      , Arg.String (fun fname -> options.from_file_name <- Some fname)
      , " <file>  Read source from file (preferred over positional arg)" )
    ; ( "--ll"
      , Arg.Unit (fun () -> options.show_ll <- true)
      , "         Show ANF after lambda lifting and exit" )
    ; ( "--gc-stats"
      , Arg.Unit (fun () -> options.gc_stats <- true)
      , "     Enable GC statistics and force a collection at program start/end" )
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
    match options.from_file_name, options.input_file_name with
    | Some path, _ -> read_file path
    | None, Some path -> read_file path
    | None, None -> read_channel_to_string stdin
  in
  compile_and_write options source_code
;;
