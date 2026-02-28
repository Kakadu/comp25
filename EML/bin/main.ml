(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EML_lib.Frontend.Parser
open EML_lib.Middleend.Anf
open EML_lib.Backend.Ricsv.Runner

let compile_to_asm src out_ppf =
  match parse src with
  | Error e ->
    Printf.eprintf "Parse error: %s\n%!" e;
    exit 1
  | Ok ast ->
    (match anf_program ast with
     | Error e ->
       Printf.eprintf "ANF error: %s\n%!" e;
       exit 1
     | Ok anf -> gen_program out_ppf anf)
;;

let () =
  let args = Sys.argv in
  let nargs = Array.length args in
  (* Parse command line: [EML.exe] [-fromfile <src>] [-o <dst>] *)
  let src_file = ref None in
  let out_file = ref None in
  let i = ref 1 in
  while !i < nargs do
    (match args.(!i) with
     | "-fromfile" ->
       incr i;
       if !i < nargs then src_file := Some args.(!i)
     | "-o" ->
       incr i;
       if !i < nargs then out_file := Some args.(!i)
     | _ -> ());
    incr i
  done;
  let src =
    match !src_file with
    | Some path -> In_channel.(with_open_text path input_all)
    | None -> In_channel.input_all stdin
  in
  match !out_file with
  | None -> compile_to_asm src Format.std_formatter
  | Some path ->
    let oc = open_out path in
    let ppf = Format.formatter_of_out_channel oc in
    compile_to_asm src ppf;
    Format.pp_print_flush ppf ();
    close_out oc
;;
