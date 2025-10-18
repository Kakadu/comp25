(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OMLet.Ast
open OMLet.Parser
open OMLet.Codegen
open OMLet.CodegenTypes
open OMLet.Anf
open Base
open Stdio

type stop_after =
  | SA_parsing
  | SA_never

type opts =
  { mutable dump_parsetree : bool
  ; mutable stop_after : stop_after
  ; mutable input_file : string option
  }

let eval ast =
  let _ = show_constructions ast in
  ()
;;

let run_single dump_parsetree stop_after eval input_source =
  let text =
    match input_source with
    | Some file_name -> In_channel.read_all file_name |> Stdlib.String.trim
    | None -> In_channel.input_all stdin |> Stdlib.String.trim
  in
  match parse text with
  | Error e -> Stdlib.Format.printf "Parsing error: %s\n%!" e
  | Ok ast ->
    if dump_parsetree then print_endline (show_constructions ast);
    let anf = anf_constructions ast in
    let instructions = codegen_aconstructions anf in
    (*let instructions = codegen ast in*)
    let _ = Stdlib.Format.fprintf Stdlib.Format.std_formatter ".global _start\n" in
    let _ = Stdlib.List.iter pp_instr instructions in
    (match stop_after with
     | SA_parsing -> ()
     | SA_never -> eval ast)
;;

let () =
  let opts = { dump_parsetree = false; stop_after = SA_never; input_file = None } in
  let () =
    Stdlib.Arg.parse
      [ ( "-dparsetree"
        , Stdlib.Arg.Unit (fun () -> opts.dump_parsetree <- true)
        , "Dump parse tree, don't evaluate anything" )
      ; ( "-stop-after"
        , Stdlib.Arg.String
            (function
              | "parsing" -> opts.stop_after <- SA_parsing
              | _ -> failwith "Bad argument for -stop-after")
        , "Stop after parsing" )
      ; ( "-fromfile"
        , Stdlib.Arg.String (fun filename -> opts.input_file <- Some filename)
        , "Read code from the specified file" )
      ]
      (fun _ ->
         Stdlib.Format.eprintf "Positional arguments are not supported\n";
         Stdlib.exit 1)
      "Read-Eval-Print-Loop for custom language"
  in
  run_single opts.dump_parsetree opts.stop_after (fun ast -> eval ast) opts.input_file
;;
