(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OMLet.Ast
open OMLet.Parser
open OMLet.Codegen
open OMLet.CodegenTypes
open OMLet.Anf
open OMLet.AnfPrettyPrinter
open OMLet.ResultCounter.ResultCounterMonad
open Base
open Stdio

type stop_after =
  | SA_parsing
  | SA_never

type opts =
  { mutable dump_parsetree : bool
  ; mutable dump_anf : bool
  ; mutable stop_after : stop_after
  ; mutable input_file : string option
  }

let eval ast =
  let _ = show_constructions ast in
  ()
;;

let run_single dump_parsetree dump_anf stop_after eval input_source =
  let text =
    match input_source with
    | Some file_name -> In_channel.read_all file_name |> Stdlib.String.trim
    | None -> In_channel.input_all stdin |> Stdlib.String.trim
  in
  match parse text with
  | Error e -> Stdlib.Format.printf "Parsing error: %s\n%!" e
  | Ok ast ->
    if dump_parsetree
    then (
      print_endline (show_constructions ast);
      ())
    else (
      match run (anf_constructions ast) 0 with
      | Result.Error e -> Stdlib.Format.printf "%a@." pp_anf_error e
      | Result.Ok (anf, _) ->
        if dump_anf
        then (
          Stdlib.Format.printf "%a@." pp_aconstructions anf;
          ())
        else (
          let () =
            match codegen_program anf with
            | Error e -> Stdlib.Format.printf "Codegen error: %s\n%!" e
            | Ok (_, instructions) ->
              let () =
                Stdlib.Format.fprintf Stdlib.Format.std_formatter ".global _start\n"
              in
              Stdlib.List.iter pp_instr instructions
          in
          match stop_after with
          | SA_parsing -> ()
          | SA_never -> eval ast))
;;

let () =
  let opts =
    { dump_parsetree = false; dump_anf = false; stop_after = SA_never; input_file = None }
  in
  let () =
    Stdlib.Arg.parse
      [ ( "-dparsetree"
        , Stdlib.Arg.Unit (fun () -> opts.dump_parsetree <- true)
        , "Dump parse tree, don't evaluate anything" )
      ; ( "-dumpanf"
        , Stdlib.Arg.Unit (fun () -> opts.dump_anf <- true)
        , "Dump ANF representation" )
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
  run_single
    opts.dump_parsetree
    opts.dump_anf
    opts.stop_after
    (fun ast -> eval ast)
    opts.input_file
;;
