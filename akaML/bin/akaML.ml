(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Stdio

type opts =
  { mutable dump_parsetree : bool
  ; mutable inference : bool
  ; mutable input_file : string option
  }

let pp_global_error ppf = function
  | #Inferencer.error as e -> Inferencer.pp_error ppf e
;;

let run_single dump_parsetree inference input_source =
  let run text env_infer =
    let ast = Parser.parse text in
    match ast with
    | Error error ->
      print_endline (Format.asprintf "Parsing error: %s" error);
      env_infer
    | Ok ast ->
      if dump_parsetree
      then (
        print_endline (Ast.show_structure ast);
        env_infer)
      else (
        match Inferencer.run_inferencer env_infer ast with
        | Error e_infer ->
          print_endline (Format.asprintf "Inferencer error: %a" pp_global_error e_infer);
          env_infer
        | Ok (env_infer, out_infer_list) ->
          if inference
          then (
            List.iter
              (function
                | Some id, type' ->
                  print_endline
                    (Format.asprintf "val %s : %a" id Pprinter.pp_core_type type')
                | None, type' ->
                  print_endline (Format.asprintf "- : %a" Pprinter.pp_core_type type'))
              out_infer_list;
            env_infer)
          else (
            print_endline "In progress...";
            env_infer))
  in
  let env_infer = Inferencer.env_with_print_funs in
  match input_source with
  | Some file_name ->
    let text = In_channel.read_all file_name |> String.trim in
    let _ = run text env_infer in
    ()
  | None ->
    let rec input_lines lines env_infer =
      match In_channel.input_line stdin with
      | Some line ->
        if line = ";;" || String.ends_with ~suffix:";;" line
        then (
          let env_infer = run (lines ^ line) env_infer in
          input_lines "" env_infer)
        else input_lines (lines ^ line) env_infer
      | None -> ()
    in
    let _ = input_lines "" env_infer in
    ()
;;

let () =
  let options = { dump_parsetree = false; inference = false; input_file = None } in
  let () =
    let open Arg in
    parse
      [ ( "-dparsetree"
        , Unit (fun () -> options.dump_parsetree <- true)
        , "Dump parse tree, don't evaluate anything" )
      ; ( "-inference"
        , Unit (fun () -> options.inference <- true)
        , "Inference, don't evaluate anything" )
      ; ( "-fromfile"
        , String (fun filename -> options.input_file <- Some filename)
        , "Read code from the file" )
      ]
      (fun _ ->
         Format.eprintf "Positional arguments are not supported\n";
         exit 1)
      "Read-Eval-Print-Loop for custom language"
  in
  run_single options.dump_parsetree options.inference options.input_file
;;
