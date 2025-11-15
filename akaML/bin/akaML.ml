[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Stdio

type opts =
  { mutable dump_parsetree : bool
  ; mutable inference : bool
  ; mutable anf : bool
  ; mutable gc : bool
  ; mutable input_file : string option
  ; mutable output_file : string option
  }

let pp_global_error ppf = function
  | #Inferencer.error as e -> Inferencer.pp_error ppf e
;;

let compiler options =
  let run text env_infer out_channel =
    let ast = Parser.parse text in
    match ast with
    | Error error ->
      Out_channel.output_string out_channel (Format.asprintf "Parsing error: %s\n" error);
      env_infer
    | Ok ast ->
      if options.dump_parsetree
      then (
        Out_channel.output_string out_channel (Ast.show_structure ast ^ "\n");
        env_infer)
      else (
        match Inferencer.run_inferencer env_infer ast with
        | Error e_infer ->
          Out_channel.output_string
            out_channel
            (Format.asprintf "Inferencer error: %a\n" pp_global_error e_infer);
          env_infer
        | Ok (env_infer, out_infer_list) ->
          if options.inference
          then (
            Base.List.iter out_infer_list ~f:(function
              | Some id, type' ->
                Out_channel.output_string
                  out_channel
                  (Format.asprintf "val %s : %a\n" id Pprinter.pp_core_type type')
              | None, type' ->
                Out_channel.output_string
                  out_channel
                  (Format.asprintf "- : %a\n" Pprinter.pp_core_type type'));
            env_infer)
          else (
            let ppf = Format.formatter_of_out_channel out_channel in
            let cc_ast =
              Middleend.Lambda_lifting.lambda_lifting
                (Middleend.Closure_conversion.closure_conversion ast)
            in
            match Middleend.Anf_core.anf_structure cc_ast with
            | Error e_anf ->
              Format.eprintf "ANF transformation error: %s\n%!" e_anf;
              env_infer
            | Ok anf_ast ->
              if options.anf
              then (
                Out_channel.output_string
                  out_channel
                  (Format.asprintf "%a\n" Middleend.Anf_pprinter.pp_a_structure anf_ast);
                env_infer)
              else (
                Format.fprintf
                  ppf
                  "%a\n%!"
                  (RiscV.Codegen.gen_a_structure ~enable_gc:options.gc)
                  anf_ast;
                env_infer)))
  in
  let env_infer = Inferencer.env_with_print_funs_and_gc in
  let match_output_file input =
    match options.output_file with
    | Some out_name ->
      Out_channel.with_file out_name ~f:(fun oc ->
        let (_ : Inferencer.TypeEnv.t) = run input env_infer oc in
        ())
    | None ->
      let (_ : Inferencer.TypeEnv.t) = run input env_infer Out_channel.stdout in
      ()
  in
  match options.input_file with
  | Some file_name ->
    let text = In_channel.read_all file_name |> String.trim in
    match_output_file text
  | None ->
    let input = In_channel.input_all stdin |> String.trim in
    match_output_file input
;;

let () =
  let options =
    { dump_parsetree = false
    ; inference = false
    ; anf = false
    ; gc = false
    ; input_file = None
    ; output_file = None
    }
  in
  let () =
    let open Arg in
    parse
      [ ( "-dparsetree"
        , Unit (fun () -> options.dump_parsetree <- true)
        , "Dump parse tree, don't evaluate anything" )
      ; ( "-inference"
        , Unit (fun () -> options.inference <- true)
        , "Inference, don't evaluate anything" )
      ; ( "-anf"
        , Unit (fun () -> options.anf <- true)
        , "Show programm after anf, don't evaluate anything" )
      ; ( "-gc"
        , Unit (fun () -> options.gc <- true)
        , "Use garbage collector, don't evaluate anything" )
      ; ( "-fromfile"
        , String (fun filename -> options.input_file <- Some filename)
        , "Read code from the file" )
      ; ( "-o"
        , String (fun filename -> options.output_file <- Some filename)
        , "Write code to the file" )
      ]
      (fun _ ->
         Format.eprintf "Positional arguments are not supported\n";
         exit 1)
      "Compiler for custom language"
  in
  compiler options
;;
