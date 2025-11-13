(** Copyright 2025-2026, Rodion Suvorov, Dmitriy Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Codegen
open Format
open Inferencer.Infer
open Inferencer.InferTypes
open Middle.Anf
open Middle.Anf_pp
open Middle.CC
open Middle.LL
open Result

let ( >>= ) = bind
let ( >|> ) f g = fun x -> f x >>= g
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

type aml_pipeline_error =
  | InferError of Inferencer.InferTypes.error
  | AnfError of string
  | CcError of string
  | LlError of string

let pp_aml_pipeline_error = function
  | InferError err -> Format.printf "%a" pp_inf_err err
  | AnfError msg -> Format.eprintf "ANF transform error: %s\n" msg
  | CcError msg -> Format.eprintf "CC transform error: %s\n" msg
  | LlError msg -> Format.eprintf "LL transform error: %s\n" msg
;;

let run_pipeline input_file pipeline on_success =
  let program = Parser.parse_str (read_file input_file) in
  match pipeline program with
  | Ok result -> on_success result
  | Error err -> pp_aml_pipeline_error err
;;

let run_infer program =
  map_error (fun err -> InferError err) (run_infer_program program env_with_things)
  >>= fun _ -> Ok program
;;

let run_anf program = map_error (fun err -> AnfError err) (anf_transform program)
let run_cc program = map_error (fun err -> CcError err) (cc_transform program)
let run_ll program = map_error (fun err -> LlError err) (ll_transform program)
let run_codegen program = Ok (asprintf "%a" codegen program)

let compile input_file output_file =
  run_pipeline
    input_file
    (run_infer >|> run_anf >|> run_cc >|> run_ll >|> run_codegen)
    (fun asm ->
       write_file output_file asm;
       printf "Generated: %s\n" output_file)
;;

let dump_anf input_file =
  run_pipeline input_file (run_infer >|> run_anf) (fun program ->
    pp_anf std_formatter program;
    pp_print_flush std_formatter ())
;;

let dump_cc_anf input_file =
  run_pipeline
    input_file
    (run_infer >|> run_anf >|> run_cc)
    (fun program ->
       pp_anf std_formatter program;
       pp_print_flush std_formatter ())
;;

let dump_cc_ll_anf input_file =
  run_pipeline
    input_file
    (run_infer >|> run_anf >|> run_cc >|> run_ll)
    (fun program ->
       pp_anf std_formatter program;
       pp_print_flush std_formatter ())
;;

let () =
  match Array.to_list Sys.argv with
  | [ _exe; "--dump-anf"; input_file ] -> dump_anf input_file
  | [ _exe; "--dump-cc-anf"; input_file ] -> dump_cc_anf input_file
  | [ _exe; "--dump-cc-ll-anf"; input_file ] -> dump_cc_ll_anf input_file
  | [ _exe; input_file; output_file ] -> compile input_file output_file
  | _ ->
    prerr_endline usage_msg;
    exit 1
;;
