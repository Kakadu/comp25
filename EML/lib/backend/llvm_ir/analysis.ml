(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Frontend.Ast
open Middleend.Anf
open Runtime.Primitives

type function_layout =
  { func_name : string
  ; asm_name : string
  ; params : immediate list
  ; body : anf_expr
  ; is_rec : bool
  }

type analysis_result =
  { functions : function_layout list
  ; resolve : int -> string -> (string * int) option
  }

let rec params_of_anf = function
  | AnfExpr (ComplexLambda (pats, body)) ->
    let imms =
      List.filter_map
        (function
          | PatVariable id -> Some (ImmediateVar id)
          | _ -> None)
        pats
    in
    let rest, inner = params_of_anf body in
    imms @ rest, inner
  | other -> [], other
;;

let analyze (program : anf_program) =
  let raw =
    List.filter_map
      (function
        | AnfValue (rec_flag, (func_name, arity, body), _) ->
          let params, body = params_of_anf body in
          Some (func_name, arity, params, body, rec_flag = Rec)
        | AnfEval _ -> None)
      program
  in
  let is_valid_linker_ident name =
    String.length name > 0
    && String.for_all
         (fun c ->
            (c >= 'a' && c <= 'z')
            || (c >= 'A' && c <= 'Z')
            || (c >= '0' && c <= '9')
            || Char.equal c '_')
         name
  in
  let mangle_operator_for_linker name =
    "op_"
    ^ Base.String.concat_map name ~f:(function
      | '*' -> "_star"
      | '+' -> "_plus"
      | '-' -> "_minus"
      | '/' -> "_slash"
      | '=' -> "_eq"
      | '<' -> "_lt"
      | '>' -> "_gt"
      | '!' -> "_bang"
      | '&' -> "_amp"
      | '|' -> "_bar"
      | '^' -> "_hat"
      | '@' -> "_at"
      | '~' -> "_tilde"
      | '?' -> "_q"
      | '.' -> "_dot"
      | ':' -> "_colon"
      | '%' -> "_percent"
      | '$' -> "_dollar"
      | c
        when (c >= 'a' && c <= 'z')
             || (c >= 'A' && c <= 'Z')
             || (c >= '0' && c <= '9')
             || Char.equal c '_' -> String.make 1 c
      | c -> "_u" ^ Int.to_string (Char.code c))
  in
  let mangle_reserved name =
    if is_reserved name
    then "eml_" ^ name
    else if String.equal name "_start"
    then "eml_start"
    else name
  in
  let asm_name_for_func func_name =
    let base =
      if is_valid_linker_ident func_name
      then func_name
      else mangle_operator_for_linker func_name
    in
    mangle_reserved base
  in
  let functions, _ =
    List.fold_left
      (fun (reversed_functions, counts) (func_name, _arity, params, body, is_rec) ->
         let base_asm_name = asm_name_for_func func_name in
         let duplicate_index =
           Base.Map.find counts func_name |> Option.value ~default:0
         in
         let updated_counts =
           Base.Map.set counts ~key:func_name ~data:(duplicate_index + 1)
         in
         let asm_name =
           if duplicate_index = 0
           then base_asm_name
           else base_asm_name ^ "_" ^ Int.to_string duplicate_index
         in
         ( { func_name; asm_name; params; body; is_rec } :: reversed_functions
         , updated_counts ))
      ([], Base.Map.empty (module Base.String))
      raw
  in
  let functions = List.rev functions in
  let has_main =
    List.exists (fun func_layout -> String.equal func_layout.func_name "main") functions
  in
  let functions =
    if has_main
    then functions
    else (
      let synthetic_main =
        { func_name = "main"
        ; asm_name = "main"
        ; params = []
        ; body = AnfExpr (ComplexImmediate (ImmediateConst (ConstInt 0)))
        ; is_rec = false
        }
      in
      functions @ [ synthetic_main ])
  in
  let resolver func_index var_name =
    let rec find i =
      if i < 0
      then None
      else (
        match Base.List.nth functions i with
        | None -> None
        | Some func_layout when String.equal func_layout.func_name var_name ->
          Some (func_layout.asm_name, List.length func_layout.params)
        | Some _ -> find (i - 1))
    in
    let start_index =
      match Base.List.nth functions func_index with
      | Some func_layout
        when func_layout.is_rec && String.equal func_layout.func_name var_name ->
        func_index
      | _ -> func_index - 1
    in
    find start_index
  in
  { functions; resolve = resolver }
;;
