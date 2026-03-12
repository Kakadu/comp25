(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Frontend.Ast
open Middleend.Anf
open Runtime.Primitives

let word_size = 8

type function_layout =
  { func_name : string
  ; asm_name : string
  ; params : immediate list
  ; body : anf_expr
  ; is_rec : bool
  ; slots_count : int
  ; max_stack_args : int
  ; max_create_tuple_array_bytes : int
  }

type analysis_result =
  { arity_map : (string, int, Base.String.comparator_witness) Base.Map.t
  ; functions : function_layout list
  ; resolve : int -> string -> (string * int) option
  }

let sum_by f xs = List.fold_left (fun acc x -> acc + f x) 0 xs
let max_by f xs = List.fold_left (fun acc x -> max acc (f x)) 0 xs

let rec slots_in_imm = function
  | ImmediateVar _ | ImmediateConst _ -> 0

and slots_in_cexpr = function
  | ComplexImmediate imm -> slots_in_imm imm
  | ComplexUnit -> 0
  | ComplexBinOper (_, left, right) -> slots_in_imm left + slots_in_imm right
  | ComplexUnarOper (_, imm) -> slots_in_imm imm
  | ComplexTuple (first, second, rest) ->
    let elts = first :: second :: rest in
    List.length elts + sum_by slots_in_imm elts
  | ComplexField (imm, _) -> slots_in_imm imm
  | ComplexList imm_list ->
    let n = List.length imm_list in
    n + sum_by slots_in_imm imm_list
  | ComplexApp (first, second, rest) ->
    (* +1 for curried-call intermediate; +1 per arg for spill_dangerous_args.
       +8 for spill_caller_saved_vars_to_frame at start of every invocation (can spill a0-a7).
       +N when nargs >= 2: margin so partial stays above argv (confirmed: overwrite → eml_applyN gets c=0x3). *)
    let args = first :: second :: rest in
    let argument_count = List.length args in
    let additional_margin = if argument_count >= 2 then 12 else 0 in
    1 + 8 + argument_count + additional_margin + sum_by slots_in_imm args
  | ComplexOption None -> 0
  | ComplexOption (Some imm) -> slots_in_imm imm
  | ComplexLambda (_, body) -> slots_in_anf body
  | ComplexBranch (cond, then_e, else_e) ->
    1 + slots_in_imm cond + slots_in_anf then_e + slots_in_anf else_e

and slots_in_anf = function
  | AnfExpr cexp -> slots_in_cexpr cexp
  | AnfLet (_, _, cexp, cont) -> 1 + slots_in_cexpr cexp + slots_in_anf cont
;;

let rec max_stack_args_cexpr = function
  | ComplexImmediate _ | ComplexUnit -> 0
  | ComplexBinOper (_, left, right) ->
    max (max_stack_args_imm left) (max_stack_args_imm right)
  | ComplexUnarOper (_, imm) -> max_stack_args_imm imm
  | ComplexTuple (first, second, rest) ->
    max_by max_stack_args_imm (first :: second :: rest)
  | ComplexField (imm, _) -> max_stack_args_imm imm
  | ComplexList imm_list -> max_by max_stack_args_imm imm_list
  | ComplexApp (_first, second, rest) ->
    let argument_count = 1 + List.length rest in
    let required_stack_words = argument_count in
    let max_nested_argument_pressure = max_by max_stack_args_imm (second :: rest) in
    max required_stack_words max_nested_argument_pressure
  | ComplexOption None -> 0
  | ComplexOption (Some imm) -> max_stack_args_imm imm
  | ComplexLambda (_, body) -> max_stack_args_anf body
  | ComplexBranch (cond, then_e, else_e) ->
    max
      (max_stack_args_imm cond)
      (max (max_stack_args_anf then_e) (max_stack_args_anf else_e))

and max_stack_args_imm = function
  | ImmediateVar _ | ImmediateConst _ -> 0

and max_stack_args_anf = function
  | AnfExpr cexp -> max_stack_args_cexpr cexp
  | AnfLet (_, _, cexp, cont) -> max (max_stack_args_cexpr cexp) (max_stack_args_anf cont)
;;

let rec max_create_tuple_array_cexpr = function
  | ComplexImmediate _ | ComplexUnit -> 0
  | ComplexBinOper (_, left, right) ->
    max (max_create_tuple_array_imm left) (max_create_tuple_array_imm right)
  | ComplexUnarOper (_, imm) -> max_create_tuple_array_imm imm
  | ComplexTuple (first, second, rest) ->
    let elts = first :: second :: rest in
    let here = List.length elts * word_size in
    max here (max_by max_create_tuple_array_imm elts)
  | ComplexField (imm, _) -> max_create_tuple_array_imm imm
  | ComplexList imm_list ->
    let bytes_per_cons_cell = 2 * word_size in
    let bytes_from_elements = sum_by max_create_tuple_array_imm imm_list in
    (bytes_per_cons_cell * List.length imm_list) + bytes_from_elements
  | ComplexApp (_f, second, rest) -> max_by max_create_tuple_array_imm (second :: rest)
  | ComplexOption None -> 0
  | ComplexOption (Some imm) -> max_create_tuple_array_imm imm
  | ComplexLambda (_, body) -> max_create_tuple_array_anf body
  | ComplexBranch (cond, then_e, else_e) ->
    max
      (max_create_tuple_array_imm cond)
      (max (max_create_tuple_array_anf then_e) (max_create_tuple_array_anf else_e))

and max_create_tuple_array_imm = function
  | ImmediateVar _ | ImmediateConst _ -> 0

and max_create_tuple_array_anf = function
  | AnfExpr cexp -> max_create_tuple_array_cexpr cexp
  | AnfLet (_, _, cexp, cont) ->
    max (max_create_tuple_array_cexpr cexp) (max_create_tuple_array_anf cont)
;;

let rec params_of_anf = function
  | AnfExpr (ComplexLambda (pats, body)) ->
    let imms =
      List.filter_map
        (function
          | PatVariable id -> Some (ImmediateVar id)
          | _ -> None)
        pats
    in
    let remaining_parameters, inner_body = params_of_anf body in
    imms @ remaining_parameters, inner_body
  | other -> [], other
;;

let arity_map_of_program (program : anf_program) =
  let add_function_arity map (function_identifier, arity, _) =
    Base.Map.set map ~key:function_identifier ~data:arity
  in
  List.fold_left
    (fun map -> function
       | AnfValue (_, (function_identifier, arity, _), and_binds) ->
         let map = Base.Map.set map ~key:function_identifier ~data:arity in
         List.fold_left add_function_arity map and_binds
       | _ -> map)
    (Base.Map.empty (module Base.String))
    program
;;

let analyze (program : anf_program) =
  let arity_map = arity_map_of_program program in
  let analyzed_functions_raw =
    List.filter_map
      (function
        | AnfValue (rec_flag, (func_name, arity, body), _) ->
          let params, body = params_of_anf body in
          Some
            ( func_name
            , arity
            , params
            , body
            , rec_flag = Rec
            , slots_in_anf body
            , max_stack_args_anf body
            , max_create_tuple_array_anf body )
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
      (fun (reversed_functions, generated_name_counts)
        ( func_name
        , _arity
        , params
        , body
        , is_rec
        , slots_count
        , max_stack_args
        , max_create_tuple_array_bytes ) ->
         let base_asm_name = asm_name_for_func func_name in
         let duplicate_index =
           Base.Map.find generated_name_counts func_name |> Option.value ~default:0
         in
         let updated_generated_name_counts =
           Base.Map.set generated_name_counts ~key:func_name ~data:(duplicate_index + 1)
         in
         let asm_name =
           if duplicate_index = 0
           then base_asm_name
           else base_asm_name ^ "_" ^ Int.to_string duplicate_index
         in
         ( { func_name
           ; asm_name
           ; params
           ; body
           ; is_rec
           ; slots_count
           ; max_stack_args
           ; max_create_tuple_array_bytes
           }
           :: reversed_functions
         , updated_generated_name_counts ))
      ([], Base.Map.empty (module Base.String))
      analyzed_functions_raw
  in
  let functions = List.rev functions in
  let has_main = List.exists (fun fn -> String.equal fn.func_name "main") functions in
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
        ; slots_count = 0
        ; max_stack_args = 0
        ; max_create_tuple_array_bytes = 0
        }
      in
      functions @ [ synthetic_main ])
  in
  let arity_map =
    if has_main then arity_map else Base.Map.set arity_map ~key:"main" ~data:0
  in
  let resolver current_function_index variable_name =
    let rec find_visible_function = function
      | i when i < 0 -> None
      | i ->
        (match Base.List.nth functions i with
         | None -> None
         | Some fn when String.equal fn.func_name variable_name ->
           Some (fn.asm_name, List.length fn.params)
         | Some _ -> find_visible_function (i - 1))
    in
    let start_index =
      match Base.List.nth functions current_function_index with
      | Some fn when fn.is_rec && String.equal fn.func_name variable_name ->
        current_function_index
      | _ -> current_function_index - 1
    in
    find_visible_function start_index
  in
  { arity_map; functions; resolve = resolver }
;;
