(** Copyright 2024,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Parser
open Angstrom
open Ast
open Stdlib.Format

let get_op_pr id =
  let open Expression in
  match id with
  | Exp_ident "&&" -> 3
  | Exp_ident "||" -> 2
  | Exp_ident ">"
  | Exp_ident "<"
  | Exp_ident ">="
  | Exp_ident "<="
  | Exp_ident "<>"
  | Exp_ident "=" -> 4
  | Exp_ident "+" | Exp_ident "-" -> 5
  | Exp_ident "*" | Exp_ident "/" -> 6
  | Exp_ident name ->
    (match name.[0] with
     | '*' | '/' | '%' -> 6
     | '+' | '-' -> 5
     | '=' | '<' | '>' | '|' | '&' | '$' -> 4
     | _ -> 0)
  | Exp_if (_, _, _) -> 1
  | Exp_let (_, _, _)
  | Exp_match (_, _)
  | Exp_function _
  | Exp_fun (_, _)
  | Exp_constant _ -> 0
  | Exp_apply (_, _) | Exp_construct _ -> 7
  | _ -> 0
;;

let pprint_constant fmt =
  let open Constant in
  function
  | Const_integer n -> fprintf fmt "%d" n
  | Const_char c -> fprintf fmt "'%c'" c
  | Const_string s -> fprintf fmt "%S" s
;;

let rearr_typvars typ =
  let open Base in
  let open TypeExpr in
  let var_counter = ref 0 in
  let rec rename t var_map =
    match t with
    | Type_arrow (t1, t2) ->
      let t1', map1 = rename t1 var_map in
      let t2', map2 = rename t2 map1 in
      Type_arrow (t1', t2'), map2
    | Type_tuple (t1, t2, tl) ->
      let t1', map1 = rename t1 var_map in
      let t2', map2 = rename t2 map1 in
      let ts = tl in
      List.fold_left ts ~init:([], map2) ~f:(fun (acc_ts, acc_map) t_elem ->
        let t_elem', new_map = rename t_elem acc_map in
        t_elem' :: acc_ts, new_map)
      |> fun (rev_ts, final_map) -> Type_tuple (t1', t2', List.rev rev_ts), final_map
    | Type_var tv_ref ->
      (match !tv_ref with
       | Unbound _ -> Type_var tv_ref, var_map
       | Link linked_t -> rename linked_t var_map)
    | Quant_type_var id ->
      (match Map.find var_map id with
       | Some new_id -> Quant_type_var new_id, var_map
       | None ->
         let idx = !var_counter in
         var_counter := idx + 1;
         let new_id =
           if idx < 26
           then String.make 1 (Char.of_int_exn (97 + idx))
           else (
             let prefix_count = (idx / 26) - 1 in
             let suffix_idx = Int.rem idx 26 in
             "'"
             ^ String.make (prefix_count + 1) (Char.of_int_exn (97 + (idx / 26) - 1))
             ^ String.make 1 (Char.of_int_exn (97 + suffix_idx)))
         in
         let new_map = Map.set var_map ~key:id ~data:new_id in
         Quant_type_var new_id, new_map)
    | Type_construct (id, args) ->
      List.fold_left args ~init:([], var_map) ~f:(fun (acc_args, acc_map) arg ->
        let arg', new_map = rename arg acc_map in
        arg' :: acc_args, new_map)
      |> fun (rev_args, final_map) -> Type_construct (id, List.rev rev_args), final_map
  in
  fst (rename typ (Map.empty (module String)))
;;

let rec pprint_type_tuple fmt =
  let open Stdlib.Format in
  let open TypeExpr in
  function
  | [] -> ()
  | [ h ] ->
    (match h with
     | Type_arrow (_, _) -> fprintf fmt "(%a)" pprint_type h
     | _ -> fprintf fmt "%a" pprint_type h)
  | h :: tl ->
    (match h with
     | Type_arrow (_, _) -> fprintf fmt "(%a) * %a" pprint_type h pprint_type_tuple tl
     | _ -> fprintf fmt "%a * %a" pprint_type h pprint_type_tuple tl)

and pprint_type_list_with_parens fmt ty_list =
  let open Stdlib.Format in
  let rec print_types fmt = function
    | [] -> ()
    | [ ty ] -> pprint_type_with_parens_if_tuple fmt ty
    | ty :: rest ->
      fprintf fmt "%a %a" pprint_type_with_parens_if_tuple ty print_types rest
  in
  print_types fmt ty_list

and pprint_type fmt typ =
  let open TypeExpr in
  let rec is_arrow = function
    | Type_arrow _ -> true
    | Type_var { contents = Link t } -> is_arrow t
    | _ -> false
  in
  let rec is_tuple = function
    | Type_tuple _ -> true
    | Type_var { contents = Link t } -> is_tuple t
    | _ -> false
  in
  let open Stdlib.Format in
  match typ with
  | Type_arrow (t1, t2) when is_arrow t1 ->
    fprintf fmt "(%a) -> %a" pprint_type t1 pprint_type t2
  | Type_arrow (t1, t2) -> fprintf fmt "%a -> %a" pprint_type t1 pprint_type t2
  | Type_tuple (t1, t2, tl) ->
    fprintf
      fmt
      "%s"
      (Base.String.concat
         ~sep:" * "
         (List.map
            ~f:(fun t ->
              if is_tuple t || is_arrow t
              then asprintf "(%a)" pprint_type t
              else asprintf "%a" pprint_type t)
            (t1 :: t2 :: tl)))
  | Type_var { contents = Unbound (id, _) } -> fprintf fmt "'%s" id
  | Type_var { contents = Link t } -> pprint_type fmt t
  | Quant_type_var id -> fprintf fmt "'%s" id
  | Type_construct (name, []) -> fprintf fmt "%s" name
  | Type_construct (name, ty_list) ->
    fprintf fmt "%a %s" pprint_type_list_with_parens ty_list name

and pprint_type_with_parens_if_tuple fmt ty =
  let open Stdlib.Format in
  match ty with
  | Type_tuple _ -> fprintf fmt "(%a)" pprint_type ty
  | _ -> pprint_type fmt ty
;;

let filter_env (env : (ident * TypeExpr.t) list) (names : ident list) =
  List.fold_left
    ~f:(fun acc name ->
      match Stdlib.List.assoc_opt name env, Stdlib.List.assoc_opt name acc with
      | Some ty, None -> (name, ty) :: acc
      | _ -> acc)
    ~init:[]
    names
;;

let pprint_env env names =
  let open Stdlib.Format in
  let new_env = filter_env env names in
  List.iter
    ~f:(fun (key, typ) ->
      if
        String.length key > 0
        && Stdlib.Char.code key.[0] >= 65
        && Stdlib.Char.code key.[0] <= 90
      then ()
      else if String.equal key "-"
      then printf "%s : %a\n" key pprint_type typ
      else (
        let typ = rearr_typvars typ in
        printf "val %s : %a\n" key pprint_type typ))
    new_env
;;

let rec pprint_pattern fmt =
  let open Pattern in
  function
  | Pat_constraint (p, tye) -> fprintf fmt "(%a : %a)" pprint_pattern p pprint_type tye
  | Pat_any -> fprintf fmt "_"
  | Pat_var id when is_operator_char id.[0] -> fprintf fmt "(%s)" id
  | Pat_var id -> fprintf fmt "%s" id
  | Pat_constant c -> pprint_constant fmt c
  | Pat_tuple (p1, p2, pl) ->
    fprintf
      fmt
      "(%s)"
      (String.concat
         ~sep:", "
         (List.map (p1 :: p2 :: pl) ~f:(fun p -> asprintf "%a" pprint_pattern p)))
  | Pat_construct (id, None) -> fprintf fmt "(%s)" id
  | Pat_construct (id, Some p) ->
    (match p with
     | Pat_tuple _ -> fprintf fmt "(%s (%a))" id pprint_pattern p
     | _ -> fprintf fmt "%s %a" id pprint_pattern p)
;;

let pprint_rec fmt =
  let open Expression in
  function
  | Nonrecursive -> fprintf fmt ""
  | Recursive -> fprintf fmt "rec "
;;

let rec pprint_expression fmt n =
  let open Expression in
  function
  | Exp_ident id -> fprintf fmt "%s" id
  | Exp_constant ct -> pprint_constant fmt ct
  | Exp_tuple (ex1, ex2, exl) ->
    fprintf
      fmt
      "(%s)"
      (String.concat
         ~sep:", "
         (List.map (ex1 :: ex2 :: exl) ~f:(fun ex ->
            let op_pr_t = get_op_pr ex in
            asprintf "%a" (fun fmt -> pprint_expression fmt (op_pr_t + 1)) ex)))
  | Exp_function (cs1, csl) when n > 0 ->
    fprintf fmt "(%a)" pprint_function_with_cases (cs1, csl, n + 1)
  | Exp_function (cs1, csl) ->
    fprintf fmt "%a" pprint_function_with_cases (cs1, csl, n + 1)
  | Exp_fun ((pt1, ptl), exp) ->
    let if_string =
      asprintf
        "fun%s -> %a"
        (String.concat
           ~sep:""
           (List.map (pt1 :: ptl) ~f:(fun p -> asprintf " %a" pprint_pattern p)))
        (fun fmt -> pprint_expression fmt n)
        exp
    in
    if n > 0 then fprintf fmt "(%s)" if_string else fprintf fmt "%s" if_string
  | Exp_apply (Exp_apply (id, first), second) ->
    let op_pr = get_op_pr id in
    let format_apply =
      match id, second with
      | Exp_ident op, _ when is_operator_char op.[0] ->
        (* Binary operator case *)
        let left_pr, right_pr =
          if List.mem [ 2; 3 ] op_pr ~equal:Int.equal
          then op_pr + 1, op_pr
          else op_pr, op_pr + 1
        in
        asprintf
          "%a %s %a"
          (fun fmt -> pprint_expression fmt left_pr)
          first
          op
          (fun fmt -> pprint_expression fmt right_pr)
          second
      | _ ->
        (* Not a binary operator - regular function application *)
        asprintf
          "%a %a"
          (fun fmt -> pprint_expression fmt (op_pr + 1))
          (Exp_apply (id, first))
          (fun fmt -> pprint_expression fmt (op_pr + 1))
          second
    in
    if n > op_pr then fprintf fmt "(%s)" format_apply else fprintf fmt "%s" format_apply
  | Exp_apply (f, arg) ->
    (* Handle other application cases *)
    let op_pr = get_op_pr f in
    let format_apply =
      asprintf
        "%a %a"
        (fun fmt -> pprint_expression fmt (op_pr + 1))
        f
        (fun fmt -> pprint_expression fmt (op_pr + 1))
        arg
    in
    if n > op_pr then fprintf fmt "(%s)" format_apply else fprintf fmt "%s" format_apply
  | Exp_match (ex, (cs, csl)) ->
    let op_pr1 = get_op_pr ex in
    let match_string =
      asprintf
        "match %a with\n  | %s"
        (fun fmt -> pprint_expression fmt (op_pr1 + 1))
        ex
        (String.concat
           ~sep:"\n  | "
           (List.map (cs :: csl) ~f:(fun cs ->
              asprintf "%a" (fun fmt -> pprint_case fmt n) cs)))
    in
    if n > 0 then fprintf fmt "(%s)" match_string else fprintf fmt "%s" match_string
  | Exp_constraint (ex, tye) ->
    fprintf fmt "(%a : %a)" (fun fmt -> pprint_expression fmt (n + 1)) ex pprint_type tye
  | Exp_if (ex1, ex2, None) ->
    let if_string =
      asprintf
        "if %a\n  then %a"
        (fun fmt -> pprint_expression fmt (n + 1))
        ex1
        (fun fmt -> pprint_expression fmt (n + 1))
        ex2
    in
    if n > 0 then fprintf fmt "(%s)" if_string else fprintf fmt "%s" if_string
  | Exp_if (ex1, ex2, Some ex3) ->
    let if_string =
      asprintf
        "if %a\n  then %a\n  else %a"
        (fun fmt -> pprint_expression fmt (n + 1))
        ex1
        (fun fmt -> pprint_expression fmt (n + 1))
        ex2
        (fun fmt -> pprint_expression fmt (n + 1))
        ex3
    in
    if n > 0 then fprintf fmt "(%s)" if_string else fprintf fmt "%s" if_string
  | Exp_let (rec_fl, (vbind1, vbindl), ex) ->
    let let_string =
      asprintf
        "let %a%s in %a"
        pprint_rec
        rec_fl
        (String.concat
           ~sep:" and "
           (List.map (vbind1 :: vbindl) ~f:(fun vb ->
              asprintf "%a" (fun fmt -> pprint_value_binding fmt n) vb)))
        (fun fmt -> pprint_expression fmt (n + 1))
        ex
    in
    if n > 0 then fprintf fmt "(%s)" let_string else fprintf fmt "%s" let_string
  | Exp_construct (id, None) -> fprintf fmt "(%s)" id
  | Exp_construct (id, Some exp) ->
    fprintf fmt "(%s (%a))" id (fun fmt -> pprint_expression fmt (n + 1)) exp

and pprint_value_binding fmt n vb =
  let open Expression in
  fprintf
    fmt
    "%a = %a"
    pprint_pattern
    vb.pat
    (fun fmt -> pprint_expression fmt (n + 1))
    vb.expr

and pprint_case fmt n case =
  let open Expression in
  fprintf
    fmt
    "%a -> %a"
    pprint_pattern
    case.first
    (fun fmt -> pprint_expression fmt (n + 1))
    case.second

and pprint_function_with_cases fmt (cs, csl, n) =
  fprintf
    fmt
    "function %s"
    (String.concat
       (List.map (cs :: csl) ~f:(fun c ->
          asprintf "\n  | %a" (fun fmt -> pprint_case fmt n) c)))
;;

let pprint_structure_item fmt n =
  let open Structure in
  function
  | Str_eval exp -> fprintf fmt "%a ;;\n" (fun fmt -> pprint_expression fmt n) exp
  | Str_value (rec_flag, (vbind1, vbindl)) ->
    let bindings_str =
      match vbind1 :: vbindl with
      | [] -> ""
      | _ ->
        String.concat
          ~sep:" and\n  "
          (List.map (vbind1 :: vbindl) ~f:(fun vb ->
             asprintf "%a" (fun fmt -> pprint_value_binding fmt n) vb))
    in
    fprintf fmt "let %a%s;;\n\n" pprint_rec rec_flag bindings_str
;;

let pprint_program fmt = List.iter ~f:(pprint_structure_item fmt 0)

let pp printer parser str =
  match parse_string ~consume:Angstrom.Consume.All parser str with
  | Ok res -> printer std_formatter res
  | Error _ -> Stdio.print_endline "Syntax error"
;;
