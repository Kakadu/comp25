[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

let pp_rec_flag ppf = function
  | Recursive -> Format.pp_print_string ppf "Recursive"
  | Nonrecursive -> Format.pp_print_string ppf "Nonrecursive"
;;

let pp_ident ppf id = Format.pp_print_string ppf ("\"" ^ id ^ "\"")

let pp_constant ppf = function
  | Const_char c ->
    Format.pp_print_string ppf ("(Const_char '" ^ Base.String.of_char c ^ "')")
  | Const_string s -> Format.pp_print_string ppf ("(Const_string \"" ^ s ^ "\")")
  | Const_integer i ->
    Format.pp_print_string ppf ("(Const_integer " ^ string_of_int i ^ ")")
;;

let pp_pattern ppf = function
  | Pat_any -> Format.fprintf ppf "Pat_any"
  | Pat_var var -> Format.fprintf ppf "Pat_var %a" pp_ident var
  | Pat_constant const -> Format.fprintf ppf "Pat_constant (%a))" pp_constant const
  | _ -> failwith "pp"
;;

let is_infix_binop = function
  | "=" | "+" | "-" | "*" | "/" | "<" | "<=" | ">" | ">=" -> true
  | _ -> false
;;

(* ANF pattern *)
type a_pat =
  | APat_var of ident
  | APat_constant of constant
[@@deriving show { with_path = false }]

(* Immediate/atom expression *)
type i_exp =
  | IExp_ident of ident
  | IExp_constant of constant
  | IExp_fun of a_pat * a_exp
[@@deriving show { with_path = false }]

(* Computation/complex expression *)
and c_exp =
  | CIExp of i_exp
  | CExp_tuple of i_exp * i_exp * i_exp list
  | CExp_apply of i_exp * i_exp * i_exp list
  | CExp_ifthenelse of c_exp * a_exp * a_exp option
(* | CExp_fun of a_pat * a_pat list * a_exp *)
[@@deriving show { with_path = false }]

(* ANF expression *)
and a_exp =
  | ACExp of c_exp
  | AExp_let of rec_flag * pattern * c_exp * a_exp
[@@deriving show { with_path = false }]

(* ANF value_binding *)
and a_value_binding =
  { pat : a_pat
  ; exp : a_exp
  }
[@@deriving show { with_path = false }]

(* ANF structure_item *)
type a_structure_item =
  | AStruct_eval of a_exp
  | AStruct_value of rec_flag * a_value_binding * a_value_binding list
[@@deriving show { with_path = false }]

type structure = a_structure_item list [@@deriving show { with_path = false }]

(** Generator ident *)
module Style = struct
  open Format

  let pp_comma ppf () = fprintf ppf "@,, "
  let pp_sep ppf () = fprintf ppf " "
  let pp_rec_flag = Pprinter.pp_rec_flag
  let pp_ident = Pprinter.pp_ident
  let pp_constant = Pprinter.pp_constant

  let pp_a_pat ppf = function
    | APat_var var -> pp_ident ppf var
    | APat_constant const -> pp_constant ppf const
  ;;

  let rec pp_i_exp ppf = function
    | IExp_ident id -> pp_ident ppf id
    | IExp_constant const -> pp_constant ppf const
    | IExp_fun (a_pat, a_exp) ->
      fprintf ppf "(fun %a@ ->@ " pp_a_pat a_pat;
      fprintf ppf "%a)" pp_a_exp a_exp

  and pp_c_exp ppf = function
    | CIExp i_exp -> pp_i_exp ppf i_exp
    | CExp_tuple (i_exp1, i_exp2, i_exp_list) ->
      pp_open_hvbox ppf 0;
      fprintf ppf "( ";
      fprintf
        ppf
        "%a@]"
        (pp_print_list ~pp_sep:pp_comma pp_i_exp)
        (i_exp1 :: i_exp2 :: i_exp_list);
      fprintf ppf " )"
    | CExp_apply (i_exp1, i_exp2, i_exp_list) ->
      pp_open_box ppf 2;
      fprintf ppf "%a@]" (pp_print_list ~pp_sep pp_i_exp) (i_exp1 :: i_exp2 :: i_exp_list);
      pp_close_box ppf ()
    | CExp_ifthenelse (c_exp, a_exp, None) ->
      fprintf ppf "(";
      pp_open_box ppf 0;
      fprintf ppf "if %a@ " pp_c_exp c_exp;
      fprintf ppf "@[<v 2>then %a@]" pp_a_exp a_exp;
      fprintf ppf ")";
      pp_close_box ppf ()
    | CExp_ifthenelse (c_exp, a_exp1, Some a_exp2) ->
      fprintf ppf "(";
      pp_open_box ppf 0;
      fprintf ppf "if %a@ " pp_c_exp c_exp;
      fprintf ppf "@[<v 2>then %a@]@ " pp_a_exp a_exp1;
      fprintf ppf "@[<v 2>else %a@]" pp_a_exp a_exp2;
      fprintf ppf ")";
      pp_close_box ppf ()

  and pp_a_exp ppf = function
    | ACExp c_exp -> pp_c_exp ppf c_exp
    | AExp_let (rec_flag, pat, c_exp, a_exp) ->
      fprintf ppf "(";
      pp_open_hvbox ppf 0;
      pp_rec_flag ppf rec_flag;
      fprintf ppf "%a =@]@ " Pprinter.pp_pattern pat;
      fprintf ppf "@[<hv>%a@]@]" pp_c_exp c_exp;
      fprintf ppf " in@ %a" pp_a_exp a_exp;
      fprintf ppf ")"

  and pp_value_binding ppf =
    pp_open_hvbox ppf 0;
    function
    | { pat; exp } ->
      fprintf ppf "%a =@]@ " pp_a_pat pat;
      fprintf ppf "@[<hv>%a@]@]" pp_a_exp exp
  ;;

  let pp_structure_item ppf = function
    | AStruct_eval a_exp ->
      fprintf ppf "@[<hv>%a@];;" pp_a_exp a_exp;
      pp_print_flush ppf ()
    | AStruct_value (rec_flag, vb, []) ->
      pp_rec_flag ppf rec_flag;
      pp_value_binding ppf vb;
      pp_print_if_newline ppf ();
      pp_print_cut ppf ();
      fprintf ppf ";;";
      pp_print_flush ppf ()
    | _ -> failwith "Not implemented"
  ;;

  let pp_structure ppf ast =
    if Base.List.is_empty ast
    then fprintf ppf ";;"
    else
      fprintf ppf "@[%a@]" (pp_print_list ~pp_sep:pp_force_newline pp_structure_item) ast;
    pp_print_flush ppf ()
  ;;
end

let reset_gen_id, gen_id =
  let n = ref 0 in
  ( (fun () -> n := 0)
  , fun () ->
      incr n;
      !n )
;;

let gen_id_str : _ =
  fun ?(prefix = "temp") () ->
  let n = gen_id () in
  Printf.sprintf "%s%d" prefix n
;;

let gen_ident ?(prefix = "temp") () = gen_id_str ~prefix ()

let anf_pat = function
  | Pat_var var -> APat_var var
  | Pat_constant const -> APat_constant const
  | _ -> failwith "Pat: Not implemented"
;;

let i_to_c_exp i_exp = CIExp i_exp
let i_to_a_exp i_exp = ACExp (i_to_c_exp i_exp)

let a_exp_let_non c_exp k =
  let id = gen_ident () in
  AExp_let (Nonrecursive, Pat_var id, c_exp, k @@ IExp_ident id)
;;

let anf_exp exp (exp_with_hole : i_exp -> a_exp) =
  let rec f exp k =
    match exp with
    | Expression.Exp_ident id -> k @@ IExp_ident id
    | Exp_constant const -> k @@ IExp_constant const
    | Exp_apply
        (Exp_apply (Exp_ident opr, (Exp_constant _ as exp1)), (Exp_constant _ as exp2))
      when is_infix_binop opr ->
      f exp1 (fun i_exp1 ->
        f exp2 (fun i_exp2 ->
          let c_exp = CExp_apply (IExp_ident opr, i_exp1, [ i_exp2 ]) in
          a_exp_let_non c_exp k))
    | Exp_apply (Exp_ident opr, (Exp_constant _ as exp)) when is_unary_minus opr ->
      f exp (fun i_exp ->
        let c_exp = CExp_apply (IExp_ident opr, i_exp, []) in
        a_exp_let_non c_exp k)
    | Exp_apply (exp1, exp2) ->
      f exp1 (fun i_exp1 ->
        f exp2 (fun i_exp2 ->
          let c_exp = CExp_apply (i_exp1, i_exp2, []) in
          a_exp_let_non c_exp k))
    | Exp_ifthenelse (if_exp, then_exp, None) ->
      f if_exp (fun i_if_exp ->
        let c_exp = CExp_ifthenelse (i_to_c_exp i_if_exp, f then_exp i_to_a_exp, None) in
        a_exp_let_non c_exp k)
    | Exp_ifthenelse (if_exp, then_exp, Some else_exp) ->
      f if_exp (fun i_if_exp ->
        let c_exp =
          CExp_ifthenelse
            (i_to_c_exp i_if_exp, f then_exp i_to_a_exp, Some (f else_exp i_to_a_exp))
        in
        a_exp_let_non c_exp k)
    | Exp_tuple (exp1, exp2, []) ->
      f exp1 (fun i_exp1 ->
        f exp2 (fun i_exp2 ->
          let c_exp = CExp_tuple (i_exp1, i_exp2, []) in
          a_exp_let_non c_exp k))
    | Exp_fun (pat, [], exp) ->
      i_to_a_exp
      @@ IExp_fun (anf_pat pat, f exp (fun i_exp -> a_exp_let_non (i_to_c_exp i_exp) k))
    | _ -> failwith "Exp: Not implemented"
  in
  f exp exp_with_hole
;;

let anf_value_binding (vb : Expression.value_binding_exp) =
  { pat = anf_pat vb.pat; exp = anf_exp vb.exp i_to_a_exp }
;;

let anf_structure_item = function
  | Struct_eval exp -> AStruct_eval (anf_exp exp i_to_a_exp)
  | Struct_value (rec_flag, vb, vbs) ->
    AStruct_value (rec_flag, anf_value_binding vb, List.map anf_value_binding vbs)
;;

let anf_structure = List.map anf_structure_item
