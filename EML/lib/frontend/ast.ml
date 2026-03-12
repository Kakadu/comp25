(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

type ident = string [@@deriving show { with_path = false }]

type is_rec =
  | NonRec
  | Rec
[@@deriving show { with_path = false }]

type bin_oper =
  | Plus (* [+] *)
  | Minus (* [-] *)
  | Multiply (* [*] *)
  | Division (* [/] *)
  | And (* [&&] *)
  | Or (* [||] *)
  | GreatestEqual (* [>=] *)
  | LowestEqual (* [<=] *)
  | GreaterThan (* [>] *)
  | LowerThan (* [<] *)
  | Equal (* [=] *)
  | NotEqual (* [<>] *)
  | Custom of string (* user-defined: ( ** ), ( @@ ), etc. *)
[@@deriving show { with_path = false }]

type unar_oper =
  | Negative (* [-x] *)
  | Not (* [not x]*)
[@@deriving show { with_path = false }]

type const =
  | ConstInt of int (* Integer constant: Example - [21] *)
  | ConstBool of bool (* Boolean constant: Example - [true] or [false] *)
  | ConstString of string (* String constant: Example - "I like OCaml!" *)
  | ConstChar of char (* Character constant: Example - ['a'] *)
[@@deriving show { with_path = false }]

type binder = int [@@deriving show { with_path = false }]

type ty =
  | TyVar of ident
  | TyPrim of string
  | TyArrow of ty * ty
  | TyList of ty
  | TyTuple of ty list
  | TyOption of ty
[@@deriving show { with_path = false }]

type pattern =
  | PatVariable of ident (* [x] *)
  | PatConst of const (* [21] or [true] or [false] *)
  | PatTuple of pattern * pattern * pattern list (* (x1; x2 ... xn) *)
  | PatAny
  | PatType of pattern * ty
  | PatUnit
  | PatList of pattern list
  | PatOption of pattern option
  | PatConstruct of ident * pattern option
[@@deriving show { with_path = false }]

type expr =
  | ExpIdent of ident (* ExpIdent "x" *)
  | ExpConst of const (* ExpConst (ConstInt 666) *)
  | ExpBranch of expr * expr * expr option
  | ExpBinOper of bin_oper * expr * expr (* ExpBinOper(Plus, 1, 2) *)
  | ExpUnarOper of unar_oper * expr (* ExpUnarOper(not, x)*)
  | ExpTuple of expr * expr * expr list (* ExpTuple[x1; x2 .. xn] *)
  | ExpList of expr list (* ExpList[x1; x2 .. xn] *)
  | ExpLambda of pattern * pattern list * expr
  | ExpTypeAnnotation of expr * ty
  | ExpLet of is_rec * bind * bind list * expr
  | ExpApply of expr * expr
  | ExpOption of expr option
  | ExpFunction of bind * bind list
  | ExpMatch of expr * bind * bind list
  | ExpConstruct of ident * expr option
[@@deriving show { with_path = false }]

and bind = pattern * expr [@@deriving show { with_path = false }]

type structure =
  | SEval of expr
  | SValue of is_rec * bind * bind list
[@@deriving show { with_path = false }]

type program = structure list [@@deriving show { with_path = false }]

let bin_op_list = [ "*"; "/"; "+"; "-"; "^"; ">="; "<="; "<>"; "="; ">"; "<"; "&&"; "||" ]
let builtin_op_list = [ "*"; "/"; "+"; "-"; ">="; "<="; "<>"; "="; ">"; "<"; "&&"; "||" ]

let builtin_op_of_string = function
  | "*" -> Some Multiply
  | "/" -> Some Division
  | "+" -> Some Plus
  | "-" -> Some Minus
  | ">=" -> Some GreatestEqual
  | "<=" -> Some LowestEqual
  | "<>" -> Some NotEqual
  | "=" -> Some Equal
  | ">" -> Some GreaterThan
  | "<" -> Some LowerThan
  | "&&" -> Some And
  | "||" -> Some Or
  | _ -> None
;;

let builtin_op_to_string = function
  | Multiply -> "*"
  | Division -> "/"
  | Plus -> "+"
  | Minus -> "-"
  | GreatestEqual -> ">="
  | LowestEqual -> "<="
  | NotEqual -> "<>"
  | Equal -> "="
  | GreaterThan -> ">"
  | LowerThan -> "<"
  | And -> "&&"
  | Or -> "||"
  | Custom s -> s
;;

let rec pp_ty fmt = function
  | TyPrim x -> fprintf fmt "%s" x
  | TyVar x -> fprintf fmt "%s" x
  | TyArrow (l, r) ->
    (match l, r with
     | TyArrow _, _ -> fprintf fmt "(%a) -> %a" pp_ty l pp_ty r
     | _, _ -> fprintf fmt "%a -> %a" pp_ty l pp_ty r)
  | TyTuple elems ->
    fprintf
      fmt
      "(%a)"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " * ") pp_ty)
      elems
  | TyList ty ->
    (match ty with
     | TyArrow _ | TyTuple _ -> fprintf fmt "(%a) list" pp_ty ty
     | _ -> fprintf fmt "%a list" pp_ty ty)
  | TyOption ty ->
    (match ty with
     | TyArrow _ | TyTuple _ -> fprintf fmt "(%a) option" pp_ty ty
     | _ -> fprintf fmt "%a option" pp_ty ty)
;;
