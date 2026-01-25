type ident = string

let pp_ident ppf ident =
  match String.get ident 0 with
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> Format.fprintf ppf "%s" ident
  | _ -> Format.fprintf ppf "(%s)" ident
;;

let pp_sep_space ppf () = Format.fprintf ppf " "
let pp_sep_quote ppf () = Format.fprintf ppf ", "

type ty =
  | Int
  | Bool
  | List of ty
  | Unit
[@@deriving show, variants]

type pattern =
  | PUnit (** () *)
  | Plug (** _ *)
  | Ident of ident
  | PTuple of pattern list
[@@deriving variants]

let rec pp_pattern ppf = function
  | PUnit -> Format.fprintf ppf "()"
  | Plug -> Format.fprintf ppf "_"
  | Ident s -> Format.fprintf ppf "%s" s
  | PTuple ss ->
    Format.fprintf ppf "(%a)" (Format.pp_print_list ~pp_sep:pp_sep_quote pp_pattern) ss
;;

type decl_body = pattern [@@deriving show]

type rec_flag =
  | Rec
  | NonRec
[@@deriving variants]

type const =
  | CInt of int
  | CUnit
[@@deriving variants]

type expr =
  | Const of const
  | Var of ident
  | Tuple of expr list
  | App of expr * expr
  | Let of rec_flag * pattern * expr * expr
  | Ite of expr * expr * expr
  | Fun of pattern list * expr
[@@deriving variants]

let fun_ args = function
  | Fun (args', body') -> fun_ (args @ args') body'
  | body -> fun_ args body
;;

let pp_const ppf = function
  | CInt c -> Format.fprintf ppf "%d" c
  | CUnit -> Format.fprintf ppf "()"
;;

let rec pp_expr ppf = function
  | Const c -> Format.fprintf ppf "%a" pp_const c
  | Var ident -> Format.fprintf ppf "%a" pp_ident ident
  | Tuple exprs ->
    Format.fprintf ppf "(%a)" (Format.pp_print_list ~pp_sep:pp_sep_quote pp_expr) exprs
  | App ((Fun _ as f), arg) -> Format.fprintf ppf "(%a) %a" pp_expr f pp_expr arg
  | App (f, (Const _ as arg)) | App (f, (Var _ as arg)) ->
    Format.fprintf ppf "%a %a" pp_expr f pp_expr arg
  | App (f, arg) -> Format.fprintf ppf "%a (%a)" pp_expr f pp_expr arg
  | Let (rec_flag, pattern, bind, body) ->
    let rec_flag =
      match rec_flag with
      | Rec -> "rec "
      | NonRec -> ""
    in
    Format.fprintf
      ppf
      "let %s%a =@;<1 2>@[<hv>%a@]@;<1 0>in@;<1 0>%a"
      rec_flag
      pp_pattern
      pattern
      pp_expr
      bind
      pp_expr
      body
  | Fun (patterns, body) ->
    Format.fprintf
      ppf
      "fun %a ->@;<1 2>@[<hv>%a@]"
      (Format.pp_print_list ~pp_sep:pp_sep_space pp_pattern)
      patterns
      pp_expr
      body
  | Ite (cond, then_, else_) ->
    Format.fprintf
      ppf
      "if %a then@;<1 2>@[<hv>%a@]@;<1 0>else@;<1 2>@[<hv>%a@]"
      pp_expr
      cond
      pp_expr
      then_
      pp_expr
      else_
;;

type top_level = LetDecl of rec_flag * pattern * expr [@@deriving variants]

let pp_top_level ppf = function
  | LetDecl (rec_flag, pattern, body) ->
    let rec_flag =
      match rec_flag with
      | Rec -> "rec "
      | NonRec -> ""
    in
    Format.fprintf
      ppf
      "@[<hv>let %s%a =@;<1 2>@[<hv>%a@]@;<0 0>;;@]@."
      rec_flag
      pp_pattern
      pattern
      pp_expr
      body
;;
