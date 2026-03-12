(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Angstrom

let is_keyword = function
  | "let"
  | "match"
  | "in"
  | "if"
  | "then"
  | "else"
  | "fun"
  | "rec"
  | "true"
  | "false"
  | "Some"
  | "and"
  | "function"
  | "None"
  | "with" -> true
  | _ -> false
;;

let is_lowercase = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_uppercase = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_operator_char = function
  | '!'
  | '$'
  | '%'
  | '&'
  | '*'
  | '+'
  | '-'
  | '.'
  | '/'
  | ':'
  | '<'
  | '='
  | '>'
  | '?'
  | '@'
  | '^'
  | '|'
  | '~' -> true
  | _ -> false
;;

let is_operator_char_infix = function
  | '!'
  | '$'
  | '%'
  | '&'
  | '*'
  | '+'
  | '-'
  | '.'
  | '/'
  | '<'
  | '='
  | '>'
  | '?'
  | '@'
  | '^'
  | '~' -> true
  | '|' | ':' | _ -> false
;;

let is_custom_power_op op =
  String.length op >= 2 && String.equal (String.sub op ~pos:0 ~len:2) "**"
;;

let first_char op = String.get op 0

let is_custom_mul_op op =
  (not (is_custom_power_op op))
  &&
  match first_char op with
  | '*' | '/' | '%' -> true
  | _ -> false
;;

let is_custom_add_op op =
  match first_char op with
  | '+' | '-' -> true
  | _ -> false
;;

let is_custom_concat_op op =
  match first_char op with
  | '@' | '^' -> true
  | _ -> false
;;

let is_custom_cmp_op op =
  match first_char op with
  | '=' | '<' | '>' | '|' | '&' | '$' -> true
  | _ -> false
;;

let is_custom_lowest_op op =
  match first_char op with
  | '!' | '?' | '~' | '.' -> true
  | _ -> false
;;

let white_space = take_while Char.is_whitespace
let token s = white_space *> string s
let token1 s = white_space *> s
let parse_parens p = token "(" *> p <* token ")"

let is_separator = function
  | ')'
  | '('
  | '<'
  | '>'
  | '@'
  | ','
  | ';'
  | ':'
  | '\\'
  | '"'
  | '/'
  | '['
  | ']'
  | '?'
  | '='
  | '{'
  | '}'
  | ' '
  | '\r'
  | '\t'
  | '\n'
  | '*'
  | '-' -> true
  | _ -> false
;;

let token2 str =
  token str *> peek_char
  >>= function
  | Some c when is_separator c -> return str <* white_space
  | None -> return str <* white_space
  | _ -> fail (Printf.sprintf "There is no separator after %S." str)
;;

let parse_const_int =
  let sign = choice [ token "" ] in
  let num = take_while1 Char.is_digit in
  lift2 (fun s n -> ConstInt (Int.of_string (s ^ n))) sign num
;;

let parse_const_char =
  string "\'" *> any_char <* string "\'" >>| fun char_value -> ConstChar char_value
;;

let parse_const_bool =
  choice
    [ token "true" *> return (ConstBool true); token "false" *> return (ConstBool false) ]
;;

let parse_const_string =
  token "\"" *> take_till (Char.equal '\"') <* token "\"" >>| fun s -> ConstString s
;;

let parse_const =
  white_space
  *> choice [ parse_const_int; parse_const_char; parse_const_string; parse_const_bool ]
;;

let parse_unar_oper = choice [ token "-" *> return Negative; token "not" *> return Not ]

let parse_ident =
  let parse_first_char =
    satisfy (fun ch -> is_lowercase ch || is_uppercase ch || Char.equal ch '_')
    >>| Char.escaped
  in
  let parse_other_chars =
    take_while (fun ch ->
      is_lowercase ch || is_uppercase ch || is_digit ch || Char.equal ch '_')
  in
  let parse_regular_ident =
    token1 @@ lift2 ( ^ ) parse_first_char parse_other_chars
    >>= fun s -> if is_keyword s then fail "It is not identifier" else return s
  in
  let parse_op_ident =
    white_space *> char '(' *> white_space *> take_while1 is_operator_char
    <* white_space
    <* char ')'
  in
  parse_regular_ident <|> parse_op_ident
;;

let parse_base_type =
  choice
    [ token "int" *> return (TyPrim "int")
    ; token "bool" *> return (TyPrim "bool")
    ; token "string" *> return (TyPrim "string")
    ; token "unit" *> return (TyPrim "unit")
    ; token "char" *> return (TyPrim "char")
    ]
;;

let rec parse_type_list t =
  let* base = t in
  white_space
  *> token "list"
  *> (parse_type_list (return (TyList base)) <|> return (TyList base))
;;

let parse_tuple_type parse_type =
  let* fst_type = parse_type in
  let* snd_type = token "*" *> parse_type in
  let* type_list = many (token "*" *> parse_type) in
  return (TyTuple (fst_type :: snd_type :: type_list))
;;

let rec parse_arrow_type parse_type =
  let* type1 = parse_type in
  let* type2 = token "->" *> (parse_arrow_type parse_type <|> parse_type) in
  return (TyArrow (type1, type2))
;;

let parse_type =
  let base_type = parse_base_type in
  let list_type = parse_type_list base_type <|> base_type in
  let tuple_type = parse_tuple_type list_type <|> list_type in
  parse_arrow_type tuple_type <|> tuple_type
;;

let parse_pattern_with_type parse_pattern =
  let* pat = white_space *> token "(" *> parse_pattern in
  let* constr =
    white_space *> token ":" *> white_space *> parse_type <* white_space <* token ")"
  in
  return (PatType (pat, constr))
;;

let parse_pattern_var = parse_ident >>| fun id -> PatVariable id
let parse_pattern_const = parse_const >>| fun c -> PatConst c
let parse_pattern_any = token2 "_" *> return PatAny

let parse_pattern_tuple parse_pattern =
  let parse_unparenthesized =
    lift3
      (fun p1 p2 rest -> PatTuple (p1, p2, rest))
      parse_pattern
      (token "," *> parse_pattern)
      (many (token "," *> parse_pattern))
    <* white_space
  in
  parse_parens parse_unparenthesized <|> parse_unparenthesized
;;

let parse_keyword = choice [ token "true"; token "false"; token "None"; token "()" ]

let parse_option parse =
  let* tag = token2 "Some" in
  let* opt = parse >>| Option.some in
  return (tag, opt)
;;

let parse_construct parse construct func =
  token "[" *> sep_by (token ";") parse
  <* token "]"
  >>| List.fold_right ~init:(construct ("[]", None)) ~f:func
;;

let parse_list parse construct tuple =
  let rec go acc =
    token "::" *> parse
    >>= (fun elem ->
    go elem >>| fun rest -> construct ("::", Some (tuple (acc, rest, []))))
    <|> return acc
  in
  parse >>= go
;;

let parse_pattern_construct parse_elem parse_pat =
  choice
    [ (parse_option (parse_elem <|> parse_parens parse_pat)
       >>| fun (t, p) -> PatConstruct (t, p))
    ; parse_construct
        parse_elem
        (fun (t, p) -> PatConstruct (t, p))
        (fun p acc -> PatConstruct ("::", Some (PatTuple (p, acc, []))))
    ; parse_list
        parse_elem
        (fun (t, p) -> PatConstruct (t, p))
        (fun (a, b, c) -> PatTuple (a, b, c))
    ]
;;

let parse_base_pat =
  choice
    [ parse_pattern_any
    ; parse_pattern_var
    ; parse_pattern_const
    ; (parse_keyword >>| fun tag -> PatConstruct (tag, None))
    ]
;;

let parse_pattern =
  white_space
  *> fix (fun pat ->
    let atom =
      choice
        [ parse_base_pat
        ; parse_pattern_construct parse_base_pat pat
        ; parse_pattern_with_type pat
        ; parse_parens pat
        ]
    in
    let tuple = parse_pattern_construct atom pat <|> atom in
    let lst = parse_pattern_construct tuple pat <|> tuple in
    parse_pattern_tuple lst <|> lst)
;;

let parse_left_associative expr oper right_expr =
  let rec go acc = lift2 (fun f x -> f acc x) oper right_expr >>= go <|> return acc in
  expr >>= go
;;

let parse_expr_bin_oper parse_bin_op tkn =
  token tkn *> return (fun e1 e2 -> ExpBinOper (parse_bin_op, e1, e2))
;;

let parse_right_associative expr oper =
  let rec parse () =
    expr
    >>= fun left ->
    oper >>= (fun combine -> parse () >>| fun right -> combine left right) <|> return left
  in
  parse ()
;;

let multiply = parse_expr_bin_oper Multiply "*"
let division = parse_expr_bin_oper Division "/"
let plus = parse_expr_bin_oper Plus "+"
let minus = parse_expr_bin_oper Minus "-"

let compare =
  choice
    [ parse_expr_bin_oper Equal "="
    ; parse_expr_bin_oper NotEqual "<>"
    ; parse_expr_bin_oper LowestEqual "<="
    ; parse_expr_bin_oper LowerThan "<"
    ; parse_expr_bin_oper GreatestEqual ">="
    ; parse_expr_bin_oper GreaterThan ">"
    ]
;;

let and_op = parse_expr_bin_oper And "&&"
let or_op = parse_expr_bin_oper Or "||"

let parse_custom_infix_when pred =
  white_space *> take_while1 is_operator_char_infix
  >>= fun op ->
  if Option.is_some (builtin_op_of_string op)
  then fail "builtin"
  else if pred op
  then return (fun e1 e2 -> ExpBinOper (Custom op, e1, e2))
  else fail "custom_op_mismatch"
;;

let parse_expr_ident = parse_ident >>| fun x -> ExpIdent x
let parse_expr_const = parse_const >>| fun c -> ExpConst c

let parse_expr_with_type parse_expr =
  let parse_annotated_type = token ":" *> parse_type in
  lift2 (fun expr t -> ExpTypeAnnotation (expr, t)) parse_expr parse_annotated_type
;;

let parse_expr_branch parse_expr =
  lift3
    (fun cond t f -> ExpBranch (cond, t, f))
    (token "if" *> parse_expr)
    (token "then" *> parse_expr)
    (option None (token "else" *> parse_expr >>| Option.some))
;;

let parse_expr_unar_oper parse_expr =
  parse_unar_oper >>= fun op -> parse_expr >>= fun expr -> return (ExpUnarOper (op, expr))
;;

let parse_expr_list parse_expr =
  parse_list
    parse_expr
    (fun (tag, exp_opt) -> ExpConstruct (tag, exp_opt))
    (fun (fst_exp, snd_exp, exp_list) -> ExpTuple (fst_exp, snd_exp, exp_list))
;;

let parse_expr_apply e right =
  parse_left_associative e (return (fun e1 e2 -> ExpApply (e1, e2))) right
;;

let parse_expr_lambda parse_expr =
  token2 "fun" *> sep_by1 white_space parse_pattern
  <* token "->"
  >>= fun params ->
  parse_expr
  >>| fun body ->
  match params with
  | pat :: pats -> ExpLambda (pat, pats, body)
  | [] -> body
;;

let parse_case parse_expr =
  white_space
  *> option () (token "|" *> return ())
  *> lift2 (fun pat exp -> pat, exp) parse_pattern (token "->" *> parse_expr)
;;

let parse_expr_function parse_expr =
  token2 "function"
  *>
  let* case_list = sep_by1 (token "|") (parse_case parse_expr) in
  return (ExpFunction (List.hd_exn case_list, List.drop case_list 1))
;;

let parse_expr_match parse_expr =
  let* exp = token2 "match" *> parse_expr <* token2 "with" in
  let* case_list = sep_by1 (token "|") (parse_case parse_expr) in
  return (ExpMatch (exp, List.hd_exn case_list, List.drop case_list 1))
;;

let parse_expr_tuple parse_expr =
  let commas = token "," in
  let tuple =
    lift3
      (fun e1 e2 rest -> ExpTuple (e1, e2, rest))
      (parse_expr <* commas)
      parse_expr
      (many (commas *> parse_expr))
    <* white_space
  in
  parse_parens tuple <|> tuple
;;

let parse_body parse_expr =
  many1 parse_pattern
  >>= fun patterns ->
  token "=" *> parse_expr
  >>| fun body ->
  match patterns with
  | pat :: pats -> ExpLambda (pat, pats, body)
  | [] -> body
;;

let parse_expr_sequence parse_expr =
  parse_left_associative
    parse_expr
    (token ";" *> return (fun exp1 exp2 -> ExpLet (NonRec, (PatUnit, exp1), [], exp2)))
    parse_expr
;;

let parse_expr_construct parse_expr =
  let cons_one exp acc = ExpConstruct ("::", Some (ExpTuple (exp, acc, []))) in
  let rec unfold_sequence = function
    | ExpLet (NonRec, (PatUnit, e1), [], e2) ->
      let rest, last = unfold_sequence e2 in
      e1 :: rest, last
    | e -> [], e
  in
  let rec fold_elem (from_parens, exp) acc =
    if from_parens
    then cons_one exp acc
    else (
      match exp with
      | ExpLet (NonRec, (PatUnit, e1), [], e2) ->
        let rest, last = unfold_sequence e2 in
        let acc' = fold_elem (false, last) acc in
        let acc'' =
          List.fold_right rest ~init:acc' ~f:(fun e a -> fold_elem (false, e) a)
        in
        fold_elem (false, e1) acc''
      | _ -> cons_one exp acc)
  in
  let elem_parser =
    parse_parens (parse_expr_sequence parse_expr)
    >>| (fun exp -> true, exp)
    <|> (parse_expr >>| fun exp -> false, exp)
  in
  parse_construct elem_parser (fun (t, e) -> ExpConstruct (t, e)) fold_elem
;;

let parse_annotated_rhs parse_expr opr =
  token ":" *> parse_type
  >>= fun t -> token opr *> parse_expr >>| fun expr -> ExpTypeAnnotation (expr, t)
;;

let parse_fun_binding parse_expr =
  let* id = parse_pattern_var in
  let* params = many1 parse_pattern in
  let pat = List.hd_exn params
  and pats = List.drop params 1 in
  let mk_body body = ExpLambda (pat, pats, body) in
  choice
    [ (parse_annotated_rhs parse_expr "="
       >>= function
       | ExpTypeAnnotation (expr, t) -> return (PatType (id, t), mk_body expr)
       | _ -> fail "expected type annotation")
    ; (token "=" *> parse_expr >>| fun expr -> id, mk_body expr)
    ]
;;

let parse_simple_binding parse_expr =
  let* pat = parse_pattern in
  choice
    [ (parse_annotated_rhs parse_expr "="
       >>= function
       | ExpTypeAnnotation (expr, t) -> return (PatType (pat, t), expr)
       | _ -> fail "expected type annotation")
    ; (token "=" *> parse_expr >>| fun expr -> pat, expr)
    ]
;;

let parse_value_binding_list parse_expr =
  let parse_binding = parse_fun_binding parse_expr <|> parse_simple_binding parse_expr in
  sep_by1 (token2 "and") (white_space *> parse_binding)
;;

let parse_base_expr =
  choice
    [ parse_expr_ident
    ; parse_expr_const
    ; (parse_keyword >>| fun tag -> ExpConstruct (tag, None))
    ]
;;

let parse_expr_construct_keyword_some parse_expr =
  parse_option (parse_base_expr <|> parse_parens parse_expr)
  >>| fun (tag, exp_opt) -> ExpConstruct (tag, exp_opt)
;;

let parse_expr_let parse_expr =
  token "let"
  *> lift4
       (fun rec_flag value_bindings and_bindings body ->
          ExpLet (rec_flag, value_bindings, and_bindings, body))
       (token "rec" *> (take_while1 Char.is_whitespace *> return Rec) <|> return NonRec)
       (lift2
          (fun pat expr -> pat, expr)
          parse_pattern
          (token "=" *> parse_expr <|> parse_body parse_expr))
       (many
          (token "and"
           *> lift2
                (fun pat expr -> pat, expr)
                parse_pattern
                (token "=" *> parse_expr <|> parse_body parse_expr)))
       (token "in" *> parse_expr)
;;

let parse_top_expr parse_expr =
  choice
    [ parse_expr_let parse_expr
    ; parse_expr_function parse_expr
    ; parse_expr_lambda parse_expr
    ; parse_expr_match parse_expr
    ; parse_expr_branch parse_expr
    ]
;;

let parse_exp_apply e right =
  let app = parse_expr_apply e right in
  let app = parse_expr_unar_oper app <|> app in
  let power = parse_right_associative app (parse_custom_infix_when is_custom_power_op) in
  let ops1 =
    parse_left_associative
      power
      (parse_custom_infix_when is_custom_mul_op <|> multiply <|> division)
      power
  in
  let ops2 =
    parse_left_associative
      ops1
      (parse_custom_infix_when is_custom_add_op <|> plus <|> minus)
      ops1
  in
  let concat =
    parse_right_associative ops2 (parse_custom_infix_when is_custom_concat_op)
  in
  let cmp =
    parse_left_associative
      concat
      (parse_custom_infix_when is_custom_cmp_op
       <|> parse_custom_infix_when is_custom_lowest_op
       <|> compare)
      concat
  in
  let bool_and = parse_right_associative cmp and_op in
  parse_right_associative bool_and or_op
;;

let parse_expr =
  white_space
  *> fix (fun expr ->
    let term =
      choice
        [ parse_base_expr
        ; parse_expr_construct_keyword_some expr
        ; parse_parens (parse_expr_with_type expr)
        ; parse_expr_construct expr
        ; parse_top_expr expr
        ; parse_parens expr
        ]
    in
    let func = parse_exp_apply term term <|> term in
    let lst = parse_expr_list func <|> func in
    let tuple = parse_expr_tuple lst <|> lst in
    let seq = parse_expr_sequence tuple <|> tuple in
    let lambda = parse_expr_lambda expr <|> seq in
    choice
      [ parse_expr_let expr
      ; parse_expr_function expr
      ; parse_expr_lambda expr
      ; parse_expr_match expr
      ; parse_expr_branch expr
      ; lambda
      ])
;;

let parse_structure =
  let parse_eval = parse_expr >>| fun e -> SEval e in
  let parse_value =
    token "let"
    *> lift2
         (fun r id_list -> SValue (r, List.hd_exn id_list, List.drop id_list 1))
         (token "rec" *> (take_while1 Char.is_whitespace *> return Rec) <|> return NonRec)
         (parse_value_binding_list parse_expr)
  in
  let parse_structure_item = choice [ parse_eval; parse_value ] in
  parse_structure_item <* option () (token ";;" >>| ignore)
;;

let parse_program =
  let definitions_or_exprs =
    white_space *> many parse_structure <* option () (token ";;" >>| ignore)
  in
  definitions_or_exprs <* white_space
;;

let parse input = parse_string ~consume:All parse_program input
