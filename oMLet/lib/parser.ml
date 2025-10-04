(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Base
open Ast
open TypedTree

(*---------------------Control characters---------------------*)

let pwhitespace = take_while Char.is_whitespace
let pws1 = take_while1 Char.is_whitespace
let pstoken s = pwhitespace *> string s
let ptoken s = pwhitespace *> s
let pparens p = pstoken "(" *> p <* pstoken ")"
let psqparens p = pstoken "[" *> p <* pstoken "]"

(*------------------Prefix operators-----------------*)

let ppref_op =
  let pref_op =
    ptoken
      (let* first_char =
         take_while1 (function
           | '|'
           | '~'
           | '?'
           | '<'
           | '>'
           | '!'
           | '&'
           | '*'
           | '/'
           | '='
           | '+'
           | '-'
           | '@'
           | '^' -> true
           | _ -> false)
       in
       let* rest =
         take_while (function
           | '.'
           | ':'
           | '|'
           | '~'
           | '?'
           | '<'
           | '>'
           | '!'
           | '&'
           | '*'
           | '/'
           | '='
           | '+'
           | '-'
           | '@'
           | '^' -> true
           | _ -> false)
       in
       match first_char, rest with
       | "|", "" -> fail "Prefix operator cannot be called | "
       | "~", "" -> fail "Prefix operator cannot be called ~ "
       | "?", "" -> fail "Prefix operator cannot be called ? "
       | _ -> return (Ident (first_char ^ rest)))
  in
  pparens pref_op
;;

let pEinf_op pexpr =
  ppref_op
  >>= fun inf_op ->
  lift2
    (fun left right -> Apply (Apply (Variable inf_op, left), right))
    (pws1 *> pexpr)
    (pwhitespace *> pexpr)
;;

(* let pEinf_op =
   pwhitespace *> pinf_op >>= fun inf_op -> return (fun e1 e2 -> Efun_application (Efun_application (Evar inf_op, e1), e2))
   ;; *)

(*-------------------------Constants/Variables-------------------------*)

let pint =
  pwhitespace *> take_while1 Char.is_digit
  >>= fun str ->
  match Stdlib.int_of_string_opt str with
  | Some n -> return (Int_lt n)
  | None -> fail "Integer value exceeds the allowable range for the int type"
;;

let pbool =
  choice [ pstoken "true" *> return true; pstoken "false" *> return false ]
  >>| fun x -> Bool_lt x
;;

let pstr =
  pwhitespace *> char '"' *> take_till (Char.equal '"')
  <* char '"'
  >>| fun x -> String_lt x
;;

let punit = pstoken "()" *> return Unit_lt
let const = choice [ pint; pbool; pstr; punit ]

let varname =
  ptoken
    (let* first_char =
       take_while1 (fun ch -> Char.is_lowercase ch || Char.equal ch '_')
     in
     let* rest =
       take_while (fun ch ->
         Char.is_alpha ch || Char.is_digit ch || Char.equal ch '_' || Char.equal ch '\'')
     in
     match first_char, rest with
     | _, _ when KeywordChecker.is_keyword (first_char ^ rest) ->
       fail "Variable name conflicts with a keyword"
     | "_", "" -> fail "Variable cannot be called _"
     | _ -> return (first_char ^ rest))
;;

let patomic_type =
  choice
    [ pstoken "int" *> return (Primitive "int")
    ; pstoken "string" *> return (Primitive "string")
    ; pstoken "bool" *> return (Primitive "bool")
    ; pstoken "unit" *> return (Primitive "unit")
    ]
;;

let plist_type ptype_opt = ptype_opt >>= fun t -> pstoken "list" *> return (Type_list t)

let ptuple_type ptype_opt =
  let star = pstoken "*" in
  lift3
    (fun t1 t2 rest -> Type_tuple (t1, t2, rest))
    ptype_opt
    (star *> ptype_opt)
    (many (star *> ptype_opt))
;;

let rec pfun_type ptype_opt =
  ptype_opt
  >>= fun left ->
  pstoken "->" *> pfun_type ptype_opt
  >>= (fun right -> return (Arrow (left, right)))
  <|> return left
;;

let poption_type ptype_opt = ptype_opt >>= fun t -> pstoken "option" *> return (TOption t)
(* let precord_type = varname >>= fun t -> return (TRecord t) *)

let ptype_helper =
  fix (fun typ ->
    (* let atom = patomic_type <|> pparens typ <|> precord_type in *)
    let atom = patomic_type <|> pparens typ in
    let list = plist_type atom <|> atom in
    let option = poption_type list <|> list in
    let tuple = ptuple_type option <|> option in
    let func = pfun_type tuple <|> tuple in
    func)
;;

let ptype =
  let t = ptype_helper in
  pstoken ":" *> t
;;

let pident = lift (fun t -> Ident t) varname <|> ppref_op
let pat_var = pident >>| fun x -> PVar x
let pat_const = const >>| fun x -> PConst x
let pat_any = pstoken "_" *> return Wild

let pat_tuple pat =
  let commas = pstoken "," in
  let tuple =
    lift3
      (fun p1 p2 rest -> PTuple (p1, p2, rest))
      pat
      (commas *> pat)
      (many (commas *> pat))
    <* pwhitespace
  in
  pparens tuple <|> tuple
;;

let pat_list pat =
  let semicols = pstoken ";" in
  psqparens (sep_by semicols pat >>| fun patterns -> PList patterns)
;;

let rec pat_cons pat =
  let cons =
    pat
    >>= fun head ->
    pstoken "::" *> pat_cons pat
    >>= (fun tail -> return (PCons (head, tail)))
    <|> return head
  in
  pparens cons <|> cons
;;

let pat_option pat =
  lift
    (fun e -> POption e)
    (pstoken "Some" *> pat >>| (fun e -> Some e) <|> (pstoken "None" >>| fun _ -> None))
;;

let pat_ty pat =
  let ty_pat = lift2 (fun pat ty -> PConstraint (pat, ty)) pat ptype in
  ty_pat <|> pparens ty_pat
;;

let ppattern =
  fix (fun pat ->
    let patom =
      pat_const <|> pat_var <|> pat_any <|> pparens pat <|> pparens (pat_ty pat)
    in
    let poption = pat_option patom <|> patom in
    let pptuple = pat_tuple poption <|> poption in
    let pplist = pat_list pptuple <|> pptuple in
    let pcons = pat_cons pplist <|> pplist in
    let pty = pat_ty pcons <|> pcons in
    pty)
;;

(*------------------Binary operators-----------------*)

let pbinop op token =
  pwhitespace *> pstoken token *> return (fun e1 e2 -> Bin_expr (op, e1, e2))
;;

let add = pbinop Binary_add "+"
let sub = pbinop Binary_subtract "-"
let mult = pbinop Binary_multiply "*"
let div = pbinop Binary_divide "/"

let relation =
  choice
    [ pbinop Binary_equal "="
    ; pbinop Binary_unequal "<>"
    ; pbinop Binary_less_or_equal "<="
    ; pbinop Binary_greater_or_equal ">="
    ; pbinop Binary_less "<"
    ; pbinop Binary_greater ">"
    ]
;;

let logic = choice [ pbinop Logical_and "&&"; pbinop Logical_or "||" ]
let cons = pbinop Binary_cons "::"

(*------------------Unary operators-----------------*)

let punop op token =
  pwhitespace *> pstoken token *> return (fun e1 -> Unary_expr (op, e1))
;;

let negation = punop Unary_not "not" <* pws1
let neg_sign = punop Unary_minus "-"
(* let pos_sign = punop Positive "+" *)

(*------------------------Expressions----------------------*)

let chain e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

let rec chainr e op =
  let* left = e in
  (let* f = op in
   let* right = chainr e op in
   return (f left right))
  <|> return left
;;

let un_chain e op =
  fix (fun self -> op >>= (fun unop -> self >>= fun e -> return (unop e)) <|> e)
;;

let rec pbody pexpr =
  ppattern
  >>= fun p ->
  many ppattern
  >>= fun patterns ->
  pbody pexpr <|> (pstoken "=" *> pexpr >>| fun e -> Lambda (p, patterns, e))
;;

let p_let_bind p_expr =
  let* name = ppattern <|> (pparens ppref_op >>| fun oper -> PVar oper) in
  let* args = many ppattern in
  let* body = pstoken "=" *> p_expr in
  return (Let_bind (name, args, body))
;;

let plet pexpr =
  pstoken "let"
  *> lift4
       (fun rec_flag value_bindings and_bindings body ->
          LetIn (rec_flag, value_bindings, and_bindings, body))
       (pstoken "rec" *> (pws1 *> return Rec) <|> return Nonrec)
       (p_let_bind pexpr)
       (many (pstoken "and" *> p_let_bind pexpr))
       (pstoken "in" *> pexpr)
;;

let pEfun pexpr =
  (* if there's only one argument, ascription without parentheses is possible *)
  let single_arg =
    lift2
      (fun arg body -> Lambda (arg, [], body))
      (pstoken "fun" *> pws1 *> ppattern)
      (pstoken "->" *> pexpr)
  in
  let mult_args =
    lift3
      (fun arg args body -> Lambda (arg, args, body))
      (pstoken "fun" *> pws1 *> ppattern)
      (many ppattern)
      (pstoken "->" *> pexpr)
  in
  single_arg <|> mult_args
;;

let pElist pexpr =
  let semicols = pstoken ";" in
  psqparens (sep_by semicols pexpr <* (semicols <|> pwhitespace) >>| fun x -> List x)
;;

let pEtuple pexpr =
  let commas = pstoken "," in
  let tuple =
    lift3
      (fun e1 e2 rest -> Tuple (e1, e2, rest))
      (pexpr <* commas)
      pexpr
      (many (commas *> pexpr))
    <* pwhitespace
  in
  pparens tuple <|> tuple
;;

let pEconst = const >>| fun x -> Const x
let pEvar = pident >>| fun x -> Variable x
let pEapp e = chain e (return (fun e1 e2 -> Apply (e1, e2)))

let pEoption pexpr =
  lift
    (fun e -> Option e)
    (pstoken "Some" *> pexpr >>| (fun e -> Some e) <|> (pstoken "None" >>| fun _ -> None))
;;

let pbranch pexpr =
  lift3
    (fun e1 e2 e3 -> If_then_else (e1, e2, e3))
    (pstoken "if" *> pexpr)
    (pstoken "then" *> pexpr)
    (pstoken "else" *> pexpr >>| (fun e3 -> Some e3) <|> return None)
;;

let pEmatch pexpr =
  let parse_case =
    lift2 (fun pat exp -> pat, exp) (ppattern <* pstoken "->") (pwhitespace *> pexpr)
  in
  let match_cases =
    lift3
      (fun e case case_l -> Match (e, case, case_l))
      (pstoken "match" *> pexpr <* pstoken "with")
      ((pstoken "|" <|> pwhitespace) *> parse_case)
      (many (pstoken "|" *> parse_case))
  in
  let function_cases =
    lift2
      (fun case case_l -> Function (case, case_l))
      (pstoken "function" *> pstoken "|" *> parse_case
       <|> pstoken "function" *> pwhitespace *> parse_case)
      (many (pstoken "|" *> parse_case))
  in
  function_cases <|> match_cases
;;

let pEconstraint pexpr = lift2 (fun expr t -> EConstraint (expr, t)) pexpr ptype

let pexpr =
  fix (fun expr ->
    let atom_expr =
      choice
        [ pEconst
        ; pEvar
        ; pparens expr
        ; pElist expr
        ; pEfun expr
        ; pEoption expr
        ; pEmatch expr (* ; pErecord expr *)
        ; pparens (pEconstraint expr)
        ]
    in
    let let_expr = plet expr in
    let ite_expr = pbranch (expr <|> atom_expr) <|> atom_expr in
    let inf_op = pEinf_op (ite_expr <|> atom_expr) <|> ite_expr in
    let app_expr = pEapp (inf_op <|> atom_expr) <|> inf_op in
    let un_expr = choice [ un_chain app_expr negation; un_chain app_expr neg_sign ] in
    let factor_expr = chain un_expr (mult <|> div) in
    let sum_expr = chain factor_expr (add <|> sub) in
    let rel_expr = chain sum_expr relation in
    let log_expr = chain rel_expr logic in
    let tuple_expr = pEtuple log_expr <|> log_expr in
    (* let field_expr = pEfield_access tuple_expr <|> tuple_expr in
       let cons_expr = chainr field_expr cons in *)
    let cons_expr = chainr tuple_expr cons in
    choice [ let_expr; cons_expr ])
;;

let pconstruction =
  let pseval = pexpr >>| fun e -> Expr e in
  let psvalue =
    pstoken "let"
    *> lift3
         (fun r id id_list -> Let (r, id, id_list))
         (pstoken "rec" *> (pws1 *> return Rec) <|> return Nonrec)
         (p_let_bind pexpr)
         (many (pstoken "and" *> p_let_bind pexpr))
    >>| fun s -> Statement s
  in
  choice [ pseval; psvalue ]
;;

let pconstructions =
  let semicolons = many (pstoken ";;") in
  sep_by semicolons pconstruction <* semicolons <* pwhitespace
;;

let parse str = parse_string ~consume:All pconstructions str
