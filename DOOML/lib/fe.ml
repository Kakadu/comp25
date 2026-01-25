open Angstrom
module Map = Base.Map.Poly

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let whitespace = take_while is_whitespace
let whitespace1 = take_while1 is_whitespace
let token f = whitespace *> f <* whitespace

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_idschar = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

let is_idchar = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9' -> true
  | _ -> false
;;

let is_fbinopchar = function
  | '$' | '&' | '*' | '+' | '-' | '/' | '=' | '>' | '@' | '^' | '|' | '%' | '<' | '#' ->
    true
  | _ -> false
;;

let is_sbinopchar = function
  | '$'
  | '&'
  | '*'
  | '+'
  | '-'
  | '/'
  | '='
  | '>'
  | '@'
  | '^'
  | '|'
  | '%'
  | '<'
  | '!'
  | '.'
  | ':'
  | '?'
  | '~' -> true
  | _ -> false
;;

let is_funopchar = function
  | '?' | '~' | '!' -> true
  | _ -> false
;;

let is_sunopchar = is_sbinopchar

let kws =
  Map.of_alist_exn
    [ "rec", ()
    ; "let", ()
    ; "in", ()
    ; "type", ()
    ; "fun", ()
    ; "if", ()
    ; "then", ()
    ; "else", ()
    ]
;;

let spf = Format.asprintf
let pp_sep_space ppf () = Format.fprintf ppf " "
let pp_sep_newline ppf () = Format.fprintf ppf "\n"
let failf fmt = Format.kasprintf fail fmt
let parens f = token (char '(') *> f <* token (char ')')

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

let kw =
  let* kw = take_while1 is_idchar |> token in
  if Map.mem kws kw then return kw else fail (spf "expected keyword, but found %s" kw)
;;

let ident =
  let un_ident =
    let* hd = satisfy is_funopchar in
    let* tl = take_while is_sunopchar in
    String.cat (String.make 1 hd) tl |> return
  in
  let binop_ident =
    let* hd = satisfy is_fbinopchar in
    let* tl = take_while is_sbinopchar in
    String.cat (String.make 1 hd) tl |> return
  in
  let* ident =
    take_while1 is_idchar |> token <|> parens un_ident <|> parens binop_ident
  in
  if Map.find kws ident |> Option.is_none
  then ident |> return
  else failf "expected ident, but found keyword %s" ident
;;

let punit = (string "()" |> token) *> return Ast.punit
let plug = (string "_" |> token) *> return Ast.plug

let ptuple pattern =
  let tuple =
    let* fpattern = pattern in
    let* patterns = many (token (char ',') *> pattern) in
    return (Ast.ptuple (fpattern :: patterns))
  in
  parens tuple
;;

let pattern =
  fix (fun pattern ->
    punit <|> plug <|> ptuple pattern <|> (ident >>= fun ident -> return (Ast.ident ident)))
;;

let const =
  (string "()" |> token) *> return (Ast.cunit |> Ast.const)
  <|>
  let* v = take_while1 is_digit |> token in
  v |> int_of_string |> Ast.cint |> Ast.const |> return
;;

let var =
  let* ident = ident in
  Ast.var ident |> return
;;

let rec_flag =
  let rec_ = token (string "rec") *> return Ast.rec_ in
  rec_ <|> return Ast.nonrec_
;;

let let_ expr =
  let* rec_flag = rec_flag in
  let* pattern = pattern <?> "expected pattern after 'let'" in
  let* _ = token (char '=') in
  let* body =
    expr <?> spf "expected expression after 'let %a ='" Ast.pp_pattern pattern
  in
  let* _ = token (string "in") <?> "expected \'in\' after introducing new let binding" in
  let* expr = expr <?> "expected expression after 'let ... = ... in'" in
  Ast.let_ rec_flag pattern body expr |> return
;;

let fun_ expr =
  let* patterns = many1 pattern <?> "expected one or more function argument names" in
  let* _ = token (string "->") in
  let* expr =
    expr
    <?> spf
          "expected expression after 'fun %a ' "
          (Format.pp_print_list ~pp_sep:pp_sep_space Ast.pp_pattern)
          patterns
  in
  Ast.fun_ patterns expr |> return
;;

let app expr =
  let* f = expr in
  let* args = many1 expr in
  match args with
  | hd :: tl -> List.fold_left Ast.app (Ast.app f hd) tl |> return
  | _ -> assert false
;;

let binops expr =
  let binop op expr =
    let op = token op in
    chainl1 expr op
  in
  let combine op =
    string op *> return (fun lhs rhs -> Ast.app (Ast.app (Ast.var op) lhs) rhs)
  in
  (* Stolen from https://ocaml.org/manual/5.1/api/Ocaml_operators.html. *)
  expr
  |> binop (combine "*" <|> combine "/")
  |> binop (combine "+" <|> combine "-")
  |> binop
       (combine "<="
        <|> combine "=="
        <|> combine "<>"
        <|> combine "="
        <|> combine "<"
        <|> combine ">")
  |> binop (combine ">=" <|> combine ">")
  |> binop (combine "&&")
  |> binop (combine "||")
;;

let ite expr =
  let* cond = expr <?> spf "expected expression inside 'if' condition" in
  let* _ = token (string "then") <?> spf "expected 'then'" in
  let* then_ = expr <?> spf "expected expression inside 'then' clause" in
  let* _ = token (string "else") <?> spf "expected 'else'" in
  let* else_ = expr <?> spf "expected expression inside 'else' clause" in
  Ast.ite cond then_ else_ |> return
;;

let tuple expr =
  let tuple =
    let* fexpr = expr in
    let* exprs = many (token (char ',') *> expr) in
    return (Ast.tuple (fexpr :: exprs))
  in
  parens tuple
;;

let expr =
  fix (fun expr ->
    let expr' =
      let* c = whitespace *> peek_char in
      match c with
      | Some '0' .. '9' -> const
      | Some '(' ->
        let* r = const <|> parens expr <|> tuple expr <|> var in
        r |> return
      | _ -> var
    in
    let expr' = app expr' <|> expr' in
    let expr' = binops expr' in
    let expr' =
      (let* kw = kw in
       match kw with
       | "let" -> let_ expr
       | "fun" -> fun_ expr
       | "if" -> ite expr
       | _ -> fail "")
      <|> expr'
    in
    expr')
;;

let ty = fail "types and ADTs are not implemented yet"

let let_decl =
  let* rec_flag = rec_flag in
  let* pattern = pattern <?> "expected pattern after 'let'" in
  let* _ = token (char '=') in
  let* body =
    expr <?> spf "expected expression after 'let %a ='" Ast.pp_pattern pattern
  in
  let* _ = token (string ";;") in
  Ast.letdecl rec_flag pattern body |> return
;;

let top_level =
  let* kw = kw in
  match kw with
  | "let" ->
    let* () = commit in
    let_decl
  | "type" ->
    let* () = commit in
    ty
  | _ -> fail "expected top level declaration"
;;

let parse = parse_string ~consume:Consume.All (many1 top_level)

let parse_and_print code =
  match parse_string ~consume:Consume.All (many1 top_level) code with
  | Ok res ->
    Format.printf "%a" (Format.pp_print_list ~pp_sep:pp_sep_newline Ast.pp_top_level) res
  | Error res ->
    Format.printf
      "%s"
      (res
       |> String.split_on_char '>'
       |> List.rev
       |> List.map String.trim
       |> String.concat "\n")
;;

let%expect_test "const definition to 15" =
  parse_and_print "let const_15 = fun () -> 15;;";
  [%expect {| let const_15 = fun () -> 15;; |}]
;;

let%expect_test "simple algebraic expression" =
  parse_and_print "let sub = fun a b c -> a + b + c;;";
  [%expect {| let sub = fun a b c -> (+) ((+) a b) c;; |}]
;;

let%expect_test "find stddev" =
  parse_and_print "let stddev = fun a b c -> a * a + ((b * b) + (c * c));;";
  [%expect {| let stddev = fun a b c -> (+) ((*) a a) ((+) ((*) b b) ((*) c c));; |}]
;;

let%expect_test "use let ins for test" =
  parse_and_print
    {|
    let some_test = fun a b c ->
      let a_plus_b = a + b in
      let b_plus_c = b + c in
      let c_plus_a = c + a in
      5
    ;;
  |};
  [%expect
    {|
    let some_test =
      fun a b c ->
        let a_plus_b =
          (+) a b
        in
        let b_plus_c =
          (+) b c
        in
        let c_plus_a =
          (+) c a
        in
        5
    ;;
    |}]
;;

let%expect_test "wrong let ins" =
  parse_and_print
    {|
    let some_test = fun () ->
      let % = 5 in 10
    ;;
  |};
  [%expect
    {| expected expression after 'let some_test =': expected ident, but found keyword fun |}]
;;

let%expect_test "simple call" =
  parse_and_print "let swap = fun a b k -> k b a;;";
  [%expect {| let swap = fun a b k -> k b a;; |}]
;;

let%expect_test "simple call with parens" =
  parse_and_print "let wrong_swap = fun a b k -> k (b a);;";
  [%expect {| let wrong_swap = fun a b k -> k (b a);; |}]
;;

let%expect_test "factorial" =
  parse_and_print
    {|
    let rec fac = fun n ->
      if n = 0 then
        1
      else
        n * (fac (n - 1))
    ;;
  |};
  [%expect {| let rec fac = fun n -> if (=) n 0 then 1 else (*) n (fac ((-) n 1));; |}]
;;

let%expect_test "factorial" =
  parse_and_print
    {|
    let f = 15;;
    let g = f + 3;;
    let h = g * 15;;
  |};
  [%expect
    {|
    let f = 15;;

    let g = (+) f 3;;

    let h = (*) g 15;;
    |}]
;;

let%expect_test "ite in ite" =
  parse_and_print
    {|
    let f = if (if 1 then 0 else 0) then 1 else 0;;
  |};
  [%expect
    {| let f = if if 1 then 0 else 0 then 1 else 0;; |}]
;;

let%expect_test "tuples and plugs" =
  parse_and_print
    {|
    let () = _;;
    let _ = _;;
    let (a, b) = _;;
  |};
  [%expect
    {|
    let () = _;;

    let _ = _;;

    let (a, b) = _;;
    |}]
;;

let%expect_test "quick check" =
  let top_levels =
    parse
      {|
    let rec fac = fun n ->
      if n = 0 then
        1
      else
        n * (fac (n - 1))
    ;;
  |}
    |> Result.get_ok
  in
  let top_levels' =
    Format.asprintf
      "%a"
      (Format.pp_print_list ~pp_sep:pp_sep_newline Ast.pp_top_level)
      top_levels
    |> parse
    |> Result.map_error (fun err ->
      Format.printf "%s" err;
      err)
    |> Result.get_ok
  in
  Format.printf "%b" (top_levels = top_levels');
  [%expect {| true |}]
;;
