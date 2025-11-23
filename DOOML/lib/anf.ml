type immexpr =
  | ImmNum of int
  | ImmId of string
[@@deriving variants]

let pp_immexpr ppf = function
  | ImmNum d -> Format.fprintf ppf "%d" d
  | ImmId s -> Format.fprintf ppf "%s" s
;;

type cexpr =
  | CImm of immexpr
  | CIte of immexpr * immexpr * immexpr
  | CApp of string * immexpr list
[@@deriving variants, show]

let pp_cexpr ppf = function
  | CImm imm -> Format.fprintf ppf "%a" pp_immexpr imm
  | CIte (cond_, then_, else_) ->
    Format.fprintf
      ppf
      "if %a then %a else %a"
      pp_immexpr
      cond_
      pp_immexpr
      then_
      pp_immexpr
      else_
  | CApp (s, immexprs) ->
    Format.fprintf
      ppf
      "(%s) %a"
      s
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") pp_immexpr)
      immexprs
;;

type aexpr =
  | ALet of string * cexpr * aexpr
  | AExpr of cexpr
[@@deriving variants, show]

let rec pp_aexpr ppf = function
  | ALet (name, cexpr, aexpr) ->
    Format.fprintf ppf "let %s = %a in @ %a" name pp_cexpr cexpr pp_aexpr aexpr
  | AExpr cexpr -> Format.fprintf ppf "%a" pp_cexpr cexpr
;;

type ctx = { syms : string list }

let addsym (ctx : ctx) v = { syms = v :: ctx.syms }

let gensym ?prefix (ctx : ctx) =
  let prefix = Option.value prefix ~default:"sup" in
  let rec aux i =
    let v = String.cat prefix (Int.to_string i) in
    if List.mem v ctx.syms then aux (i + 1) else v
  in
  let v = aux (List.length ctx.syms) in
  v, addsym ctx v
;;

let rec anf ctx (k : ctx -> immexpr -> aexpr) = function
  | Ast.Const d -> k ctx (immnum d)
  | Var s ->
    let ctx = addsym ctx s in
    k ctx (immid s)
  | App _ as app ->
    let rec aux ctx immexprs = function
      | Ast.Var s ->
        let sym, ctx = gensym ctx in
        alet sym (capp s immexprs) (k ctx (immid sym))
      | App (f', expr') ->
        anf ctx (fun ctx immexpr -> aux ctx (immexpr :: immexprs) f') expr'
      | f ->
        anf
          ctx
          (fun ctx immf ->
             let sym, ctx = gensym ctx in
             let sym', ctx = gensym ctx in
             alet sym (cimm immf) (alet sym' (capp sym immexprs) (k ctx (immid sym'))))
          f
    in
    aux ctx [] app
  | Let (_rec, name, bind, expr) ->
    let name =
      match name with
      | PUnit -> "()"
      | Plug -> "_"
      | Ident s -> s
      | Tuple _ -> failwith "tbd"
    in
    let ctx = addsym ctx name in
    anf ctx (fun ctx immbind -> alet name (cimm immbind) (anf ctx k expr)) bind
  | Ite (cond_, then_, else_) ->
    anf
      ctx
      (fun ctx immcond ->
         anf
           ctx
           (fun ctx immthen ->
              anf
                ctx
                (fun ctx immelse ->
                   alet "ite" (cite immcond immthen immelse) (k ctx (immid "ite")))
                else_)
           then_)
      cond_
  | Fun _ -> failwith "should be CC/LL first"
;;

let anf = anf { syms = [] } (fun _ctx imm -> aexpr (cimm imm))

let%expect_test "basic" =
  let ast =
    Fe.parse
      {|
    let f =
      let q = f ((g + sup0) * (2 * i)) in
      q ;;
  |}
    |> Result.get_ok
    |> List.hd
  in
  match ast with
  | LetDecl (_, _name, body) ->
    Format.printf "@[<v 2>@ %a@]@." pp_aexpr (anf body);
    [%expect {|
      let sup2 = (*) 2 i in
      let sup5 = (+) g sup0 in
      let sup6 = (*) sup5 sup2 in
      let sup7 = (f) sup6 in
      let q = sup7 in
      q
      |}]
;;
