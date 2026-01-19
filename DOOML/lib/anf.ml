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
  | CIte of immexpr * aexpr * aexpr
  | CApp of string * immexpr list

and aexpr =
  | ALet of string * cexpr * aexpr
  | AExpr of cexpr

let cimm imm = CImm imm
let cite cond_ then_ else_ = CIte (cond_, then_, else_)
let capp f args = CApp (f, args)
let alet bind v body = ALet (bind, v, body)
let aexpr cexpr = AExpr cexpr

let rec pp_cexpr ppf = function
  | CImm imm -> Format.fprintf ppf "%a" pp_immexpr imm
  | CIte (cond_, then_, else_) ->
    Format.fprintf
      ppf
      "if %a then@;<1 2>@[<hv>%a@]@;<1 0>else@;<1 2>@[<hv>%a@]"
      pp_immexpr
      cond_
      pp_aexpr
      then_
      pp_aexpr
      else_
  | CApp (s, immexprs) ->
    Format.fprintf
      ppf
      "(%s) %a"
      s
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " ") pp_immexpr)
      immexprs

and pp_aexpr ppf = function
  | ALet (name, cexpr, aexpr) ->
    Format.fprintf
      ppf
      "let %s =@;<1 2>@[<hv>%a@]@;<1 0>in@;<1 0>%a"
      name
      pp_cexpr
      cexpr
      pp_aexpr
      aexpr
  | AExpr cexpr -> Format.fprintf ppf "%a" pp_cexpr cexpr
;;

type decl = Decl of Ast.rec_flag * string * string list * aexpr [@@deriving variants]

let pp_decl ppf = function
  | Decl (rec_flag, name, args, body) ->
    let rec_flag =
      match rec_flag with
      | Rec -> "rec "
      | NonRec -> ""
    in
    Format.fprintf
      ppf
      "@[<hv>let %s%s %a =@;<1 2>@[<hv>%a@]@;<0 0>;;@]@."
      rec_flag
      name
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf " ")
         Format.pp_print_string)
      args
      pp_aexpr
      body
;;

module Ctx = struct
  type t = { syms : string list }

  let addsym v (ctx : t) = { syms = v :: ctx.syms }

  let gensym ?prefix () (ctx : t) =
    let prefix = Option.value prefix ~default:"sup" in
    let rec aux i =
      let v = String.cat prefix (Int.to_string i) in
      if List.mem v ctx.syms then aux (i + 1) else v
    in
    let v = aux (List.length ctx.syms) in
    v, addsym v ctx
  ;;
end

module State = struct
  include State.M (Ctx)

  let addsym sym ctx = put (Ctx.addsym sym ctx) ()
  let gensym = Ctx.gensym
end

open State

let rec arg = function
  | Ast.PUnit -> return ("()", [])
  | Ast.Plug -> return ("_", [])
  | Ast.Ident name -> return (name, [])
  | Ast.PTuple els ->
    let* els =
      List.fold_right
        (fun a acc ->
           let* acc = acc in
           let* a = arg a in
           return (a :: acc))
        els
        (return [])
    in
    let* sym = gensym () in
    let lets =
      List.mapi (fun i (name, _) -> name, capp "nth" [ immid sym; immnum i ]) els
    in
    let lets = lets @ List.concat_map snd els in
    return (sym, lets)
;;

let rec anf (k : immexpr -> Ctx.t -> aexpr * Ctx.t) = function
  | Ast.Const d ->
    let* ret = k (immnum d) in
    return ret
  | Var s ->
    let* () = addsym s in
    let* ret = k (immid s) in
    return ret
  | Tuple _ -> 
        failwith "todo tuples anf"
  | App _ as app ->
    let rec aux immexprs = function
      | Ast.Var s ->
        let* sym = gensym () in
        let* expr = k (immid sym) in
        let ret = alet sym (capp s immexprs) expr in
        return ret
      | App (f', expr') ->
        anf
          (fun immexpr ->
             let* f' = aux (immexpr :: immexprs) f' in
             return f')
          expr'
      | f ->
        anf
          (fun immf ->
             let* sym = gensym () in
             let* sym' = gensym () in
             let* expr = k (immid sym') in
             return (alet sym (cimm immf) (alet sym' (capp sym immexprs) expr)))
          f
    in
    aux [] app
  | Let (_rec, name, bind, expr) ->
    let* name, lets = arg name in
    let* () = addsym name in
    let* ret =
      anf
        (fun immbind ->
           let* expr = anf k expr in
           let expr =
             List.fold_right (fun (name, bind) acc -> alet name bind acc) lets expr
           in
           let ret = alet name (cimm immbind) expr in
           return ret)
        bind
    in
    return ret
  | Ite (cond_, then_, else_) ->
    let* ret =
      anf
        (fun immcond ->
           let* then_ = anf (fun imm -> return (aexpr (cimm imm))) then_ in
           let* else_ = anf (fun imm -> return (aexpr (cimm imm))) else_ in
           let* sym = gensym ~prefix:"ite" () in
           let* expr = k (immid sym) in
           let ret = alet sym (cite immcond then_ else_) expr in
           return ret)
        cond_
    in
    return ret
  | Fun _ -> failwith "should be CC/LL first"
;;

let anf program =
  List.fold_right
    (fun a acc ->
       let* acc = acc in
       match a with
       | Ast.LetDecl (rec_flag, name, Fun (args, body)) ->
         let* args =
           List.fold_right
             (fun a acc ->
                let* acc = acc in
                let* a = arg a in
                return (a :: acc))
             args
             (return [])
         in
         let args, lets = List.split args in
         let lets = List.concat lets in
         let* body = anf (fun imm -> return (aexpr (cimm imm))) body in
         let* name, lets' = arg name in
         let body =
           List.fold_right (fun (name, bind) acc -> alet name bind acc) lets body
         in
         let ret = decl rec_flag name args body in
         let lets' =
           List.map (fun (name, bind) -> decl Ast.nonrec_ name [] (aexpr bind)) lets'
         in
         return ((ret :: lets') @ acc)
       | Ast.LetDecl (rec_flag, name, v) ->
         let* v = anf (fun imm -> return (aexpr (cimm imm))) v in
         let* name, lets = arg name in
         let v = List.fold_right (fun (name, bind) acc -> alet name bind acc) lets v in
         let ret = decl rec_flag name [] v in
         return (ret :: acc))
    program
    (return [])
    { syms = [] }
  |> fst
;;

let%expect_test "basic" =
  let ast =
    Fe.parse
      {|
    let f =
      let q = f ((g + sup0) * (2 * i)) in
      q ;;
  |}
    |> Result.get_ok
  in
  Format.printf
    "%a"
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline pp_decl)
    (anf ast);
  [%expect
    {|
    let f  =
      let sup2 =
        (*) 2 i
      in
      let sup5 =
        (+) g sup0
      in
      let sup6 =
        (*) sup5 sup2
      in
      let sup7 =
        (f) sup6
      in
      let q =
        sup7
      in
      q
    ;;
    |}]
;;

let%expect_test "ite" =
  let ast =
    Fe.parse
      {|
    let rec fac =
      if (k = 1) then (1) else (fac (k - 1) * k)
    ;;
  |}
    |> Result.map_error (fun err -> Format.printf "Error %s" err)
    |> Result.get_ok
  in
  Format.printf
    "%a"
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline pp_decl)
    (anf ast);
  [%expect
    {|
    let rec fac  =
      let sup1 =
        (=) k 1
      in
      let ite7 =
        if sup1 then
          1
        else
          let sup4 =
            (-) k 1
          in
          let sup5 =
            (fac) sup4
          in
          let sup6 =
            (*) sup5 k
          in
          sup6
      in
      ite7
    ;;
    |}]
;;

let%expect_test "task 2" =
  let asts =
    Fe.parse
      {|
    let main =
      let x = if (if 0 then 1 else 2)
              then 0 else 1 in
      large x
    ;;
  |}
    |> Result.map_error (fun err -> Format.printf "Error %s" err)
    |> Result.get_ok
    |> Cc.cc
    |> Ll.ll
    |> anf
  in
  Format.printf "%a" (Format.pp_print_list ~pp_sep:Format.pp_print_newline pp_decl) asts;
  [%expect
    {|
    let main  =
      let ite1 =
        if 0 then 1 else 2
      in
      let ite2 =
        if ite1 then 0 else 1
      in
      let x =
        ite2
      in
      let sup4 =
        (large) x
      in
      sup4
    ;;
    |}]
;;

let%expect_test "task 3" =
  let asts =
    Fe.parse
      {|
    let (f, g) = fun a (b, c) ->
      let (b1, b2) = b in
      let (c1, c2) = c in
      b1 + b2 + c1 + c2
    ;;
  |}
    |> Result.map_error (fun err -> Format.printf "Error %s" err)
    |> Result.get_ok
    |> Cc.cc
    |> Ll.ll
    |> anf
  in
  Format.printf "%a" (Format.pp_print_list ~pp_sep:Format.pp_print_newline pp_decl) asts;
  [%expect
    {|
    let sup14 a sup0 =
      let b =
        (nth) sup0 0
      in
      let c =
        (nth) sup0 1
      in
      let sup1 =
        b
      in
      let b1 =
        (nth) sup1 0
      in
      let b2 =
        (nth) sup1 1
      in
      let sup4 =
        c
      in
      let c1 =
        (nth) sup4 0
      in
      let c2 =
        (nth) sup4 1
      in
      let sup11 =
        (+) b1 b2
      in
      let sup12 =
        (+) sup11 c1
      in
      let sup13 =
        (+) sup12 c2
      in
      sup13
    ;;

    let f  = (nth) sup14 0;;

    let g  = (nth) sup14 1;;
    |}]
;;
