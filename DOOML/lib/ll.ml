module Scope = struct
  include Set.Make (String)
end

module Ctx = struct
  type t =
    { lifts : (Ast.pattern * Ast.pattern list * Ast.expr) list
    ; symbols : Scope.t
    }

  let gen ?prefix (ctx : t) =
    let prefix = Option.value ~default:"_" prefix in
    let v = Scope.cardinal ctx.symbols in
    let rec aux off =
      let name = String.concat "_" [ prefix; Int.to_string (v + off) ] in
      if not (Scope.mem name ctx.symbols) then name else aux (off + 1)
    in
    aux 0
  ;;

  let rec of_pattern = function
    | Ast.PUnit | Plug -> Scope.empty
    | Ident s -> Scope.singleton s
    | PTuple patterns ->
      List.map of_pattern patterns |> List.fold_left Scope.union Scope.empty
  ;;

  let extend pattern (ctx : t) =
    let names = of_pattern pattern in
    { ctx with symbols = Scope.union ctx.symbols names }
  ;;

  let lift args body (ctx : t) =
    let name = gen ctx in
    let v = Ast.var name in
    let name = Ast.ident name in
    let ctx = extend name ctx in
    v, { ctx with lifts = (name, args, body) :: ctx.lifts }
  ;;

  let empty = { lifts = []; symbols = Scope.empty }
end

module State = struct
  include State.M (Ctx)

  let empty = Ctx.empty
  let lift = Ctx.lift
  let extend v ctx = put (Ctx.extend v ctx) ()
end

open State

let rec ll = function
  | Ast.Const _ as c -> return c
  | Var _ as v -> return v
  | Tuple exprs ->
    let rec ll_list = function
      | [] -> return []
      | hd :: tl ->
        let* hd = ll hd in
        let* tl = ll_list tl in
        return (hd :: tl)
    in
    let* exprs = ll_list exprs in
    return (Ast.tuple exprs)
  | Fun (args', body') ->
    let* body' = ll body' in
    let* f = lift args' body' in
    return f
  | App (f, g) ->
    let* f = ll f in
    let* g = ll g in
    let a = Ast.app f g in
    return a
  | Let (rec_flag, pattern, bind, body) ->
    let* () =
      match rec_flag with
      | Ast.Rec -> extend pattern
      | Ast.NonRec -> return ()
    in
    let* bind = ll bind in
    let* () = extend pattern in
    let* body = ll body in
    let l = Ast.let_ rec_flag pattern bind body in
    return l
  | Ite (cond_, then_, else_) ->
    let* cond_ = ll cond_ in
    let* then_ = ll then_ in
    let* else_ = ll else_ in
    let ite = Ast.ite cond_ then_ else_ in
    return ite
;;

let ll =
  let ctx = ref empty in
  List.concat_map (function Ast.LetDecl (rec_flag, pattern, body) ->
      (ctx
       := match rec_flag with
          | Ast.Rec -> extend pattern !ctx |> snd
          | Ast.NonRec -> !ctx);
      let lifts, body =
        match body with
        | Fun (args, body) ->
          let body, ctx = ll body !ctx in
          ctx.lifts, Ast.fun_ args body
        | ast ->
          let ast, ctx = ll ast !ctx in
          ctx.lifts, ast
      in
      ctx := extend pattern !ctx |> snd;
      Ast.letdecl rec_flag pattern body
      :: List.map
           (fun (name, args, body) -> Ast.letdecl Ast.nonrec_ name (Ast.fun_ args body))
           lifts
      |> List.rev)
;;

let%expect_test "basic" =
  let asts =
    Fe.parse
      {|
      let f =
        (fun q p ->
          (fun q p a b c ->
            let g =
              (fun a -> fun d -> (+) a d) a
            in
            let h =
              (fun g b -> fun e -> (+) b (g e)) g b
            in
            let i =
              (fun p g h c -> fun f -> (+) ((+) ((+) c (h f)) (g f)) p) p g h c
            in
            i ((+) ((+) ((+) a b) c) q)) q p)
      ;;
  |}
    |> Result.map_error (fun err -> Format.printf "Error %s" err)
    |> Result.get_ok
  in
  Format.printf
    "%a"
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline Ast.pp_top_level)
    (ll asts);
  [%expect
    {|
    let __0 = fun a d -> (+) a d;;

    let __2 = fun g b e -> (+) b (g e);;

    let __4 = fun p g h c f -> (+) ((+) ((+) c (h f)) (g f)) p;;

    let __6 =
      fun q p a b c ->
        let g =
          __0 a
        in
        let h =
          __2 g b
        in
        let i =
          __4 p g h c
        in
        i ((+) ((+) ((+) a b) c) q)
    ;;

    let f = fun q p -> __6 q p;;
    |}]
;;
