module Scope = Set.Make (String)

module Ctx = struct
  type t =
    { captured : string list
    ; locals : Scope.t
    ; globals : Scope.t
    ; recs : Scope.t
    }

  let rec of_pattern = function
    | Ast.PUnit | Plug -> Scope.empty
    | Ident s -> Scope.singleton s
    | Tuple patterns ->
      List.map of_pattern patterns |> List.fold_left Scope.union Scope.empty
  ;;

  let mem s (ctx : t) =
    Scope.mem s ctx.locals || Scope.mem s ctx.recs || Scope.mem s ctx.globals
  ;;

  let capture name (ctx : t) =
    if mem name ctx
    then ctx
    else { ctx with locals = Scope.add name ctx.locals; captured = name :: ctx.captured }
  ;;

  let extend pattern (ctx : t) =
    let names = of_pattern pattern in
    { ctx with locals = Scope.union ctx.locals names }
  ;;

  let global pattern (ctx : t) =
    let names = of_pattern pattern in
    { ctx with globals = Scope.union ctx.globals names }
  ;;

  let rec_ pattern (ctx : t) =
    let names = of_pattern pattern in
    { ctx with locals = Scope.union ctx.locals names; recs = names }
  ;;

  let nonrec_ pattern (ctx : t) =
    let names = of_pattern pattern in
    { ctx with locals = Scope.union ctx.locals names; recs = Scope.empty }
  ;;

  let up (ctx' : t) (ctx : t) =
    let captured =
      ctx.captured
      |> List.filter (fun local ->
        (not (Scope.mem local ctx'.locals)) && not (List.mem local ctx'.captured))
    in
    let captured = captured @ ctx'.captured in
    { ctx' with captured }
  ;;

  let empty =
    let scope =
      Scope.of_list (Builtin.all |> List.map (fun (builtin : Builtin.t) -> builtin.name))
    in
    let locals = scope in
    let captured = [] in
    { locals; captured; globals = Scope.empty; recs = Scope.empty }
  ;;

  let of_args args (ctx : t) =
    let scope =
      Scope.of_list (Builtin.all |> List.map (fun (builtin : Builtin.t) -> builtin.name))
    in
    let locals =
      List.map of_pattern args |> List.fold_left Scope.union scope |> Scope.union ctx.recs
    in
    let captured = [] in
    { locals; captured; globals = ctx.globals; recs = Scope.empty }
  ;;
end

module State = struct
  include State.M (Ctx)

  let empty = Ctx.empty
  let of_args v ctx = get (Ctx.of_args v ctx)
  let extend v ctx = put (Ctx.extend v ctx) ()
  let global v ctx = put (Ctx.global v ctx) ()
  let rec_ v ctx = put (Ctx.rec_ v ctx) ()
  let nonrec_ v ctx = put (Ctx.nonrec_ v ctx) ()
  let capture v ctx = put (Ctx.capture v ctx) ()
  let up v ctx = put (Ctx.up v ctx) ()
end

open State

let rec cc = function
  | Ast.Const _ as c -> return c
  | Var name as v ->
    let* () = capture name in
    return v
  | Fun (args', body') ->
    (* fix rec here *)
    let* ctx' = of_args args' in
    let body'', ctx'' = cc body' ctx' in
    let f = Ast.fun_ args' body'' in
    let captured = ctx''.captured in
    (match captured with
     | [] -> return f
     | captured ->
       let args = List.map Ast.ident captured in
       let f = Ast.fun_ args f in
       List.fold_left
         (fun f v ->
            let* f = f in
            let* v = cc (Ast.var v) in
            Ast.app f v |> return)
         (return f)
         captured)
  | App (f, g) ->
    let* f = cc f in
    let* g = cc g in
    let a = Ast.app f g in
    return a
  | Let (rec_flag, pattern, bind, body) ->
    let* bind =
      let* ctx = get in
      let* () =
        match rec_flag with
        | Ast.Rec -> rec_ pattern
        | Ast.NonRec -> nonrec_ pattern
      in
      let* bind = cc bind in
      let* () = up ctx in
      return bind
    in
    let* () = extend pattern in
    let* body = cc body in
    let l = Ast.let_ rec_flag pattern bind body in
    return l
  | Ite (cond_, then_, else_) ->
    let* cond_ = cc cond_ in
    let* then_ = cc then_ in
    let* else_ = cc else_ in
    let ite = Ast.ite cond_ then_ else_ in
    return ite
;;

let cc asts =
  List.fold_left
    (fun acc (Ast.LetDecl (rec_flag, pattern, body)) ->
       let* acc = acc in
       let* body =
         let* ctx = get in
         let* () =
           match rec_flag with
           | Ast.Rec -> rec_ pattern
           | Ast.NonRec -> nonrec_ pattern
         in
         let* bind = cc body in
         let* () = up ctx in
         return bind
       in
       let* () = global pattern in
       let ret = Ast.letdecl rec_flag pattern body in
       return (ret :: acc))
    (return [])
    asts
    empty
  |> fst
  |> List.rev
;;

let%expect_test "basic" =
  let ast =
    Fe.parse
      {|
    let f = fun a b c ->
      let g = fun d -> d + a in
      let h = fun e -> e + b in
      (g c) + (h c)
    ;;
  |}
    |> Result.map_error (fun err -> Format.printf "Error %s" err)
    |> Result.get_ok
  in
  Format.printf
    "%a"
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline Ast.pp_top_level)
    (cc ast);
  [%expect
    {|
    let f =
      fun a b c ->
        let g =
          (fun a d -> (+) d a) a
        in
        let h =
          (fun b e -> (+) e b) b
        in
        (+) (g c) (h c)
    ;;
    |}]
;;

let%expect_test "basic 2" =
  let ast =
    Fe.parse
      {|
    let f = fun a b c ->
      let g = fun d -> a + d in
      let h = fun e -> b + g e in
      let i = fun f -> c + h f + g f + p in
      i (a + b + c + q)
    ;;
  |}
    |> Result.map_error (fun err -> Format.printf "Error %s" err)
    |> Result.get_ok
  in
  Format.printf
    "%a"
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline Ast.pp_top_level)
    (cc ast);
  [%expect
    {|
    let f =
      (fun q p a b c ->
        let g =
          (fun a d -> (+) a d) a
        in
        let h =
          (fun g b e -> (+) b (g e)) g b
        in
        let i =
          (fun p g h c f -> (+) ((+) ((+) c (h f)) (g f)) p) p g h c
        in
        i ((+) ((+) ((+) a b) c) q)) q p
    ;;
    |}]
;;

let%expect_test "recursion" =
  let ast =
    Fe.parse
      {|
    let rec f = fun a b ->
      let p = f a in
      let rec g = fun n -> if (n = 1) then f (p n) n else g (f a b) in
      g (a + b)
    ;;
  |}
    |> Result.map_error (fun err -> Format.printf "Error %s" err)
    |> Result.get_ok
  in
  Format.printf
    "%a"
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline Ast.pp_top_level)
    (cc ast);
  [%expect
    {|
    let rec f =
      fun a b ->
        let p =
          f a
        in
        let rec g =
          (fun b a p f n -> if (=) n 1 then f (p n) n else g (f a b)) b a p f
        in
        g ((+) a b)
    ;;
    |}]
;;

let%expect_test "multiple" =
  let ast =
    Fe.parse
      {|
    let rec f = fun n ->
      if n = 1 then 1
      else (f (n - 1)) * n
    ;;

    let main = fun () ->
      let _ = print_int (f 1) in
      let _ = print_int (f 2) in
      let _ = print_int (f 5) in
      print_int (f 8)
    ;;
  |}
    |> Result.map_error (fun err -> Format.printf "Error %s" err)
    |> Result.get_ok
  in
  Format.printf
    "%a"
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline Ast.pp_top_level)
    (cc ast);
  [%expect
    {|
    let rec f = fun n -> if (=) n 1 then 1 else (*) (f ((-) n 1)) n;;

    let main =
      fun () ->
        let _ =
          print_int (f 1)
        in
        let _ =
          print_int (f 2)
        in
        let _ =
          print_int (f 5)
        in
        print_int (f 8)
    ;;
    |}]
;;

let%expect_test "multiple 2" =
  let ast =
    Fe.parse
      {|
    let rec f = fun n ->
      if n = 1 then 1
      else (f (n - 1)) * n
    ;;

    let b = 1;;
    let main = fun () ->
      let c = 3 in
      let f = fun () -> print_int (f a) in
      let f = fun () -> print_int (f (a + b)) in
      let f = fun () -> print_int (f (a + c)) in
      print_int (f 8)
    ;;
  |}
    |> Result.map_error (fun err -> Format.printf "Error %s" err)
    |> Result.get_ok
  in
  Format.printf
    "%a"
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline Ast.pp_top_level)
    (cc ast);
  [%expect
    {|
    let rec f = fun n -> if (=) n 1 then 1 else (*) (f ((-) n 1)) n;;

    let b = 1;;

    let main =
      (fun a () ->
        let c =
          3
        in
        let f =
          (fun a () -> print_int (f a)) a
        in
        let f =
          (fun a () -> print_int (f ((+) a b))) a
        in
        let f =
          (fun c a () -> print_int (f ((+) a c))) c a
        in
        print_int (f 8)) a
    ;;
    |}]
;;
