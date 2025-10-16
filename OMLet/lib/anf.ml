open Ast

type immexpr =
  | ImmNum of int                     (* 42 *)
  | ImmId of ident                    (* a *)

type cbinop =
  | CPlus   (* 42 + a *)
  | CMinus  (* 42 - a *)
  | CMul    (* 42 * a *)
  | CDiv    (* 42 / a *)
  | CEq     (* 42 = a *)
  | CNeq    (* 42 != a *)
  | CLt     (* 42 < a *)
  | CLte    (* 42 <= a *)
  | CGt     (* 42 > a *)
  | CGte    (* 42 >= a *)

type cexpr =
  | CBinop of cbinop * immexpr * immexpr
  | CIte of cexpr * aexpr * aexpr     (* if (42 > a) then 42 else a *)
  | CImmexpr of immexpr
  | CLam of ident * aexpr             (* fun a -> a + 42 *)
  | CApp of immexpr * immexpr list    (* func_name arg1 arg2 ... argn *)

and aexpr =
  | ALet of ident * cexpr * aexpr
  | ACExpr of cexpr

type aconstruction =
  | AExpr of aexpr
  | AStatement of is_recursive * (ident * aexpr) list

type aconstructions = aconstruction list

let count = ref 0

let gen_temp base =
  count := !count + 1;
  Ident (Stdlib.Format.sprintf "%s_%d" base !count)
;;

let binop_map = function
  | Binary_add -> "res_of_plus", CPlus
  | Binary_subtract -> "res_of_minus", CMinus
  | Binary_multiply -> "res_of_mul", CMul
  | Binary_divide -> "res_of_div", CDiv
  | Binary_equal -> "eq", CEq
  | Binary_unequal -> "neq", CNeq
  | Binary_less -> "lt", CLt
  | Binary_less_or_equal -> "lte", CLte
  | Binary_greater -> "gt", CGt
  | Binary_greater_or_equal -> "gte", CGte
  | _ -> failwith "NYI"
;;

let rec anf (e : expr) (expr_with_hole : immexpr -> aexpr) =
  let anf_binop opname op left right expr_with_hole =
    let varname = gen_temp opname in
    anf left (fun limm ->
      anf right (fun rimm ->
        ALet (varname, CBinop (op, limm, rimm), expr_with_hole (ImmId varname))))
  in
  match e with
  | Const (Int_lt n) -> expr_with_hole (ImmNum n)
  | Variable id -> expr_with_hole (ImmId id)
  | Bin_expr (op, l, r) ->
    let opname, op_name = binop_map op in
    anf_binop opname op_name l r expr_with_hole
  | LetIn (_, Let_bind (PVar id, [], expr), [], body) ->
    anf expr (fun immval -> ALet (id, CImmexpr immval, anf body expr_with_hole))
  | LetIn (_, Let_bind (PConst Unit_lt, [], expr), [], body) ->
    anf expr (fun _ -> anf body expr_with_hole)
  | LetIn (_, Let_bind (PVar id, args, expr), [], body) ->
    let arg_names =
      List.map
        (function
          | PVar s -> s
          | _ -> failwith "complex patterns NYI")
        args
    in
    let value = anf expr (fun imm -> ACExpr (CImmexpr imm)) in
    let clams =
      List.fold_right (fun id body -> ACExpr (CLam (id, body))) arg_names value
    in
    let cclams =
      match clams with
      | ACExpr c -> c
      | _ -> failwith "unreachable"
    in
    ALet (id, cclams, anf body expr_with_hole)
  (* | If_then_else(cond, thn, Some els) -> 
    let v = gen_temp "cond" in 
    anf cond (fun condimm ->
      ALet(v, CIte(CImmexpr(condimm), anf thn aexpr_but_as_cexpr, anf els aexpr_but_as_cexpr), (expr_with_hole (ImmId v))))  *)
  | If_then_else (cond, thn, Some els) ->
    anf cond (fun condimm ->
      ACExpr (CIte (CImmexpr condimm, anf thn expr_with_hole, anf els expr_with_hole)))
  | Apply (f, args) ->
    anf f (fun fimm ->
      anf args (fun argimm ->
        let name = gen_temp "res_of_apply" in
        ALet (name, CApp (fimm, [ argimm ]), expr_with_hole (ImmId name))))
  | _ -> failwith "anf expr NYI"
;;

let anf_construction (c : construction) : aconstruction =
  match c with
  | Statement (Let (flag, Let_bind (PVar id, [], expr), [])) ->
    let value = anf expr (fun immval -> ACExpr (CImmexpr immval)) in
    AStatement (flag, [ id, value ])
  | Statement (Let (flag, Let_bind (PVar name, args, expr), [])) ->
    let arg_names =
      List.map
        (function
          | PVar s -> s
          | _ -> failwith "complex patterns NYI")
        args
    in
    let value = anf expr (fun imm -> ACExpr (CImmexpr imm)) in
    let clams =
      List.fold_right (fun id body -> ACExpr (CLam (id, body))) arg_names value
    in
    (* let lambda =
      List.fold_right (fun arg body -> CLam (arg, body)) arg_names value
    in *)
    (* let lambda = CLam (arg_names, body_anf) in *)
    AStatement (flag, [ name, clams ])
  (* let value = anf expr (fun immval -> ACExpr (CImmexpr immval)) in
    let lambda = CLam (arg, value) in
    AStatement (flag, [name, ACExpr lambda]) *)
  (* | Statement (Let (flag, Let_bind((PVar name), args_list, expr), [])) ->
  let value = anf expr (fun immval -> ACExpr (CImmexpr immval)) in
    (* Flatten nested CLam from args_list *)
    let lambda =
      List.fold_right (fun (PVar Ident arg) body -> CLam ([arg], body)) args_list value
    in
    AStatement (flag, [name, ACExpr lambda]) *)
  | Expr e -> AExpr (anf e (fun immval -> ACExpr (CImmexpr immval)))
  | _ -> failwith "anf construction NYI"
;;

let rec anf_constructions (cs : construction list) : aconstructions =
  match cs with
  | c :: rest -> anf_construction c :: anf_constructions rest
  | [] -> []
;;
