open Ast

type immexpr =
  | ImmNum of int
  | ImmId of ident

type cexpr =
  | CPlus of immexpr * immexpr
  | CMinus of immexpr * immexpr
  | CMul of immexpr * immexpr
  | CDiv of immexpr * immexpr
  | CIte of cexpr * aexpr * aexpr
  | CEq of immexpr * immexpr
  | CNeq of immexpr * immexpr
  | CLt of immexpr * immexpr
  | CLte of immexpr * immexpr
  | CGt of immexpr * immexpr
  | CGte of immexpr * immexpr
  | CImmexpr of immexpr
  | CLam of ident * aexpr
  | CApp of immexpr * immexpr list

and aexpr =
  | ALet of ident * cexpr * aexpr
  | ACExpr of cexpr

type astatement = ALetStatement of is_recursive * (ident * aexpr) list

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
  | Binary_add -> "res_of_plus", fun (l, r) -> CPlus (l, r)
  | Binary_subtract -> "res_of_minus", fun (l, r) -> CMinus (l, r)
  | Binary_multiply -> "res_of_mul", fun (l, r) -> CMul (l, r)
  | Binary_divide -> "res_of_div", fun (l, r) -> CDiv (l, r)
  | Binary_equal -> "eq", fun (l, r) -> CEq (l, r)
  | Binary_unequal -> "neq", fun (l, r) -> CNeq (l, r)
  | Binary_less -> "lt", fun (l, r) -> CLt (l, r)
  | Binary_less_or_equal -> "lte", fun (l, r) -> CLte (l, r)
  | Binary_greater -> "gt", fun (l, r) -> CGt (l, r)
  | Binary_greater_or_equal -> "gte", fun (l, r) -> CGte (l, r)
  | _ -> failwith "NYI"
;;

let rec anf (e : expr) (expr_with_hole : immexpr -> aexpr) =
  let anf_binop opname ctor left right expr_with_hole =
    let varname = gen_temp opname in
    anf left (fun limm ->
      anf right (fun rimm ->
        ALet (varname, ctor (limm, rimm), expr_with_hole (ImmId varname))))
  in
  match e with
  | Const (Int_lt n) -> expr_with_hole (ImmNum n)
  | Variable id -> expr_with_hole (ImmId id)
  | Bin_expr (op, l, r) ->
    let opname, ctor = binop_map op in
    anf_binop opname ctor l r expr_with_hole
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
  | _ -> failwith "NYI"
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
  | _ -> failwith "NYI"
;;

let rec anf_constructions (cs : construction list) : aconstructions =
  match cs with
  | c :: rest -> anf_construction c :: anf_constructions rest
  | [] -> []
;;
