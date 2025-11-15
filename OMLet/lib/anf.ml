(** Copyright 2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Sofya Kozyreva <k81sofia@gmail.com>, Vyacheslav Kochergin <vyacheslav.kochergin1@gmail.com> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format

type immexpr =
  | ImmNum of int (* 42 *)
  | ImmId of ident (* a *)
[@@deriving show { with_path = false }]

type cbinop =
  | CPlus (* 42 + a *)
  | CMinus (* 42 - a *)
  | CMul (* 42 * a *)
  | CDiv (* 42 / a *)
  | CEq (* 42 = a *)
  | CNeq (* 42 != a *)
  | CLt (* 42 < a *)
  | CLte (* 42 <= a *)
  | CGt (* 42 > a *)
  | CGte (* 42 >= a *)
[@@deriving show { with_path = false }]

type cexpr =
  | CBinop of cbinop * immexpr * immexpr
  | CIte of cexpr * aexpr * aexpr option (* if (42 > a) then 42 else a *)
  | CImmexpr of immexpr
  | CLam of ident * aexpr (* fun a -> a + 42 *)
  | CApp of immexpr * immexpr list (* func_name arg1 arg2 ... argn *)
[@@deriving show { with_path = false }]

and aexpr =
  | ALet of ident * cexpr * aexpr
  | ACExpr of cexpr
[@@deriving show { with_path = false }]

type aconstruction =
  | AExpr of aexpr
  | AStatement of is_recursive * (ident * aexpr) list
[@@deriving show { with_path = false }]

type aconstructions = aconstruction list [@@deriving show { with_path = false }]

type anf_error =
  | Unreachable
  | Not_Yet_Implemented of string
[@@deriving show { with_path = false }]

let pp_anf_error fmt = function
  | Unreachable -> fprintf fmt "Panic: reached unreachable state in ANF computation"
  | Not_Yet_Implemented str ->
    fprintf fmt "ANF for this structure is not yet implemented: %s" str
[@@deriving show { with_path = false }]
;;

open ResultCounter.ResultCounterMonad
open Syntax

let gen_temp base =
  let* c = read in
  let new_c = c + 1 in
  let* () = write new_c in
  return (Ident (Stdlib.Format.sprintf "%s_%d" base c))
;;

let binop_map = function
  | Binary_add -> return ("res_of_plus", CPlus)
  | Binary_subtract -> return ("res_of_minus", CMinus)
  | Binary_multiply -> return ("res_of_mul", CMul)
  | Binary_divide -> return ("res_of_div", CDiv)
  | Binary_equal -> return ("eq", CEq)
  | Binary_unequal -> return ("neq", CNeq)
  | Binary_less -> return ("lt", CLt)
  | Binary_less_or_equal -> return ("lte", CLte)
  | Binary_greater -> return ("gt", CGt)
  | Binary_greater_or_equal -> return ("gte", CGte)
  | _ -> fail (Not_Yet_Implemented "binary operator")
;;

let rec collect_app_args e =
  match e with
  | Apply (f, a) ->
    let fn, args = collect_app_args f in
    fn, args @ [ a ]
  | _ -> e, []
;;

module FuncSet = Set.Make (struct
    type t = ident

    let compare = compare
  end)

type state =
  { lifted_lams : (ident * cexpr) list
  ; lifted_letins : (ident * cexpr) list
  ; functions : FuncSet.t
  }

let empty_state = { lifted_lams = []; lifted_letins = []; functions = FuncSet.empty }

(* let pp_state fmt (state : state) =
  let pp_list fmt lst =
    Format.fprintf fmt "[";
    List.iter (fun (Ident id, _) -> Format.fprintf fmt "%s; " id) lst;
    Format.fprintf fmt "]"
  in
  Format.fprintf
    fmt
    "{ lifted_lams = %a; lifted_letins = %a }"
    pp_list
    state.lifted_lams
    pp_list
    state.lifted_letins
;; *)

let rec anf (state : state) e expr_with_hole =
  let anf_binop opname op left right expr_with_hole =
    let* varname = gen_temp opname in
    let* left_anf, state1 =
      anf state left (fun limm ->
        let* right_anf, state2 =
          anf state right (fun rimm ->
            let* inner, state3 = expr_with_hole (ImmId varname) in
            return (ALet (varname, CBinop (op, limm, rimm), inner), state3))
        in
        return (right_anf, state2))
    in
    return (left_anf, state1)
  in
  match e with
  | Const (Int_lt n) -> expr_with_hole (ImmNum n)
  | Variable id -> expr_with_hole (ImmId id)
  | Bin_expr (op, l, r) ->
    let* opname, op_name = binop_map op in
    anf_binop opname op_name l r expr_with_hole
  | LetIn (_, Let_bind (PVar id, [], expr), [], body) ->
    let* body_anf, state1 = anf state body expr_with_hole in
    anf state1 expr (fun immval -> return (ALet (id, CImmexpr immval, body_anf), state1))
  | LetIn (_, Let_bind (PConst Unit_lt, [], expr), [], body) ->
    anf state expr (fun _ -> anf state body expr_with_hole)
  | LetIn (_, Let_bind (Wild, [], expr), [], body) ->
    anf state expr (fun _ -> anf state body expr_with_hole)
  | LetIn (_, Let_bind (PVar id, args, expr), [], body) ->
    let* arg_names =
      List.fold_right
        (fun pat acc ->
           let* names = acc in
           match pat with
           | PVar s -> return (s :: names)
           | _ -> fail (Not_Yet_Implemented "complex patterns"))
        args
        (return [])
    in
    let* value, state1 =
      anf state expr (fun imm -> return (ACExpr (CImmexpr imm), state))
    in
    let clams =
      List.fold_right (fun id body -> ACExpr (CLam (id, body))) arg_names value
    in
    let* cclams =
      match clams with
      | ACExpr c -> return c
      | _ -> fail Unreachable
    in
    let* body, state2 = anf state1 body expr_with_hole in
    let state3 =
      if List.mem (id, cclams) state2.lifted_letins
      then state2
      else
        { state2 with
          lifted_letins = state2.lifted_letins @ [ id, cclams ]
        ; functions = FuncSet.add id state2.functions
        }
    in
    return (body, state3)
  | If_then_else (cond, thn, Some els) ->
    let* thn, state1 = anf state thn expr_with_hole in
    let* els, state2 = anf state1 els expr_with_hole in
    anf state2 cond (fun condimm ->
      return (ACExpr (CIte (CImmexpr condimm, thn, Some els)), state2))
  | Apply (f, args) ->
    let f, arg_exprs = collect_app_args (Apply (f, args)) in
    anf state f (fun fimm ->
      let rec anf_args acc st = function
        | [] ->
          let* varname = gen_temp "res_of_app" in
          let* e, state1 = expr_with_hole (ImmId varname) in
          return (ALet (varname, CApp (fimm, List.rev acc), e), state1)
        | [ Const Unit_lt ] ->
          let* varname = gen_temp "res_of_app" in
          let* e, state1 = expr_with_hole (ImmId varname) in
          return (ALet (varname, CApp (fimm, List.rev acc), e), state1)
        | expr :: rest -> anf st expr (fun immval -> anf_args (immval :: acc) st rest)
      in
      anf_args [] state arg_exprs)
  | Lambda (first, rest, body) ->
    let* arg_names =
      List.fold_right
        (fun pat acc ->
           let* names = acc in
           match pat with
           | PVar s -> return (s :: names)
           | _ -> fail (Not_Yet_Implemented "complex patterns"))
        (first :: rest)
        (return [])
    in
    let* varname = gen_temp "lam" in
    let* e, state1 = expr_with_hole (ImmId varname) in
    let pair_exists lst (id, ce) =
      List.exists (fun (id', ce') -> id = id' && ce = ce') lst
    in
    let merge_unique_pairs lst1 lst2 =
      lst2
      |> List.fold_left
           (fun acc pair -> if pair_exists lst1 pair then acc else pair :: acc)
           lst1
    in
    let lifted_lams = merge_unique_pairs state1.lifted_lams state.lifted_lams in
    let state2 = { state1 with lifted_lams } in
    let* body, state2 =
      anf state2 body (fun imm -> return (ACExpr (CImmexpr imm), state2))
    in
    let clams =
      List.fold_right (fun id body -> ACExpr (CLam (id, body))) arg_names body
    in
    let* cclams =
      match clams with
      | ACExpr c -> return c
      | _ -> fail Unreachable
    in
    let* lifted_name = gen_temp "lifted_lam" in
    let state3 =
      if List.mem (lifted_name, cclams) state2.lifted_lams
      then state2
      else
        { state2 with
          lifted_lams = state2.lifted_lams @ [ lifted_name, cclams ]
        ; functions = FuncSet.add lifted_name state2.functions
        }
    in
    (* Format.printf "state3 %a@. \n" pp_state state3; *)
    return (ALet (varname, CImmexpr (ImmId lifted_name), e), state3)
  | _ ->
    (* Stdlib.Format.printf "%a@." pp_expr e; *)
    fail (Not_Yet_Implemented "ANF expr")
;;

let anf_construction (state : state) = function
  | Statement (Let (flag, Let_bind (PVar id, [], expr), [])) ->
    let* value, state1 =
      anf state expr (fun immval -> return (ACExpr (CImmexpr immval), state))
    in
    return (AStatement (flag, [ id, value ]), state1)
  | Statement (Let (flag, Let_bind (PVar name, args, expr), [])) ->
    let* arg_names =
      List.fold_right
        (fun pat acc ->
           let* names = acc in
           match pat with
           | PVar s -> return (s :: names)
           | _ -> fail (Not_Yet_Implemented "complex patterns"))
        args
        (return [])
    in
    let* value, state1 =
      anf state expr (fun imm -> return (ACExpr (CImmexpr imm), state))
    in
    let clams =
      List.fold_right (fun id body -> ACExpr (CLam (id, body))) arg_names value
    in
    let state2 = { state1 with functions = FuncSet.add name state1.functions } in
    return (AStatement (flag, [ name, clams ]), state2)
  | Expr e ->
    let* inner, state1 =
      anf state e (fun immval -> return (ACExpr (CImmexpr immval), state))
    in
    return (AExpr inner, state1)
  | _ -> fail (Not_Yet_Implemented "ANF construction")
;;

(*
   let name = func1 func2 args... in ...
   should become 
   let name1 = func2 args in 
   let name2 = func1 name1 in ...
*)

let rec refine_applications (state : state) (ae : aexpr) =
  match ae with
  | ALet (id, CApp (f, args), body) ->
    let* body', state' = refine_applications state body in
    (match args with
     | [] -> return (ALet (id, CApp (f, []), body'), state')
     | inner_func :: inner_args ->
       (match inner_func, inner_args with
        | ImmId name, _ ->
          (match FuncSet.find_opt name state.functions with
           | Some func ->
             let* inner = gen_temp "res_of_inner" in
             let new_let =
               ALet
                 ( inner
                 , CApp (ImmId func, inner_args)
                 , ALet (id, CApp (f, [ ImmId inner ]), body') )
             in
             let* new_new_let, state'' = refine_applications state' new_let in
             return (new_new_let, state'')
           | None -> return (ALet (id, CApp (f, args), body'), state'))
        | _, _ -> return (ALet (id, CApp (f, args), body'), state')))
  | ALet (id, ce, body) ->
    let* body', state' = refine_applications state body in
    return (ALet (id, ce, body'), state')
  | _ -> return (ae, state)
;;

let refine_applications_aconstr = function
  | AStatement (flag, [ (name, ae) ]), state ->
    let* ae', state' = refine_applications state ae in
    return (AStatement (flag, [ name, ae' ]), state')
  | AExpr ae, state ->
    let* ae', state' = refine_applications state ae in
    return (AExpr ae', state')
  | _ -> fail (Not_Yet_Implemented "ANF construction")
;;

let rec anf_constructions (state : state) = function
  | c :: rest ->
    let* c_anf, state1 = anf_construction state c in
    let* c_anf, state1 = refine_applications_aconstr (c_anf, state1) in
    let* rest_anf, state2 = anf_constructions state1 rest in
    return (c_anf :: rest_anf, state2)
  | [] -> return ([], state)
;;

(* ---------- Closure conversion & Lambda lifting ---------- *)

module IdentSet = Set.Make (struct
    type t = ident

    let compare = compare
  end)

(* let pp_identset fmt s =
  Format.fprintf fmt "{";
  IdentSet.iter (fun (Ident id) -> Format.fprintf fmt "%s; " id) s;
  Format.fprintf fmt "}"
;; *)

let find_lifted (id : ident) (lams : (ident * cexpr) list) : cexpr option =
  match List.find_opt (fun (lam_id, _) -> lam_id = id) lams with
  | Some (_, expr) -> Some expr
  | None -> None
;;

(* ---------- collect free vars: from fun m -> k (m*n) we get {k, n} ---------- *)
let rec free_vars_imm lams = function
  | ImmId id ->
    (match find_lifted id lams with
     | Some cexpr -> free_vars_cexpr lams cexpr
     | None -> IdentSet.singleton id)
  | ImmNum _ -> IdentSet.empty

and free_vars_aexpr lams (expr : aexpr) : IdentSet.t =
  match expr with
  | ALet (id, cexpr, body) ->
    let fv_c = free_vars_cexpr lams cexpr in
    let fv_b = free_vars_aexpr lams body in
    IdentSet.union fv_c (IdentSet.remove id fv_b)
  | ACExpr c -> free_vars_cexpr lams c

and free_vars_cexpr lams = function
  | CImmexpr imm -> free_vars_imm lams imm
  | CBinop (_, l, r) -> IdentSet.union (free_vars_imm lams l) (free_vars_imm lams r)
  | CApp (ImmId f, args) ->
    let fv_args =
      List.fold_left
        (fun acc -> function
           | ImmId id -> IdentSet.add id acc
           | _ -> acc)
        IdentSet.empty
        args
    in
    IdentSet.add f fv_args
  | CApp (_, args) ->
    List.fold_left
      (fun acc -> function
         | ImmId id -> IdentSet.add id acc
         | _ -> acc)
      IdentSet.empty
      args
  | CLam (param, body) ->
    let fv_body = free_vars_aexpr lams body in
    (* Format.printf
      "Free vars in lambda (param=%s): %a@."
      (show_ident param)
      pp_identset
      fv_body; *)
    IdentSet.remove param fv_body
  | CIte (cond, t, fopt) ->
    let fv_cond = free_vars_cexpr lams cond in
    let fv_t = free_vars_aexpr lams t in
    let fv_f =
      match fopt with
      | Some e -> free_vars_aexpr lams e
      | None -> IdentSet.empty
    in
    IdentSet.union fv_cond (IdentSet.union fv_t fv_f)
;;

module IdentMap = Map.Make (struct
    type t = ident

    let compare = compare
  end)

let collect_freevars_map (lams : (ident * cexpr) list) : IdentSet.t IdentMap.t =
  List.fold_left
    (fun acc (id, expr) ->
       let fv = free_vars_cexpr lams expr in
       IdentMap.add id fv acc)
    IdentMap.empty
    lams
;;

(* ---------- add free vars: from fun m -> k (m*n) to fun k n m -> k (m*n) ---------- *)
let add_free_args_lam (lams : (ident * cexpr) list) : (ident * cexpr) list =
  List.map
    (fun (id, expr) ->
       let fv = free_vars_cexpr lams expr in
       let fv_list = IdentSet.elements fv in
       let wrapped =
         List.fold_right (fun fv_id acc -> CLam (fv_id, ACExpr acc)) fv_list expr
       in
       id, wrapped)
    lams
;;

(* ---------- apply args: from fun k n m -> k (m*n) to (fun k n m -> k (m*n)) k n ---------- *)
let rec apply_lifted_args_aexpr env (ae : aexpr) : aexpr =
  match ae with
  | ALet (id, ce, body) ->
    ALet (id, apply_lifted_args_cexpr env ce, apply_lifted_args_aexpr env body)
  | ACExpr c -> ACExpr (apply_lifted_args_cexpr env c)

and apply_lifted_args_cexpr env (ce : cexpr) : cexpr =
  match ce with
  | CImmexpr (ImmId lam_id) when IdentMap.mem lam_id env ->
    let fv = IdentSet.elements (IdentMap.find lam_id env) in
    CApp (ImmId lam_id, List.map (fun v -> ImmId v) fv)
  | CApp (ImmId f, args) -> CApp (ImmId f, args)
  | CLam (id, body) -> CLam (id, apply_lifted_args_aexpr env body)
  | CIte (cond, thn, els) ->
    CIte
      ( apply_lifted_args_cexpr env cond
      , apply_lifted_args_aexpr env thn
      , Option.map (apply_lifted_args_aexpr env) els )
  | CBinop (op, l, r) -> CBinop (op, l, r)
  | _ -> ce
;;

let apply_lifted_args_aconstruction env = function
  | AExpr ae -> AExpr (apply_lifted_args_aexpr env ae)
  | AStatement (flag, binds) ->
    let binds' = List.map (fun (id, ae) -> id, apply_lifted_args_aexpr env ae) binds in
    AStatement (flag, binds')
;;

(* ---------- lift lambdas ---------- *)
let lift_program (state : state) acs =
  let lifted_lams = state.lifted_lams @ state.lifted_letins in
  let lifted_top_level =
    List.map (fun (id, ce) -> AStatement (Nonrec, [ id, ACExpr ce ])) lifted_lams
  in
  return (lifted_top_level @ acs)
;;

let anf_and_lift_program ast =
  let* anf_program, final_state = anf_constructions empty_state ast in
  (* Format.printf "final state %a@. \n" pp_state final_state; *)
  let fv_map = collect_freevars_map final_state.lifted_lams in
  let wrapped = add_free_args_lam final_state.lifted_lams in
  let lams =
    List.map (fun (id, expr) -> id, apply_lifted_args_cexpr fv_map expr) wrapped
  in
  let letins =
    List.map
      (fun (id, expr) -> id, apply_lifted_args_cexpr fv_map expr)
      final_state.lifted_letins
  in
  let anf_program_with_apps =
    List.map (apply_lifted_args_aconstruction fv_map) anf_program
  in
  let final_state' = { final_state with lifted_lams = lams; lifted_letins = letins } in
  lift_program final_state' anf_program_with_apps
;;
