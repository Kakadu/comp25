module Map = Map.Make (String)
module Set = Set.Make (String)

type reg =
  | Zero
  | Ra
  | Sp
  | Gp
  | Arg of int
  | Saved of int
  | Temp of int
[@@deriving variants]

let rv = arg 0
let bp = saved 0

let pp_reg ppf =
  Format.(
    function
    | Zero -> fprintf ppf "zero"
    | Ra -> fprintf ppf "ra"
    | Sp -> fprintf ppf "sp"
    | Gp -> fprintf ppf "gp"
    | Arg c -> fprintf ppf "a%d" c
    | Saved c -> fprintf ppf "s%d" c
    | Temp c -> fprintf ppf "t%d" c)
;;

type instr =
  (* Arithmetic instructions *)
  | Addi of reg * reg * int
  | Add of reg * reg * reg
  | Sh1add of reg * reg * reg
  | Sh2add of reg * reg * reg
  | Sh3add of reg * reg * reg
  | Add_wu of reg * reg * reg
  | Sh1add_wu of reg * reg * reg
  | Sh2add_wu of reg * reg * reg
  | Sh3add_wu of reg * reg * reg
  | Addiw of reg * reg * int
  | Addw of reg * reg * reg
  | Sub of reg * reg * reg
  | Subw of reg * reg * reg
  | Neg of reg * reg
  | Negw of reg * reg
  | Mul of reg * reg * reg
  | Mulw of reg * reg * reg
  | Mulh of reg * reg * reg
  | Mulhu of reg * reg * reg
  | Mulhsu of reg * reg * reg
  | Div of reg * reg * reg
  | Divu of reg * reg * reg
  | Rem of reg * reg * reg
  | Remu of reg * reg * reg
  | Min of reg * reg * reg
  | Max of reg * reg * reg
  | Minu of reg * reg * reg
  | Maxu of reg * reg * reg
  (* Comparison instructions *)
  | Seqz of reg * reg
  | Snez of reg * reg
  | Slti of reg * reg * int
  | Slt of reg * reg * reg
  | Sltiu of reg * reg * int
  | Sltu of reg * reg * reg
  (* Bit manipulation instructions *)
  | Bexti of reg * reg * int
  | Bext of reg * reg * reg
  | Andi of reg * reg * int
  | And of reg * reg * reg
  | Andn of reg * reg * reg
  | Bclri of reg * reg * int
  | Bclr of reg * reg * reg
  | Ori of reg * reg * int
  | Or of reg * reg * reg
  | Orn of reg * reg * reg
  | Bseti of reg * reg * int
  | Bset of reg * reg * reg
  | Xori of reg * reg * int
  | Xor of reg * reg * reg
  | Xnor of reg * reg * reg
  | Binvi of reg * reg * int
  | Binv of reg * reg * reg
  | Not of reg * reg
  | Orc_b of reg * reg
  (* Shift instructions *)
  | Slli of reg * reg * int
  | Sll of reg * reg * reg
  | Slliw of reg * reg * int
  | Sllw of reg * reg * reg
  | Slli_wu of reg * reg * int
  | Srli of reg * reg * int
  | Srl of reg * reg * reg
  | Srliw of reg * reg * int
  | Srlw of reg * reg * reg
  | Srai of reg * reg * int
  | Sra of reg * reg * reg
  | Sraiw of reg * reg * int
  | Sraw of reg * reg * reg
  | Rori of reg * reg * int
  | Ror of reg * reg * reg
  | Rol of reg * reg * reg
  | Roriw of reg * reg * int
  | Rorw of reg * reg * reg
  | Rolw of reg * reg * reg
  (* Count instructions *)
  | Clz of reg * reg
  | Clzw of reg * reg
  | Ctz of reg * reg
  | Ctzw of reg * reg
  | Cpop of reg * reg
  | Cpopw of reg * reg
  (* Jump and branch instructions *)
  | J of string (* label *)
  | Jal of reg * string
  | Jr of reg * int option
  | Jalr of reg * reg * int option
  | Call of string (* symbol *)
  | Tail of string (* symbol *)
  | Ret
  | Beq of reg * reg * string (* label *)
  | Bne of reg * reg * string (* label *)
  | Blt of reg * reg * string (* label *)
  | Bgt of reg * reg * string (* label *)
  | Bge of reg * reg * string (* label *)
  | Ble of reg * reg * string (* label *)
  | Bltu of reg * reg * string (* label *)
  | Bgtu of reg * reg * string (* label *)
  | Bgeu of reg * reg * string (* label *)
  | Bleu of reg * reg * string (* label *)
  (* Load & store *)
  | La of reg * string
  | Ld of reg * int * reg
  | Sd of reg * int * reg
[@@deriving variants]

let mv rd rs = addi rd rs 0
let li rd v = addi rd zero v

let _pp_instr fmt =
  Format.(
    function
    (* Arithmetic instructions *)
    | Addi (rd, rs, imm) -> fprintf fmt "addi %a, %a, %d" pp_reg rd pp_reg rs imm
    | Add (rd, rs1, rs2) -> fprintf fmt "add %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Sh1add (rd, rs1, rs2) ->
      fprintf fmt "sh1add %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Sh2add (rd, rs1, rs2) ->
      fprintf fmt "sh2add %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Sh3add (rd, rs1, rs2) ->
      fprintf fmt "sh3add %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Add_wu (rd, rs1, rs2) ->
      fprintf fmt "add.wu %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Sh1add_wu (rd, rs1, rs2) ->
      fprintf fmt "sh1add.wu %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Sh2add_wu (rd, rs1, rs2) ->
      fprintf fmt "sh2add.wu %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Sh3add_wu (rd, rs1, rs2) ->
      fprintf fmt "sh3add.wu %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Addiw (rd, rs, imm) -> fprintf fmt "addiw %a, %a, %d" pp_reg rd pp_reg rs imm
    | Addw (rd, rs1, rs2) -> fprintf fmt "addw %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Sub (rd, rs1, rs2) -> fprintf fmt "sub %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Subw (rd, rs1, rs2) -> fprintf fmt "subw %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Neg (rd, rs) -> fprintf fmt "neg %a, %a" pp_reg rd pp_reg rs
    | Negw (rd, rs) -> fprintf fmt "negw %a, %a" pp_reg rd pp_reg rs
    | Mul (rd, rs1, rs2) -> fprintf fmt "mul %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Mulw (rd, rs1, rs2) -> fprintf fmt "mulw %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Mulh (rd, rs1, rs2) -> fprintf fmt "mulh %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Mulhu (rd, rs1, rs2) ->
      fprintf fmt "mulhu %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Mulhsu (rd, rs1, rs2) ->
      fprintf fmt "mulhsu %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Div (rd, rs1, rs2) -> fprintf fmt "div %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Divu (rd, rs1, rs2) -> fprintf fmt "divu %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Rem (rd, rs1, rs2) -> fprintf fmt "rem %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Remu (rd, rs1, rs2) -> fprintf fmt "remu %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Min (rd, rs1, rs2) -> fprintf fmt "min %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Max (rd, rs1, rs2) -> fprintf fmt "max %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Minu (rd, rs1, rs2) -> fprintf fmt "minu %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Maxu (rd, rs1, rs2) -> fprintf fmt "maxu %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    (* Comparison instructions *)
    | Seqz (rd, rs) -> fprintf fmt "seqz %a, %a" pp_reg rd pp_reg rs
    | Snez (rd, rs) -> fprintf fmt "snez %a, %a" pp_reg rd pp_reg rs
    | Slti (rd, rs, imm) -> fprintf fmt "slti %a, %a, %d" pp_reg rd pp_reg rs imm
    | Slt (rd, rs1, rs2) -> fprintf fmt "slt %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Sltiu (rd, rs, imm) -> fprintf fmt "sltiu %a, %a, %d" pp_reg rd pp_reg rs imm
    | Sltu (rd, rs1, rs2) -> fprintf fmt "sltu %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    (* Bit manipulation instructions *)
    | Bexti (rd, rs, imm) -> fprintf fmt "bexti %a, %a, %d" pp_reg rd pp_reg rs imm
    | Bext (rd, rs1, rs2) -> fprintf fmt "bext %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Andi (rd, rs, imm) -> fprintf fmt "andi %a, %a, %d" pp_reg rd pp_reg rs imm
    | And (rd, rs1, rs2) -> fprintf fmt "and %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Andn (rd, rs1, rs2) -> fprintf fmt "andn %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Bclri (rd, rs, imm) -> fprintf fmt "bclri %a, %a, %d" pp_reg rd pp_reg rs imm
    | Bclr (rd, rs1, rs2) -> fprintf fmt "bclr %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Ori (rd, rs, imm) -> fprintf fmt "ori %a, %a, %d" pp_reg rd pp_reg rs imm
    | Or (rd, rs1, rs2) -> fprintf fmt "or %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Orn (rd, rs1, rs2) -> fprintf fmt "orn %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Bseti (rd, rs, imm) -> fprintf fmt "bseti %a, %a, %d" pp_reg rd pp_reg rs imm
    | Bset (rd, rs1, rs2) -> fprintf fmt "bset %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Xori (rd, rs, imm) -> fprintf fmt "xori %a, %a, %d" pp_reg rd pp_reg rs imm
    | Xor (rd, rs1, rs2) -> fprintf fmt "xor %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Xnor (rd, rs1, rs2) -> fprintf fmt "xnor %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Binvi (rd, rs, imm) -> fprintf fmt "binvi %a, %a, %d" pp_reg rd pp_reg rs imm
    | Binv (rd, rs1, rs2) -> fprintf fmt "binv %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Not (rd, rs) -> fprintf fmt "not %a, %a" pp_reg rd pp_reg rs
    | Orc_b (rd, rs) -> fprintf fmt "orc.b %a, %a" pp_reg rd pp_reg rs
    (* Shift instructions *)
    | Slli (rd, rs, imm) -> fprintf fmt "slli %a, %a, %d" pp_reg rd pp_reg rs imm
    | Sll (rd, rs1, rs2) -> fprintf fmt "sll %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Slliw (rd, rs, imm) -> fprintf fmt "slliw %a, %a, %d" pp_reg rd pp_reg rs imm
    | Sllw (rd, rs1, rs2) -> fprintf fmt "sllw %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Slli_wu (rd, rs, imm) -> fprintf fmt "slli.wu %a, %a, %d" pp_reg rd pp_reg rs imm
    | Srli (rd, rs, imm) -> fprintf fmt "srli %a, %a, %d" pp_reg rd pp_reg rs imm
    | Srl (rd, rs1, rs2) -> fprintf fmt "srl %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Srliw (rd, rs, imm) -> fprintf fmt "srliw %a, %a, %d" pp_reg rd pp_reg rs imm
    | Srlw (rd, rs1, rs2) -> fprintf fmt "srlw %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Srai (rd, rs, imm) -> fprintf fmt "srai %a, %a, %d" pp_reg rd pp_reg rs imm
    | Sra (rd, rs1, rs2) -> fprintf fmt "sra %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Sraiw (rd, rs, imm) -> fprintf fmt "sraiw %a, %a, %d" pp_reg rd pp_reg rs imm
    | Sraw (rd, rs1, rs2) -> fprintf fmt "sraw %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Rori (rd, rs, imm) -> fprintf fmt "rori %a, %a, %d" pp_reg rd pp_reg rs imm
    | Ror (rd, rs1, rs2) -> fprintf fmt "ror %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Rol (rd, rs1, rs2) -> fprintf fmt "rol %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Roriw (rd, rs, imm) -> fprintf fmt "roriw %a, %a, %d" pp_reg rd pp_reg rs imm
    | Rorw (rd, rs1, rs2) -> fprintf fmt "rorw %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Rolw (rd, rs1, rs2) -> fprintf fmt "rolw %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    (* Count instructions *)
    | Clz (rd, rs) -> fprintf fmt "clz %a, %a" pp_reg rd pp_reg rs
    | Clzw (rd, rs) -> fprintf fmt "clzw %a, %a" pp_reg rd pp_reg rs
    | Ctz (rd, rs) -> fprintf fmt "ctz %a, %a" pp_reg rd pp_reg rs
    | Ctzw (rd, rs) -> fprintf fmt "ctzw %a, %a" pp_reg rd pp_reg rs
    | Cpop (rd, rs) -> fprintf fmt "cpop %a, %a" pp_reg rd pp_reg rs
    | Cpopw (rd, rs) -> fprintf fmt "cpopw %a, %a" pp_reg rd pp_reg rs
    (* Jump and branch instructions *)
    | J label -> fprintf fmt "j %s" label
    | Jal (rd, imm) -> fprintf fmt "jal %a, %s" pp_reg rd imm
    | Jr (rs, None) -> fprintf fmt "jr %a" pp_reg rs
    | Jr (rs, Some imm) -> fprintf fmt "jr %a, %d" pp_reg rs imm
    | Jalr (rd, rs, None) -> fprintf fmt "jalr %a, %a" pp_reg rd pp_reg rs
    | Jalr (rd, rs, Some imm) -> fprintf fmt "jalr %a, %a, %d" pp_reg rd pp_reg rs imm
    | Call symbol -> fprintf fmt "call %s" symbol
    | Tail symbol -> fprintf fmt "tail %s" symbol
    | Ret -> fprintf fmt "ret"
    | Beq (rs1, rs2, label) -> fprintf fmt "beq %a, %a, %s" pp_reg rs1 pp_reg rs2 label
    | Bne (rs1, rs2, label) -> fprintf fmt "bne %a, %a, %s" pp_reg rs1 pp_reg rs2 label
    | Blt (rs1, rs2, label) -> fprintf fmt "blt %a, %a, %s" pp_reg rs1 pp_reg rs2 label
    | Bgt (rs1, rs2, label) -> fprintf fmt "bgt %a, %a, %s" pp_reg rs1 pp_reg rs2 label
    | Bge (rs1, rs2, label) -> fprintf fmt "bge %a, %a, %s" pp_reg rs1 pp_reg rs2 label
    | Ble (rs1, rs2, label) -> fprintf fmt "ble %a, %a, %s" pp_reg rs1 pp_reg rs2 label
    | Bltu (rs1, rs2, label) -> fprintf fmt "bltu %a, %a, %s" pp_reg rs1 pp_reg rs2 label
    | Bgtu (rs1, rs2, label) -> fprintf fmt "bgtu %a, %a, %s" pp_reg rs1 pp_reg rs2 label
    | Bgeu (rs1, rs2, label) -> fprintf fmt "bgeu %a, %a, %s" pp_reg rs1 pp_reg rs2 label
    | Bleu (rs1, rs2, label) -> fprintf fmt "bleu %a, %a, %s" pp_reg rs1 pp_reg rs2 label
    (* Load & Store *)
    | La (rd, label) -> fprintf fmt "la %a, %s" pp_reg rd label
    | Ld (rd, offset, rs) -> fprintf fmt "ld %a, %d(%a)" pp_reg rd offset pp_reg rs
    | Sd (rs, offset, rd) -> fprintf fmt "sd %a, %d(%a)" pp_reg rs offset pp_reg rd)
;;

type loc =
  | Mem of int
  | Reg of reg
[@@deriving variants]

type line =
  | Instr of instr
  | Comment of string
  | Label of string
  | Etc of string
[@@deriving variants]

let todo () = failwith "todo"

module Ctx = struct
  type t =
    { code : line list
    ; temp_regs : bool Map.t
    ; fail : string option
    ; arities : int Map.t
    ; offset : int
    ; offsets : int Map.t
    ; lbls : Set.t
    }

  let arities =
    Builtin.all
    |> List.map (fun (builtin : Builtin.t) -> builtin.name, builtin.arity)
    |> Map.of_list
  ;;

  let empty =
    { code = []
    ; temp_regs = Map.empty
    ; fail = None
    ; arities
    ; offset = -16
    ; offsets = Map.empty
    ; lbls = Set.empty
    }
  ;;

  let arity name (ctx : t) = Map.find_opt name ctx.arities
  let comment (c : string) (ctx : t) = { ctx with code = comment c :: ctx.code }
  let etc (c : string) (ctx : t) = { ctx with code = etc c :: ctx.code }

  let lbl ?prefix (ctx : t) =
    let rec aux cnt =
      let prefix = Option.value ~default:"lbl" prefix in
      let name = String.concat "" [ prefix; Int.to_string cnt ] in
      if Set.mem name ctx.lbls
      then aux (cnt + 1)
      else name, { ctx with lbls = Set.add name ctx.lbls }
    in
    aux 0
  ;;

  let label (l : string) (ctx : t) = { ctx with code = label l :: ctx.code }
  let emit (i : instr) (ctx : t) = { ctx with code = instr i :: ctx.code }

  (*let as_reg (loc : loc) (ctx : t) = match loc with
    | Reg reg -> reg
    (*| Mem ->*)
  ;;*)

  let as_arg (n : int) (loc : loc) (ctx : t) =
    match loc with
    | Reg (Arg n' as r) when n' = n -> r, ctx
    | Reg rs ->
      let rd = arg n in
      let ctx = emit (mv rd rs) ctx in
      rd, ctx
    | Mem offset ->
      let rd = arg n in
      let ctx = emit (ld rd offset bp) ctx in
      rd, ctx
  ;;

  (*| Mem ->*)

  let fail (msg : string) (ctx : t) = { ctx with fail = Option.some msg }
  let total () (ctx : t) = ctx.offset, { ctx with offset = -16 }
  let offset (var : string) (ctx : t) = Map.find_opt var ctx.offsets

  let push r (ctx : t) =
    let new_offset = ctx.offset - 8 in
    let ctx = emit (addi sp sp ~-8) ctx in
    let ctx = emit (sd r 0 sp) ctx in
    new_offset, { ctx with offset = new_offset }
  ;;

  let var (name : string) (ctx : t) =
    let offset, ctx = push zero ctx in
    let offsets = Map.add name offset ctx.offsets in
    offset, { ctx with offsets }
  ;;

  (*addi sp, sp, -8
    sd   t0, 0(sp)*)

  let args args' ctx =
    List.fold_right
      (fun (i, a) acc ->
         let offset, ctx = var a acc in
         let ctx = if i < 8 then emit (sd (arg i) offset bp) ctx else todo () in
         ctx)
      (args' |> List.mapi (fun i a -> i, a))
      ctx
  ;;

  let define name argc ctx = { ctx with arities = Map.add name argc arities }
end

module State = struct
  include State.M (Ctx)

  let arity name = get >>= fun ctx -> return (Ctx.arity name ctx)
  let total = Ctx.total
  let offset var = get >>= fun ctx -> return (Ctx.offset var ctx)
  let emit instr ctx = put (Ctx.emit instr ctx) ()
  let label lbl ctx = put (Ctx.label lbl ctx) ()
  let comment c ctx = put (Ctx.comment c ctx) ()
  let etc c ctx = put (Ctx.etc c ctx) ()
  let as_arg = Ctx.as_arg
  let var = Ctx.var
  let push = Ctx.push
  let define name argc ctx = put (Ctx.define name argc ctx) ()
  let args args' ctx = put (Ctx.args args' ctx) ()
  let empty = Ctx.empty
  let lbl = Ctx.lbl
  (*let as_reg = Ctx.as_reg*)
end

open State

let spf = Format.asprintf
let failf fmt = Format.kasprintf failwith fmt

let immexpr reg = function
  | Anf.ImmNum c ->
    let* () = emit (li reg c) in
    return reg
  | Anf.ImmUnit -> failf "todo"
  | ImmId var ->
    let* offset = offset var in
    (match offset with
     | Some offset ->
       let* () = emit (ld reg offset bp) in
       return reg
     | None -> failf "Unknown variable %s" var)
  | Anf.ImmTuple _ -> failf "todo"
;;

let rec cexpr =
  let capp name args =
    let binop op lhs rhs =
      let op =
        fun rd rs1 rs2 ->
        let return v = [ v rd rs1 rs2 ] in
        match op with
        | "+" -> return add
        | "-" -> return sub
        | "*" -> return mul
        | "/" -> return div
        | "<" -> return slt
        | "<=" -> [ slt rd rs2 rs1; xori rd rd 1 ]
        | ">" -> [ slt rd rs2 rs1 ]
        | ">=" -> [ slt rd rs1 rs2; xori rd rd 1 ]
        | "=" -> [ sub (temp 0) rs1 rs2; seqz rd (temp 0) ]
        | "<>" -> [ sub (temp 0) rs1 rs2; snez rd (temp 0) ]
        | _ -> assert false
      in
      let* lhs = immexpr (arg 0) lhs in
      let* rhs = immexpr (arg 1) rhs in
      let* () =
        List.fold_left
          (fun acc instr ->
             let* _ = acc in
             emit instr)
          (return ())
          (op rv lhs rhs)
      in
      return rv
    in
    match name, args with
    | (("+" | "-" | "*" | "/" | "<" | ">" | "<=" | ">=" | "=") as op), [ lhs; rhs ] ->
      binop op lhs rhs
    | name, args ->
      let* arity = arity name in
      let* arity =
        match arity with
        | Some arity -> return (`Fun arity)
        | None ->
          let* offset = offset name in
          let* arity =
            match offset with
            | Some offset ->
              let* () = comment "here should be a partial call" in
              return (`Closure offset)
            | None -> todo ()
          in
          return arity
      in
      let argc = List.length args in
      (match arity with
       | `Fun arity when arity == argc ->
         let* _ =
           List.fold_left
             (fun acc (i, a) ->
                let* acc = acc in
                let* a = immexpr (arg i) a in
                return (a :: acc))
             (return [])
             (args |> List.mapi (fun i arg -> i, arg))
         in
         let* () = emit (jal ra name) in
         return rv
       | `Fun arity when argc < arity ->
         let* _offset =
           List.fold_right
             (fun a acc ->
                let* _ = acc in
                let* a = immexpr (temp 0) a in
                let* offset = push a in
                return offset)
             args
             (return 0)
         in
         let* () = emit (la (arg 0) name) in
         let* () = emit (li (arg 1) arity) in
         let* () = emit (li (arg 2) argc) in
         let* () = emit (mv (arg 3) sp) in
         let* () = emit (jal ra "create_closure") in
         return rv
       | `Closure offset ->
         let* _offset =
           List.fold_right
             (fun a acc ->
                let* _ = acc in
                let* a = immexpr (temp 0) a in
                let* offset = push a in
                return offset)
             args
             (return 0)
         in
         let* () = emit (ld (arg 0) offset bp) in
         let* () = emit (li (arg 1) argc) in
         let* () = emit (mv (arg 2) sp) in
         let* () = emit (jal ra "closure_apply") in
         return rv
       | `Fun arity ->
         failf
           "Too many arguments (%d) are passed for the function %s, expected %d"
           argc
           name
           arity)
  in
  let cite cond_ then_ else_ =
    let* cond_ = immexpr (temp 0) cond_ in
    let* then_lbl = lbl ~prefix:"then" in
    let* end_lbl = lbl ~prefix:"end" in
    let* () = emit (bne cond_ zero then_lbl) in
    let* _else_ = aexpr else_ in
    let* () = emit (j end_lbl) in
    let* () = label then_lbl in
    let* _then_ = aexpr then_ in
    let* () = label end_lbl in
    return rv
  in
  function
  | Anf.CImm imm -> immexpr rv imm
  | CIte (cond_, then_, else_) -> cite cond_ then_ else_
  | CApp (name, args) -> capp name args

and aexpr = function
  | Anf.AExpr expr -> cexpr expr
  | ALet (bind, v, body) ->
    let* bind = var bind in
    let* v = cexpr v in
    let* () = emit (sd v bind bp) in
    aexpr body
;;

let decl rec_flag name args' body =
  let* () =
    comment
      (spf
         "%s(%a)"
         name
         (Format.pp_print_list
            ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
            Format.pp_print_string)
         args')
  in
  let* () = label name in
  let* () = emit (mv (temp 0) sp) in
  let* () = emit (addi sp sp ~-8) in
  let* () = emit (sd ra 0 sp) in
  let* () = emit (addi sp sp ~-8) in
  let* () = emit (sd bp 0 sp) in
  let* () = emit (mv bp (temp 0)) in
  let* () =
    match rec_flag with
    | Ast.Rec -> define name (List.length args')
    | Ast.NonRec -> return ()
  in
  let* () = args args' in
  let* () = comment (spf "Prelude of %s ended here" name) in
  let* _ = aexpr body in
  let* () = comment (spf "Body of %s ended here" name) in
  let* total = total () in
  let* () = emit (addi sp sp ~-total) in
  let* () = emit (ld ra ~-8 bp) in
  let* () = emit (ld bp ~-16 bp) in
  let* () =
    match rec_flag with
    | Ast.Rec -> return ()
    | Ast.NonRec -> define name (List.length args')
  in
  let* () = if name <> "main" then emit ret else emit (jal ra "exit2") in
  return ()
;;

let riscv decls =
  let ctx = empty in
  let init =
    let* () = etc ".global main" in
    return ()
  in
  let ctx = init ctx |> snd in
  let ctx =
    List.fold_left
      (fun acc (Anf.Decl (rec_flag, name, args, body)) ->
         let* _acc = acc in
         let* _d = decl rec_flag name args body in
         return ())
      (return ())
      decls
      ctx
    |> snd
  in
  ctx.code |> List.rev
;;

let pp_code =
  Format.pp_print_list ~pp_sep:Format.pp_print_newline (fun ppf -> function
    | Etc l -> Format.fprintf ppf "%s" l
    | Label l -> Format.fprintf ppf "%s:" l
    | Comment c -> Format.fprintf ppf "# %s:" c
    | Instr instr -> Format.fprintf ppf "  %a" _pp_instr instr)
;;

let%expect_test "basic" =
  let ast =
    Fe.parse
      {|
    let rec f = fun n ->
      if n = 1 then 1
      else (f (n - 1)) * n
    ;;
  |}
    |> Result.map_error (fun err -> Format.printf "Error %s" err)
    |> Result.get_ok
  in
  Format.printf
    "%a"
    pp_code
    (Cc.cc ast
     |> fun asts ->
     Format.printf
       "CC: %a\n\n"
       (Format.pp_print_list ~pp_sep:Format.pp_print_newline Ast.pp_top_level)
       asts;
     asts
     |> Ll.ll
     |> fun asts ->
     Format.printf
       "LL: %a\n\n"
       (Format.pp_print_list ~pp_sep:Format.pp_print_newline Ast.pp_top_level)
       asts;
     asts
     |> Anf.anf
     |> fun asts ->
     Format.printf
       "ANF %a\n\n"
       (Format.pp_print_list ~pp_sep:Format.pp_print_newline Anf.pp_decl)
       asts;
     asts |> riscv);
  [%expect
    {|
    CC: let rec f = fun n -> if (=) n 1 then 1 else (*) (f ((-) n 1)) n;;


    LL: let rec f = fun n -> if (=) n 1 then 1 else (*) (f ((-) n 1)) n;;


    ANF let rec f n =
            let sup1 =
              (=) n 1
            in
            let ite7 =
              if sup1 then
                1
              else
                let sup4 =
                  (-) n 1
                in
                let sup5 =
                  (f) sup4
                in
                let sup6 =
                  (*) sup5 n
                in
                sup6
            in
            ite7
          ;;


    .global main
    # f(n):
    f:
      addi t0, sp, 0
      addi sp, sp, -8
      sd ra, 0(sp)
      addi sp, sp, -8
      sd s0, 0(sp)
      addi s0, t0, 0
      addi sp, sp, -8
      sd zero, 0(sp)
      sd a0, -24(s0)
    # Prelude of f ended here:
      addi sp, sp, -8
      sd zero, 0(sp)
      ld a0, -24(s0)
      addi a1, zero, 1
      sub t0, a0, a1
      seqz a0, t0
      sd a0, -32(s0)
      addi sp, sp, -8
      sd zero, 0(sp)
      ld t0, -32(s0)
      bne t0, zero, then0
      addi sp, sp, -8
      sd zero, 0(sp)
      ld a0, -24(s0)
      addi a1, zero, 1
      sub a0, a0, a1
      sd a0, -48(s0)
      addi sp, sp, -8
      sd zero, 0(sp)
      ld a0, -48(s0)
      jal ra, f
      sd a0, -56(s0)
      addi sp, sp, -8
      sd zero, 0(sp)
      ld a0, -56(s0)
      ld a1, -24(s0)
      mul a0, a0, a1
      sd a0, -64(s0)
      ld a0, -64(s0)
      j end0
    then0:
      addi a0, zero, 1
    end0:
      sd a0, -40(s0)
      ld a0, -40(s0)
    # Body of f ended here:
      addi sp, sp, 64
      ld ra, -8(s0)
      ld s0, -16(s0)
      ret
    |}]
;;
