[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type reg =
  | Zero
  | RA
  | SP
  | A of int
  | T of int
  | S of int
[@@deriving eq]

type offset = reg * int

let pp_reg ppf =
  let open Format in
  function
  | Zero -> fprintf ppf "zero"
  | RA -> fprintf ppf "ra"
  | SP -> fprintf ppf "sp"
  | A n -> fprintf ppf "a%d" n
  | T n -> fprintf ppf "t%d" n
  | S n -> fprintf ppf "s%d" n
;;

let pp_offset ppf offset = Format.fprintf ppf "%d(%a)" (snd offset) pp_reg (fst offset)

type instr =
  | Addi of reg * reg * int
  | Add of reg * reg * reg
  | Sub of reg * reg * reg
  | Mul of reg * reg * reg
  | Srli of reg * reg * int
  | Slli of reg * reg * int
  | Xori of reg * reg * int
  | Xor of reg * reg * reg
  | Slt of reg * reg * reg
  | Seqz of reg * reg
  | Snez of reg * reg
  | Li of reg * int
  | La of reg * string
  | Mv of reg * reg
  | Ld of reg * offset
  | Sd of reg * offset
  | Beq of reg * reg * string
  | J of string
  | Label of string
  | Call of string
  | Ret
  | Ecall

let pp_instr ppf =
  let open Format in
  function
  | Addi (rd, rs, imm) -> fprintf ppf "addi %a, %a, %d" pp_reg rd pp_reg rs imm
  | Add (rd, rs1, rs2) -> fprintf ppf "add %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | Sub (rd, rs1, rs2) -> fprintf ppf "sub %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | Mul (rd, rs1, rs2) -> fprintf ppf "mul %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | Srli (rd, rs1, imm) -> fprintf ppf "srli %a, %a, %d" pp_reg rd pp_reg rs1 imm
  | Slli (rd, rs1, imm) -> fprintf ppf "slli %a, %a, %d" pp_reg rd pp_reg rs1 imm
  | Xori (rd, rs1, imm) -> fprintf ppf "xori %a, %a, %d" pp_reg rd pp_reg rs1 imm
  | Xor (rd, rs1, rs2) -> fprintf ppf "xor %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | Slt (rd, rs1, rs2) -> fprintf ppf "slt %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
  | Seqz (rd, rs) -> fprintf ppf "seqz %a, %a" pp_reg rd pp_reg rs
  | Snez (rd, rs) -> fprintf ppf "snez %a, %a" pp_reg rd pp_reg rs
  | Li (rd, imm) -> fprintf ppf "li %a, %d" pp_reg rd imm
  | La (rd, s) -> fprintf ppf "la %a, %s" pp_reg rd s
  | Mv (rd, rs) -> fprintf ppf "mv %a, %a" pp_reg rd pp_reg rs
  | Ld (rd, ofs) -> fprintf ppf "ld %a, %a" pp_reg rd pp_offset ofs
  | Sd (rs, ofs) -> fprintf ppf "sd %a, %a" pp_reg rs pp_offset ofs
  | Beq (rs1, rs2, s) -> fprintf ppf "beq %a, %a, %s" pp_reg rs1 pp_reg rs2 s
  | J s -> fprintf ppf "j %s" s
  | Label s -> fprintf ppf "%s:" s
  | Call s -> fprintf ppf "call %s" s
  | Ret -> fprintf ppf "ret"
  | Ecall -> fprintf ppf "ecall"
;;

let addi k rd rs imm = k @@ Addi (rd, rs, imm)
let add k rd rs1 rs2 = k @@ Add (rd, rs1, rs2)
let sub k rd rs1 rs2 = k @@ Sub (rd, rs1, rs2)
let mul k rd rs1 rs2 = k @@ Mul (rd, rs1, rs2)
let srli k rd rs1 imm = k @@ Srli (rd, rs1, imm)
let slli k rd rs1 imm = k @@ Slli (rd, rs1, imm)
let xori k rd rs1 imm = k @@ Xori (rd, rs1, imm)
let xor k rd rs1 rs2 = k @@ Xor (rd, rs1, rs2)
let slt k rd rs1 rs2 = k @@ Slt (rd, rs1, rs2)
let seqz k rd rs = k (Seqz (rd, rs))
let snez k rd rs = k (Snez (rd, rs))
let li k rd imm = k (Li (rd, imm))
let la k rd s = k (La (rd, s))
let mv k rd rs = k (Mv (rd, rs))
let ld k rd ofs = k (Ld (rd, ofs))
let sd k rd ofs = k (Sd (rd, ofs))
let beq k rs1 rs2 s = k @@ Beq (rs1, rs2, s)
let j k s = k (J s)
let label k s = k (Label s)
let call k s = k (Call s)
let ret k = k Ret
let ecall k = k Ecall
let to_tag_integer n = 1 + (n lsl 1)
