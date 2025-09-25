(** Copyright 2025-2026, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

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
  | Li of reg * int
  | Ecall
  | Call of string
  | Ret
  | Ld of reg * offset
  | Sd of reg * offset
  | Mv of reg * reg
  | Beq of reg * reg * string
  | Blt of reg * reg * string
  | Ble of reg * reg * string
  | Slt of reg * reg * reg
  | Xori of reg * reg * int
  | J of string
  | Label of string

let pp_instr ppf =
  let open Format in
  function
  | Addi (r1, r2, n) -> fprintf ppf "addi %a, %a, %d" pp_reg r1 pp_reg r2 n
  | Add (r1, r2, r3) -> fprintf ppf "add  %a, %a, %a" pp_reg r1 pp_reg r2 pp_reg r3
  | Sub (r1, r2, r3) -> fprintf ppf "sub %a, %a, %a" pp_reg r1 pp_reg r2 pp_reg r3
  | Mul (r1, r2, r3) -> fprintf ppf "mul %a, %a, %a" pp_reg r1 pp_reg r2 pp_reg r3
  | Li (rd, imm) -> fprintf ppf "li %a, %d" pp_reg rd imm
  | Ecall -> fprintf ppf "ecall"
  | Call f -> fprintf ppf "call %s" f
  | Ret -> fprintf ppf "ret"
  | Ld (rd, ofs) -> fprintf ppf "ld %a, %a" pp_reg rd pp_offset ofs
  | Sd (rs, ofs) -> fprintf ppf "sd %a, %a" pp_reg rs pp_offset ofs
  | Mv (rd, rs) -> fprintf ppf "mv %a, %a" pp_reg rd pp_reg rs
  | Beq (r1, r2, offset) -> fprintf ppf "beq %a, %a, %s" pp_reg r1 pp_reg r2 offset
  | Blt (r1, r2, offset) -> fprintf ppf "blt %a, %a, %s" pp_reg r1 pp_reg r2 offset
  | Ble (r1, r2, offset) -> fprintf ppf "ble %a, %a, %s" pp_reg r1 pp_reg r2 offset
  | Slt (r1, r2, r3) -> fprintf ppf "slt %a, %a, %a" pp_reg r1 pp_reg r2 pp_reg r3
  | Xori (r1, r2, n) -> fprintf ppf "xori %a, %a, %d" pp_reg r1 pp_reg r2 n
  | J s -> fprintf ppf "j %s" s
  | Label s -> fprintf ppf "%s:" s
;;

let addi k r1 r2 n = k @@ Addi (r1, r2, n)
let add k r1 r2 r3 = k @@ Add (r1, r2, r3)
let sub k r1 r2 r3 = k @@ Sub (r1, r2, r3)
let mul k r1 r2 r3 = k @@ Mul (r1, r2, r3)
let li k r n = k (Li (r, n))
let ecall k = k Ecall
let call k name = k (Call name)
let ret k = k Ret
let ld k a b = k (Ld (a, b))
let sd k a b = k (Sd (a, b))
let mv k a b = k (Mv (a, b))
let beq k r1 r2 r3 = k @@ Beq (r1, r2, r3)
let blt k r1 r2 r3 = k @@ Blt (r1, r2, r3)
let ble k r1 r2 r3 = k @@ Ble (r1, r2, r3)
let slt k r1 r2 r3 = k @@ Slt (r1, r2, r3)
let xori k r1 r2 n = k @@ Xori (r1, r2, n)
let j k s = k (J s)
let label k s = k (Label s)
