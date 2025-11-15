(** Copyright 2024,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type reg =
  | A of int (*function args*)
  | T of int (*temporary*)
  | S of int (*saved*)
  | RA
  | SP
  | Zero
[@@deriving eq]

type offset = reg * int [@@deriving eq]

let pp_reg ppf =
  let open Format in
  function
  | A n -> fprintf ppf "a%d" n
  | T n -> fprintf ppf "t%d" n
  | S n -> fprintf ppf "s%d" n
  | Zero -> fprintf ppf "zero"
  | RA -> fprintf ppf "ra"
  | SP -> fprintf ppf "sp"
;;

let pp_offset ppf (reg, off) = Format.fprintf ppf "%d(%a)" off pp_reg reg

type instr =
  | Addi of reg * reg * int (* ADD immediate *)
  | Add of reg * reg * reg (* ADD *)
  | Sub of reg * reg * reg (* SUB *)
  | Mul of reg * reg * reg (* MUL *)
  | Slt of reg * reg * reg (* SLT: set less than (signed) *)
  | Seqz of reg * reg (* SEQZ: set equal zero *)
  | Snez of reg * reg (* SNEZ: set not equal zero *)
  | Xor of reg * reg * reg (* XOR *)
  | Xori of reg * reg * int (* XOR immediate *)
  | Beq of reg * reg * string (* BEQ: branch if equal *)
  | Blt of reg * reg * string (* BLT: branch if less than *)
  | Ble of reg * reg * string (* BLE: branch if less or equal *)
  | Lla of reg * string (* LLA: load address *)
  | Li of reg * int (* LI: load immediate *)
  | Ld of reg * offset (* LD: load doubleword *)
  | Sd of reg * offset (* SD: store doubleword *)
  | Mv of reg * reg (* MV: move *)
  | Comment of string (* Assembler comment *)
  | Label of string (* Assembler label *)
  | Call of string (* CALL *)
  | J of string (* J: jump *)
  | Ecall (* ECALL *)
  | Ret (* return *)
  | La of reg * string (* Load Address of labeled function into the reg *)
  | Slli of reg * reg * int (* << imm *)
  | Srai of reg * reg * int (* >> imm *)
[@@deriving eq]

let pp_instr ppf =
  let open Format in
  function
  | Addi (r1, r2, im) -> fprintf ppf "addi %a, %a, %d" pp_reg r1 pp_reg r2 im
  | Add (rd, r1, r2) -> fprintf ppf "add %a, %a, %a" pp_reg rd pp_reg r1 pp_reg r2
  | Sub (rd, r1, r2) -> fprintf ppf "sub %a, %a, %a" pp_reg rd pp_reg r1 pp_reg r2
  | Mul (rd, r1, r2) -> fprintf ppf "mul %a, %a, %a" pp_reg rd pp_reg r1 pp_reg r2
  | Slt (rd, r1, r2) -> fprintf ppf "slt %a, %a, %a" pp_reg rd pp_reg r1 pp_reg r2
  | Xor (rd, r1, r2) -> fprintf ppf "xor %a, %a, %a" pp_reg rd pp_reg r1 pp_reg r2
  | Xori (rd, r1, im) -> fprintf ppf "xori %a, %a, %d" pp_reg rd pp_reg r1 im
  | Li (r1, n) -> fprintf ppf "li %a, %d" pp_reg r1 n
  | Ecall -> fprintf ppf "ecall"
  | Call s -> fprintf ppf "call %s" s
  | Ret -> fprintf ppf "ret"
  | Lla (r1, s) -> fprintf ppf "lla %a, %s" pp_reg r1 s
  | Ld (r1, offset) -> fprintf ppf "ld %a, %a" pp_reg r1 pp_offset offset
  | Sd (r1, offset) -> fprintf ppf "sd %a, %a" pp_reg r1 pp_offset offset
  | Seqz (rd, r1) -> fprintf ppf "seqz %a, %a" pp_reg rd pp_reg r1
  | Snez (rd, r1) -> fprintf ppf "snez %a, %a" pp_reg rd pp_reg r1
  | Mv (r1, r2) -> fprintf ppf "mv %a, %a" pp_reg r1 pp_reg r2
  | Beq (r1, r2, s) -> fprintf ppf "beq %a, %a, %s" pp_reg r1 pp_reg r2 s
  | Blt (r1, r2, s) -> fprintf ppf "blt %a, %a, %s" pp_reg r1 pp_reg r2 s
  | Ble (r1, r2, s) -> fprintf ppf "ble %a, %a, %s" pp_reg r1 pp_reg r2 s
  | La (r, label) -> fprintf ppf "la %a, %s" pp_reg r label
  | J s -> fprintf ppf "j %s" s
  | Label s -> fprintf ppf "%s:" s
  | Comment s -> fprintf ppf " # %s" s
  | Slli (rd, r1, sh) -> fprintf ppf "slli %a, %a, %d" pp_reg rd pp_reg r1 sh
  | Srai (rd, r1, sh) -> fprintf ppf "srai %a, %a, %d" pp_reg rd pp_reg r1 sh
;;

let addi k r1 r2 n = k @@ Addi (r1, r2, n)
let add k rd r1 r2 = k @@ Add (rd, r1, r2)
let sub k rd r1 r2 = k @@ Sub (rd, r1, r2)
let mul k rd r1 r2 = k @@ Mul (rd, r1, r2)
let slt k rd r1 r2 = k @@ Slt (rd, r1, r2)
let xor k rd r1 r2 = k @@ Xor (rd, r1, r2)
let xori k rd r1 im = k @@ Xori (rd, r1, im)
let li k r n = k (Li (r, n))
let ecall k = k Ecall
let call k name = k (Call name)
let ret k = k Ret
let lla k a s = k (Lla (a, s))
let ld k a b = k (Ld (a, b))
let sd k a b = k (Sd (a, b))
let mv k a b = k (Mv (a, b))
let beq k r1 r2 r3 = k @@ Beq (r1, r2, r3)
let blt k r1 r2 r3 = k @@ Blt (r1, r2, r3)
let ble k r1 r2 r3 = k @@ Ble (r1, r2, r3)
let seqz k rd r1 = k (Seqz (rd, r1))
let snez k rd r1 = k (Snez (rd, r1))
let j k s = k @@ J s
let la k r label = k (La (r, label))
let comment k s = k (Comment s)
let label k s = k (Label s)
let slli k rd r sh = k @@ Slli (rd, r, sh)
let srai k rd r sh = k @@ Srai (rd, r, sh)
