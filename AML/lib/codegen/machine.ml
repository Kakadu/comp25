(** Copyright 2025-2026, Rodion Suvorov, Dmitriy Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type reg =
  | X0
  | A of int
  | RA
  | SP
  | T of int
  | S of int
  | ROff of int * reg
[@@deriving eq]

let x0 = X0
let ra = RA
let sp = SP
let fp = S 0
let a0 = A 0
let t0 = T 0
let t1 = T 1
let t2 = T 2

let rec pp_reg ppf =
  let open Format in
  function
  | X0 -> fprintf ppf "x0"
  | A n -> fprintf ppf "a%d" n
  | T n -> fprintf ppf "t%d" n
  | S n -> fprintf ppf "s%d" n
  | RA -> fprintf ppf "ra"
  | SP -> fprintf ppf "sp"
  | ROff (n, r) -> fprintf ppf "%d(%a)" n pp_reg r
;;

type instr =
  | Addi of reg * reg * int
  | Add of reg * reg * reg
  | Sub of reg * reg * reg
  | Mul of reg * reg * reg
  | Slt of reg * reg * reg
  | Xori of reg * reg * int
  | Beq of reg * reg * string
  | Bne of reg * reg * string
  | Blt of reg * reg * string
  | Jal of reg * string
  | J of string
  | Ret
  | Ld of reg * reg
  | Sd of reg * reg
  | Li of reg * int
  | Ecall
  | Label of string
  | Directive of string

let pp_instr ppf =
  let open Format in
  function
  | Addi (r1, r2, n) -> fprintf ppf "addi %a, %a, %d" pp_reg r1 pp_reg r2 n
  | Add (r1, r2, r3) -> fprintf ppf "add %a, %a, %a" pp_reg r1 pp_reg r2 pp_reg r3
  | Sub (r1, r2, r3) -> fprintf ppf "sub %a, %a, %a" pp_reg r1 pp_reg r2 pp_reg r3
  | Mul (r1, r2, r3) -> fprintf ppf "mul %a, %a, %a" pp_reg r1 pp_reg r2 pp_reg r3
  | Slt (r1, r2, r3) -> fprintf ppf "slt %a, %a, %a" pp_reg r1 pp_reg r2 pp_reg r3
  | Xori (r1, r2, n) -> fprintf ppf "xori %a, %a, %d" pp_reg r1 pp_reg r2 n
  | Beq (r1, r2, s) -> fprintf ppf "beq %a, %a, %s" pp_reg r1 pp_reg r2 s
  | Bne (r1, r2, s) -> fprintf ppf "bne %a, %a, %s" pp_reg r1 pp_reg r2 s
  | Blt (r1, r2, s) -> fprintf ppf "blt %a, %a, %s" pp_reg r1 pp_reg r2 s
  | Jal (r1, s) -> fprintf ppf "jal %a, %s" pp_reg r1 s
  | J s -> fprintf ppf "j %s" s
  | Ret -> fprintf ppf "ret"
  | Sd (r1, r2) -> fprintf ppf "sd %a, %a" pp_reg r1 pp_reg r2
  | Ld (r1, r2) -> fprintf ppf "ld %a, %a" pp_reg r1 pp_reg r2
  | Li (r1, n) -> fprintf ppf "li %a, %d" pp_reg r1 n
  | Ecall -> fprintf ppf "ecall"
  | Label s -> fprintf ppf "%s:" s
  | Directive s -> fprintf ppf "%s" s
;;

let addi k r1 r2 n = k @@ Addi (r1, r2, n)
let add k r1 r2 r3 = k @@ Add (r1, r2, r3)
let sub k r1 r2 r3 = k @@ Sub (r1, r2, r3)
let mul k r1 r2 r3 = k @@ Mul (r1, r2, r3)
let slt k r1 r2 r3 = k @@ Slt (r1, r2, r3)
let xori k r1 r2 n = k @@ Xori (r1, r2, n)
let beq k r1 r2 s = k @@ Beq (r1, r2, s)
let bne k r1 r2 s = k @@ Bne (r1, r2, s)
let blt k r1 r2 s = k @@ Blt (r1, r2, s)
let ecall k = k Ecall
let ret k = k Ret
let jal k r1 s = k @@ Jal (r1, s)
let j k s = k (J s)
let ld k rd rs = k (Ld (rd, rs))
let sd k rs rd = k (Sd (rs, rd))
let li k r n = k (Li (r, n))
let label k s = k (Label s)
let directive k s = k (Directive s)
let mv k rd rs = k @@ Addi (rd, rs, 0)
let code : (instr * string) Queue.t = Queue.create ()
let emit ?(comm = "") instr = instr (fun i -> Queue.add (i, comm) code)

let rec flush_queue ppf =
  if Queue.is_empty code
  then ()
  else
    let open Format in
    let i, comm = Queue.pop code in
    (match i with
     | Label _ ->
       fprintf ppf "%a" pp_instr i;
       if comm <> "" then fprintf ppf " # %s" comm;
       fprintf ppf "\n"
     | _ ->
       fprintf ppf "  %a" pp_instr i;
       if comm <> "" then fprintf ppf " # %s" comm;
       fprintf ppf "\n");
    flush_queue ppf
;;
