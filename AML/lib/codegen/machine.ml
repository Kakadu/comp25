(** Copyright 2025-2026, Rodion Suvorov, Dmitriy Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type reg =
  | X0 (* hardwired to 0, ignores writes *)
  | A of int (* a0-a1 return value or function argument. a2-a7 function argument *)
  | RA (* return address for jumps *)
  | SP (* stack pointer *)
  | T of int (* t0-t6 temporary register *)
  | S of int (* s0 saved register or frame pointer. s1-s11 saved register *)
  | ROff of int * reg (* memory address with offset, e.g. 8(sp) *)
[@@deriving eq]

let x0 = X0
let ra = RA
let sp = SP
let fp = S 0
let a0 = A 0
let a1 = A 1
let a2 = A 2
let t0 = T 0
let t1 = T 1
let t2 = T 2
let t3 = T 3

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
  | Addi of
      reg
      * reg
      * int (* addi rd,rs1,imm. Adds the sign-extended 12-bit immediate to register rs1 *)
  | Add of reg * reg * reg
    (* add rd,rs1,rs2. adds the registers rs1 and rs2 and stores the result in rd *)
  | Sub of reg * reg * reg
    (* sub rd,rs1,rs2. subs the register rs2 from rs1 and stores the result in rd *)
  | Mul of reg * reg * reg
    (* mul rd,rs1,rs2. performs multiplication of signed rs1 by signed rs2 and places the lower bits in the destination register *)
  | Slt of reg * reg * reg
    (* slt rd,rs1,rs2. place the value 1 in register rd if register rs1 is less than register rs2 when both are treated as signed numbers, else 0 is written to rd *)
  | Xori of reg * reg * int
    (* xori rd,rs1,imm. performs bitwise XOR on register rs1 and the sign-extended 12-bit immediate and place the result in rd *)
  | Srai of reg * reg * int (* Shift Left Arith Immediate *)
  | Slli of reg * reg * int (* Shift Left Logical Immediate *)
  | Beq of reg * reg * string
    (* beq rs1,rs2,offset. take the branch if registers rs1 and rs2 are equal *)
  | Bne of reg * reg * string
    (* bne rs1,rs2,offset. take the branch if registers rs1 and rs2 are not equal *)
  | Blt of reg * reg * string
    (* blt rs1,rs2,offset. take the branch if registers rs1 is less than rs2, using signed comparison *)
  | Jal of
      reg * string (* jal rd,offset. jump to address and place return address in rd *)
  | J of string (* j offset. unconditional control transfer *)
  | Jalr of reg (* jump and link register*)
  | Ret (* jumps to the address stored in ra *)
  | Ld of
      reg * reg (* ld rd,uimm(rs1). load a 64-bit value from memory into register rd *)
  | Sd of
      reg * reg (* sd rs2,offset(rs1). store 64-bit, values from register rs2 to memory *)
  | Li of reg * int
  (* li rd,uimm. load the sign-extended 6-bit immediate, imm, into register rd *)
  | La of reg * string (* load address *)
  | Ecall (* make a request to the supporting execution environment *)
  | Label of string (* label in the assembly code, marking a location to jump to *)
  | Directive of string (* assembler directive, e.g. ".globl" *)
  | Call of string

let pp_instr ppf =
  let open Format in
  function
  | Addi (r1, r2, n) -> fprintf ppf "addi %a, %a, %d" pp_reg r1 pp_reg r2 n
  | Add (r1, r2, r3) -> fprintf ppf "add %a, %a, %a" pp_reg r1 pp_reg r2 pp_reg r3
  | Sub (r1, r2, r3) -> fprintf ppf "sub %a, %a, %a" pp_reg r1 pp_reg r2 pp_reg r3
  | Mul (r1, r2, r3) -> fprintf ppf "mul %a, %a, %a" pp_reg r1 pp_reg r2 pp_reg r3
  | Slt (r1, r2, r3) -> fprintf ppf "slt %a, %a, %a" pp_reg r1 pp_reg r2 pp_reg r3
  | Xori (r1, r2, n) -> fprintf ppf "xori %a, %a, %d" pp_reg r1 pp_reg r2 n
  | Srai (r1, r2, n) -> fprintf ppf "srai %a, %a, %d" pp_reg r1 pp_reg r2 n
  | Slli (r1, r2, n) -> fprintf ppf "slli %a, %a, %d" pp_reg r1 pp_reg r2 n
  | Beq (r1, r2, s) -> fprintf ppf "beq %a, %a, %s" pp_reg r1 pp_reg r2 s
  | Bne (r1, r2, s) -> fprintf ppf "bne %a, %a, %s" pp_reg r1 pp_reg r2 s
  | Blt (r1, r2, s) -> fprintf ppf "blt %a, %a, %s" pp_reg r1 pp_reg r2 s
  | Jal (r1, s) -> fprintf ppf "jal %a, %s" pp_reg r1 s
  | J s -> fprintf ppf "j %s" s
  | Jalr r -> fprintf ppf "jalr %a" pp_reg r
  | Ret -> fprintf ppf "ret\n  "
  | Sd (r1, r2) -> fprintf ppf "sd %a, %a" pp_reg r1 pp_reg r2
  | Ld (r1, r2) -> fprintf ppf "ld %a, %a" pp_reg r1 pp_reg r2
  | Li (r1, n) -> fprintf ppf "li %a, %d" pp_reg r1 n
  | La (r1, s) -> fprintf ppf "la %a, %s" pp_reg r1 s
  | Ecall -> fprintf ppf "ecall"
  | Label s -> fprintf ppf "%s:" s
  | Directive s -> fprintf ppf "%s" s
  | Call l -> fprintf ppf "call %s" l
;;

let addi k r1 r2 n = k @@ Addi (r1, r2, n)
let add k r1 r2 r3 = k @@ Add (r1, r2, r3)
let sub k r1 r2 r3 = k @@ Sub (r1, r2, r3)
let mul k r1 r2 r3 = k @@ Mul (r1, r2, r3)
let slt k r1 r2 r3 = k @@ Slt (r1, r2, r3)
let xori k r1 r2 n = k @@ Xori (r1, r2, n)
let srai k r1 r2 n = k @@ Srai (r1, r2, n)
let slli k r1 r2 n = k @@ Slli (r1, r2, n)
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
let call k l = k (Call l)
let la k r s = k (La (r, s))
let jalr k r = k (Jalr r)

let pp_instrs ppf (instrs : instr list) =
  let open Format in
  List.iter
    (fun i ->
       match i with
       | Label _ -> fprintf ppf "%a\n" pp_instr i
       | _ -> fprintf ppf "  %a\n" pp_instr i)
    (List.rev instrs)
;;
