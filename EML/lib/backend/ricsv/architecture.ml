(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

module Riscv_backend = struct
  type reg =
    | Zero
    | RA
    | SP
    | A of int
    | T of int
    | S of int
  [@@deriving eq]

  type offset = reg * int

  type instr =
    | Addi of reg * reg * int (* сложение с константой: rd = rs + imm *)
    | Ld of reg * offset (* загрузка 8 байт из памяти: rd = mem[base + offset] *)
    | Sd of reg * offset (* сохранение 8 байт в память: mem[base + offset] = rd *)
    | Mv of reg * reg (* копирование регистра: rd = rs *)
    | Li of reg * int (* загрузить константу: rd = imm *)
    | Add of reg * reg * reg (* сложение: rd = rs1 + rs2 *)
    | Sub of reg * reg * reg (* вычитание: rd = rs1 - rs2 *)
    | Call of string (* вызов функции по имени *)
    | Ret (* возврат из функции *)
    | Beq of
        reg * reg * string (* переход если равно: если rs1 == rs2, переход на метку *)
    | J of string (* безусловный переход на метку *)
    | Label of string (* метка: именованная точка в коде, цель для Beq/J *)
    | La of reg * string (* загрузить адрес: rd = адрес метки *)
    (* Сравнения и логика *)
    | Slt of reg * reg * reg (* записать в rd 1 если rs1 < rs2, иначе 0 *)
    | Seqz of reg * reg (* записать в rd 1 если rs == 0, иначе 0 *)
    | Snez of reg * reg (* записать в rd 1 если rs != 0, иначе 0 *)
    | Xori of reg * reg * int (* xor регистра с константой: rd = rs ^ imm *)
    | Xor of reg * reg * reg (* xor двух регистров: rd = rs1 ^ rs2 *)
    | Mul of reg * reg * reg (* умножение: rd = rs1 * rs2 *)
    | Div of reg * reg * reg (* целочисленное деление: rd = rs1 / rs2 *)
    | Slli of reg * reg * int (* логический сдвиг влево на константу: rd = rs << imm *)
    | Srli of reg * reg * int (* логический сдвиг вправо на константу: rd = rs >>> imm *)

  let pp_reg ppf = function
    | Zero -> fprintf ppf "zero"
    | RA -> fprintf ppf "ra"
    | SP -> fprintf ppf "sp"
    | A n -> fprintf ppf "a%d" n
    | T n -> fprintf ppf "t%d" n
    | S 0 -> fprintf ppf "fp"
    | S n -> fprintf ppf "s%d" n
  ;;

  let pp_offset ppf offset = fprintf ppf "%d(%a)" (snd offset) pp_reg (fst offset)

  let pp_instr ppf = function
    | Addi (rd, rs, imm) -> fprintf ppf "addi %a, %a, %d" pp_reg rd pp_reg rs imm
    | Add (rd, rs1, rs2) -> fprintf ppf "add %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Sub (rd, rs1, rs2) -> fprintf ppf "sub %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Mul (rd, rs1, rs2) -> fprintf ppf "mul %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Div (rd, rs1, rs2) -> fprintf ppf "div %a, %a, %a" pp_reg rd pp_reg rs1 pp_reg rs2
    | Slli (rd, rs1, imm) -> fprintf ppf "slli %a, %a, %d" pp_reg rd pp_reg rs1 imm
    | Srli (rd, rs1, imm) -> fprintf ppf "srli %a, %a, %d" pp_reg rd pp_reg rs1 imm
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
  ;;

  let tag_int n = (2 * n) + 1
  let fp = S 0
  let sp = SP
  let ra = RA
  let zero = Zero
  let a0 = A 0
  let a1 = A 1
  let a2 = A 2
  let a3 = A 3
  let a4 = A 4
  let a5 = A 5
  let a6 = A 6
  let a7 = A 7
  let t0 = T 0
  let t1 = T 1
  let result_reg = a0
  let addi rd rs imm = [ Addi (rd, rs, imm) ]
  let ld rd ofs = [ Ld (rd, ofs) ]
  let sd rs ofs = [ Sd (rs, ofs) ]
  let mv rd rs = [ Mv (rd, rs) ]
  let li rd imm = [ Li (rd, imm) ]
  let add rd rs1 rs2 = [ Add (rd, rs1, rs2) ]
  let sub rd rs1 rs2 = [ Sub (rd, rs1, rs2) ]
  let call s = [ Call s ]
  let ret () = [ Ret ]
  let beq rs1 rs2 lbl = [ Beq (rs1, rs2, lbl) ]
  let j lbl = [ J lbl ]
  let label s = [ Label s ]
  let la rd s = [ La (rd, s) ]
  let slt rd rs1 rs2 = [ Slt (rd, rs1, rs2) ]
  let seqz rd rs = [ Seqz (rd, rs) ]
  let snez rd rs = [ Snez (rd, rs) ]
  let xori rd rs imm = [ Xori (rd, rs, imm) ]
  let xor rd rs1 rs2 = [ Xor (rd, rs1, rs2) ]
  let mul rd rs1 rs2 = [ Mul (rd, rs1, rs2) ]
  let div rd rs1 rs2 = [ Div (rd, rs1, rs2) ]
  let slli rd rs imm = [ Slli (rd, rs, imm) ]
  let srli rd rs imm = [ Srli (rd, rs, imm) ]
  let add_tag_items dst delta = [ Addi (dst, dst, delta) ]
  let arg_regs = [ a0; a1; a2; a3; a4; a5; a6; a7 ]
  let candidate_regs_for_spill = arg_regs
  let arg_regs_count = 8
  let word_size = 8

  (* RISC-V ABI: stack must be 16-byte aligned at call boundaries *)
  let stack_align = 16
  let frame_header_size = 2 * word_size
  let saved_fp_offset = 0
  let saved_ra_offset = word_size

  (* addi/sd/ld immediate is 12-bit signed: -2048 .. 2047 *)
  let max_addi_imm = 2048

  let rec sub_sp bytes_to_subtract =
    if bytes_to_subtract <= 0
    then []
    else if bytes_to_subtract <= max_addi_imm
    then addi sp sp (-bytes_to_subtract)
    else addi sp sp (-max_addi_imm) @ sub_sp (bytes_to_subtract - max_addi_imm)
  ;;

  let addi_or_li_add destination_register source_register immediate_value =
    if immediate_value >= -max_addi_imm && immediate_value <= max_addi_imm - 1
    then addi destination_register source_register immediate_value
    else li t0 immediate_value @ add destination_register source_register t0
  ;;

  (* Store at sp+offset; use direct sd when offset in 12-bit range *)
  let sd_at_sp_offset reg offset =
    if offset >= -2048 && offset <= 2047
    then sd reg (sp, offset)
    else addi_or_li_add t0 sp offset @ sd reg (t0, 0)
  ;;

  type location =
    | Loc_reg of reg
    | Loc_mem of offset

  let prologue ~enable_gc ~name ~stack_size =
    let ra_offset = stack_size - saved_ra_offset in
    let fp_offset = stack_size - frame_header_size in
    let fp_imm = stack_size - frame_header_size in
    let base =
      label name
      @ sub_sp stack_size
      @ sd_at_sp_offset ra ra_offset
      @ sd_at_sp_offset fp fp_offset
      @ addi_or_li_add fp sp fp_imm
    in
    if enable_gc && String.equal name "main"
    then base @ call "init_gc" @ mv a0 fp @ call "set_ptr_stack"
    else base
  ;;

  let epilogue ~enable_gc ~is_main =
    let base =
      (if enable_gc && is_main then call "destroy_gc" else [])
      @ addi sp fp frame_header_size
      @ ld ra (fp, saved_ra_offset)
      @ ld fp (fp, saved_fp_offset)
    in
    if is_main then base @ li a0 0 @ ret () else base @ ret ()
  ;;

  let format_item ppf i =
    (match i with
     | Label _ -> fprintf ppf "%a" pp_instr i
     | _ -> fprintf ppf "  %a" pp_instr i);
    fprintf ppf "\n"
  ;;
end
