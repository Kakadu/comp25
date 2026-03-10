(** Copyright 2026,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Format
open Machine
open Target

let rec elim_sd_ld_same_reg = function
  | (Sd (r1, o1), _) :: (Ld (r2, o2), _) :: rest
    when equal_reg r1 r2 && equal_offset o1 o2 -> elim_sd_ld_same_reg rest
  | x :: rest -> x :: elim_sd_ld_same_reg rest
  | [] -> []
;;

let rec elim_sd_ld_same_offset = function
  | ((Sd (r1, o1), _) as i1) :: (Ld (r2, o2), _) :: rest
    when (not (equal_reg r1 r2)) && equal_offset o1 o2 ->
    let new_i = Mv (r2, r1), "" in
    elim_sd_ld_same_offset (i1 :: new_i :: rest)
  | x :: rest -> x :: elim_sd_ld_same_offset rest
  | [] -> []
;;

let rec elim_addi_sd_mv_addi = function
  | (Addi (_, _, aimm1), _)
    :: (Sd (sr, _), _)
    :: ((Mv (_, mr2), _) as mv_i)
    :: (Addi (_, _, aimm2), _)
    :: rest
    when aimm1 == -aimm2 && equal_reg sr mr2 -> elim_addi_sd_mv_addi (mv_i :: rest)
  | x :: rest -> x :: elim_addi_sd_mv_addi rest
  | [] -> []
;;

let rec fold_branch = function
  | (Slt (sld, sl1, sl2), _) :: (Beq (brc, Zero, label), _) :: rest when equal_reg sld brc
    ->
    let br = Ble (sl2, sl1, label), "" in
    fold_branch (br :: rest)
  | ((Li (lid, _), _) as li)
    :: (Xor (xd, x1, x2), _)
    :: (Seqz (sd, s1), _)
    :: (Beq (br, Zero, label), _)
    :: rest
    when equal_reg lid x2 && equal_reg xd s1 && equal_reg sd br ->
    let br = Bne (x1, x2, label), "" in
    fold_branch (li :: br :: rest)
  | ((Li (lid, _), _) as li)
    :: (Slt (sld, sl1, sl2), _)
    :: (Seqz (sd, s1), _)
    :: (Beq (br, Zero, label), _)
    :: rest
    when equal_reg lid sl1 && equal_reg sld s1 && equal_reg sd br ->
    let br = Blt (lid, sl2, label), "" in
    fold_branch (li :: br :: rest)
  | ((Li (lid, _), _) as li)
    :: (Xor (xd, x1, x2), _)
    :: (Snez (snd, sn1), _)
    :: (Beq (br, Zero, label), _)
    :: rest
    when equal_reg lid x2 && equal_reg xd sn1 && equal_reg snd br ->
    let br = Beq (x1, lid, label), "" in
    fold_branch (li :: br :: rest)
  | ((Mv (mvd, mv1), _) as li)
    :: (Xor (xd, x1, x2), _)
    :: (Snez (snd, sn1), _)
    :: (Beq (br, Zero, label), _)
    :: rest
    when equal_reg mvd x2 && equal_reg xd sn1 && equal_reg snd br ->
    let br = Beq (x1, mv1, label), "" in
    fold_branch (li :: br :: rest)
  (* | XOR :: SNEZ :: BEQ *)
  | x :: rest -> x :: fold_branch rest
  | [] -> []
;;

let peephole code : (instr * string) Queue.t =
  let code = Queue.to_list code in
  let optimized = elim_sd_ld_same_reg code in
  let optimized = elim_sd_ld_same_offset optimized in
  let optimized = elim_addi_sd_mv_addi optimized in
  let optimized = fold_branch optimized in
  Queue.of_list optimized
;;

let optimize code : (instr * string) Queue.t =
  let code = peephole code in
  code
;;
