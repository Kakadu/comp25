(** Copyright 2026,  Mikhail Gavrilenko, Danila Rudnev-Stepanyan, Daniel Vlasenko*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Machine

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

let rec fold_constants = function
  | (Li (lid, lii), _) :: (Sub (srd, sr1, sr2), _) :: rest when equal_reg lid sr2 ->
    let addi = Addi (srd, sr1, -lii), "" in
    fold_constants (addi :: rest)
  | (Addi (aid1, ai1, aii1), _) :: (Addi (aid2, ai2, aii2), _) :: rest
    when equal_reg aid1 ai1 && equal_reg aid1 ai2 ->
    let addi = Addi (aid2, ai1, aii1 + aii2), "" in
    fold_constants (addi :: rest)
  | x :: rest -> x :: fold_constants rest
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

let is_arg_reg r =
  equal_reg r (A 0)
  || equal_reg r (A 1)
  || equal_reg r (A 2)
  || equal_reg r (A 3)
  || equal_reg r (A 4)
  || equal_reg r (A 5)
  || equal_reg r (A 6)
  || equal_reg r (A 7)
;;

let reg_used_in_instr reg instr =
  match instr with
  | Add (_, r1, r2)
  | Sub (_, r1, r2)
  | Mul (_, r1, r2)
  | Slt (_, r1, r2)
  | Xor (_, r1, r2)
    when equal_reg reg r1 || equal_reg reg r2 -> true
  | Seqz (_, r1)
  | Snez (_, r1)
  | Beq (_, r1, _)
  | Addi (_, r1, _)
  | Xori (_, r1, _)
  | Sd (r1, _)
  | Mv (_, r1)
  | Slli (_, r1, _)
  | Srai (_, r1, _)
    when equal_reg reg r1 -> true
  | Call _ when is_arg_reg reg -> true
  | _ -> false
;;

let reg_modified_in_instr reg instr =
  match instr with
  | Add (r1, _, _)
  | Sub (r1, _, _)
  | Mul (r1, _, _)
  | Slt (r1, _, _)
  | Xor (r1, _, _)
  | Seqz (r1, _)
  | Snez (r1, _)
  | Addi (r1, _, _)
  | Xori (r1, _, _)
  | Mv (r1, _)
  | Slli (r1, _, _)
  | Srai (r1, _, _)
  | Lla (r1, _)
  | Li (r1, _)
  | Ld (r1, _)
  | La (r1, _)
    when equal_reg reg r1 -> true
  | Call _ when is_arg_reg reg -> true
  | _ -> false
;;

let rec elim_inverse_mv = function
  | (Mv (md1, mv1), _) :: (Mv (md2, mv2), _) :: rest
    when equal_reg md1 mv2 && equal_reg mv1 md2 -> elim_inverse_mv rest
  | (Mv (md1, mv1), _) :: ((i2, _) as i2c) :: (Mv (md2, mv2), _) :: rest
    when equal_reg md1 mv2 && equal_reg mv1 md2 && not (reg_used_in_instr md1 i2) ->
    elim_inverse_mv (i2c :: rest) (* can eliminate both *)
  | ((Mv (md1, mv1), _) as i1) :: ((i2, _) as i2c) :: (Mv (md2, mv2), _) :: rest
    when equal_reg md1 mv2 && equal_reg mv1 md2 && not (reg_modified_in_instr md1 i2) ->
    elim_inverse_mv (i1 :: i2c :: rest) (* can eliminate the second one *)
  | x :: rest -> x :: elim_inverse_mv rest
  | [] -> []
;;

let peephole code : (instr * string) Queue.t =
  let code = Queue.to_list code in
  let optimized = elim_sd_ld_same_reg code in
  let optimized = elim_sd_ld_same_offset optimized in
  let optimized = elim_addi_sd_mv_addi optimized in
  let optimized = fold_constants optimized in
  let optimized = elim_inverse_mv optimized in
  let optimized = fold_branch optimized in
  Queue.of_list optimized
;;

let optimize code : (instr * string) Queue.t =
  let code = peephole code in
  code
;;
