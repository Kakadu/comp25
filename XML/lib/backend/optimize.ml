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
    when aimm1 = -aimm2 && equal_reg sr mr2 -> elim_addi_sd_mv_addi (mv_i :: rest)
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

let reg_used_in_instr reg = function
  | Add (_, r1, r2)
  | Sub (_, r1, r2)
  | Mul (_, r1, r2)
  | Slt (_, r1, r2)
  | Xor (_, r1, r2)
  | Sd (r1, (r2, _))
  | Ld (r1, (r2, _))
    when equal_reg reg r1 || equal_reg reg r2 -> true
  | Seqz (_, r1)
  | Snez (_, r1)
  | Beq (_, r1, _)
  | Addi (_, r1, _)
  | Xori (_, r1, _)
  | Mv (_, r1)
  | Slli (_, r1, _)
  | Srai (_, r1, _)
    when equal_reg reg r1 -> true
  | Call _ when is_arg_reg reg -> true
  | _ -> false
;;

let reg_modified_in_instr reg = function
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

let rec elim_mv_before_call = function
  | (Li (ld, lii), _) :: (Mv (md, m1), _) :: ((Call _, _) as ci) :: rest
    when equal_reg ld m1 ->
    let li = Li (md, lii), "" in
    elim_mv_before_call (li :: ci :: rest)
  | (Ld (ld, off), _) :: (Mv (md, m1), _) :: ((Call _, _) as ci) :: rest
    when equal_reg ld m1 ->
    let li = Ld (md, off), "" in
    elim_mv_before_call (li :: ci :: rest)
  | (Mv (md1, mv1), _) :: (Mv (md2, mv2), _) :: ((Call _, _) as ci) :: rest
    when equal_reg md1 mv2 ->
    let mv = Mv (md2, mv1), "" in
    elim_mv_before_call (mv :: ci :: rest)
  | x :: rest -> x :: elim_mv_before_call rest
  | [] -> []
;;

let rec fold_mv_smth = function
  | (Mv (md, mv1), _) :: (Sd (sd, off), _) :: rest when equal_reg md sd ->
    let i = Sd (mv1, off), "" in
    fold_mv_smth (i :: rest)
  | (Mv (md, mv1), _) :: (Addi (ad, a1, aimm), _) :: rest when equal_reg md a1 ->
    let i = Addi (ad, mv1, aimm), "" in
    fold_mv_smth (i :: rest)
  | (Mv (md, mv1), _) :: (Add (ad, a1, a2), _) :: rest
    when equal_reg md a1 || equal_reg md a2 ->
    let add = if equal_reg md a1 then Add (ad, mv1, a2), "" else Add (ad, a1, mv1), "" in
    fold_mv_smth (add :: rest)
  | x :: rest -> x :: fold_mv_smth rest
  | [] -> []
;;

let rec fold_addi_sp = function
  (* can't just do as is like with ld, but it happens on A registers, so i listed the cases *)
  | (Addi (SP, SP, aimm1), _)
    :: (Sd (r1, (SP, off1)), _)
    :: (Addi (SP, SP, aimm2), _)
    :: (Sd (r2, (SP, off2)), _)
    :: (Addi (SP, SP, aimm3), _)
    :: (Sd (r3, (SP, off3)), _)
    :: (Addi (SP, SP, aimm4), _)
    :: (Sd (r4, (SP, off4)), _)
    :: (Addi (SP, SP, aimm5), _)
    :: ((Sd (_, (SP, _)), _) as sdi5)
    :: rest ->
    let addi = Addi (SP, SP, aimm1 + aimm2 + aimm3 + aimm4 + aimm5), "" in
    let sdi1 = Sd (r1, (SP, off1 - aimm1 - aimm2 - aimm3 - aimm4)), "" in
    let sdi2 = Sd (r2, (SP, off2 - aimm2 - aimm3 - aimm4)), "" in
    let sdi3 = Sd (r3, (SP, off3 - aimm3 - aimm4)), "" in
    let sdi4 = Sd (r4, (SP, off4 - aimm4)), "" in
    fold_addi_sp (addi :: sdi1 :: sdi2 :: sdi3 :: sdi4 :: sdi5 :: rest)
  | (Addi (SP, SP, aimm1), _)
    :: (Sd (r1, (SP, off1)), _)
    :: (Addi (SP, SP, aimm2), _)
    :: (Sd (r2, (SP, off2)), _)
    :: (Addi (SP, SP, aimm3), _)
    :: (Sd (r3, (SP, off3)), _)
    :: (Addi (SP, SP, aimm4), _)
    :: ((Sd (_, (SP, _)), _) as sdi4)
    :: rest ->
    let addi = Addi (SP, SP, aimm1 + aimm2 + aimm3 + aimm4), "" in
    let sdi1 = Sd (r1, (SP, off1 - aimm1 - aimm2 - aimm3)), "" in
    let sdi2 = Sd (r2, (SP, off2 - aimm2 - aimm3)), "" in
    let sdi3 = Sd (r3, (SP, off3 - aimm3)), "" in
    fold_addi_sp (addi :: sdi1 :: sdi2 :: sdi3 :: sdi4 :: rest)
  | (Addi (SP, SP, aimm1), _)
    :: (Sd (r1, (SP, off1)), _)
    :: (Addi (SP, SP, aimm2), _)
    :: (Sd (r2, (SP, off2)), _)
    :: (Addi (SP, SP, aimm3), _)
    :: ((Sd (_, (SP, _)), _) as sdi3)
    :: rest ->
    let addi = Addi (SP, SP, aimm1 + aimm2 + aimm3), "" in
    let sdi1 = Sd (r1, (SP, off1 - aimm1 - aimm2)), "" in
    let sdi2 = Sd (r2, (SP, off2 - aimm2)), "" in
    fold_addi_sp (addi :: sdi1 :: sdi2 :: sdi3 :: rest)
  | (Addi (SP, SP, aimm1), _)
    :: (Sd (sd1, (SP, _)), _)
    :: (Addi (SP, SP, aimm2), _)
    :: ((Sd (_, (SP, si2)), _) as sdi2)
    :: rest ->
    let addi = Addi (SP, SP, aimm1 + aimm2), "" in
    let sdi1 = Sd (sd1, (SP, si2 - aimm2)), "" in
    fold_addi_sp (addi :: sdi1 :: sdi2 :: rest)
  | (Addi (SP, SP, aimm1), _)
    :: (Ld (ld, (SP, limm)), _)
    :: (Addi (SP, SP, aimm2), _)
    :: rest ->
    let li = Ld (ld, (SP, aimm1 - limm)), "" in
    let addi = Addi (SP, SP, aimm1 + aimm2), "" in
    fold_addi_sp (li :: addi :: rest)
  | ((Ld (_, (SP, _)), _) as ldi1)
    :: (Addi (SP, SP, aimm1), _)
    :: (Ld (ld2, (SP, li2)), _)
    :: (Addi (SP, SP, aimm2), _)
    :: rest ->
    let ldi2 = Ld (ld2, (SP, aimm1 - li2)), "" in
    let addi = Addi (SP, SP, aimm1 + aimm2), "" in
    fold_addi_sp (ldi1 :: ldi2 :: addi :: rest)
  | (Addi (SP, SP, aimm1), _) :: ((i, _) as i2) :: (Addi (SP, SP, aimm2), _) :: rest
    when aimm1 = -aimm2 && not (reg_used_in_instr SP i) -> fold_addi_sp (i2 :: rest)
  | x :: rest -> x :: fold_addi_sp rest
  | [] -> []
;;

let rec elim_sd_before_ret = function
  | (Sd (_, _), _) :: ((Ret, _) as ret) :: rest -> elim_sd_before_ret (ret :: rest)
  | (Sd (_, _), _) :: addi_sp :: ld_ra :: ld_s0 :: ((Ret, _) as ret) :: rest ->
    elim_sd_before_ret (addi_sp :: ld_ra :: ld_s0 :: ret :: rest)
  | (Sd (_, (reg, _)), _)
    :: ((i, _) as ic)
    :: addi_sp
    :: ld_ra
    :: ld_s0
    :: ((Ret, _) as ret)
    :: rest
    when not (reg_used_in_instr reg i) ->
    elim_sd_before_ret (ic :: addi_sp :: ld_ra :: ld_s0 :: ret :: rest)
  | x :: rest -> x :: elim_sd_before_ret rest
  | [] -> []
;;

let rec elim_id_fun = function
  | (Addi (SP, SP, _), _)
    :: (Sd (RA, _), _)
    :: (Sd (S 0, _), _)
    :: (Addi (S 0, _, _), _)
    :: (Addi (SP, _, _), _)
    :: (Ld (RA, _), _)
    :: (Ld (S 0, _), _)
    :: ((Ret, _) as ret)
    :: rest -> elim_id_fun (ret :: rest)
  | x :: rest -> x :: elim_id_fun rest
  | [] -> []
;;

let rec elim_mv_before_branch = function
  | (Mv (md, mv1), _) :: ((Li (ld, _), _) as li) :: (Bne (br1, br2, label), _) :: rest
    when equal_reg md br1 && equal_reg ld br2 ->
    let b = Bne (mv1, ld, label), "" in
    fold_branch (li :: b :: rest)
  | (Mv (md, mv1), _) :: ((Li (ld, _), _) as li) :: (Blt (br1, br2, label), _) :: rest
    when equal_reg md br2 && equal_reg ld br1 ->
    let b = Blt (ld, mv1, label), "" in
    fold_branch (li :: b :: rest)
  | (Mv (md, mv1), _) :: ((Li (ld, _), _) as li) :: (Ble (br1, br2, label), _) :: rest
    when equal_reg md br2 && equal_reg ld br1 ->
    let b = Ble (ld, mv1, label), "" in
    fold_branch (li :: b :: rest)
  | x :: rest -> x :: elim_mv_before_branch rest
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
  let optimized = elim_mv_before_call optimized in
  let optimized = fold_mv_smth optimized in
  let optimized = fold_addi_sp optimized in
  let optimized = elim_sd_before_ret optimized in
  let optimized = elim_mv_before_branch optimized in
  let optimized = elim_id_fun optimized in
  Queue.of_list optimized
;;

let optimize code : (instr * string) Queue.t =
  let code = peephole code in
  code
;;
