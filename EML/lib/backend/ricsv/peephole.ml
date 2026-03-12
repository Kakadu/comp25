(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Architecture
open Riscv_backend

let is_small_addi_imm imm = imm >= -2048 && imm <= 2047

let write_reg = function
  | Addi (rd, _, _)
  | Ld (rd, _)
  | Mv (rd, _)
  | Li (rd, _)
  | Add (rd, _, _)
  | Sub (rd, _, _)
  | La (rd, _)
  | Slt (rd, _, _)
  | Seqz (rd, _)
  | Snez (rd, _)
  | Xori (rd, _, _)
  | Xor (rd, _, _)
  | Mul (rd, _, _)
  | Div (rd, _, _)
  | Slli (rd, _, _)
  | Srli (rd, _, _) -> Some rd
  | Sd _ | Call _ | Ret | Beq _ | J _ | Label _ -> None
;;

let reads_reg instruction reg =
  let same register = equal_reg register reg in
  match instruction with
  | Addi (_, rs, _)
  | Mv (_, rs)
  | Seqz (_, rs)
  | Snez (_, rs)
  | Xori (_, rs, _)
  | Srli (_, rs, _)
  | Slli (_, rs, _) -> same rs
  | Sd (rs, (base, _)) -> same rs || same base
  | Ld (_, (base, _)) -> same base
  | Add (_, rs1, rs2)
  | Sub (_, rs1, rs2)
  | Slt (_, rs1, rs2)
  | Xor (_, rs1, rs2)
  | Mul (_, rs1, rs2)
  | Div (_, rs1, rs2) -> same rs1 || same rs2
  | Beq (rs1, rs2, _) -> same rs1 || same rs2
  | Li _ | Call _ | Ret | J _ | Label _ | La _ -> false
;;

let reg_used_later reg instructions =
  List.exists (fun instruction -> reads_reg instruction reg) instructions
;;

let is_power_of_two positive_value =
  positive_value > 0 && positive_value land (positive_value - 1) = 0
;;

let log2_power_of_two n =
  let rec loop power value =
    if value = 1 then power else loop (power + 1) (value lsr 1)
  in
  loop 0 n
;;

let replace_reg from_register to_register instruction =
  let replace register =
    if equal_reg register from_register then to_register else register
  in
  match instruction with
  | Add (rd, rs1, rs2) -> Add (rd, replace rs1, replace rs2)
  | Sub (rd, rs1, rs2) -> Sub (rd, replace rs1, replace rs2)
  | Mul (rd, rs1, rs2) -> Mul (rd, replace rs1, replace rs2)
  | Div (rd, rs1, rs2) -> Div (rd, replace rs1, replace rs2)
  | Xor (rd, rs1, rs2) -> Xor (rd, replace rs1, replace rs2)
  | Slt (rd, rs1, rs2) -> Slt (rd, replace rs1, replace rs2)
  | Beq (rs1, rs2, label) -> Beq (replace rs1, replace rs2, label)
  | Addi (rd, rs, imm) -> Addi (rd, replace rs, imm)
  | Xori (rd, rs, imm) -> Xori (rd, replace rs, imm)
  | Srli (rd, rs, imm) -> Srli (rd, replace rs, imm)
  | Slli (rd, rs, imm) -> Slli (rd, replace rs, imm)
  | Seqz (rd, rs) -> Seqz (rd, replace rs)
  | Snez (rd, rs) -> Snez (rd, replace rs)
  | Sd (rs, (base, offset)) -> Sd (replace rs, (replace base, offset))
  | Ld _ | Mv _ | Li _ | Call _ | Ret | J _ | Label _ | La _ -> instruction
;;

let simplify_single = function
  | Mv (rd, rs) when equal_reg rd rs -> None
  | Addi (rd, rs, 0) when equal_reg rd rs -> None
  | Addi (rd, rs, 0) -> Some (Mv (rd, rs))
  | Xori (rd, rs, 0) when equal_reg rd rs -> None
  | Xori (rd, rs, 0) -> Some (Mv (rd, rs))
  | Add (rd, rs, Zero) when equal_reg rd rs -> None
  | Add (rd, Zero, rs) when equal_reg rd rs -> None
  | Add (rd, rs, Zero) -> Some (Mv (rd, rs))
  | Add (rd, Zero, rs) -> Some (Mv (rd, rs))
  | Sub (rd, rs, Zero) when equal_reg rd rs -> None
  | Sub (rd, rs, Zero) -> Some (Mv (rd, rs))
  | instruction -> Some instruction
;;

let simplify_pair first second rest =
  match first, second with
  | Mv (target_register, source_register), next_instruction
    when reads_reg next_instruction target_register ->
    let safe_to_drop_mv =
      match write_reg next_instruction with
      | Some written_register when equal_reg written_register target_register -> true
      | _ -> not (reg_used_later target_register rest)
    in
    if safe_to_drop_mv
    then Some [ replace_reg target_register source_register next_instruction ]
    else None
  | ( Li (constant_register, constant_value)
    , Add (destination_register, left_register, right_register) )
    when is_small_addi_imm constant_value ->
    if equal_reg right_register constant_register
    then Some [ Addi (destination_register, left_register, constant_value) ]
    else if equal_reg left_register constant_register
    then Some [ Addi (destination_register, right_register, constant_value) ]
    else None
  | ( Li (constant_register, constant_value)
    , Mul (destination_register, left_register, right_register) )
    when is_power_of_two constant_value ->
    let shift_amount = log2_power_of_two constant_value in
    if equal_reg right_register constant_register
    then Some [ Slli (destination_register, left_register, shift_amount) ]
    else if equal_reg left_register constant_register
    then Some [ Slli (destination_register, right_register, shift_amount) ]
    else None
  | J l1, Label l2 when String.equal l1 l2 -> Some []
  | Sd (_, (base1, offset1)), Sd (rs2, (base2, offset2))
    when equal_reg base1 base2 && offset1 = offset2 -> Some [ Sd (rs2, (base2, offset2)) ]
  | Sd (stored_reg, (base1, offset1)), Ld (loaded_reg, (base2, offset2))
    when equal_reg base1 base2 && offset1 = offset2 ->
    if equal_reg stored_reg loaded_reg
    then Some [ first ]
    else Some [ first; Mv (loaded_reg, stored_reg) ]
  | Ld (rd1, (base1, offset1)), Ld (rd2, (base2, offset2))
    when equal_reg base1 base2 && offset1 = offset2 ->
    if equal_reg rd1 rd2 then Some [ first ] else Some [ first; Mv (rd2, rd1) ]
  | Addi (rd1, rs1, imm1), Addi (rd2, rs2, imm2)
    when equal_reg rd1 rs1 && equal_reg rd2 rs2 && equal_reg rd1 rd2 ->
    let merged = imm1 + imm2 in
    if is_small_addi_imm merged
    then if merged = 0 then Some [] else Some [ Addi (rd1, rs1, merged) ]
    else None
  | _ ->
    (match write_reg first, write_reg second with
     | Some written_first, Some written_second
       when equal_reg written_first written_second && not (reads_reg second written_first)
       -> Some [ second ]
     | _ -> None)
;;

let simplify_triple first second third rest =
  match first, second, third with
  | Li (left_register, left_const), Li (right_register, right_const), Beq (rs1, rs2, label)
    when equal_reg rs1 left_register && equal_reg rs2 right_register ->
    if left_const = right_const
    then
      Some [ Li (left_register, left_const); Li (right_register, right_const); J label ]
    else Some [ Li (left_register, left_const); Li (right_register, right_const) ]
  | Mv (target_register, source_register), middle_instruction, next_instruction
    when reads_reg next_instruction target_register
         && (not (reads_reg middle_instruction target_register))
         &&
         match write_reg middle_instruction with
         | Some written_register -> not (equal_reg written_register target_register)
         | None -> true ->
    let safe_to_drop_mv =
      match write_reg next_instruction with
      | Some written_register when equal_reg written_register target_register -> true
      | _ -> not (reg_used_later target_register rest)
    in
    if safe_to_drop_mv
    then
      Some
        [ middle_instruction
        ; replace_reg target_register source_register next_instruction
        ]
    else None
  | Mv (first_target, first_source), Mv (second_target, second_source), Add (dst, rs1, rs2)
    when equal_reg first_source second_source
         && equal_reg rs1 first_target
         && equal_reg rs2 second_target -> Some [ Add (dst, first_source, first_source) ]
  | Mv (first_target, first_source), Mv (second_target, second_source), Mul (dst, rs1, rs2)
    when equal_reg first_source second_source
         && equal_reg rs1 first_target
         && equal_reg rs2 second_target -> Some [ Mul (dst, first_source, first_source) ]
  | Mv (first_target, first_source), Mv (second_target, second_source), Sub (dst, rs1, rs2)
    when equal_reg first_source second_source
         && equal_reg rs1 first_target
         && equal_reg rs2 second_target -> Some [ Sub (dst, first_source, first_source) ]
  | Mv (first_target, first_source), Mv (second_target, second_source), Div (dst, rs1, rs2)
    when equal_reg first_source second_source
         && equal_reg rs1 first_target
         && equal_reg rs2 second_target -> Some [ Div (dst, first_source, first_source) ]
  | Mv (first_target, first_source), Mv (second_target, second_source), Xor (dst, rs1, rs2)
    when equal_reg first_source second_source
         && equal_reg rs1 first_target
         && equal_reg rs2 second_target -> Some [ Xor (dst, first_source, first_source) ]
  | Mv (first_target, first_source), Mv (second_target, second_source), Slt (dst, rs1, rs2)
    when equal_reg first_source second_source
         && equal_reg rs1 first_target
         && equal_reg rs2 second_target -> Some [ Slt (dst, first_source, first_source) ]
  | _ -> None
;;

let one_pass instructions =
  let rec loop changed acc = function
    | first :: second :: third :: rest ->
      (match simplify_triple first second third rest with
       | Some rewritten ->
         let rewritten_reversed = List.rev_append rewritten acc in
         loop true rewritten_reversed rest
       | None ->
         (match simplify_pair first second (third :: rest) with
          | Some rewritten ->
            let rewritten_reversed = List.rev_append rewritten acc in
            loop true rewritten_reversed (third :: rest)
          | None ->
            (match simplify_single first with
             | None -> loop true acc (second :: third :: rest)
             | Some simplified ->
               loop changed (simplified :: acc) (second :: third :: rest))))
    | first :: second :: rest ->
      (match simplify_pair first second rest with
       | Some rewritten ->
         let rewritten_reversed = List.rev_append rewritten acc in
         loop true rewritten_reversed rest
       | None ->
         (match simplify_single first with
          | None -> loop true acc (second :: rest)
          | Some simplified -> loop changed (simplified :: acc) (second :: rest)))
    | [ last ] ->
      (match simplify_single last with
       | None -> List.rev acc, true
       | Some simplified -> List.rev (simplified :: acc), changed)
    | [] -> List.rev acc, changed
  in
  loop false [] instructions
;;

let same_memory_key (base1, offset1) (base2, offset2) =
  equal_reg base1 base2 && offset1 = offset2
;;

let find_cached_load key cache =
  List.find_map
    (fun (cached_key, cached_register) ->
       if same_memory_key cached_key key then Some cached_register else None)
    cache
;;

let remove_cached_key key cache =
  List.filter (fun (cached_key, _) -> not (same_memory_key cached_key key)) cache
;;

let invalidate_register register cache =
  List.filter
    (fun ((base, _), cached_register) ->
       not (equal_reg cached_register register || equal_reg base register))
    cache
;;

let track_load_cache instructions =
  let rec loop changed cache acc = function
    | [] -> List.rev acc, changed
    | instruction :: rest ->
      (match instruction with
       | Ld (destination_register, key) ->
         (match find_cached_load key cache with
          | Some cached_register when equal_reg cached_register destination_register ->
            loop true cache acc rest
          | Some cached_register ->
            let cache_without_destination =
              invalidate_register destination_register cache
            in
            let next_cache = (key, destination_register) :: cache_without_destination in
            loop true next_cache (Mv (destination_register, cached_register) :: acc) rest
          | None ->
            let cache_without_destination =
              invalidate_register destination_register cache
            in
            let next_cache = (key, destination_register) :: cache_without_destination in
            loop changed next_cache (instruction :: acc) rest)
       | Sd (stored_register, key) ->
         let next_cache =
           remove_cached_key key cache
           |> fun cache_without_key -> (key, stored_register) :: cache_without_key
         in
         loop changed next_cache (instruction :: acc) rest
       | Call _ | Ret | Beq _ | J _ | Label _ -> loop changed [] (instruction :: acc) rest
       | _ ->
         let next_cache =
           match write_reg instruction with
           | Some written_register -> invalidate_register written_register cache
           | None -> cache
         in
         loop changed next_cache (instruction :: acc) rest)
  in
  loop false [] [] instructions
;;

let reads_slot (slot_base, slot_offset) = function
  | Ld (_, (base, offset)) -> equal_reg base slot_base && offset = slot_offset
  | _ -> false
;;

let stores_slot (slot_base, slot_offset) = function
  | Sd (_, (base, offset)) -> equal_reg base slot_base && offset = slot_offset
  | _ -> false
;;

let writes_slot_base (slot_base, _) instruction =
  match write_reg instruction with
  | Some written_register -> equal_reg written_register slot_base
  | None -> false
;;

let can_prove_store_dead ~allow_drop_at_block_end slot following_instructions =
  let rec walk = function
    | [] -> allow_drop_at_block_end
    | instruction :: rest ->
      (match
         ( reads_slot slot instruction
         , stores_slot slot instruction
         , writes_slot_base slot instruction )
       with
       | true, _, _ -> false
       | _, true, _ -> true
       | _, _, true -> false
       | _ -> walk rest)
  in
  walk following_instructions
;;

let eliminate_dead_stores_in_block ~allow_drop_at_block_end block =
  let rec loop changed acc = function
    | [] -> List.rev acc, changed
    | Sd (_, slot) :: rest when can_prove_store_dead ~allow_drop_at_block_end slot rest ->
      loop true acc rest
    | (Sd (_, _) as store_instruction) :: rest ->
      loop changed (store_instruction :: acc) rest
    | instruction :: rest -> loop changed (instruction :: acc) rest
  in
  loop false [] block
;;

let eliminate_local_dead_stores instructions =
  let is_barrier = function
    | Call _ | Ret | Beq _ | J _ | Label _ -> true
    | _ -> false
  in
  let rec split_non_barrier acc = function
    | instruction :: rest when not (is_barrier instruction) ->
      split_non_barrier (instruction :: acc) rest
    | remaining -> List.rev acc, remaining
  in
  let rec process changed acc = function
    | [] -> List.rev acc, changed
    | instructions ->
      let block, remaining = split_non_barrier [] instructions in
      let allow_drop_at_block_end =
        match remaining with
        | Ret :: _ -> true
        | _ -> false
      in
      let optimized_block, block_changed =
        eliminate_dead_stores_in_block ~allow_drop_at_block_end block
      in
      (match remaining with
       | barrier :: tail ->
         process
           (changed || block_changed)
           (barrier :: List.rev_append optimized_block acc)
           tail
       | [] -> List.rev (List.rev_append optimized_block acc), changed || block_changed)
  in
  process false [] instructions
;;

let find_redundant_restore_store loaded_register loaded_slot following_instructions =
  let is_barrier = function
    | Call _ | Ret | Beq _ | J _ | Label _ -> true
    | _ -> false
  in
  let rec search prefix = function
    | [] -> List.rev_append prefix [], false
    | instruction :: rest
      when is_barrier instruction || writes_slot_base loaded_slot instruction ->
      List.rev_append prefix (instruction :: rest), false
    | Sd (stored_register, store_slot) :: rest
      when same_memory_key loaded_slot store_slot
           && equal_reg stored_register loaded_register ->
      List.rev_append prefix rest, true
    | Sd (stored_register, store_slot) :: rest when same_memory_key loaded_slot store_slot
      -> List.rev_append prefix (Sd (stored_register, store_slot) :: rest), false
    | instruction :: rest ->
      (match write_reg instruction with
       | Some written_register when equal_reg written_register loaded_register ->
         List.rev_append prefix (instruction :: rest), false
       | _ -> search (instruction :: prefix) rest)
  in
  search [] following_instructions
;;

let eliminate_redundant_restore_stores instructions =
  let rec loop changed acc = function
    | (Ld (loaded_register, loaded_slot) as load_instruction) :: rest ->
      let new_rest, removed =
        find_redundant_restore_store loaded_register loaded_slot rest
      in
      loop (changed || removed) (load_instruction :: acc) new_rest
    | instruction :: rest -> loop changed (instruction :: acc) rest
    | [] -> List.rev acc, changed
  in
  loop false [] instructions
;;

let optimize instructions =
  let rec fixed_point current =
    let after_local, changed_local = one_pass current in
    let after_load_cache, changed_cache = track_load_cache after_local in
    let after_redundant_store, changed_redundant_store =
      eliminate_redundant_restore_stores after_load_cache
    in
    let after_dead_store, changed_dead_store =
      eliminate_local_dead_stores after_redundant_store
    in
    if changed_local || changed_cache || changed_redundant_store || changed_dead_store
    then fixed_point after_dead_store
    else after_dead_store
  in
  fixed_point instructions
;;
