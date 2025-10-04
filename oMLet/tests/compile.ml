(** Copyright 2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Sofya Kozyreva <k81sofia@gmail.com> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open OMLet

let () =
  let source =
    "let rec fac n =\n\
    \  if n <= 1\n\
    \  then 1\n\
    \  else let n1 = n-1 in\n\
    \       let m = fac n1 in\n\
    \       n*m\n\n\
     let main = fac 4"
  in
  match Parser.parse source with
  | Error e -> Stdlib.Format.printf "Parsing error: %s\n%!" e
  | Ok ast ->
    let asm = Codegen.codegen ast in
    Stdlib.List.iter CodegenTypes.pp_instr asm
;;
(* let () =
  let asm =
    "
.global _start
.fac_0:
        addi sp, sp, -64
        sd ra, 0(sp)
        sd a0, 8(sp)
        ld a0, 8(sp)
        mv t0, a0
        li a0, 1
        mv t1, a0
        slt a0, t0, t1
        xori a0, a0, 1
        mv t0, a0
        xori t0, t0, 1
        beq x0, t0, .else_1
        li a0, 1
        j .join_2
.else_1:
        ld a0, 8(sp)
        mv t1, a0
        li a0, 1
        mv t2, a0
        sub a0, t1, t2
        sd a0, 16(sp)
        sd t0, 24(sp)
        sd t1, 32(sp)
        sd t0, 40(sp)
        sd t1, 48(sp)
        sd t2, 56(sp)
        ld a0, 16(sp)
        call .fac_0
        ld t2, 56(sp)
        ld t1, 48(sp)
        ld t0, 40(sp)
        ld t1, 32(sp)
        ld t0, 24(sp)
        sd a0, 24(sp)
        ld a0, 8(sp)
        mv t1, a0
        ld a0, 24(sp)
        mv t2, a0
        mul a0, t1, t2
.join_2:
        ld ra, 0(sp)
        addi sp, sp, 64
        ret 
_start:
        addi sp, sp, -64
        li a0, 4
        call .fac_0
        sd a0, 64(sp)
        addi sp, sp, 64
        li a7, 93
ecall
"
  in
  print_string asm
;; *)
