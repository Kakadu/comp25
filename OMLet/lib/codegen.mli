(** Copyright 2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Sofya Kozyreva <k81sofia@gmail.com>, Vyacheslav Kochergin <vyacheslav.kochergin1@gmail.com> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module InfoMap : Map.S with type key = string

type state =
  { label_factory : int (* for creating unique ite and function labels *)
  ; is_start_label_put : bool
    (* for now, this is the only way to write _start label at suitable place and do it exactly once *)
  ; a_regs : CodegenTypes.reg list
  ; free_regs : CodegenTypes.reg list
  ; stack : int
  ; info : CodegenTypes.meta_info InfoMap.t
  ; compiled : CodegenTypes.instr list
  }

module type StateErrorMonadType = sig
  type ('s, 'a) t

  val return : 'a -> ('s, 'a) t
  val ( >>= ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
  val fail : string -> ('s, 'a) t
  val read : ('s, 's) t
  val write : 's -> ('s, unit) t
  val run : ('s, 'a) t -> 's -> ('s * 'a, string) result

  module Syntax : sig
    val ( let* ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
  end
end

val codegen_program
  :  Anf.aconstruction list
  -> (state * CodegenTypes.instr list, string) result
