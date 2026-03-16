(** Copyright 2025-2026, Georgiy Belyanin, Ignat Sergeev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val emit_ir : ?triple:string -> Anf.decl list -> Llvm.llmodule
val optimize_ir : ?triple:string -> Llvm.llmodule -> unit
val emit_binary : ?triple:string -> ?features:string -> Llvm.llmodule -> string -> unit
