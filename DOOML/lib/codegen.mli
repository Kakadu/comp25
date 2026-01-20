val emit_ir : ?triple:string -> Anf.decl list -> Llvm.llmodule
val optimize_ir : ?triple:string -> Llvm.llmodule -> unit
val emit_binary : ?triple:string -> ?features:string -> Llvm.llmodule -> string -> unit
