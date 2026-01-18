[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Llvm
open Printf

module type S = sig
  val context : Llvm.llcontext
  val module_ : Llvm.llmodule
  val builder : Llvm.llbuilder
  val build_store : Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue
  val build_call : ?name:string -> lltype -> llvalue -> llvalue list -> llvalue
  val lookup_func_exn : string -> llvalue
  val lookup_func : string -> llvalue option
  val lookup_func_type_exn : string -> lltype
  val define_func : string -> Llvm.lltype -> Llvm.lltype array -> Llvm.llvalue
  val declare_func : string -> Llvm.lltype -> Llvm.lltype array -> Llvm.llvalue
  val has_toplevel_func : string -> bool
  val build_add : ?name:string -> llvalue -> llvalue -> llvalue
  val build_sub : ?name:string -> llvalue -> llvalue -> llvalue
  val build_mul : ?name:string -> llvalue -> llvalue -> llvalue
  val build_sdiv : ?name:string -> llvalue -> llvalue -> llvalue
  val build_icmp : ?name:string -> Icmp.t -> llvalue -> llvalue -> llvalue
  val build_ret : llvalue -> llvalue
  val build_br : llbasicblock -> llvalue
  val build_cond_br : llvalue -> llbasicblock -> llbasicblock -> llvalue
  val build_phi : ?name:string -> (llvalue * llbasicblock) list -> llvalue
  val build_array_alloca : ?name:string -> lltype -> llvalue -> llvalue
  val build_gep : ?name:string -> llvalue -> llvalue array -> llvalue

  (** [set_metadata v kind fmt] sets metadata to value [v] of kind [k].
      Returns this value [v]. Useful for attaching debugging *)
  val set_metadata
    :  llvalue
    -> string
    -> ('a, Format.formatter, unit, llvalue) format4
    -> 'a

  (* ?? *)

  val build_ptrtoint : ?name:string -> llvalue -> lltype -> llvalue
  val build_inttoptr : ?name:string -> llvalue -> lltype -> llvalue
  val build_pointercast : ?name:string -> llvalue -> lltype -> llvalue
  val build_intcast : ?name:string -> llvalue -> lltype -> llvalue
  val position_at_end : llbasicblock -> unit
  val append_block : ?name:string -> llvalue -> llbasicblock
  val insertion_block : unit -> llbasicblock

  (** Just aliases *)

  val void_type : Llvm.lltype
  val block_parent : Llvm.llbasicblock -> Llvm.llvalue
  val entry_block : Llvm.llvalue -> Llvm.llbasicblock
  val i64_type : Llvm.lltype
  val i1_type : Llvm.lltype
  val function_type : lltype -> lltype array -> lltype
  val const_int : Llvm.lltype -> int -> Llvm.llvalue
  val params : Llvm.llvalue -> Llvm.llvalue array
  val pp_value : Format.formatter -> llvalue -> unit
end

let make context builder module_ =
  let module L : S = struct
    let context = context
    let builder = builder
    let module_ = module_
    let func_types : (string, Llvm.lltype) Hashtbl.t = Hashtbl.create 100
    let build_store a b = Llvm.build_store a b builder

    let build_call ?(name = "") typ f args =
      build_call typ f (Array.of_list args) name builder
    ;;

    let has_toplevel_func fname =
      match lookup_function fname module_ with
      | Some _ -> true
      | None -> false
    ;;

    let lookup_func_exn fname =
      match lookup_function fname module_ with
      | Some f -> f
      | None -> failwith (sprintf "Function '%s' not found" fname)
    ;;

    let add_func_type name typ = Hashtbl.add func_types name typ

    let lookup_func_type_exn name =
      match Hashtbl.find_opt func_types name with
      | Some t -> t
      | None -> failwith (sprintf "Function '%s' not found" name)
    ;;

    let lookup_func fname = lookup_function fname module_

    let declare_func name ret params =
      let typ = Llvm.function_type ret params in
      add_func_type name typ;
      Llvm.declare_function name typ module_
    ;;

    let define_func name ret params =
      let typ = Llvm.function_type ret params in
      add_func_type name typ;
      Llvm.define_function name typ module_
    ;;

    let build_add ?(name = "") l r = build_add l r name builder
    let build_sub ?(name = "") l r = build_sub l r name builder
    let build_mul ?(name = "") l r = build_mul l r name builder
    let build_sdiv ?(name = "") l r = build_sdiv l r name builder
    let build_icmp ?(name = "") op l r = build_icmp op l r name builder
    let build_ptrtoint ?(name = "") e typ = Llvm.build_ptrtoint e typ name builder
    let build_inttoptr ?(name = "") e typ = Llvm.build_inttoptr e typ name builder
    let build_pointercast ?(name = "") f typ = Llvm.build_pointercast f typ name builder
    let build_intcast ?(name = "") v typ = Llvm.build_intcast v typ name builder
    let build_ret v = build_ret v builder
    let build_br bb = build_br bb builder
    let build_cond_br c tb fb = build_cond_br c tb fb builder
    let build_phi ?(name = "") rules = build_phi rules name builder
    let build_array_alloca ?(name = "") typ n = Llvm.build_array_alloca typ n name builder
    let build_gep ?(name = "") v ind = Llvm.build_gep (type_of v) v ind name builder

    let set_metadata v kind fmt =
      Format.kasprintf
        (fun s ->
           Llvm.set_metadata v (Llvm.mdkind_id context kind) (Llvm.mdstring context s);
           v)
        fmt
    ;;

    let position_at_end bb = Llvm.position_at_end bb builder
    let insertion_block () = Llvm.insertion_block builder
    let append_block ?(name = "") f = Llvm.append_block context name f

    (* Aliases *)
    let block_parent = Llvm.block_parent
    let entry_block = Llvm.entry_block
    let void_type = Llvm.void_type context
    let i64_type = Llvm.i64_type context
    let i1_type = Llvm.i1_type context
    let function_type = Llvm.function_type
    let const_int = Llvm.const_int
    let params = Llvm.params
    let pp_value ppf x = Format.fprintf ppf "%s" (Llvm.string_of_llvalue x)
  end
  in
  (module L : S)
;;
