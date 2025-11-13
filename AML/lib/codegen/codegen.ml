(** Copyright 2025-2026, Rodion Suvorov, Dmitriy Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Machine
open Ast
open Ast.Pattern
open Middle.Anf_types

(**
░██████              ░██                                              ░██      ░██████      ░██                  ░██               
  ░██                ░██                                              ░██     ░██   ░██     ░██                  ░██               
  ░██  ░████████  ░████████  ░███████  ░██░████ ░████████   ░██████   ░██    ░██         ░████████  ░██████   ░████████  ░███████  
  ░██  ░██    ░██    ░██    ░██    ░██ ░███     ░██    ░██       ░██  ░██     ░████████     ░██          ░██     ░██    ░██    ░██ 
  ░██  ░██    ░██    ░██    ░█████████ ░██      ░██    ░██  ░███████  ░██            ░██    ░██     ░███████     ░██    ░█████████ 
  ░██  ░██    ░██    ░██    ░██        ░██      ░██    ░██ ░██   ░██  ░██     ░██   ░██     ░██    ░██   ░██     ░██    ░██        
░██████░██    ░██     ░████  ░███████  ░██      ░██    ░██  ░█████░██ ░██      ░██████       ░████  ░█████░██     ░████  ░███████
*)
module State = struct
  type env = (ident, reg, String.comparator_witness) Map.t

  type t =
    { env : env
    ; frame_offset : int
    ; label_id : int
    ; instructions : instr list
    ; arity_map : (ident, int, String.comparator_witness) Map.t
    }

  let initial_state arity_map =
    { env = Map.empty (module String)
    ; frame_offset = 0
    ; label_id = 0
    ; instructions = []
    ; arity_map
    }
  ;;
end

(**
  ░██████                    ░██                                                  ░██████      ░██                  ░██                  ░███     ░███                                         ░██ 
 ░██   ░██                   ░██                                                 ░██   ░██     ░██                  ░██                  ░████   ░████                                         ░██ 
░██         ░███████   ░████████  ░███████   ░████████  ░███████  ░████████     ░██         ░████████  ░██████   ░████████  ░███████     ░██░██ ░██░██  ░███████  ░████████   ░██████    ░████████ 
░██        ░██    ░██ ░██    ░██ ░██    ░██ ░██    ░██ ░██    ░██ ░██    ░██     ░████████     ░██          ░██     ░██    ░██    ░██    ░██ ░████ ░██ ░██    ░██ ░██    ░██       ░██  ░██    ░██ 
░██        ░██    ░██ ░██    ░██ ░█████████ ░██    ░██ ░█████████ ░██    ░██            ░██    ░██     ░███████     ░██    ░█████████    ░██  ░██  ░██ ░██    ░██ ░██    ░██  ░███████  ░██    ░██ 
 ░██   ░██ ░██    ░██ ░██   ░███ ░██        ░██   ░███ ░██        ░██    ░██     ░██   ░██     ░██    ░██   ░██     ░██    ░██           ░██       ░██ ░██    ░██ ░██    ░██ ░██   ░██  ░██   ░███ 
  ░██████   ░███████   ░█████░██  ░███████   ░█████░██  ░███████  ░██    ░██      ░██████       ░████  ░█████░██     ░████  ░███████     ░██       ░██  ░███████  ░██    ░██  ░█████░██  ░█████░██ 
                                                   ░██                                                                                                                                             
                                             ░███████
*)
module Cg = struct
  open State

  type 'a t = Cg of (State.t -> ('a, string) Result.t * State.t)

  let run (state : State.t) (Cg f) : ('a, string) Result.t * State.t = f state
  let return x = Cg (fun state -> Ok x, state)
  let error msg = Cg (fun state -> Error msg, state)

  let ( let* ) (Cg m) f =
    Cg
      (fun state ->
        match m state with
        | Error msg, st -> Error msg, st
        | Ok res, new_state ->
          let (Cg m') = f res in
          m' new_state)
  ;;

  let ( >> ) (m1 : unit t) (m2 : 'a t) : 'a t =
    let* () = m1 in
    m2
  ;;

  let get_state = Cg (fun state -> Ok state, state)
  let set_state new_state = Cg (fun _ -> Ok (), new_state)

  let emit instr =
    let k instr =
      let* state = get_state in
      set_state { state with instructions = instr :: state.instructions }
    in
    instr k
  ;;

  let fresh_label prefix =
    let* state = get_state in
    let id = state.label_id in
    let* () = set_state { state with label_id = id + 1 } in
    return (Printf.sprintf ".L%s_%d" prefix id)
  ;;

  let fresh_fun_symbol () =
    let* st = get_state in
    let id = st.label_id in
    let* () = set_state { st with label_id = id + 1 } in
    return (Printf.sprintf "lam_%d" id)
  ;;

  (* allocates space for a new local variable on the stack. returns stack location *)
  let allocate_local_var id =
    let* state = get_state in
    let new_offset = state.frame_offset + 8 in
    let location = ROff (-new_offset, fp) in
    let new_env = Map.set state.env ~key:id ~data:location in
    let* () = set_state { state with frame_offset = new_offset; env = new_env } in
    return location
  ;;

  let rec map_m f = function
    | [] -> return ()
    | x :: xs ->
      let* () = f x in
      map_m f xs
  ;;
end

(**
░██     ░██            ░██                                           
░██     ░██            ░██                                           
░██     ░██  ░███████  ░██ ░████████   ░███████  ░██░████  ░███████  
░██████████ ░██    ░██ ░██ ░██    ░██ ░██    ░██ ░███     ░██        
░██     ░██ ░█████████ ░██ ░██    ░██ ░█████████ ░██       ░███████  
░██     ░██ ░██        ░██ ░███   ░██ ░██        ░██             ░██ 
░██     ░██  ░███████  ░██ ░██░█████   ░███████  ░██       ░███████  
                           ░██                                       
                           ░██
*)
module Helpers = struct
  open Cg

  let align16 n = if n land 15 = 0 then n else n + (16 - (n land 15))

  (* unroll curried function definitions. e.g., CFun(p1, ACE(CFun(p2, body))) -> ([p1, p2], body) *)
  let rec unroll_fun_chain (params : Ast.Pattern.t list) (aexp : aexpr) =
    match aexp with
    | ACE (CFun (param, body)) -> unroll_fun_chain (params @ [ Pat_var param ]) body
    | _ -> params, aexp
  ;;

  let gen_bin_op op dst r1 r2 =
    let tag_bool_result = emit slli dst dst 1 >> emit addi dst dst 1 in
    match op with
    | Add -> emit add dst r1 r2 >> emit addi dst dst (-1)
    | Sub -> emit sub dst r1 r2 >> emit addi dst dst 1
    | Mul ->
      emit srai t2 r1 1
      >> emit srai t3 r2 1
      >> emit mul dst t2 t3
      >> emit slli dst dst 1
      >> emit addi dst dst 1
    | Le -> emit slt dst r2 r1 >> emit xori dst dst 1 >> tag_bool_result
    | Lt -> emit slt dst r1 r2 >> tag_bool_result
    | Eq ->
      emit sub t2 r1 r2
      >> emit slt dst x0 t2
      >> emit slt t3 t2 x0
      >> emit add dst dst t3
      >> emit xori dst dst 1
      >> tag_bool_result
    | Neq ->
      emit sub t2 r1 r2
      >> emit slt dst x0 t2
      >> emit slt t3 t2 x0
      >> emit add dst dst t3
      >> tag_bool_result
  ;;
end

(**
  ░██████                                      ░██████                    ░██                                             
 ░██   ░██                                    ░██   ░██                   ░██                                             
░██         ░███████  ░██░████  ░███████     ░██         ░███████   ░████████  ░███████   ░████████  ░███████  ░████████  
░██        ░██    ░██ ░███     ░██    ░██    ░██        ░██    ░██ ░██    ░██ ░██    ░██ ░██    ░██ ░██    ░██ ░██    ░██ 
░██        ░██    ░██ ░██      ░█████████    ░██        ░██    ░██ ░██    ░██ ░█████████ ░██    ░██ ░█████████ ░██    ░██ 
 ░██   ░██ ░██    ░██ ░██      ░██            ░██   ░██ ░██    ░██ ░██   ░███ ░██        ░██   ░███ ░██        ░██    ░██ 
  ░██████   ░███████  ░██       ░███████       ░██████   ░███████   ░█████░██  ░███████   ░█████░██  ░███████  ░██    ░██ 
                                                                                                ░██                       
                                                                                          ░███████
*)
module Gen = struct
  open Cg
  open Helpers
  open State

  (* reg to pass argv ptr *)
  type argv_slot =
    | ArgvA1
    | ArgvA2

  (* generate code to build args on the stack *)
  let rec with_call_frame (argv : argv_slot) (args : immexpr list) (k : unit Cg.t)
    : unit Cg.t
    =
    let argc = List.length args in
    let need_pad = argc land 1 = 1 in
    (if need_pad then emit addi sp sp (-8) >> emit sd x0 (ROff (0, sp)) else return ())
    >> emit addi sp sp (-(8 * argc)) (* allocate space for args *)
    >> map_m (* store them *)
         (fun (i, arg) -> gen_immexpr t0 arg >> emit sd t0 (ROff (i * 8, sp)))
         (List.mapi args ~f:(fun i a -> i, a))
    >> (match argv with
      (* put argv ptr in reg *)
      | ArgvA1 -> emit addi a1 sp 0
      | ArgvA2 -> emit addi a2 sp 0)
    >> k
    >> emit addi sp sp (8 * argc) (* clean up stack *)
    >> if need_pad then emit addi sp sp 8 else return ()

  (* generate code to load immexpr into dst reg *)
  and gen_immexpr (dst : reg) (imm : immexpr) : unit Cg.t =
    match imm with
    | ImmNum i -> emit li dst ((i lsl 1) + 1)
    | ImmId id ->
      let* st = get_state in
      (match Map.find st.env id with
       | Some stack_slot -> emit ld dst stack_slot (* if local var -> load from stack *)
       | None ->
         (* if not -> global fun | closure *)
         (match Map.find st.arity_map id with
          | Some arity ->
            (* generate code for closure to pass functions as values *)
            emit la a0 id
            >> emit li a1 arity
            >> emit call "closure_alloc"
            >> if not (equal_reg dst a0) then emit mv dst a0 else return ()
          | None -> emit la dst id))

  (* generates code for aexpr *)
  and gen_expr (dst : reg) (aexpr : aexpr) : unit Cg.t =
    match aexpr with
    | ACE cexpr -> gen_cexpr dst cexpr
    | ALet (_rec_flag, id, cexpr, body) ->
      (* generate code for cexpr and put result in t0 *)
      gen_cexpr t0 cexpr
      >>
      let* loc = allocate_local_var id in
      (* store result from t0 to stack *)
      emit sd t0 loc
      (* generate code for body *)
      >> gen_expr dst body

  (* generates code for cexpr *)
  and gen_cexpr (dst : reg) (cexpr : cexpr) : unit Cg.t =
    match cexpr with
    | CImm imm -> gen_immexpr dst imm
    | CBinop (bop, imm1, imm2) ->
      gen_immexpr t0 imm1 >> gen_immexpr t1 imm2 >> gen_bin_op bop dst t0 t1
    | CApp (ImmId fname, args) ->
      let argc = List.length args in
      let* st = get_state in
      (match Map.find st.env fname with
       | Some _loc ->
         (* if fname is variable -> fname is closure *)
         (* a0 = closure, a1 = argc, a2 = argv *)
         with_call_frame
           ArgvA2
           args
           (gen_immexpr a0 (ImmId fname) (* load closure in a0 *)
            >> emit li a1 argc
            >> emit call "closure_apply")
         >> if not (equal_reg dst a0) then emit mv dst a0 else return ()
       | None ->
         (* if not -> fname is global *)
         (match Map.find st.arity_map fname with
          | None ->
            error (Printf.sprintf "Codegen: function %s not found in arity_map" fname)
          | Some total_arity ->
            if argc = total_arity
            then
              (* call the function directly *)
              (* a0 = argc, a1 = argv *)
              with_call_frame ArgvA1 args (emit li a0 argc >> emit call fname)
              >> if not (equal_reg dst a0) then emit mv dst a0 else return ()
            else if argc < total_arity
            then
              (* partial application *)
              (* create new closure: a0 = closure_alloc(&fname, total_arity) *)
              (* apply args: a0 = closure_apply(a0, argc, argv) *)
              with_call_frame
                ArgvA2
                args
                (emit la a0 fname (* a0 = &fname *)
                 >> emit li a1 total_arity (* a1 = total_arity *)
                 >> emit call "closure_alloc" (* a0 = new_closure *)
                 >> emit li a1 argc (* a1 = argc *)
                 >> emit call "closure_apply")
              (* a0 = new closure *)
              >> if not (equal_reg dst a0) then emit mv dst a0 else return ()
            else (
              (* over-application *)
              let prefix, rest = List.split_n args total_arity in
              let restc = List.length rest in
              (* make full call. result is a closure in a0 *)
              with_call_frame ArgvA1 prefix (emit li a0 total_arity >> emit call fname)
              (* apply the rest args to the resulting closure *)
              >> (if restc = 0
                  then return ()
                  else
                    emit mv t3 a0 (* save resulting closure *)
                    >> with_call_frame
                         ArgvA2
                         rest
                         (emit mv a0 t3 (* restore closure to a0 *)
                          >> emit li a1 restc (* a1 = rest_argc *)
                          >> emit call "closure_apply"))
              >> if not (equal_reg dst a0) then emit mv dst a0 else return ())))
    | CApp (ImmNum _, _) -> error "unreachable: numeric callee"
    | CIte (cond_imm, then_aexpr, else_aexpr) ->
      let* l_else = fresh_label "else" in
      let* l_end = fresh_label "endif" in
      gen_immexpr t0 cond_imm
      >> emit li t1 1
      >> emit beq t0 t1 l_else
      >> gen_expr dst then_aexpr
      >> emit j l_end
      >> emit label l_else
      >> gen_expr dst else_aexpr
      >> emit label l_end
    | CFun (param, body) ->
      (* compile lambda into a new top-level function with prefix lam_ and generate code to create closure for it *)
      let params, fun_body = unroll_fun_chain [ Pat_var param ] body in
      let* lam_name = fresh_fun_symbol () in
      (* compile the lambda's body as a new global function *)
      gen_func lam_name params fun_body
      >>
      (* update arity map *)
      let* st = get_state in
      let arity_map' = Map.set st.arity_map ~key:lam_name ~data:(List.length params) in
      set_state { st with arity_map = arity_map' }
      >>
      (* generate code to create closure. put closure ptr in a0 *)
      emit la a0 lam_name
      >> emit li a1 (List.length params)
      >> emit call "closure_alloc"
      >> if not (equal_reg dst a0) then emit mv dst a0 else return ()

  (* count size of local vars for aexpr for stack mgmt *)
  and count_local_vars = function
    | ALet (_, _, _, body) -> 1 + count_local_vars body
    | ACE cexpr ->
      (match cexpr with
       | CIte (_, then_expr, else_expr) ->
         Int.max (count_local_vars then_expr) (count_local_vars else_expr)
       | _ -> 0)

  (* generate code to bind parameters. assume argv ptr in a1 to caller stack. copy each arg from argv to local stack *)
  and bind_params_from_argv (params : Ast.Pattern.t list) : unit Cg.t =
    map_m
      (fun (i, pat) ->
         match pat with
         | Pat_var id ->
           let* slot = allocate_local_var id in
           (* load arg from the caller's argv and store it *)
           emit ld t0 (ROff (i * 8, a1)) >> emit sd t0 slot
         | _ -> error "only simple variables are supported in parameters")
      (List.mapi params ~f:(fun i p -> i, p))

  (* generate code for a top-level function *)
  and gen_func name args body =
    (* Stack Layout:
     * [ ...           ]
     * [ local_N       ] -N*8(fp)
     * [ ...           ]
     * [ local_1       ] -24(fp)
     * [ old fp (s0)   ] -16(fp)
     * [ return addr   ] -8(fp)
     * [ fp ] -> [ sp ]
    *)
    let locals_count = count_local_vars body + List.length args in
    let stack_size = align16 (16 + (locals_count * 8)) in
    (* save the global state. will restore it after generating function body *)
    let* st0 = get_state in
    let arity_map' = Map.set st0.arity_map ~key:name ~data:(List.length args) in
    set_state { st0 with arity_map = arity_map' }
    (* function prologue *)
    >> emit directive (Printf.sprintf ".globl %s" name)
    >> emit directive (Printf.sprintf ".type %s, @function" name)
    >> emit label name
    >> emit addi sp sp (-stack_size)
    >> emit sd ra (ROff (stack_size - 8, sp))
    >> emit sd fp (ROff (stack_size - 16, sp))
    >> emit addi fp sp stack_size
    >>
    (* init runtime stuff if main *)
    (if String.equal name "main"
     then emit call "heap_init" >> emit la t0 "ML_STACK_BASE" >> emit sd fp (ROff (0, t0))
     else return ())
    >>
    (* create local env for this function *)
    let* global_state = get_state in
    let initial_cg_state =
      { global_state with
        env = Map.empty (module String)
      ; frame_offset = 16 (* offset after ra/fp *)
      }
    in
    set_state initial_cg_state
    (* function body *)
    >> bind_params_from_argv args
    >> gen_expr a0 body
    >>
    (* restore the global state with updated instructions, label count, arity map *)
    let* final_state = get_state in
    set_state
      { global_state with
        label_id = final_state.label_id
      ; instructions = final_state.instructions
      ; arity_map = final_state.arity_map
      }
    (* function epilogue *)
    >> emit ld ra (ROff (stack_size - 8, sp))
    >> emit ld fp (ROff (stack_size - 16, sp))
    >> emit addi sp sp stack_size
    >>
    if String.equal name "main"
    then emit li a0 0 >> emit li (A 7) 93 >> emit ecall
    else emit ret
  ;;

  (* generate code for each top-level function (whole program) *)
  let gen_program (s : aprogram) =
    emit directive ".text"
    >> map_m
         (function
           | AStr_value (_rec_flag, name, expr) ->
             let params, body = Helpers.unroll_fun_chain [] expr in
             gen_func name params body
           | AStr_eval expr -> gen_func "main" [] expr)
         s
  ;;
end

let codegen ppf (s : aprogram) =
  let initial_arity_map =
    List.fold
      s
      ~init:(Map.empty (module String))
      ~f:(fun acc -> function
        | AStr_value (_, name, expr) ->
          let params, _ = Helpers.unroll_fun_chain [] expr in
          Map.set acc ~key:name ~data:(List.length params)
        | _ -> acc)
  in
  let initial_arity_map =
    initial_arity_map
    |> Map.set ~key:"print_int" ~data:1
    |> Map.set ~key:"collect" ~data:1
    |> Map.set ~key:"get_heap_start" ~data:1
    |> Map.set ~key:"get_heap_fin" ~data:1
    |> Map.set ~key:"print_gc_status" ~data:1
  in
  let initial_state = State.initial_state initial_arity_map in
  let computation = Gen.gen_program s in
  let result, final_state = Cg.run initial_state computation in
  match result with
  | Ok () -> pp_instrs ppf final_state.instructions
  | Error msg -> Stdlib.Format.fprintf ppf ";; Codegen error: %s\n" msg
;;
