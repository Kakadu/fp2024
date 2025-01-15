(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Event

module Ident = struct
  type t = ident

  let compare = compare
end

module MapIdent = Map.Make (Ident)

type nil = Nil

type chan_value =
  | Chan_initialized of type' channel
  (** Initialized channel, may either be opened or closed *)
  | Chan_uninitialized of nil

type value =
  | Value_int of int
  | Value_string of string
  | Value_bool of bool
  | Value_array of int * value list
  | Value_func of func_value
  | Value_chan of chan_value
  | Value_nil of nil

and builtin =
  | Print
  | Println
  | Make
  | Recover
  | Len
  | Panic

and is_closure =
  | Closure of value MapIdent.t
  | Default

and func_value =
  | Func_initialized of is_closure * anon_func
  | Func_uninitialized of nil
  | Func_builtin of builtin

type is_for_env =
  | For
  | Default

type local_env =
  { exec_block : block
  ; var_map : value MapIdent.t
  ; env_type : is_for_env
  }

and closure_frame =
  | Simple
  | Closure of local_env * local_env list

type stack_frame =
  { local_envs : local_env * local_env list
  ; deferred_funcs : stack_frame list
  ; closure_envs : closure_frame
  }

type sleeping_state =
  | Ready
  | Sending of ident
  | Recieving of ident

type goroutine =
  { stack : stack_frame * stack_frame list
  ; id : int
  }

type eval_state =
  { global_env : value MapIdent.t
  ; running : goroutine option
  ; sleeping : (sleeping_state * goroutine) list
  }

module Monad = struct
  include BaseMonad

  type 'a t = (eval_state, 'a) BaseMonad.t

  (* global env *)

  let read_global =
    read
    >>= function
    | { global_env } -> return global_env
  ;;

  let write_global new_global =
    read
    >>= function
    | { running; sleeping } -> write { global_env = new_global; running; sleeping }
  ;;

  let save_global_id ident value =
    let* global = read_global in
    write_global (MapIdent.add ident value global)
  ;;

  (* goroutines *)

  let read_sleeping =
    read
    >>= function
    | { sleeping } -> return sleeping
  ;;

  let write_sleeping new_goroutines =
    read
    >>= function
    | { global_env; running } -> write { global_env; running; sleeping = new_goroutines }
  ;;

  let add_sleeping sleeping_state goroutine =
    let* goroutines = read_sleeping in
    write_sleeping ((sleeping_state, goroutine) :: goroutines)
  ;;

  let read_running =
    read
    >>= function
    | { running } -> return running
  ;;

  let read_running_fail =
    read
    >>= function
    | { running = Some goroutine } -> return goroutine
    | { running = None } -> fail (Runtime_error (DevOnly No_goroutine_running))
  ;;

  let write_running new_goroutine =
    read
    >>= function
    | { global_env; sleeping } -> write { global_env; sleeping; running = new_goroutine }
  ;;

  let run_goroutine goroutine =
    read_running
    >>= function
    | None -> write_running (Some goroutine)
    | Some _ -> fail (Runtime_error (DevOnly Two_goroutine_running))
  ;;

  let stop_running_goroutine sleeping_state =
    read_running_fail
    >>= function
    | goroutine -> write_running None *> add_sleeping sleeping_state goroutine
  ;;

  (* single goroutine's stack *)

  let read_stack =
    read_running_fail
    >>= function
    | { stack } -> return stack
  ;;

  let write_stack new_stack =
    read_running_fail
    >>= function
    | { id } -> write_running (Some { id; stack = new_stack })
  ;;

  let read_stack_frame =
    read_stack
    >>= function
    | hd, _ -> return hd
  ;;

  let write_stack_frame new_frame =
    read_stack
    >>= function
    | _, tl -> write_stack (new_frame, tl)
  ;;

  let add_stack_frame new_frame =
    read_stack
    >>= function
    | hd, tl -> write_stack (new_frame, hd :: tl)
  ;;

  let delete_stack_frame =
    read_stack
    >>= function
    | _, hd :: tl -> write_stack (hd, tl)
    | _, [] -> fail (Runtime_error (DevOnly Not_enough_stack_frames))
  ;;

  (* local env *)

  let read_local_envs =
    read_stack_frame
    >>= function
    | { local_envs } -> return local_envs
  ;;

  let write_local_envs new_local_envs =
    read_stack_frame
    >>= function
    | { deferred_funcs; closure_envs } ->
      write_stack_frame { deferred_funcs; closure_envs; local_envs = new_local_envs }
  ;;

  let add_env block env_type =
    let* hd, tl = read_local_envs in
    write_local_envs ({ exec_block = block; env_type; var_map = MapIdent.empty }, hd :: tl)
  ;;

  let delete_env =
    read_local_envs
    >>= function
    | _, hd :: tl -> write_local_envs (hd, tl)
    | _, [] -> fail (Runtime_error (DevOnly Not_enough_local_envs))
  ;;

  let read_env_type =
    let* { env_type }, _ = read_local_envs in
    return env_type
  ;;

  let save_local_id ident value =
    let* { exec_block; env_type; var_map }, tl = read_local_envs in
    let new_map = MapIdent.add ident value var_map in
    write_local_envs ({ exec_block; env_type; var_map = new_map }, tl)
  ;;

  (* deferred funcs *)

  let read_deferred =
    read_stack_frame
    >>= function
    | { deferred_funcs } -> return deferred_funcs
  ;;

  let write_deferred new_deferred =
    read_stack_frame
    >>= function
    | { local_envs; closure_envs } ->
      write_stack_frame { local_envs; deferred_funcs = new_deferred; closure_envs }
  ;;

  let add_deferred new_frame =
    let* deferred_funcs = read_deferred in
    write_deferred (new_frame :: deferred_funcs)
  ;;

  let delete_deferred =
    let* deferred_funcs = read_deferred in
    write_deferred (List.tl deferred_funcs)
  ;;

  (*closure_envs*)

  let read_closure_env =
    read_stack_frame
    >>= function
    | { closure_envs } -> return closure_envs
  ;;

  let write_closure_env new_closure_env =
    read_stack_frame
    >>= function
    | { local_envs; deferred_funcs } ->
      write_stack_frame { local_envs; closure_envs = new_closure_env; deferred_funcs }
  ;;

  let save_closure_id ident value =
    let* { exec_block; env_type; var_map }, tl = read_local_envs in
    let new_map = MapIdent.add ident value var_map in
    write_closure_env (Closure ({ exec_block; env_type; var_map = new_map }, tl))
  ;;

  (* exec block (processing statements) *)

  let read_exec_block =
    let* { exec_block }, _ = read_local_envs in
    return exec_block
  ;;

  let write_exec_block new_block =
    let* { env_type; var_map }, tl = read_local_envs in
    write_local_envs ({ env_type; var_map; exec_block = new_block }, tl)
  ;;

  let pop_next_statement =
    read_exec_block
    >>= function
    | hd :: tl -> write_exec_block tl *> return (Some hd)
    | [] -> return None
  ;;

  (* reading ident *)

  let read_ident ident =
    let* hd, tl = read_local_envs in
    let* global_map = read_global in
    let* closure_frame = read_closure_env in
    match closure_frame with
    | Simple ->
      let var_maps = List.map (fun { var_map } -> var_map) (hd :: tl) @ [ global_map ] in
      (match List.find_opt (fun map -> MapIdent.mem ident map) var_maps with
       | None -> fail (Runtime_error (DevOnly (Undefined_ident (ident ^ "HERE"))))
       | Some map ->
         (match MapIdent.find_opt ident map with
          | Some value -> return value
          | None -> fail (Runtime_error (DevOnly (Undefined_ident ident)))))
    | Closure (l_env, lst_envs) ->
      let var_maps = List.map (fun { var_map } -> var_map) (hd :: tl) @ [ global_map ] in
      (match List.find_opt (fun map -> MapIdent.mem ident map) var_maps with
       | None -> fail (Runtime_error (DevOnly (Undefined_ident ident)))
       | Some map ->
         (match MapIdent.find_opt ident map with
          | Some value -> return value
          | None ->
            let closure_maps =
              List.map (fun { var_map } -> var_map) (l_env :: lst_envs)
            in
            (match List.find_opt (fun map -> MapIdent.mem ident map) closure_maps with
             | Some map ->
               (match MapIdent.find_opt ident map with
                | Some value -> return value
                | None -> fail (Runtime_error (DevOnly (Undefined_ident ident))))
             | None -> fail (Runtime_error (DevOnly (Undefined_ident ident))))))
  ;;

  let save_ident ident t =
    let* hd, tl = read_local_envs in
    let* global_map = read_global in
    let* closure_frame = read_closure_env in
    match closure_frame with
    | Simple ->
      let var_map = List.map (fun { var_map } -> var_map) (hd :: tl) in
      (match List.find_opt (fun map -> MapIdent.mem ident map) var_map with
       | Some _ -> save_local_id ident t
       | None ->
         let var_map = [ global_map ] in
         (match List.find_opt (fun map -> MapIdent.mem ident map) var_map with
          | Some _ -> save_global_id ident t
          | None -> fail (Runtime_error (DevOnly (Undefined_ident ident)))))
    | Closure (l_env, lst_envs) ->
      let var_map = List.map (fun { var_map } -> var_map) (hd :: tl) in
      (match List.find_opt (fun map -> MapIdent.mem ident map) var_map with
       | Some _ -> save_local_id ident t
       | None ->
         let var_map = List.map (fun { var_map } -> var_map) (hd :: tl) in
         (match List.find_opt (fun map -> MapIdent.mem ident map) var_map with
          | Some _ -> save_closure_id ident t
          | None ->
            let var_map = List.map (fun { var_map } -> var_map) (l_env :: lst_envs) in
            (match List.find_opt (fun map -> MapIdent.mem ident map) var_map with
             | Some _ -> save_closure_id ident t
             | None -> fail (Runtime_error (DevOnly (Undefined_ident ident))))))
  ;;
end
