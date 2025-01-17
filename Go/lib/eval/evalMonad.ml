(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast

module Ident = struct
  type t = ident

  let compare = compare
end

module MapIdent = Map.Make (Ident)

type nil = Nil

type chan_value =
  | Chan_initialized of int (** Initialized chanel, identified by id *)
  | Chan_uninitialized of nil

type value =
  | Value_int of int
  | Value_string of string
  | Value_bool of bool
  | Value_array of int * value list
  | Value_func of func_value
  | Value_chan of chan_value
  | Value_tuple of value list
  | Value_nil of nil

and builtin =
  | Print
  | Println
  | Make
  | Close
  | Recover
  | Len
  | Panic

and func_type =
  | Closure of value MapIdent.t
  | FuncLit
  | Default

and func_value =
  | Func_initialized of func_type * anon_func
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
  ; returns : value option
  }

type waiting_state =
  | Ready
  | Sending of { chan_id : int }
  | Receiving of { chan_id : int }

type goroutine =
  { stack : stack_frame * stack_frame list
  ; go_id : int
  }

module WaitingGoroutine = struct
  type t =
    { state : waiting_state
    ; goroutine : goroutine
    }

  let compare = compare
end

module GoSet = Set.Make (WaitingGoroutine)

module Chan = struct
  type t =
    { chan_id : int
    ; value : value option
    }

  let compare = compare
end

module ChanSet = Set.Make (Chan)

type chanel_using_state =
  { receiving_goroutine : goroutine
  ; value : value
  }

type eval_state =
  { global_env : value MapIdent.t
  ; running : goroutine option
  ; waiting : GoSet.t
  ; chanels : ChanSet.t * int
  ; is_using_chanel : chanel_using_state option
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
    | { running; waiting; chanels; is_using_chanel } ->
      write { global_env = new_global; running; waiting; chanels; is_using_chanel }
  ;;

  let save_global_id ident value =
    let* global = read_global in
    write_global (MapIdent.add ident value global)
  ;;

  (* goroutines *)

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

  let read_running_id = read_running_fail >>= fun { go_id } -> return go_id

  let write_running new_goroutine =
    read
    >>= function
    | { global_env; waiting; chanels; is_using_chanel } ->
      write { global_env; waiting; chanels; is_using_chanel; running = new_goroutine }
  ;;

  let read_waiting =
    read
    >>= function
    | { waiting } -> return waiting
  ;;

  let write_waiting new_goroutines =
    read
    >>= function
    | { global_env; running; chanels; is_using_chanel } ->
      write { global_env; running; chanels; is_using_chanel; waiting = new_goroutines }
  ;;

  let add_waiting state goroutine =
    let* goroutines = read_waiting in
    write_waiting (GoSet.add { state; goroutine } goroutines)
  ;;

  let delete_waiting { go_id } =
    let* goroutines = read_waiting in
    match
      GoSet.find_first_opt
        (function
          | { goroutine = { go_id = id } } -> go_id = id)
        goroutines
    with
    | Some waiting_goroutine -> write_waiting (GoSet.remove waiting_goroutine goroutines)
    | _ -> return ()
  ;;

  let ready_waiting { go_id } =
    let* goroutines = read_waiting in
    match
      GoSet.find_first_opt
        (function
          | { goroutine = { go_id = id } } -> id = go_id)
        goroutines
    with
    | Some { goroutine } -> delete_waiting goroutine *> add_waiting Ready goroutine
    | _ -> return ()
  ;;

  let create_goroutine stack_frame =
    let* go_id = read_waiting >>= fun set -> return (GoSet.cardinal set) in
    let* go_id =
      read_running
      >>= function
      | Some _ -> return (go_id + 2)
      | None -> return (go_id + 1)
    in
    add_waiting Ready { stack = stack_frame, []; go_id }
  ;;

  let run_goroutine goroutine =
    read_running
    >>= function
    | None -> delete_waiting goroutine *> write_running (Some goroutine)
    | Some _ -> fail (Runtime_error (DevOnly Two_goroutine_running))
  ;;

  let stop_running_goroutine waiting_state =
    read_running_fail
    >>= function
    | goroutine -> write_running None *> add_waiting waiting_state goroutine
  ;;

  let delete_running_goroutine = write_running None

  (* chanels *)

  let read_chanels =
    read
    >>= function
    | { chanels } -> return chanels
  ;;

  let write_chanels new_chanels =
    read
    >>= function
    | { global_env; running; waiting; is_using_chanel } ->
      write { global_env; running; waiting; is_using_chanel; chanels = new_chanels }
  ;;

  let find_chanel_fail = function
    | Chan_uninitialized Nil ->
      fail (Runtime_error (Deadlock "sending to or receiving from uninitialized chanel"))
    | Chan_initialized id ->
      let* chanels, _ = read_chanels in
      (match
         ChanSet.find_first_opt
           (function
             | { chan_id } -> chan_id = id)
           chanels
       with
       | None ->
         fail (Runtime_error (Deadlock "sending to or receiving from closed chanel"))
       | Some { chan_id } -> return chan_id)
  ;;

  let create_chanel =
    let* chanels, id = read_chanels in
    write_chanels (ChanSet.add { chan_id = id; value = None } chanels, id + 1)
    *> return id
  ;;

  let close_chanel = function
    | Chan_uninitialized Nil -> fail (Runtime_error Close_of_nil_chan)
    | Chan_initialized id ->
      let* chanels, next_id = read_chanels in
      (match
         ChanSet.find_first_opt
           (function
             | { chan_id } -> chan_id = id)
           chanels
       with
       | None -> fail (Runtime_error Close_of_closed_chan)
       | Some chan -> write_chanels (ChanSet.remove chan chanels, next_id))
  ;;

  let push_chan_value id value =
    let* chanels, next_id = read_chanels in
    match
      ChanSet.find_first_opt
        (function
          | { chan_id } -> chan_id = id)
        chanels
    with
    | Some { chan_id; value = None } ->
      write_chanels
        ( ChanSet.add
            { chan_id; value = Some value }
            (ChanSet.remove { chan_id; value = None } chanels)
        , next_id )
      *> return (Some value)
    | Some { value = Some _ } -> return None
    | _ -> fail (Runtime_error (Deadlock "trying to push value to a closed chanel"))
  ;;

  let pop_chan_value id =
    let* chanels, next_id = read_chanels in
    match
      ChanSet.find_first_opt
        (function
          | { chan_id } -> chan_id = id)
        chanels
    with
    | Some { chan_id; value = Some v } ->
      write_chanels
        ( ChanSet.add
            { chan_id; value = None }
            (ChanSet.remove { chan_id; value = Some v } chanels)
        , next_id )
      *> return v
    | _ ->
      fail
        (Runtime_error
           (Deadlock "trying to read value from closed chanel (or there is no value)"))
  ;;

  (* sending state *)

  let start_using_chanel receiving_goroutine value =
    read
    >>= function
    | { running; waiting; chanels; global_env; is_using_chanel } ->
      (match is_using_chanel with
       | Some _ ->
         fail
           (Runtime_error
              (Deadlock "trying to enter sending state while in sending state"))
       | None ->
         write
           { running
           ; waiting
           ; chanels
           ; global_env
           ; is_using_chanel = Some { receiving_goroutine; value }
           })
  ;;

  let use_chanel =
    read
    >>= function
    | { running; waiting; chanels; global_env; is_using_chanel } ->
      (match is_using_chanel with
       | Some { receiving_goroutine; value } ->
         write { running; waiting; chanels; global_env; is_using_chanel = None }
         *> return (receiving_goroutine, value)
       | None ->
         fail
           (Runtime_error
              (Deadlock "trying to leave sending state while not in sending state")))
  ;;

  let is_using_chanel =
    read
    >>= function
    | { is_using_chanel = Some _ } -> return true
    | { is_using_chanel = None } -> return false
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
    | { go_id } -> write_running (Some { go_id; stack = new_stack })
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
    | { deferred_funcs; returns } ->
      write_stack_frame { deferred_funcs; local_envs = new_local_envs; returns }
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

  let update_local_id ident value =
    let* hd, tl = read_local_envs in
    let local_envs =
      List.rev
        (List.fold_left
           (fun lst env ->
             match List.find_opt (fun x -> MapIdent.mem ident x.var_map) lst with
             | Some _ -> env :: lst
             | None ->
               (match MapIdent.mem ident env.var_map with
                | true ->
                  { exec_block = env.exec_block
                  ; var_map = MapIdent.add ident value env.var_map
                  ; env_type = env.env_type
                  }
                  :: lst
                | false -> env :: lst))
           []
           (hd :: tl))
    in
    write_local_envs (List.hd local_envs, List.tl local_envs)
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
    | { local_envs; returns } ->
      write_stack_frame { local_envs; deferred_funcs = new_deferred; returns }
  ;;

  let add_deferred new_frame =
    let* deferred_funcs = read_deferred in
    write_deferred (new_frame :: deferred_funcs)
  ;;

  let delete_deferred =
    let* deferred_funcs = read_deferred in
    write_deferred (List.tl deferred_funcs)
  ;;

  (*returns*)

  let read_returns =
    read_stack_frame
    >>= function
    | { returns } -> return returns
  ;;

  let write_returns new_returns =
    read_stack_frame
    >>= function
    | { local_envs; deferred_funcs } ->
      write_stack_frame { local_envs; deferred_funcs; returns = new_returns }
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
    let var_maps = List.map (fun { var_map } -> var_map) (hd :: tl) @ [ global_map ] in
    match List.find_opt (fun map -> MapIdent.mem ident map) var_maps with
    | None -> fail (Runtime_error (DevOnly (Undefined_ident (ident ^ "HERE"))))
    | Some map ->
      (match MapIdent.find_opt ident map with
       | Some value -> return value
       | None -> fail (Runtime_error (DevOnly (Undefined_ident ident))))
  ;;

  let update_ident ident t =
    let* hd, tl = read_local_envs in
    let* global_map = read_global in
    let var_map = List.map (fun { var_map } -> var_map) (hd :: tl) in
    match List.find_opt (fun map -> MapIdent.mem ident map) var_map with
    | Some _ -> update_local_id ident t
    | None ->
      let var_map = [ global_map ] in
      (match List.find_opt (fun map -> MapIdent.mem ident map) var_map with
       | Some _ -> save_global_id ident t
       | None -> fail (Runtime_error (DevOnly (Undefined_ident ident))))
  ;;
end
