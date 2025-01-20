(** Copyright 2024-2025, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast

module Ident = struct
  type t = ident

  let compare = compare
end

module MapIdent = struct
  include Map.Make (Ident)
end

type nil = Nil

type chan_value =
  | Chan_initialized of int (** Initialized chanel, identified by id *)
  | Chan_uninitialized of nil

type builtin =
  | Print
  | Println
  | Make
  | Close
  | Recover
  | Len
  | Panic

type value =
  | Value_int of int
  | Value_string of string
  | Value_bool of bool
  | Value_array of int * value list
  | Value_func of func_value
  | Value_chan of chan_value
  | Value_tuple of value list
  | Value_nil of nil

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

type defered_frame = value * value list

type stack_frame =
  { local_envs : local_env * local_env list
  ; deferred_funcs : defered_frame list
  ; returns : value option
  ; panics : value list option
  }

type goroutine =
  { stack : stack_frame * stack_frame list
  ; go_id : int
  }

module Goroutine = struct
  type t = goroutine

  let compare = compare
end

module SendingGoroutines = struct
  type t =
    { send_queue : (goroutine * value) list
    ; chan_id : int
    }

  let compare = compare
end

module ReceivingGoroutines = struct
  type t =
    { receive_queue : goroutine list
    ; chan_id : int
    }

  let compare = compare
end

module Chan = struct
  type t =
    { chan_id : int
    ; value : value option
    }

  let compare = compare
end

module ReadySet = Set.Make (Goroutine)
module SendingSet = Set.Make (SendingGoroutines)
module ReceivingSet = Set.Make (ReceivingGoroutines)
module ChanSet = Set.Make (Chan)

type inited_by =
  | Sender
  | Receiver

type chanel_using_state =
  { sending_goroutine : goroutine
  ; receiving_goroutine : goroutine
  ; value : value
  ; inited_by : inited_by
  }

type eval_state =
  { global_env : value MapIdent.t
  ; running : goroutine option
  ; ready : ReadySet.t
  ; sending : SendingSet.t
  ; receiving : ReceivingSet.t
  ; chanels : ChanSet.t * int
  ; is_using_chanel : chanel_using_state option
  ; next_go_id : int
  }

module Monad = struct
  include BaseMonad

  type 'a t = (eval_state, 'a) BaseMonad.t

  (* global env *)

  let read_global =
    read
    >>= function
    | { global_env; _ } -> return global_env
  ;;

  let write_global global_env = read >>= fun state -> write { state with global_env }

  let save_global_id ident value =
    let* global = read_global in
    write_global (MapIdent.add ident value global)
  ;;

  (* goroutines *)

  let read_running =
    read
    >>= function
    | { running; _ } -> return running
  ;;

  let read_running_fail =
    read
    >>= function
    | { running = Some goroutine; _ } -> return goroutine
    | { running = None; _ } -> fail (Runtime_error (Dev "no goroutine running"))
  ;;

  let write_running running = read >>= fun state -> write { state with running }

  let read_ready =
    read
    >>= function
    | { ready; _ } -> return ready
  ;;

  let write_ready ready = read >>= fun state -> write { state with ready }

  let add_ready goroutine =
    let* goroutines = read_ready in
    write_ready (ReadySet.add goroutine goroutines)
  ;;

  let delete_ready { go_id; _ } =
    let* goroutines = read_ready in
    match
      ReadySet.find_first_opt
        (function
          | { go_id = id; _ } -> go_id = id)
        goroutines
    with
    | Some waiting_goroutine -> write_ready (ReadySet.remove waiting_goroutine goroutines)
    | _ -> return ()
  ;;

  let check_ready_goroutine =
    let* goroutines = read_ready in
    return (if ReadySet.is_empty goroutines then None else Some ())
  ;;

  let read_sending =
    read
    >>= function
    | { sending; _ } -> return sending
  ;;

  let write_sending sending = read >>= fun state -> write { state with sending }

  let push_to_send_queue id goroutine value =
    let* sending = read_sending in
    match
      SendingSet.find_first_opt
        (function
          | { chan_id; _ } -> chan_id = id)
        sending
    with
    | None ->
      fail (Runtime_error (Deadlock "trying to send to closed or uninitialized chanel"))
    | Some { chan_id; send_queue; _ } ->
      let sending = SendingSet.remove { chan_id; send_queue } sending in
      let sending =
        SendingSet.add { chan_id; send_queue = send_queue @ [ goroutine, value ] } sending
      in
      write_sending sending
  ;;

  let pop_from_send_queue id =
    let* sending = read_sending in
    match
      SendingSet.find_first_opt
        (function
          | { chan_id; _ } -> chan_id = id)
        sending
    with
    | None ->
      fail (Runtime_error (Deadlock "trying to send to closed or uninitialized chanel"))
    | Some { send_queue = []; _ } ->
      fail (Runtime_error (Deadlock "trying to send to closed or uninitialized chanel"))
    | Some { chan_id; send_queue = hd :: tl } ->
      let sending = SendingSet.remove { chan_id; send_queue = hd :: tl } sending in
      let sending = SendingSet.add { chan_id; send_queue = tl } sending in
      write_sending sending *> return hd
  ;;

  let is_send_queue_not_empty id =
    let* sending = read_sending in
    match
      SendingSet.find_first_opt
        (function
          | { chan_id; _ } -> chan_id = id)
        sending
    with
    | None ->
      fail (Runtime_error (Deadlock "trying to send to closed or uninitialized chanel"))
    | Some { send_queue = []; _ } -> return None
    | Some { send_queue = _ :: _; _ } -> return (Some ())
  ;;

  let read_receiving =
    read
    >>= function
    | { receiving; _ } -> return receiving
  ;;

  let write_receiving receiving = read >>= fun state -> write { state with receiving }

  let push_to_receive_queue id goroutine =
    let* receiving = read_receiving in
    match
      ReceivingSet.find_first_opt
        (function
          | { chan_id; _ } -> chan_id = id)
        receiving
    with
    | None ->
      fail
        (Runtime_error (Deadlock "trying to receive from closed or uninitialized chanel"))
    | Some { chan_id; receive_queue } ->
      let receiving = ReceivingSet.remove { chan_id; receive_queue } receiving in
      let receiving =
        ReceivingSet.add
          { chan_id; receive_queue = receive_queue @ [ goroutine ] }
          receiving
      in
      write_receiving receiving
  ;;

  let pop_from_receive_queue id =
    let* receiving = read_receiving in
    match
      ReceivingSet.find_first_opt
        (function
          | { chan_id; _ } -> chan_id = id)
        receiving
    with
    | None ->
      fail
        (Runtime_error (Deadlock "trying to receive from closed or uninitialized chanel"))
    | Some { receive_queue = []; _ } ->
      fail
        (Runtime_error (Deadlock "trying to receive from closed or uninitialized chanel"))
    | Some { chan_id; receive_queue = hd :: tl } ->
      let receiving =
        ReceivingSet.remove { chan_id; receive_queue = hd :: tl } receiving
      in
      let receiving = ReceivingSet.add { chan_id; receive_queue = tl } receiving in
      write_receiving receiving *> return hd
  ;;

  let is_receive_queue_not_empty id =
    let* receiving = read_receiving in
    match
      ReceivingSet.find_first_opt
        (function
          | { chan_id; _ } -> chan_id = id)
        receiving
    with
    | None ->
      fail
        (Runtime_error (Deadlock "trying to receive from closed or uninitialized chanel"))
    | Some { receive_queue = []; _ } -> return None
    | Some { receive_queue = _ :: _; _ } -> return (Some ())
  ;;

  let read_next_go_id =
    read
    >>= function
    | { next_go_id; _ } -> return next_go_id
  ;;

  let write_next_go_id next_go_id = read >>= fun state -> write { state with next_go_id }

  let create_goroutine stack_frame =
    let* go_id = read_next_go_id in
    add_ready { stack = stack_frame, []; go_id } *> write_next_go_id (go_id + 1)
  ;;

  let run_goroutine goroutine =
    read_running
    >>= function
    | Some _ -> fail (Runtime_error (Dev "two goroutine running"))
    | None -> write_running (Some goroutine)
  ;;

  let run_ready_goroutine =
    let* ready = read_ready in
    match ReadySet.choose_opt ready with
    | None -> return None
    | Some goroutine ->
      delete_ready goroutine *> run_goroutine goroutine *> return (Some ())
  ;;

  let delete_running_goroutine = write_running None

  (* chanels *)

  let read_chanels =
    read
    >>= function
    | { chanels; _ } -> return chanels
  ;;

  let write_chanels chanels = read >>= fun state -> write { state with chanels }

  let find_chanel_fail = function
    | Chan_uninitialized Nil ->
      fail (Runtime_error (Deadlock "sending to or receiving from uninitialized chanel"))
    | Chan_initialized id ->
      let* chanels, _ = read_chanels in
      (match
         ChanSet.find_first_opt
           (function
             | { chan_id; _ } -> chan_id = id)
           chanels
       with
       | None ->
         fail (Runtime_error (Deadlock "sending to or receiving from closed chanel"))
       | Some { chan_id; _ } -> return chan_id)
  ;;

  let create_chanel =
    let* chanels, id = read_chanels in
    write_chanels (ChanSet.add { chan_id = id; value = None } chanels, id + 1)
    *>
    let* sending = read_sending in
    write_sending (SendingSet.add { chan_id = id; send_queue = [] } sending)
    *>
    let* receiving = read_receiving in
    write_receiving (ReceivingSet.add { chan_id = id; receive_queue = [] } receiving)
    *> return id
  ;;

  let close_chanel = function
    | Chan_uninitialized Nil -> fail (Runtime_error Close_of_nil_chan)
    | Chan_initialized id ->
      let* chanels, next_id = read_chanels in
      (match
         ChanSet.find_first_opt
           (function
             | { chan_id; _ } -> chan_id = id)
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
          | { chan_id; _ } -> chan_id = id)
        chanels
    with
    | Some { chan_id; value = None } ->
      write_chanels
        ( ChanSet.add
            { chan_id; value = Some value }
            (ChanSet.remove { chan_id; value = None } chanels)
        , next_id )
      *> return None
    | Some { value = Some _; _ } -> return (Some ())
    | _ -> fail (Runtime_error (Deadlock "trying to push value to a closed chanel"))
  ;;

  let pop_chan_value id =
    let* chanels, next_id = read_chanels in
    match
      ChanSet.find_first_opt
        (function
          | { chan_id; _ } -> chan_id = id)
        chanels
    with
    | Some { chan_id; value = Some v } ->
      write_chanels
        ( chanels
          |> ChanSet.remove { chan_id; value = Some v }
          |> ChanSet.add { chan_id; value = None }
        , next_id )
      *> return v
    | _ ->
      fail
        (Runtime_error
           (Deadlock "trying to read value from closed chanel (or there is no value)"))
  ;;

  (* sending state *)

  let read_is_using_chanel =
    read
    >>= function
    | { is_using_chanel; _ } -> return is_using_chanel
  ;;

  let write_is_using_chanel is_using_chanel =
    read >>= fun state -> write { state with is_using_chanel }
  ;;

  let start_using_chanel sender_receiver_and_value =
    read_is_using_chanel
    >>= function
    | Some _ -> fail (Runtime_error (Dev "trying to use chanel which is still used"))
    | None -> write_is_using_chanel (Some sender_receiver_and_value)
  ;;

  let use_chanel =
    read_is_using_chanel
    >>= function
    | Some sender_receiver_and_value ->
      write_is_using_chanel None *> return sender_receiver_and_value
    | None ->
      fail
        (Runtime_error
           (Deadlock "trying to leave sending state while not in sending state"))
  ;;

  let is_using_chanel = read_is_using_chanel

  (* single goroutine's stack *)

  let read_stack =
    read_running_fail
    >>= function
    | { stack; _ } -> return stack
  ;;

  let write_stack new_stack =
    read_running_fail
    >>= function
    | { go_id; _ } -> write_running (Some { go_id; stack = new_stack })
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
    | _, [] -> fail (Runtime_error (Dev "trying to delete last stack frame"))
  ;;

  (* local env *)

  let read_local_envs =
    read_stack_frame
    >>= function
    | { local_envs; _ } -> return local_envs
  ;;

  let write_local_envs local_envs =
    read_stack_frame
    >>= function
    | stack_frame -> write_stack_frame { stack_frame with local_envs }
  ;;

  let add_env block env_type =
    let* hd, tl = read_local_envs in
    write_local_envs ({ exec_block = block; env_type; var_map = MapIdent.empty }, hd :: tl)
  ;;

  let delete_env =
    read_local_envs
    >>= function
    | _, hd :: tl -> write_local_envs (hd, tl)
    | _, [] -> fail (Runtime_error (Dev "trying to delete last local env"))
  ;;

  let read_env_type =
    let* { env_type; _ }, _ = read_local_envs in
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
             | None when MapIdent.mem ident env.var_map ->
               { env with var_map = MapIdent.add ident value env.var_map } :: lst
             | None -> env :: lst)
           []
           (hd :: tl))
    in
    write_local_envs (List.hd local_envs, List.tl local_envs)
  ;;

  (* deferred funcs *)

  let read_deferred =
    read_stack_frame
    >>= function
    | { deferred_funcs; _ } -> return deferred_funcs
  ;;

  let write_deferred deferred_funcs =
    read_stack_frame
    >>= fun stack_frame -> write_stack_frame { stack_frame with deferred_funcs }
  ;;

  let add_deferred new_frame =
    let* deferred_funcs = read_deferred in
    write_deferred (new_frame :: deferred_funcs)
  ;;

  (* panic *)

  let read_panics =
    read_stack_frame
    >>= function
    | { panics; _ } -> return panics
  ;;

  let write_panics panics =
    read_stack_frame >>= fun stack_frame -> write_stack_frame { stack_frame with panics }
  ;;

  (*returns*)

  let read_returns =
    read_stack_frame
    >>= function
    | { returns; _ } -> return returns
  ;;

  let write_returns returns =
    read_stack_frame >>= fun stack_frame -> write_stack_frame { stack_frame with returns }
  ;;

  (* exec block (processing statements) *)

  let read_exec_block =
    read_local_envs
    >>= function
    | { exec_block; _ }, _ -> return exec_block
  ;;

  let write_exec_block new_block =
    let* { env_type; var_map; _ }, tl = read_local_envs in
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
    let var_maps = List.map (fun { var_map; _ } -> var_map) (hd :: tl) @ [ global_map ] in
    match List.find_opt (fun map -> MapIdent.mem ident map) var_maps with
    | None -> fail (Runtime_error (TypeCheckFailed ("undefined ident " ^ ident)))
    | Some map ->
      (match MapIdent.find_opt ident map with
       | Some value -> return value
       | None -> fail (Runtime_error (TypeCheckFailed ("undefined ident " ^ ident))))
  ;;

  let update_ident ident t =
    let* hd, tl = read_local_envs in
    let* global_map = read_global in
    let var_map = List.map (fun { var_map; _ } -> var_map) (hd :: tl) in
    match List.find_opt (fun map -> MapIdent.mem ident map) var_map with
    | Some _ -> update_local_id ident t
    | None ->
      let var_map = [ global_map ] in
      (match List.find_opt (fun map -> MapIdent.mem ident map) var_map with
       | Some _ -> save_global_id ident t
       | None -> fail (Runtime_error (TypeCheckFailed ("undefined ident " ^ ident))))
  ;;
end
