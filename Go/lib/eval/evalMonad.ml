(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast

module Ident = struct
  type t = ident

  let compare = compare
end

module MapIdent = Map.Make (Ident)

type nil

type chan_value =
  | Chan_initialized of bool
  | Chan_uninitialized of nil

type value =
  | Value_int of int
  | Value_string of string
  | Value_bool of bool
  | Value_array of int * value list
  | Value_func of func_value
  | Value_chan of chan_value
  | Value_nil of nil

and func_value =
  | Func_initialized of value MapIdent.t * anon_func
  | Func_uninitialized of nil

type stack_frame =
  { local_envs : value MapIdent.t list
  ; expr_eval : value list
  ; deferred_funcs : stack_frame list (* мб тут не тот тип, но вроде должно работать *)
  }

type goroutine_state =
  | Running
  | Ready
  | Sending of ident
  | Recieving of ident

type goroutine =
  { stack : stack_frame list * stack_frame
  ; state : goroutine_state
  ; id : int
  }

type eval_state =
  { global_env : value MapIdent.t
  ; running : goroutine
  ; sleeping : goroutine list
  }

module EvalMonad = struct
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

  let add_sleeping goroutine =
    let* goroutines = read_sleeping in
    write_sleeping (goroutine :: goroutines)
  ;;

  let read_running =
    read
    >>= function
    | { running } -> return running
  ;;

  let write_running new_goroutine =
    read
    >>= function
    | { global_env; sleeping } -> write { global_env; sleeping; running = new_goroutine }
  ;;

  (* single goroutine's stack *)

  let read_stack =
    read_running
    >>= function
    | { stack } -> return stack
  ;;

  let write_stack new_stack =
    read_running
    >>= function
    | { state; id } -> write_running { state; id; stack = new_stack }
  ;;

  let read_stack_frame =
    read_stack
    >>= function
    | hd :: _, _ -> return hd
    | [], root -> return root
  ;;

  let write_stack_frame new_frame =
    read_stack
    >>= function
    | _ :: tl, root -> write_stack (new_frame :: tl, root)
    | [], _ -> write_stack ([], new_frame)
  ;;

  let add_stack_frame new_frame =
    read_stack
    >>= function
    | stack, root -> write_stack (new_frame :: stack, root)
  ;;

  let delete_stack_frame =
    read_stack
    >>= function
    | stack, root -> write_stack (List.tl stack, root)
  ;;

  (* local env *)

  let read_local_envs =
    read_stack_frame
    >>= function
    | { local_envs } -> return local_envs
  ;;

  let write_local_envs new_local_env =
    read_stack_frame
    >>= function
    | { deferred_funcs; expr_eval } ->
      write_stack_frame { deferred_funcs; expr_eval; local_envs = new_local_env }
  ;;

  let save_local_id ident value =
    let* envs = read_local_envs in
    write_local_envs (MapIdent.add ident value (List.hd envs) :: List.tl envs)
  ;;

  let add_env =
    let* local_envs = read_local_envs in
    write_local_envs (MapIdent.empty :: local_envs)
  ;;

  let delete_env =
    let* local_envs = read_local_envs in
    write_local_envs (List.tl local_envs)
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
    | { local_envs; expr_eval } ->
      write_stack_frame { local_envs; expr_eval; deferred_funcs = new_deferred }
  ;;

  let add_deferred new_frame =
    let* deferred_funcs = read_deferred in
    write_deferred (new_frame :: deferred_funcs)
  ;;

  let delete_deferred =
    let* deferred_funcs = read_deferred in
    write_deferred (List.tl deferred_funcs)
  ;;

  (* expr_eval *)

  let read_expr_eval =
    read_stack_frame
    >>= function
    | { expr_eval } -> return expr_eval
  ;;

  let write_expr_eval new_expr_eval =
    read_stack_frame
    >>= function
    | { local_envs; deferred_funcs } ->
      write_stack_frame { local_envs; deferred_funcs; expr_eval = new_expr_eval }
  ;;

  let push_value new_value =
    let* stack = read_expr_eval in
    write_expr_eval (new_value :: stack)
  ;;

  let pop_value =
    read_expr_eval
    >>= function
    | hd :: tl -> write_expr_eval tl *> return hd
    | [] -> fail (Runtime_error Not_enought_operands)
  ;;
end
