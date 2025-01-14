(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast

module Ident = struct
  type t = ident

  let compare = compare
end

module MapIdent = Map.Make (Ident)

type polymorphic_call =
  | Make
  | Print
  | Println
  | Panic
  | Len
  | Close
  | Nil
  | Recover
[@@deriving show { with_path = false }, eq]

type ctype =
  | Ctype of type'
  | Ctuple of type' list
  | CgenT of type'
  | Cpolymorphic of polymorphic_call
[@@deriving show { with_path = false }, eq]

type env = ctype MapIdent.t list
type funcs_returns = ctype list
type typecheck_state = env * funcs_returns

module CheckMonad = struct
  open Errors
  include BaseMonad

  type 'a t = (typecheck_state, 'a) BaseMonad.t

  let read_env =
    read
    >>= function
    | env, _ -> return env
  ;;

  let write_env new_env =
    read
    >>= function
    | _, funcs -> write (new_env, funcs)
  ;;

  let save_func func =
    read
    >>= function
    | env, funcs -> write (env, func :: funcs)
  ;;

  let delete_func =
    read
    >>= function
    | env, funcs -> write (env, List.tl funcs)
  ;;

  let save_ident ident t =
    let* env = read_env in
    match MapIdent.find_opt ident (List.hd env) with
    | None -> write_env (MapIdent.add ident t (List.hd env) :: List.tl env)
    | Some _ ->
      fail
        (Type_check_error (Multiple_declaration (Printf.sprintf "%s is redeclared" ident)))
  ;;

  let read_ident ident =
    let* env = read_env in
    match List.find_opt (MapIdent.mem ident) env with
    | None -> return None
    | Some map -> return (MapIdent.find_opt ident map)
  ;;

  let retrieve_ident ident =
    read_ident ident
    >>= function
    | Some t -> return t
    | None ->
      fail (Type_check_error (Undefined_ident (Printf.sprintf "%s is not defined" ident)))
  ;;

  let get_func_return_type =
    read
    >>= function
    | _, [] ->
      fail
        (Type_check_error
           (Mismatched_types "this func has no returns but they was requested"))
    | _, funcs -> return (List.hd funcs)
  ;;

  let add_env = read_env >>= fun x -> write_env (MapIdent.empty :: x)
  let delete_env = read_env >>= fun x -> write_env (List.tl x)
end
