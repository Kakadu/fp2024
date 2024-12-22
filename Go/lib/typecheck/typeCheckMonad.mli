(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast

module Ident : sig
  type t = Ast.ident

  val compare : 'a -> 'a -> int
end

module MapIdent : sig
  type key = Ident.t
  type 'a t = 'a Stdlib__Map.Make(Ident).t

  val empty : 'a t
end

type ctype =
  | Ctype of Ast.type'
  | Ctuple of Ast.type' list (** Used to check multiple returns of a function *)

val equal_ctype : ctype -> ctype -> Ppx_deriving_runtime.bool

(** MapIdent is used to map ident and it's type in global space *)
type global_env = ctype MapIdent.t

(** list of MapIdent is used to map ident and it's type in local space.
    Add MapIdent if you enter in if/for body or func literal and then delete it after checking block of statements
    If we didn't find ident in Map, we will seek it in next 'till we find it or not find it even in global space
    and fail with undefined ident error *)
type local_env = ctype MapIdent.t list

(** List of ctype that stores function return types, used to check returns in nested functions *)
type current_funcs = ctype list

(** Current typechecker state *)
type state = global_env * local_env * current_funcs

module CheckMonad : sig
  type 'a t = (state, 'a) BaseMonad.t

  val return : 'a -> 'a t
  val fail : Errors.error -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( *> ) : 'a t -> 'b t -> 'b t
  val iter : ('a -> unit t) -> 'a list -> unit t
  val iter2 : ('a -> 'b -> unit t) -> 'a list -> 'b list -> unit t
  val map : ('a -> 'b t) -> 'a list -> 'b list t
  val run : 'a t -> state -> state * ('a, Errors.error) Result.t

  (** Saves current func's return type to the state (called when moving into func body) *)
  val write_func : ctype -> unit t

  (** Deletes current func's return type from the state (called when moving out of func body) *)
  val delete_func : unit t

  (** Searches for given ident's type in global env, returns [None] if not found *)
  val read_global_ident : ident -> ctype option t

  (** Saves ident's type to local env in state *)
  val save_local_ident : ident -> ctype -> unit t

  (** Saves ident's type to global env in state *)
  val save_global_ident : ident -> ctype -> unit t

  (** Searches for given ident's type, returns [None] if not found *)
  val retrieve_ident : ident -> ctype t

  (** Returns current func return type. Used to check if it matches exprs in return stmt *)
  val get_func_return_type : ctype t

  (** Add new Map to local_env while entering a new block/anon_func/if body/for body*)
  val write_env : unit t

  (** Remove Map from local_env while leaving block/anon_func/if body/for body*)
  val delete_env : unit t

  (** Pretty print ctype *)
  val print_type : ctype -> string
end
