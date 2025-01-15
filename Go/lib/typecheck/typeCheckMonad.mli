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

type polymorphic_call =
  | Make
  | Print
  | Println
  | Panic
  | Len
  | Close
  | Nil
  | Recover

type ctype =
  | Ctype of Ast.type'
  | Ctuple of Ast.type' list (** Used to check multiple returns of a function *)
  | CgenT of Ast.type'
  | Cpolymorphic of polymorphic_call

val equal_ctype : ctype -> ctype -> Ppx_deriving_runtime.bool

(** list of MapIdent is used to map ident and it's type in local space.
    Add MapIdent if you enter in if/for body or func literal and then delete it after checking block of statements
    If we didn't find ident in Map, we will seek it in next 'till we find it or not find it even in global space
    and fail with undefined ident error *)
type env = ctype MapIdent.t list

(** List of ctype that stores function return types, used to check returns in nested functions *)
type funcs_returns = ctype list

(** Current typechecker state *)
type typecheck_state = env * funcs_returns

module CheckMonad : sig
  (** ['a t] is a typecheker that stores current state (idents and their types, external function return type)
      and the result of typechecking - ['a] (['a] or typecheck error)*)
  type 'a t = (typecheck_state, 'a) BaseMonad.t

  val return : 'a -> 'a t
  val fail : Errors.error -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( *> ) : 'a t -> 'b t -> 'b t
  val iter : ('a -> unit t) -> 'a list -> unit t
  val iter2 : ('a -> 'b -> unit t) -> 'a list -> 'b list -> unit t
  val map : ('a -> 'b t) -> 'a list -> 'b list t
  val run : 'a t -> typecheck_state -> typecheck_state * ('a, Errors.error) Result.t

  (** Saves current func's return type to the state (called when moving into func body) *)
  val save_func : ctype -> unit t

  (** Deletes current func's return type from the state (called when moving out of func body) *)
  val delete_func : unit t

  (** Saves ident's type to env's last map in state *)
  val save_ident : ident -> ctype -> unit t

  (** Searches for given ident's type, fails if it is not found *)
  val retrieve_ident : ident -> ctype t

  (** Returns current func return type. Used to check if it matches exprs in return stmt *)
  val get_func_return_type : ctype t

  (** Add new Map to env while entering a new block/anon_func/if body/for body*)
  val add_env : unit t

  (** Remove Map from env while leaving block/anon_func/if body/for body*)
  val delete_env : unit t
end
