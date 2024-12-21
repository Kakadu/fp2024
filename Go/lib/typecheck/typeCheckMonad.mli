(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

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
  | Ctuple of Ast.type' list

val equal_ctype : ctype -> ctype -> Ppx_deriving_runtime.bool

type global_env = ctype MapIdent.t
type local_env = ctype MapIdent.t list
type current_funcs = ctype list
type type_check = global_env * local_env * current_funcs

module CheckMonad : sig
  val return : 'a -> ('st, 'a) BaseMonad.t
  val fail : Errors.error -> ('st, 'b) BaseMonad.t

  val ( >>= )
    :  ('st, 'a) BaseMonad.t
    -> ('a -> ('st, 'b) BaseMonad.t)
    -> ('st, 'b) BaseMonad.t

  val ( *> ) : ('st, 'a) BaseMonad.t -> ('st, 'b) BaseMonad.t -> ('st, 'b) BaseMonad.t
  val iter : ('a -> ('st, unit) BaseMonad.t) -> 'a list -> ('st, unit) BaseMonad.t

  val iter2
    :  ('a -> 'b -> ('st, unit) BaseMonad.t)
    -> 'a list
    -> 'b list
    -> ('st, unit) BaseMonad.t

  val map : ('a -> ('st, 'b) BaseMonad.t) -> 'a list -> ('st, 'b list) BaseMonad.t
  val run : ('st, 'a) BaseMonad.t -> 'st -> 'st * ('a, Errors.error) Result.t

  type 'a t = (type_check, 'a) BaseMonad.t

  val rpf : ('a * 'b) list -> 'a list
  val rps : ('a * 'b) list -> 'b list
  val seek_local_definition_ident : MapIdent.key -> (type_check, ctype option) BaseMonad.t
  val delete_func : (type_check, unit) BaseMonad.t
  val write_func : 'a -> ('b * 'c * 'a list, unit) BaseMonad.t
  val read_global_ident : MapIdent.key -> (type_check, ctype option) BaseMonad.t
  val save_local_ident : MapIdent.key -> ctype -> (type_check, unit) BaseMonad.t
  val save_global_ident : MapIdent.key -> ctype -> (type_check, unit) BaseMonad.t
  val retrieve_ident : MapIdent.key -> (type_check, ctype) BaseMonad.t
  val get_func_return_type : ctype t
  val write_env : (type_check, unit) BaseMonad.t
  val delete_env : (type_check, unit) BaseMonad.t
end
