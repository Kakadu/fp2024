(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

open Ast

type name = string
type key = int
type fresh = int

module NMap : sig
  (** names map*)
  type 'a t
end

module KMap : sig
  (** keys map *)
  type 'a t
end

type dfs_key = key
type pe_exprs_key = key
type keys = dfs_key option * pe_exprs_key

type 'a bintree =
  | Node of 'a * 'a * 'a
  | Nul

type pattern_ext =
  | P of pe_exprs_key option * pat_ext
  | Lnk of pe_exprs_key

and pat_ext =
  | PEConst of pconst
  | PECons of pattern_ext * pattern_ext
  | PEEnum of pattern_ext list
  | PETuple of pattern_ext * pattern_ext * pattern_ext list
  | PEMaybe of pattern_ext maybe
  | PETree of pattern_ext bintree

type value =
  | VConst of const
  | VMaybe of value maybe
  | VList of value list
  | VTuple of value * value * value list
  | VClosures of keys NMap.t * (pattern * pattern list * bindingbody * binding list) list
  | VTree of value bintree

type lazy_list =
  | IntLL of int * Z.t * int (** (start, step, fin) *)
  | BoolLL of bool (** infinite list of true / infinite list of false*)
  | UnitLL (** infinite list of () *)

type intern_p = pat_ext

type crit_err =
  [ `Typing_err
  | `Not_exh
  | `Div_by_zero
  | `Negative_exponent
  ]

(** partially evaled expressions*)
type pe_expr =
  | V of value
  | ThTree of intern_p
  | ThLeaf of keys NMap.t * expression
  | Link of pe_exprs_key
  | Er of crit_err
  | LazyLst of lazy_list (** invariant: non empty*)

type ident_ext = ident * pe_exprs_key

(** definitions *)
type df_ext =
  | VarsD of pattern_ext * bindingbody * binding list
  | FunDs of ident_ext * (pattern * pattern list * bindingbody * binding list) list

type df =
  | Df of keys NMap.t * df_ext
  | Err of crit_err

type env = df KMap.t * pe_expr KMap.t * keys NMap.t

val eval
  :  env
  -> fresh
  -> binding_list
  -> (env * fresh, crit_err * (env * fresh)) Result.t

val init_env : env
val init_fresh : fresh
