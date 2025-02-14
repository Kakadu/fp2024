(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

module type R = sig
  type 'a t
  type error

  val return : 'a -> 'a t
  val fail : error -> 'a t
  val bound_error : error

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end
end

module ExtractIdents (R : R) : sig
  type t

  val extract_names_from_pattern : pattern -> t R.t
  val extract_names_from_patterns : pattern list -> t R.t
  val extract_bind_names_from_let_binds : let_bind list -> t R.t
  val extract_bind_patterns_from_let_binds : let_bind list -> pattern list
  val elements : t -> string list
end = struct
  include Stdlib.Set.Make (String)
  open R
  open R.Syntax

  let union_disjoint s1 s2 =
    let* s1 = s1 in
    let* s2 = s2 in
    if is_empty (inter s1 s2) then return (union s1 s2) else fail bound_error
  ;;

  let union_disjoint_many sets = List.fold ~init:(return empty) ~f:union_disjoint sets

  let rec extract_names_from_pattern =
    let extr = extract_names_from_pattern in
    function
    | PVar (Ident name) -> return (singleton name)
    | PList l -> union_disjoint_many (List.map l ~f:extr)
    | PCons (hd, tl) -> union_disjoint (extr hd) (extr tl)
    | PTuple (fst, snd, rest) ->
      union_disjoint_many (List.map ~f:extr (fst :: snd :: rest))
    | POption (Some p) -> extr p
    | PConstraint (p, _) -> extr p
    | POption None -> return empty
    | Wild -> return empty
    | PConst _ -> return empty
    | PActive (Ident name, _) -> return (singleton name)
  ;;

  let extract_names_from_patterns pats =
    union_disjoint_many (List.map ~f:extract_names_from_pattern pats)
  ;;

  let extract_bind_names_from_let_binds let_binds =
    union_disjoint_many
      (List.map let_binds ~f:(function Let_bind (pat, _, _) ->
         extract_names_from_pattern pat))
  ;;

  let extract_bind_patterns_from_let_binds let_binds =
    List.map let_binds ~f:(function Let_bind (pat, _, _) -> pat)
  ;;
end

module Make = ExtractIdents
