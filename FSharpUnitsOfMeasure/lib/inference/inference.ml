(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Ast

module State = struct
  open Base
  open Result

  type error =
    | Unification_failed of core_type * core_type
    | Unbound_variable of string
    | Occurs_check of string * core_type

  type 'a t = int -> int * ('a, error) Result.t

  let return x state = state, return x
  let fail e state = state, fail e

  let ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t =
    fun m f state ->
    let state, res = m state in
    match res with
    | Ok r -> f r state
    | Error e -> state, Error e
  ;;

  let bind x ~f = x >>= f
  let ( let* ) x f = bind x ~f

  let ( >>| ) : 'a t -> ('a -> 'b) -> 'b t =
    fun m f state ->
    let state, res = m state in
    match res with
    | Ok r -> return (f r) state
    | Error e -> fail e state
  ;;

  module RList = struct
    let fold_left xs ~init ~f =
      List.fold_left xs ~init ~f:(fun acc x ->
        let* acc = acc in
        f acc x)
    ;;

    let fold_right xs ~init ~f =
      List.fold_right xs ~init ~f:(fun x acc ->
        let* acc = acc in
        f x acc)
    ;;
  end

  module RMap = struct
    let fold map ~init ~f =
      Map.fold map ~init ~f:(fun ~key ~data acc ->
        let* acc = acc in
        f key data acc)
    ;;
  end

  let fresh state = state + 1, Ok state
  let run m = snd (m 0)
end

module VarSet = struct
  include Set.Make (String)

  let pp ppf set =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%s; ") set;
    Format.fprintf ppf "]"
  ;;
end

module Type = struct
  let rec occurs tvar = function
  | Type_var t -> t = tvar
  | Type_option t | Type_list t -> occurs tvar t
  | Type_func (t1, t2) -> occurs tvar t1 || occurs tvar t2
  | Type_tuple (t1, t2, trest) ->
    List.exists (occurs tvar) (t1 :: t2 :: trest)
  | _ -> false

  let free_vars =
    let rec helper acc = function
    | Type_var name -> VarSet.add name acc
    | Type_option t | Type_list t -> helper acc t
    | Type_func (t1, t2) -> helper (helper acc t1) t2
    | Type_tuple (t1, t2, trest) ->
      List.fold_left helper acc (t1 :: t2 :: trest)
    | _ -> acc
    in
    helper VarSet.empty
end

module Subst = struct
  open State
  open Base

  let empty = Map.empty (module String)

  let singleton k v =
    if Type.occurs k v then fail (Occurs_check (k, v))
    else return (Map.singleton (module String) k v)

  let remove = Map.remove

  let apply subst =
    let rec helper = function
    | Type_var name ->
      (match Map.find subst name with
      | Some name' -> name'
      | None -> (Type_var name))
    | Type_option t -> Type_option (helper t)
    | Type_list t -> Type_list (helper t)
    | Type_func (t1, t2) -> Type_func (helper t1, helper t2)
    | Type_tuple (t1, t2, trest) ->
      Type_tuple (helper t1, helper t2, List.map ~f:helper trest)
    | t -> t
    in
    helper

  let rec unify l r = match l, r with
  | Type_var a, Type_var b when String.equal a b -> return empty
  | Type_var a, t | t, Type_var a -> singleton a t
  | Type_list t1, Type_list t2 -> unify t1 t2
  | Type_option t1, Type_option t2 -> unify t1 t2
  | Type_tuple (ta1, ta2, tarest), Type_tuple (tb1, tb2, tbrest) ->
    (match
    List.fold2
    (ta1 :: ta2 :: tarest)
    (tb1 :: tb2 :: tbrest)
    ~init: (return empty)
    ~f:(fun acc t1 t2 ->
      let* subst_acc = acc in
      let* subst' = unify (apply subst_acc t1) (apply subst_acc t2) in
      compose subst_acc subst')
    with
    | Ok res -> res
    | _ -> fail (Unification_failed (l, r)))
  | Type_func (ta1, ta2), Type_func (tb1, tb2) ->
    let* subst1 = unify ta1 tb1 in
    let* subst2 = unify (apply subst1 ta2) (apply subst1 tb2) in
    compose subst1 subst2
  | Type_int, Type_int | Type_float, Type_float| Type_bool, Type_bool
  | Type_char, Type_char | Type_string, Type_string | Type_unit, Type_unit -> return empty
  | _ -> fail (Unification_failed (l, r))

    and extend k v subst =
      match Map.find subst k with
      | None ->
        let v' = apply subst v in
        let* subst' = singleton k v' in
        Map.fold subst ~init:(return subst') ~f:(fun ~key ~data acc ->
          let* acc = acc in
          let data' = apply subst' data in
          return (Map.update acc key ~f:(fun _ -> data')))
      | Some old_v ->
        let* subst' = unify v old_v in
        compose subst subst'

    and compose subst1 subst2 = RMap.fold subst2 ~init:(return subst1) ~f:extend

    and compose_all subst_lst = RList.fold_left subst_lst ~init:(return empty) ~f:compose
end
