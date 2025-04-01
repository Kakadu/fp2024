(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Ast

module Errors = struct
  type error =
    | Unification_failed of core_type * core_type
    | Unbound_variable of string
end

module State = struct
  open Errors
  open Base
  open Result

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
