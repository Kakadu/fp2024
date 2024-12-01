(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open Typedtree

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  ('a, 'b, 'c) Base.Map.t
      -> init:'d t
      -> f:('a -> 'b -> 'd -> 'd t)
      -> 'd t
  end

  val fresh : int t
  val run : 'a t -> ('a, error) Result.t
end = struct
  type 'a t =
    int -> int * ('a, error) Result.t (* a composition of result and state monad *)

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f state ->
    let last, res = m state in
    match res with
    | Result.Error x ->
      last, Error x (* if the first computation (m) fails, propagate the error *)
    | Result.Ok a ->
      f a last (* if it succeeds, pass the result (a) to the next computation (f) *)
  ;;

  (* wraps a value x into the monad without modifying the state. It returns the current state (last) and a successful result (Ok x) *)
  let return x last = last, Base.Result.return x

  (* creates a failed monadic computation, propagating the error (e) while leaving the state unchanged *)
  let fail e st = st, Base.Result.fail e
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Result.Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
  ;;

  (* syntatic sugar *)
  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  (* defines a monadic version of List.fold_left, can handle computations that may fail *)
  (* e.g. for solving multiple type constraints during inference *)
  module RList = struct
    let fold_left xs ~init ~f =
      Base.List.fold_left xs ~init ~f:(fun acc x ->
        let open Syntax in
        let* acc = acc in
        f acc x)
    ;;
  end

  module RMap = struct
    let fold_left map ~init ~f =
      Base.Map.fold map ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end

  (* generates new state for representing a new fresh variable *)
  let fresh last = last + 1, Result.Ok last

  (* runs from initial state 0 and extracts the result from the monadic computation *)
  let run monad = snd (monad 0)
end

module Type = struct
  type t = ty

  (* checks if a type variable v occurs anywhere within a given type; primarily used during type unification *)
  let rec occurs_in v = function
    | TVar b -> b = v
    | TPrim _ -> false
    | TArrow (l, r) -> occurs_in v l || occurs_in v r
    | TTuple (fst, snd, rest) ->
      occurs_in v fst || occurs_in v snd || List.exists (occurs_in v) rest
    | TList t -> occurs_in v t
  ;;

  (* computes the set of all type variables in a given type; primarily used to generalize types during type inference *)
  let type_vars =
    let rec helper acc = function
      | TVar b -> VarSet.add b acc
      | TPrim _ -> acc
      | TArrow (l, r) -> helper (helper acc l) r
      | TList t -> helper acc t
      | TTuple (fst, snd, rest) -> List.fold_left helper acc (fst :: snd :: rest)
    in
    helper VarSet.empty
  ;;
end
