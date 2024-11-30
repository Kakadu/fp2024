(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open TypedTree
open TypesPp
open Format

type error =
  [ `Occurs_check
  | `Undef_var of string
  | `Unification_failed of typ * typ
  ]

let pp_error fmt : error -> _ = function
  | `Occurs_check -> fprintf fmt "Occurs check failed"
  | `Undef_var s -> fprintf fmt "Undefined variable '%s'" s
  | `Unification_failed (fst, snd) ->
    fprintf fmt "unification failed on %a and %a" pp_typ fst pp_typ snd
;;

(* for treating result of type inference *)
module R : sig
  (* signature, smth like interface before realization *)
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

  val fresh : int t
  val run : 'a t -> ('a, error) Result.t
end = struct
  (* takes current state, runs smth, outputs new state and success / error *)
  type 'a t = int -> int * ('a, error) Result.t

  (* bind -- if applying new state to first arg is correct, then apply f to
     new argument and new state, else output error and state that caused it *)
  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
  ;;

  (* is called to cover result in fail or ok constructions *)
  let fail e st = st, Base.Result.fail e
  let return x last = last, Base.Result.return x
  let bind x ~f = x >>= f

  (* is called from x, function and state. if applying state to x is correct,
     then output applying f to x in constructor Ok, otherwise output error and
     state that caused it *)
  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  (* for applying f to all elements x of list xs with check that everything is
     correct. If it is, outputs accumulator of all applyings *)
  module RList = struct
    let fold_left xs ~init ~f =
      Base.List.fold_left xs ~init ~f:(fun acc x ->
        let open Syntax in
        let* acc = acc in
        f acc x)
    ;;
  end

  (* takes current state, returns state + 1 *)
  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

(* module with all type methods *)
module Type = struct
  type t = typ

  (* check that type of the arg is not inside of type v.
     Runs during substitution to ensure that there are no cycles*)
  let rec occurs_in v = function
    | Primary _ -> false
    | Type_var b -> b = v
    | Arrow (fst, snd) -> occurs_in v fst || occurs_in v snd
  ;;

  (* collects all type variables *)
  let free_vars =
    let rec helper acc = function
      | Primary _ -> acc
      | Type_var b -> VarSet.add b acc
      | Arrow (fst, snd) -> helper (helper acc fst) snd
    in
    helper VarSet.empty
  ;;
end

(* module of substitution *)
(*
   module Substitution : sig
   type t

   val empty : t
   val singleton : fresh -> typ -> t R.t
   val find : t -> fresh -> typ option
   val remove : t -> fresh -> t
   val apply : t -> typ -> typ
   val unify : typ -> typ -> t R.t
   val compose : t -> t -> t R.t
   val compose_all : t list -> t R.t
   end = struct
   open R
   open R.Syntax

   type t = (fresh, typ, Int.comparator_witness) Map.OrderedType

   let empty = Map.empty (module Int)

   end*)
