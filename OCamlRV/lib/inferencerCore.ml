(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open AstPrinter
open Base

let fun_type l r = AFun (l, r)
let int_type = AInt
let bool_type = ABool
let string_type = AString
let unit_type = AUnit
let tuple_type t = ATuple t
let list_type t = AList t

type error =
  [ `Occurs_check
  | `Unbound of identifier
  | `Unification_failed of type_annot * type_annot
  | `Not_implemented
  ]

let pp_error ppf : error -> _ =
  let open Stdlib.Format in
  function
  | `Occurs_check -> fprintf ppf "Occurs check failed"
  | `Unbound s -> fprintf ppf "Unbound variable '%s'" s
  | `Unification_failed (l, r) ->
    fprintf ppf "Unification failed on %a and %a" pp_annot l pp_annot r
  | `Not_implemented -> fprintf ppf "Not implemented"
;;

module VarSet = struct
  include Stdlib.Set.Make (Int)
end

module Result : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t

  module RMap : sig
    val fold : ('a, 'b, 'c) Map.t -> init:'d t -> f:('a -> 'b -> 'd -> 'd t) -> 'd t
  end
end = struct
  (** A composition: State monad after Result monad *)
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Result.fail x
    | Result.Ok a -> f a last
  ;;

  let fail e st = st, Result.fail e
  let return x last = last, Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun m f st ->
    match m st with
    | st, Ok x -> st, Result.return (f x)
    | st, Result.Error e -> st, Result.fail e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  let fresh : int t = fun last -> last + 1, Result.return last
  let run m = snd (m 0)

  module RMap = struct
    let fold m ~init ~f =
      Map.fold m ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end
end

module Type = struct
  let rec occurs_in v = function
    | AVar b -> b = v
    | AFun (l, r) -> occurs_in v l || occurs_in v r
    | ATuple tl -> List.exists tl ~f:(occurs_in v)
    | AList t -> occurs_in v t
    | AInt | ABool | AString | AUnit -> false
  ;;

  let free_vars =
    let rec helper acc = function
      | AVar b -> VarSet.add b acc
      | AFun (l, r) -> helper (helper acc l) r
      | ATuple tl -> List.fold_left tl ~init:acc ~f:helper
      | AList t -> helper acc t
      | AInt | ABool | AString | AUnit -> acc
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> type_annot -> t Result.t
  val find : t -> fresh -> type_annot option
  val remove : t -> fresh -> t
  val apply : t -> type_annot -> type_annot
  val unify : type_annot -> type_annot -> t Result.t
  val compose : t -> t -> t Result.t
  val compose_all : t list -> t Result.t
end = struct
  open Result
  open Result.Syntax

  type t = (fresh, type_annot, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)
  let mapping k v = if Type.occurs_in k v then fail `Occurs_check else return (k, v)

  let singleton k v =
    let* k, v = mapping k v in
    return (Map.singleton (module Int) k v)
  ;;

  let find s k = Map.find s k
  let remove s k = Map.remove s k

  let apply s =
    let rec helper = function
      | AInt -> AInt
      | ABool -> ABool
      | AString -> AString
      | AUnit -> AUnit
      | AVar b as ty ->
        (match find s b with
         | None -> ty
         | Some x -> x)
      | AFun (l, r) -> fun_type (helper l) (helper r)
      | AList t -> list_type (helper t)
      | ATuple ts -> tuple_type (List.map ~f:helper ts)
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | AInt, AInt -> return empty
    | ABool, ABool -> return empty
    | AString, AString -> return empty
    | AUnit, AUnit -> return empty
    | AVar a, AVar b when Int.equal a b -> return empty
    | AVar b, t | t, AVar b -> singleton b t
    | AFun (l1, r1), AFun (l2, r2) ->
      let* s1 = unify l1 l2 in
      let* s2 = unify (apply s1 r1) (apply s1 r2) in
      compose s1 s2
    | AList t1, AList t2 -> unify t1 t2
    | ATuple ts1, ATuple ts2 ->
      (match
         List.fold2
           ts1
           ts2
           ~f:(fun acc t1 t2 ->
             let* acc = acc in
             let* s = unify (apply acc t1) (apply acc t2) in
             compose acc s)
           ~init:(return empty)
       with
       | Unequal_lengths -> fail (`Unification_failed (l, r))
       | Ok s -> s)
    | _ -> fail (`Unification_failed (l, r))

  and extend k v s =
    match find s k with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RMap.fold s ~init:(return s2) ~f:(fun k v acc ->
        let v = apply s2 v in
        let* k, v = mapping k v in
        return (Map.update acc k ~f:(fun _ -> v)))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = RMap.fold s2 ~init:(return s1) ~f:extend

  let compose_all =
    List.fold_left ~init:(return empty) ~f:(fun acc s ->
      let* acc = acc in
      compose acc s)
  ;;
end

module Scheme = struct
  type t = S of VarSet.t * type_annot

  let occurs_in v (S (xs, t)) = (not (VarSet.mem v xs)) && Type.occurs_in v t
  let free_vars (S (xs, t)) = VarSet.diff (Type.free_vars t) xs

  let apply s (S (set, tp)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) set s in
    S (set, Subst.apply s2 tp)
  ;;
end

module TypeEnv = struct
  type t = (identifier, Scheme.t, String.comparator_witness) Map.t

  let extend e k v = Map.update e k ~f:(fun _ -> v)
  let remove e k = Map.remove e k
  let empty = Map.empty (module String)

  let free_vars : t -> VarSet.t =
    Map.fold ~init:VarSet.empty ~f:(fun ~key:_ ~data:s acc ->
      VarSet.union acc (Scheme.free_vars s))
  ;;

  let apply s env = Map.map env ~f:(Scheme.apply s)
  let find x env = Map.find env x
end
