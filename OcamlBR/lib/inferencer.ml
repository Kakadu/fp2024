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

module Subst : sig
  type t

  val pp : Stdlib.Format.formatter -> t -> unit
  val empty : t
  val singleton : type_var -> ty -> t R.t

  (** Getting value from substitution. May raise [Not_found] *)
  val find_exn : type_var -> t -> ty

  val find : type_var -> t -> ty option
  val apply : t -> ty -> ty
  val unify : ty -> ty -> t R.t

  (** Composition of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : t -> type_var -> t
end = struct
  open R
  open R.Syntax
  open Base

  (* an association list for substitutions *)
  type t = (type_var * ty) list

  let pp ppf subst =
    let open Stdlib.Format in
    fprintf
      ppf
      "[ %a ]"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf ", ")
         (fun ppf (k, v) -> fprintf ppf "%d -> %a" k pp_ty v))
      subst
  ;;

  let empty = []

  (* creates a substitution for variable [v] with type [ty] if no occurence is found *)
  let mapping v ty =
    if Type.occurs_in v ty then fail `Occurs_check else return (v, ty)

  let singleton v ty =
    let* mapping = mapping v ty in
    return [ mapping ]
  ;;

  let find_exn v subst =
    match List.Assoc.find ~equal:Int.equal subst v with
    | Some ty -> ty
    | None -> failwith "Variable not found in substitution"
  ;;

  let find v subst = List.Assoc.find ~equal:Int.equal subst v

  let remove subst v = List.Assoc.remove ~equal:Int.equal subst v

  (* applies a substitution to a type *)
  let apply subst =
    let rec helper = function
      | TVar v as ty ->
        (match find v subst with
         | Some ty' -> helper ty'
         | None -> ty)
      | TArrow (l, r) -> TArrow (helper l, helper r)
      | TTuple (f, s, rest) -> TTuple (helper f, helper s, List.map ~f:helper rest)
      | TList t -> TList (helper t)
      | TPrim _ as ty -> ty
    in
    helper
  ;;

  (* attempts to unify two types [ty1] and [ty2], returning a substitution *)
  let rec unify ty1 ty2 =
    match ty1, ty2 with
    | TPrim l, TPrim r when String.equal l r -> return empty
    | TVar v1, TVar v2 when v1 = v2 -> return empty
    | TVar v, ty | ty, TVar v ->
      if Type.occurs_in v ty then fail `Occurs_check else singleton v ty
    | TArrow (l1, r1), TArrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    (*| TTuple (f1, s1, rest1), TTuple (f2, s2, rest2) when List.length rest1 = List.length rest2 ->
      let* subs1 = unify f1 f2 in
      let* subs2 = unify (apply subs1 s1) (apply subs1 s2) in
    
      let* unified =
        match List.map2 rest1 rest2 ~f:(fun t1 t2 -> unify (apply subs2 t1) (apply subs2 t2)) with
        | Unequal_lengths ->
          fail (`Unification_failed (TTuple (f1, s1, rest1), TTuple (f2, s2, rest2)))
        | Ok res -> return res
      in
      
      List.fold_left unified ~init:(return (compose subs1 subs2)) ~f:(fun acc s ->
        let* s = s in
        let* acc = acc in
        compose acc s) *)
    | TList t1, TList t2 -> unify t1 t2
    | _, _ -> fail (`Unification_failed (ty1, ty2))
  
  (* extends a substitution with a new mapping for variable [v] *)
  and extend subst (v, ty) =
    match find v subst with
    | None ->
      let ty = apply subst ty in
      let* new_subs = singleton v ty in
      RList.fold_left subst ~init:(return new_subs) ~f:(fun acc (k, v_ty) ->
          let v_ty = apply new_subs v_ty in
          let* mapping = mapping k v_ty in
          return (mapping :: acc))
    | Some existing_ty ->
      let* new_subs = unify ty existing_ty in
      compose subst new_subs
    
  and compose s1 s2 = RList.fold_left s2 ~init:(return s1) ~f:extend

  let compose_all subs_list =
    RList.fold_left subs_list ~init:(return empty) ~f:compose
end

module VarSet = struct
  include VarSet

  let fold_left_m f acc set =
    fold
      (fun x acc ->
        let open R.Syntax in
        let* acc = acc in
        f acc x)
      acc
      set
  ;;
end


module Scheme = struct
  type t = scheme

  let occurs_in v = function
    | S (xs, t) -> (not (VarSet.mem v xs)) && Type.occurs_in v t
  ;;

  let free_vars = function
    | S (bs, t) -> VarSet.diff (Type.type_vars t) bs
  ;;

  let apply sub (S (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    S (names, Subst.apply s2 ty)
  ;;
  let pp = pp_scheme
end

module TypeEnv = struct
  open Base

  type t = (string * scheme) list

  let extend e h = h :: e
  let empty = []

  (* returns the set of free variables in a type environment *)
  let free_vars : t -> VarSet.t =
    List.fold_left ~init:VarSet.empty ~f:(fun acc (_, s) ->
      VarSet.union acc (Scheme.free_vars s))
  ;;

  let apply s env = List.Assoc.map env ~f:(Scheme.apply s)

  let pp ppf xs =
    Stdlib.Format.fprintf ppf "{| ";
    List.iter xs ~f:(fun (n, s) ->
      Stdlib.Format.fprintf ppf "%s -> %a; " n pp_scheme s);
    Stdlib.Format.fprintf ppf "|}%!"
  ;;

  (*retrieves a scheme associated with [name] from the environment [xs]*)
  let find_exn name xs = List.Assoc.find_exn ~equal:String.equal xs name
end
