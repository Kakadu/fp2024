(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Base
open Forest.Ast
open Forest.TypesTree

module FreshResult : sig
  type 'a t

  val return : 'a -> 'a t
  val fail : error -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val fresh : int t
  val run : 'a t -> ('a, error) Result.t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end = struct
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) monad bind_func state =
    match monad state with
    | state, Result.Error err -> state, Error err
    | state, Result.Ok value -> bind_func value state
  ;;

  let return res state = state, Result.return res
  let fail res state = state, Result.fail res

  (* let bind value f = value >>= f *)
  let fresh state = return state (state + 1)
  let run monad = snd (monad 0)
  let ( let* ) = ( >>= )
end

module VarSet = struct
  include Stdlib.Set.Make (Int)
end

module Type : sig
  val has_type_var : int -> typ -> bool
  val type_vars : typ -> VarSet.t
end = struct
  (* Checks whether the given type contains a specific type variable. *)
  let rec has_type_var var = function
    | TypConst _ -> false
    | TypVar ty -> ty = var
    | TypArrow (ty1, ty2) -> has_type_var var ty1 || has_type_var var ty2
    | TypTuple ty -> List.exists ty ~f:(has_type_var var)
    | TypList ty -> has_type_var var ty
    | TypOption ty -> has_type_var var ty
  ;;

  (* Collects all free type variables occurring in the given type. *)
  let type_vars typ =
    let rec collect_vars acc = function
      | TypConst _ -> acc
      | TypVar var_name -> VarSet.add var_name acc
      | TypArrow (ty1, ty2) -> collect_vars (collect_vars acc ty1) ty2
      | TypTuple ty ->
        let free ty = collect_vars VarSet.empty ty in
        List.fold ty ~init:acc ~f:(fun acc ty -> VarSet.union acc (free ty))
      | TypList ty -> collect_vars acc ty
      | TypOption ty -> collect_vars acc ty
    in
    collect_vars VarSet.empty typ
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : var -> typ -> t FreshResult.t
  val find : t -> var -> typ option
  val remove : t -> var -> t
  val apply : t -> typ -> typ
  val unify : typ -> typ -> t FreshResult.t
  val compose_many_sub : t list -> t FreshResult.t
  val compose : t -> t -> t FreshResult.t
end = struct
  open FreshResult

  type t = (var, typ, Int.comparator_witness) Map.t

  let empty : t = Map.empty (module Int)

  let singleton var typ =
    if Type.has_type_var var typ
    then fail (OccursCheckFailed (var, typ))
    else return (Map.singleton (module Int) var typ)
  ;;

  let find sub var = Map.find sub var
  let remove sub var = Map.remove sub var

  (* Applies a substitutions to a type, recursively replacing all matching type variables. *)
  let apply sub_map =
    let rec upd_typ = function
      | TypConst ty -> constant_typ ty
      | TypVar var_name ->
        (match find sub_map var_name with
         | Some ty -> ty
         | None -> var_typ var_name)
      | TypArrow (ty1, ty2) -> arrow_typ (upd_typ ty1) (upd_typ ty2)
      | TypTuple ty -> tup_typ (List.map ty ~f:upd_typ)
      | TypList ty -> list_typ (upd_typ ty)
      | TypOption ty -> option_typ (upd_typ ty)
    in
    upd_typ
  ;;

  (* Tries to unify two types, returning a substitution that makes them equal, or fails. *)
  let rec unify typ1 typ2 =
    match typ1, typ2 with
    | TypConst ty1, TypConst ty2 when Poly.(ty1 = ty2) -> return empty
    | TypVar a, TypVar b when a = b -> return empty
    | TypVar var, typ | typ, TypVar var -> singleton var typ
    | TypArrow (a1, a2), TypArrow (b1, b2) ->
      let* sub1 = unify a1 b1 in
      let* sub2 = unify a2 b2 in
      compose sub1 sub2
    | TypTuple ty1, TypTuple ty2 ->
      let rec unify_tup sub = function
        | [], [] -> return sub
        | t1 :: ts1, t2 :: ts2 ->
          if List.length ts1 <> List.length ts2
          then fail @@ UnificationFailed (typ1, typ2)
          else
            let* sub1 = unify (apply sub t1) (apply sub t2) in
            let* final_sub = compose sub sub1 in
            unify_tup final_sub (ts1, ts2)
        | _, _ -> fail @@ UnificationFailed (typ1, typ2)
      in
      unify_tup empty (ty1, ty2)
    | TypList a, TypList b -> unify a b
    | TypOption a, TypOption b -> unify a b
    | _ -> fail @@ UnificationFailed (typ1, typ2)

  (* Extends an existing substitution with a new variable binding, applying substitution recursively to maintain consistency. *)
  and extend sub_map new_var new_typ =
    match find sub_map new_var with
    | None ->
      let new_typ = apply sub_map new_typ in
      let* new_sub = singleton new_var new_typ in
      Map.fold sub_map ~init:(return new_sub) ~f:(fun ~key:new_var ~data:new_typ acc ->
        let* acc = acc in
        let new_typ = apply new_sub new_typ in
        return (Map.set acc ~key:new_var ~data:new_typ))
    | Some ty ->
      let* new_sub = unify new_typ ty in
      compose sub_map new_sub

  (* Composes two substitutions, applying the second to the first and merging them. *)
  and compose sub_map1 sub_map2 =
    Map.fold sub_map1 ~init:(return sub_map2) ~f:(fun ~key:var ~data:typ acc ->
      let* acc = acc in
      extend acc var typ)
  ;;

  (* Composition of several substitutions. *)
  let compose_many_sub sub_list =
    List.fold_left sub_list ~init:(return empty) ~f:(fun acc sub ->
      let* acc = acc in
      compose sub acc)
  ;;
end

type scheme = Scheme of VarSet.t * typ

module Scheme = struct
  (* Returns the set of free type variables in a type scheme, excluding the bound ones. *)
  let free = function
    | Scheme (bind_vars, ty) -> VarSet.diff (Type.type_vars ty) bind_vars
  ;;

  (* Applies a substitution to a type scheme, skipping its bound variables. *)
  let apply sub (Scheme (bind_vars, ty)) =
    let subst = VarSet.fold (fun var sub -> Subst.remove sub var) bind_vars sub in
    Scheme (bind_vars, Subst.apply subst ty)
  ;;
end

module TypeEnv : sig
  type t = (id, scheme, String.comparator_witness) Map.t

  val empty : t
  val free : t -> VarSet.t
  val apply : Subst.t -> t -> t
  val extend : t -> id -> scheme -> t
  val find : t -> id -> scheme option
  val remove : t -> id -> t
  val merge_envs : Subst.t -> t -> t -> t
end = struct
  type t = (id, scheme, String.comparator_witness) Map.t

  let empty = Map.empty (module String)

  (* Returns the set of free type variables in a type scheme, excluding the bound variables that are part of the environment. *)
  let free env =
    Map.fold env ~init:VarSet.empty ~f:(fun ~key:_ ~data:sch acc ->
      VarSet.union acc (Scheme.free sch))
  ;;

  let apply sub env = Map.map env ~f:(Scheme.apply sub)
  let extend env id scheme = Map.update env id ~f:(fun _ -> scheme)
  let find env id = Map.find env id
  let remove = Map.remove

  let merge_envs subst acc_env env_pat =
    let acc_env = apply subst acc_env in
    let env_pat = apply subst env_pat in
    Map.fold env_pat ~init:acc_env ~f:(fun ~key ~data acc_env -> extend acc_env key data)
  ;;
end
