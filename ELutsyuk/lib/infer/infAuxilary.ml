(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open TypTree
open Base
open Ast
open Parse

module FreshResult : sig
  type 'a t

  val return : 'a -> 'a t
  val fail : error -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val fresh : int t
  val run : 'a t -> ('a, error) Result.t

  module SyntSugar : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end
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

  module SyntSugar = struct
    let ( let* ) = ( >>= )
  end
end

module VarSet = struct
  include Stdlib.Set.Make (Int)
end

(* нужна или нет сигнатура *)
module Type : sig
  val has_type_var : int -> typ -> bool
  val type_vars : typ -> VarSet.t
end = struct
  type t = typ

  let rec has_type_var var = function
    | TypConst _ -> false
    | TypVar ty -> ty = var
    | TypArrow (ty1, ty2) -> has_type_var var ty1 || has_type_var var ty2
    | TypTuple ty -> List.exists ty ~f:(has_type_var var)
    | TypList ty -> has_type_var var ty
    | TypOption ty -> has_type_var var ty
  ;;

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
  open FreshResult.SyntSugar

  type t = (var, typ, Int.comparator_witness) Map.t

  let empty : t = Map.empty (module Int)

  let singleton var typ =
    if Type.has_type_var var typ
    then fail (OccursCheckFailed (var, typ))
    else return (Map.singleton (module Int) var typ)
  ;;

  let find sub var = Map.find sub var
  let remove sub var = Map.remove sub var

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

  let rec unify typ1 typ2 : t FreshResult.t =
    match typ1, typ2 with
    | TypConst ty1, TypConst ty2 when Poly.(ty1 = ty2) -> return empty
    | TypVar a, TypVar b when a = b -> return empty
    | TypVar var, typ | typ, TypVar var -> singleton var typ
    | TypArrow (a1, a2), TypArrow (b1, b2) ->
      let* sub_map1 = unify a1 b1 in
      let* sub_map2 = unify a2 b2 in
      compose sub_map1 sub_map2
    | TypTuple ty1, TypTuple ty2 ->
      (match
         List.fold2 ty1 ty2 ~init:(return empty) ~f:(fun acc item1 item2 ->
           let* subst1 = acc in
           let* subst2 = unify (apply subst1 item1) (apply subst1 item2) in
           compose subst1 subst2)
       with
       | Ok res -> res
       | _ -> fail @@ UnificationFailed (typ1, typ2))
    | TypList a, TypList b -> unify a b
    | TypOption a, TypOption b -> unify a b
    | _ -> fail @@ UnificationFailed (typ1, typ2)

  and extend sub_map new_var new_typ =
    match find sub_map new_var with
    | None ->
      let new_typ = apply sub_map new_typ in
      let* new_sub = singleton new_var new_typ in
      Map.fold sub_map ~init:(return new_sub) ~f:(fun ~key:new_var ~data:new_typ acc ->
        let* acc = acc in
        let new_typ = apply new_sub new_typ in
        return (Map.set acc ~key:new_var ~data:new_typ))
    | Some existed_typ ->
      let* new_sub = unify new_typ existed_typ in
      compose sub_map new_sub

  and compose sub_map1 sub_map2 =
    Map.fold sub_map1 ~init:(return sub_map2) ~f:(fun ~key:var ~data:typ acc ->
      let* acc = acc in
      extend acc var typ)
  ;;

  (* Composition of several substitutions *)
  let compose_many_sub sub_list =
    List.fold_left sub_list ~init:(return empty) ~f:(fun acc sub ->
      let* acc = acc in
      compose sub acc)
  ;;
end

type scheme = Scheme of VarSet.t * typ

module Scheme = struct
  let free_in_context = function
    | Scheme (bind_vars, ty) -> VarSet.diff (Type.type_vars ty) bind_vars
  ;;

  let apply sub (Scheme (bind_vars, ty)) =
    let subst = VarSet.fold (fun var sub -> Subst.remove sub var) bind_vars sub in
    Scheme (bind_vars, Subst.apply subst ty)
  ;;
end

module TypeEnv : sig
  type t = (Ast.id, scheme, Base.String.comparator_witness) Base.Map.t

  val empty : (string, 'a, Base.String.comparator_witness) Base.Map.t
  val free_in_context : t -> VarSet.t
  val apply : Subst.t -> ('a, scheme, 'b) Base.Map.t -> ('a, scheme, 'b) Base.Map.t
  val extend : ('a, 'b, 'c) Base.Map.t -> 'a -> 'b -> ('a, 'b, 'c) Base.Map.t
  val find : ('a, 'b, 'c) Base.Map.t -> 'a -> 'b option
end = struct
  type t = (id, scheme, String.comparator_witness) Map.t

  let empty = Map.empty (module String)

  let free_in_context env =
    Map.fold env ~init:VarSet.empty ~f:(fun ~key:_ ~data:sch acc ->
      VarSet.union acc (Scheme.free_in_context sch))
  ;;

  let apply sub env = Map.map env ~f:(Scheme.apply sub)
  let extend env id scheme = Map.update env id ~f:(fun _ -> scheme)
  let find env id = Map.find env id
  let remove = Map.remove
  (* let iteri env foo = Map.iteri env ~f:foo *)
end
