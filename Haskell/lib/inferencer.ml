(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

open Typedtree
open Ast

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

  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t
end = struct
  (* A compositon: State monad after Result monad *)
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
  ;;

  let fail e st = st, Base.Result.fail e
  let return x last = last, Base.Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  module RList = struct
    let fold_left xs ~init ~f =
      Base.List.fold_left xs ~init ~f:(fun acc x ->
        let open Syntax in
        let* acc = acc in
        f acc x)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module Type = struct
  type t = ty

  let rec occurs_in v = function
    | Ty_var b -> b = v
    | Ty_arrow (l, r) -> occurs_in v l || occurs_in v r
    | Ty_prim _ -> false
    | Ty_list ty | Ty_tree ty | Ty_maybe ty -> occurs_in v ty
    | Ty_tuple ty_list -> List.exists (fun ty -> occurs_in v ty) ty_list
  ;;

  let free_vars =
    let rec helper acc = function
      | Ty_var b -> VarSet.add b acc
      | Ty_arrow (l, r) -> helper (helper acc l) r
      | Ty_prim _ -> acc
      | Ty_list ty | Ty_tree ty | Ty_maybe ty -> helper acc ty
      | Ty_tuple ty_list -> List.fold_left (fun acc ty -> helper acc ty) acc ty_list
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val pp : Stdlib.Format.formatter -> t -> unit
  val empty : t
  val singleton : fresh -> ty -> t R.t

  (** Getting value from substitution. May raise [Not_found] *)
  val find_exn : fresh -> t -> ty

  val find : fresh -> t -> ty option
  val apply : t -> ty -> ty
  val unify : ty -> ty -> t R.t

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end = struct
  open R
  open R.Syntax
  open Base

  (* an association list. In real world replace it by a finite map *)
  type t = (fresh * ty) list

  let pp ppf subst =
    let open Stdlib.Format in
    fprintf
      ppf
      "[ %a ]"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf ", ")
         (fun ppf (k, v) -> fprintf ppf "%d -> %a" k Pprint.pp_ty v))
      subst
  ;;

  let empty = []
  let mapping k v = if Type.occurs_in k v then fail `Occurs_check else return (k, v)

  let singleton k v =
    let* mapping = mapping k v in
    return [ mapping ]
  ;;

  let find_exn k xs = List.Assoc.find_exn xs k ~equal:Int.equal
  let find k xs = List.Assoc.find xs k ~equal:Int.equal
  let remove xs k = List.Assoc.remove xs k ~equal:Int.equal

  let apply s =
    let rec helper = function
      | Ty_var b as ty ->
        (match find_exn b s with
         | exception Not_found_s _ -> ty
         | x -> x)
      | Ty_arrow (l, r) -> Ty_arrow (helper l, helper r)
      | Ty_list ty -> Ty_list (helper ty)
      | Ty_tuple ty_list -> Ty_tuple (List.map ty_list ~f:helper)
      | Ty_tree ty -> Ty_tree (helper ty)
      | Ty_maybe ty -> Ty_maybe (helper ty)
      | other -> other
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | Ty_prim l, Ty_prim r when String.equal l r -> return empty
    | Ty_var a, Ty_var b when Int.equal a b -> return empty
    | Ty_var b, t | t, Ty_var b -> singleton b t
    | Ty_arrow (l1, r1), Ty_arrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | Ty_list ty1, Ty_list ty2 -> unify ty1 ty2
    | Ty_tuple ty_list1, Ty_tuple ty_list2
      when List.length ty_list1 = List.length ty_list2 ->
      let rec helper ty_list1 ty_list2 acc =
        match ty_list1, ty_list2 with
        | [], [] -> return acc
        | h1 :: t1, h2 :: t2 ->
          let* subs = unify h1 h2 in
          let* acc' = compose subs acc in
          helper (List.map ~f:(apply subs) t1) (List.map ~f:(apply subs) t2) acc'
        | _, _ -> fail (`Unification_failed (l, r))
      in
      helper ty_list1 ty_list2 empty
    | Ty_tree ty1, Ty_tree ty2 -> unify ty1 ty2
    | Ty_maybe ty1, Ty_maybe ty2 -> unify ty1 ty2
    | _ -> fail (`Unification_failed (l, r))

  and extend s (k, v) =
    match List.Assoc.find s ~equal:Int.equal k with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RList.fold_left s ~init:(return s2) ~f:(fun acc (k, v) ->
        let v = apply s2 v in
        let* mapping = mapping k v in
        return (mapping :: acc))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = RList.fold_left s2 ~init:(return s1) ~f:extend

  let compose_all ss = RList.fold_left ss ~init:(return empty) ~f:compose
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
    | S (bs, t) -> VarSet.diff (Type.free_vars t) bs
  ;;

  let apply sub (S (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    S (names, Subst.apply s2 ty)
  ;;

  (* let pp = Pprint.pp_scheme *)
end

module TypeEnv = struct
  open Base

  type t = (string * scheme) list

  let extend e h = h :: e
  let empty = []

  let free_vars : t -> VarSet.t =
    List.fold_left ~init:VarSet.empty ~f:(fun acc (_, s) ->
      VarSet.union acc (Scheme.free_vars s))
  ;;

  let apply s env = List.Assoc.map env ~f:(Scheme.apply s)
  let find_exn name xs = List.Assoc.find_exn ~equal:String.equal xs name
end

open R
open R.Syntax

let unify = Subst.unify
let fresh_var = fresh >>| fun n -> Ty_var n

let instantiate : scheme -> ty R.t =
  fun (S (bs, t)) ->
  VarSet.fold_left_m
    (fun typ name ->
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return (Subst.apply s typ))
    bs
    (return t)
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
  fun env ty ->
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  S (free, ty)
;;

let lookup_env e xs =
  match Base.List.Assoc.find_exn xs ~equal:String.equal e with
  | (exception Stdlib.Not_found) | (exception Base.Not_found_s _) -> fail (`No_variable e)
  | scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

let check_for_single_type = function
  | [] -> true
  | h :: t ->
    let rec helper acc = function
      | h :: t -> if acc = h then helper acc t else false
      | [] -> true
    in
    helper h t
;;

let are_types_match t = function
  | [] -> true
  | h :: _ ->
    let rec helper h t =
      match h, t with
      | _, Ty_var _ -> true (*TODO: проверять что номер соответствует типу*)
      | TUnit, Ty_prim "()" | TInt, Ty_prim "Int" | TBool, Ty_prim "Bool" -> true
      | MaybeParam tp, Ty_maybe ty | ListParam tp, Ty_list ty | TreeParam tp, Ty_tree ty
        -> helper tp ty
      | TupleParams (tp1, tp2, tp_list), Ty_list ty_list ->
        true (*TODO: заменить в typedtree ty list на (ty, ty, ty list)*)
      | FunctionType (FuncT (tp1, tp2, tp_list)), Ty_arrow (ty1, ty2) ->
        true (*TODO: как то проверить это*)
      | _ -> false
    in
    helper h t
;;

let rec infer_expression env = function
  | Const const ->
    (match const with
     | Int _ -> return (Subst.empty, Ty_prim "Int")
     | Bool _ -> return (Subst.empty, Ty_prim "Bool")
     | Unit -> return (Subst.empty, Ty_prim "()"))
  | Identificator (Ident i) -> lookup_env i env
  (* | TupleBld *)
  (* | EJust | ENothing -> return Ty_maybe *)
  (* | ListBld  *)
  | Binop ((e1, type1), op, (e2, type2)) ->
    (match check_for_single_type type1, check_for_single_type type2 with
     | true, true ->
       let* elem_ty, expr_ty =
         match op with
         | And | Or -> return (Ty_prim "Bool", Ty_prim "Bool")
         | Plus | Minus | Divide | Mod | Multiply | Pow ->
           return (Ty_prim "Int", Ty_prim "Int")
         | _ ->
           let* t = fresh_var in
           return (t, Ty_prim "Bool")
       in
       let* s1, t1 = infer_expression env e1 in
       let* s2, t2 = infer_expression env e2 in
       (match are_types_match t1 type1, are_types_match t2 type2 with
        | true, true ->
          let* s3 = unify t1 elem_ty in
          let* s4 = unify (Subst.apply s1 t2) elem_ty in
          let* s = Subst.compose_all [ s1; s2; s3; s4 ] in
          return (s, expr_ty)
        | _ -> fail `Occurs_check)
     | _ -> fail `Occurs_check)
  | Neg (e, tp_list) ->
    if check_for_single_type tp_list
    then
      let* s, t = infer_expression env e in
      if are_types_match t tp_list
      then
        let* subs1 = unify t (Ty_prim "Int") in
        let* subs2 = Subst.compose s subs1 in
        return (subs2, Ty_prim "Int")
      else fail `Occurs_check
    else fail `Occurs_check
  | IfThenEsle ((c, c_type), (b1, b1_type), (b2, b2_type)) ->
    (match
       ( check_for_single_type c_type
       , check_for_single_type b1_type
       , check_for_single_type b2_type )
     with
     | true, true, true ->
       let* s1, t1 = infer_expression env c in
       let* s2, t2 = infer_expression env b1 in
       let* s3, t3 = infer_expression env b2 in
       (match
          ( are_types_match t1 c_type
          , are_types_match t2 b1_type
          , are_types_match t3 b2_type )
        with
        | true, true, true ->
          let* s4 = unify t1 (Ty_prim "Bool") in
          let* t = fresh_var in
          let* s5 = unify t t2 in
          let* s6 = unify t t3 in
          let* s = Subst.compose_all [ s1; s2; s3; s4; s5; s6 ] in
          return (s, Subst.apply s t2)
        | _ -> fail `Occurs_check)
     | _ -> fail `Occurs_check)
  | BinTreeBld Nul ->
    let* t = fresh_var in
    return (Subst.empty, Ty_tree t)
  (* | BinTreeBld Node () *)
  (* | Case *)
  (* | InnerBindings *)
  | _ -> return (Subst.empty, Ty_prim "")
;;
