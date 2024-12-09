(** Copyright 2024-2027, Ilia Suponev, Chirkov Dmitri *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parser
open Ast
open Typedtree
open Base
open Format
open Typedtree

type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of ty * ty
  ]

let pp_error ppf : error -> _ = function
  | `Occurs_check -> Format.fprintf ppf "Occurs check failed"
  | `No_variable s -> Format.fprintf ppf "Undefined variable '%s'" s
  | `Unification_failed (l, r) ->
    Format.fprintf ppf "unification failed on %a and %a" pp_ty l pp_ty r
;;

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
    val fold
      :  ('k, 'v, 'cmp) Base.Map.t
      -> init:'b t
      -> f:('k -> 'v -> 'b -> 'b t)
      -> 'b t
  end

  (** Creation of a fresh name from internal state *)
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
    | Result.Error x -> last, Result.fail x
    | Result.Ok a -> f a last
  ;;

  let fail e st = st, Base.Result.fail e
  let return x last = last, Base.Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Ok x -> st, Result.return (f x)
    | st, Result.Error e -> st, Result.fail e
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

  module RMap = struct
    let fold xs ~init ~f =
      Map.fold xs ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module Type = struct
  type t = ty

  let rec occurs_in v = function
    | TVar x -> x = v
    | TArrow (l, r) -> occurs_in v l || occurs_in v r
    | TList typ -> occurs_in v typ
    | TTuple t -> Base.List.exists t ~f:(occurs_in v)
    | TPrim _ -> false
  ;;

  let free_vars =
    let rec helper acc = function
      | TVar x -> VarSet.add x acc
      | TArrow (l, r) -> helper (helper acc l) r
      | TList t -> helper acc t
      | TTuple t -> List.fold_left t ~init:acc ~f:helper
      | TPrim _ -> acc
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> ty -> t R.t
  val find : t -> fresh -> ty option
  val remove : t -> fresh -> t
  val apply : t -> ty -> ty
  val unify : ty -> ty -> t R.t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
  (* val pp : Format.formatter -> (ty, ty, Base.Int.comparator_witness) Base.Map.t -> unit *)
end = struct
  open R
  open R.Syntax

  type t = (fresh, ty, Int.comparator_witness) Map.t

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
      | TVar v as ty ->
        (match find s v with
         | Some ty' -> helper ty'
         | None -> ty)
      | TArrow (l, r) -> TArrow (helper l, helper r)
      | TTuple tl -> ty_tuple (List.map ~f:helper tl)
      | TList l -> ty_list (helper l)
      | TPrim _ as ty -> ty
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TPrim l, TPrim r when String.equal l r -> return empty
    | TVar l, TVar r when l = r -> return empty
    | TVar v, t | t, TVar v -> singleton v t
    | TArrow (l1, r1), TArrow (l2, r2) ->
      let* s1 = unify l1 l2 in
      let* s2 = unify (apply s1 r1) (apply s1 r2) in
      compose s1 s2
    | TTuple t1, TTuple t2 ->
      if List.length t1 <> List.length t2
      then fail (`Unification_failed (l, r))
      else
        let* s =
          RList.fold_left (List.zip_exn t1 t2) ~init:(return empty) ~f:(fun acc (l, r) ->
            let* u = unify (apply acc l) (apply acc r) in
            compose acc u)
        in
        return s
    | TList l1, TList l2 -> unify l1 l2
    | _, _ -> fail (`Unification_failed (l, r))

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

  let compose_all ss =
    List.fold_left ss ~init:(return empty) ~f:(fun acc s ->
      let* acc = acc in
      compose acc s)
  ;;
end

module Scheme = struct
  type t = scheme

  let occurs_in v (S (xs, t)) = (not (VarSet.mem v xs)) && Type.occurs_in v t
  let free_vars (S (xs, t)) = VarSet.diff (Type.free_vars t) xs

  let apply s (S (xs, t)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) xs s in
    S (xs, Subst.apply s2 t)
  ;;
end

module TypeEnv = struct
  type t = (identifier, scheme, String.comparator_witness) Map.t

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

open R
open R.Syntax

let fresh_var = fresh >>| fun n -> TVar n

let instantiate : scheme -> ty R.t =
  fun (S (bs, t)) ->
  VarSet.fold
    (fun name typ ->
      let* typ = typ in
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return (Subst.apply s typ))
    bs
    (return t)
;;

let generalize env ty =
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  S (free, ty)
;;
let rec infer_expr env expr =
  match expr with
  | Const (IntLiteral _) ->
    return (ty_int, Subst.empty)
  | Const (BoolLiteral _) ->
    return (ty_bool, Subst.empty)
    | Const (UnitLiteral _) ->
      return (ty_unit, Subst.empty)
  | Variable x ->
    (match TypeEnv.find x env with
     | Some scheme ->
       let* inst = instantiate scheme in
       return (inst, Subst.empty)
     | None -> fail (`No_variable x))
  | Lambda (params, body) ->
    let* param_types =
      RList.fold_left params ~init:(return []) ~f:(fun acc param ->
        let* tv = fresh_var in
        return ((param, tv) :: acc))
    in
    let env_with_params =
      List.fold_left param_types ~init:env ~f:(fun env (param, t) ->
        match param with
        | PVar name -> TypeEnv.extend env name (S (VarSet.empty, t))
        | _ -> env) (**)
    in
    let* (body_type, body_subst) = infer_expr env_with_params body in
    let param_type_list = List.map ~f:snd param_types in
    let lambda_type = List.fold_right param_type_list ~init:body_type ~f:(fun t acc ->
      TArrow (t, acc))
    in
    return (lambda_type, body_subst)
  | _ -> failwith "Not implemented"

  let infer_str_item env = function
  | _ -> failwith "TODO"
;;

let infer p = 
  List.fold_left
  ~f:(fun acc item ->
    let* env = acc in
    let* env = infer_str_item env item in
    return env)
  ~init:(return TypeEnv.empty)
  p

let infer_run p = run (infer p)


