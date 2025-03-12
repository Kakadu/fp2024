(** Copyright 2024-2025, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp fmt s =
    Format.fprintf fmt "[ ";
    iter (Format.fprintf fmt "%d; ") s;
    Format.fprintf fmt "]"
  ;;
end

type fresh = int

let binder_to_alpha (b : fresh) = Int.to_string b (* TODO *)

let rec pp_ty fmt = function
  | TPrim s -> Format.fprintf fmt "%s" s
  | TVar v -> Format.fprintf fmt "'%s" (binder_to_alpha v)
  | TArrow (l, r) -> Format.fprintf fmt "(%a -> %a)" pp_ty l pp_ty r
  | TTuple (t1, t2, tl) ->
    Format.fprintf
      fmt
      "(%a * %a%a)"
      pp_ty
      t1
      pp_ty
      t2
      (Format.pp_print_list ~pp_sep:(fun _ _ -> Format.fprintf fmt " * ") pp_ty)
      tl
  | TOption o -> Format.fprintf fmt "%a option" pp_ty o
  | TList l -> Format.fprintf fmt "%a list" pp_ty l
;;

type error =
  | OccursCheck of binder * core_type
  | NoVariable of id
  | UnificationFailed of core_type * core_type
  | InvalidLeftHandSide
  | InvalidRightHandSide

let pp_error fmt = function
  | OccursCheck (v, t) ->
    Format.fprintf fmt "Occurs check: '%s in %a" (binder_to_alpha v) pp_ty t
  | NoVariable s -> Format.fprintf fmt "Undefined variable '%s'" s
  | UnificationFailed (l, r) ->
    Format.fprintf fmt "Unification failed: on %a and %a" pp_ty l pp_ty r
  | InvalidLeftHandSide -> Format.fprintf fmt "Invalid left hand side"
  | InvalidRightHandSide -> Format.fprintf fmt "Invalid right hand side"
;;

module Result : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold
      :  ('k, 'v, 'cmp) Base.Map.t
      -> init:'b t
      -> f:('k -> 'v -> 'b -> 'b t)
      -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
    val fold_right : 'a list -> init:'b t -> f:('a -> 'b -> 'b t) -> 'b t
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

  module RMap = struct
    let fold xs ~init ~f =
      Map.fold xs ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end

  module RList = struct
    let fold_left xs ~init ~f =
      Base.List.fold_left xs ~init ~f:(fun acc x ->
        let open Syntax in
        let* acc = acc in
        f acc x)
    ;;

    let fold_right xs ~init ~f =
      let open Syntax in
      Base.List.fold_right xs ~init ~f:(fun x acc ->
        let* acc = acc in
        f x acc)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

module Type = struct
  let rec occurs_in v = function
    | TPrim _ -> false
    | TVar x -> x = v
    | TArrow (l, r) -> occurs_in v l || occurs_in v r
    | TTuple (t1, t2, tl) ->
      occurs_in v t1 || occurs_in v t2 || Base.List.exists tl ~f:(occurs_in v)
    | TList l -> occurs_in v l
    | TOption typ -> occurs_in v typ
  ;;

  let free_vars =
    let rec helper acc = function
      | TPrim _ -> acc
      | TVar x -> VarSet.add x acc
      | TArrow (l, r) -> helper (helper acc l) r
      | TTuple (t1, t2, tl) ->
        let acc' = helper (helper acc t1) t2 in
        List.fold_left ~f:(fun acc t -> helper acc t) ~init:acc' tl
      | TList t -> helper acc t
      | TOption t -> helper acc t
    in
    helper VarSet.empty
  ;;
end

(* ========== Substitutions ========== *)

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> core_type -> t Result.t
  val find : t -> fresh -> core_type option
  val remove : t -> fresh -> t
  val apply : t -> core_type -> core_type
  val unify : core_type -> core_type -> t Result.t
  val compose : t -> t -> t Result.t
  val compose_all : t list -> t Result.t
end = struct
  open Result
  open Result.Syntax

  type t = (fresh, core_type, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)
  let mapping k v = if Type.occurs_in k v then fail (OccursCheck (k, v)) else return (k, v)

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
      | TArrow (l, r) -> ty_arrow (helper l, helper r)
      | TTuple (t1, t2, tl) -> ty_tuple (helper t1, helper t2, List.map ~f:helper tl)
      | TOption t -> ty_option (helper t)
      | TList t -> ty_list (helper t)
      | TPrim s -> TPrim s
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
    | TList t1, TList t2 -> unify t1 t2
    | TTuple (t11, t12, t1l), TTuple (t21, t22, t2l) ->
      if List.length t1l <> List.length t2l
      then fail (UnificationFailed (l, r))
      else (
        let rec unify_tuples subst types1 types2 =
          match types1, types2 with
          | [], [] -> return subst
          | t1 :: rest1, t2 :: rest2 ->
            let* s2 = unify (apply subst t1) (apply subst t2) in
            let* composed_subst = compose subst s2 in
            unify_tuples composed_subst rest1 rest2
          | _, _ -> fail (UnificationFailed (l, r))
        in
        unify_tuples empty (t11 :: t12 :: t1l) (t21 :: t22 :: t2l))
    | TOption t1, TOption t2 -> unify t1 t2
    | _ -> fail (UnificationFailed (l, r))

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

  and compose_all ss =
    List.fold_left ss ~init:(return empty) ~f:(fun acc s ->
      let* acc = acc in
      compose acc s)
  ;;
end

(* ========== Scheme ========== *)

module Scheme = struct
  type binder_set = VarSet.t [@@deriving show { with_path = false }]
  type t = S of binder_set * core_type [@@deriving show { with_path = false }]

  let occurs_in v (S (xs, t)) = (not (VarSet.mem v xs)) && Type.occurs_in v t
  let free_vars (S (xs, t)) = VarSet.diff (Type.free_vars t) xs

  let apply s (S (xs, t)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) xs s in
    S (xs, Subst.apply s2 t)
  ;;
end

(* ========== Env ========== *)

module TypeEnv = struct
  type t = (id, Scheme.t, String.comparator_witness) Map.t

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

open Result
open Result.Syntax

let fresh_var = fresh >>| fun n -> TVar n

let instantiate : Scheme.t -> core_type Result.t =
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
  Scheme.S (free, ty)
;;

let generalize_rec env ty x =
  let env = TypeEnv.remove env x in
  generalize env ty
;;

let rec infer_pattern env = function
  | PAny ->
    let* fresh = fresh_var in
    return (Subst.empty, env, fresh)
  | PConst c ->
    (match c with
     | CInt _ -> return (Subst.empty, env, ty_int)
     | CBool _ -> return (Subst.empty, env, ty_bool)
     | CUnit -> return (Subst.empty, env, ty_unit))
  | PVar x ->
    let* fresh = fresh_var in
    let env = TypeEnv.extend env x (Scheme.S (VarSet.empty, fresh)) in
    return (Subst.empty, env, fresh)
  | PTuple (t1, t2, tl) ->
    let* s1, env1, t1' = infer_pattern env t1 in
    let* s2, env2, t2' = infer_pattern (TypeEnv.apply s1 env1) t2 in
    let* sub, tl', env =
      RList.fold_right
        ~f:(fun p acc ->
          let sub_prev, acc, env = acc in
          let* sub_cur, env, t = infer_pattern env p in
          let* sub = Subst.compose sub_prev sub_cur in
          return (sub, t :: acc, env))
        ~init:(return (s2, [], env2))
        tl
    in
    return (sub, env, ty_tuple (t1', t2', tl'))
  | PList [] ->
    let* fresh = fresh_var in
    return (Subst.empty, env, ty_list fresh)
  | PList pl ->
    let* fresh_el_type = fresh_var in
    let* final_sub, final_env =
      RList.fold_left
        pl
        ~init:(return (Subst.empty, env))
        ~f:(fun (sub_acc, env_acc) pat ->
          let* sub_cur, env_cur, t = infer_pattern env_acc pat in
          let* unified_sub = Subst.compose sub_acc sub_cur in
          let* final_sub = Subst.unify (Subst.apply sub_cur fresh_el_type) t in
          let combined_sub = Subst.compose unified_sub final_sub in
          let* combined_sub = combined_sub in
          return (combined_sub, TypeEnv.apply final_sub env_cur))
    in
    return (final_sub, final_env, TList (Subst.apply final_sub fresh_el_type))
  | POption (Some p) ->
    let* sub, env1, t1 = infer_pattern env p in
    return (sub, env1, ty_option t1)
  | POption None ->
    let* fresh = fresh_var in
    return (Subst.empty, env, ty_option fresh)
;;

let rec infer_expression env = function
  | EConst c ->
    (match c with
     | CInt _ -> return (Subst.empty, ty_int)
     | CBool _ -> return (Subst.empty, ty_bool)
     | CUnit -> return (Subst.empty, ty_unit))
  | EVar x ->
    (match TypeEnv.find x env with
     | Some s ->
       let* t = instantiate s in
       return (Subst.empty, t)
     | None -> fail (NoVariable x))
  | EUnary (_, e) ->
    let* sub1, t1 = infer_expression env e in
    let* sub2 = Subst.unify (Subst.apply sub1 t1) ty_int in
    let* sub = Subst.compose_all [ sub1; sub2 ] in
    return (sub, Subst.apply sub ty_int)
  | EBinary (op, e1, e2) ->
    let* sub1, t1 = infer_expression env e1 in
    let* sub2, t2 = infer_expression (TypeEnv.apply sub1 env) e2 in
    let* e1t, e2t, et =
      match op with
      | Mul | Div | Add | Sub -> return (ty_int, ty_int, ty_int)
      | Eq | NEq | Lt | Lte | Gt | Gte ->
        let* fresh = fresh_var in
        return (fresh, fresh, ty_bool)
      | And | Or -> return (ty_bool, ty_bool, ty_bool)
    in
    let* sub3 = Subst.unify (Subst.apply sub2 t1) e1t in
    let* sub4 = Subst.unify (Subst.apply sub3 t2) e2t in
    let* sub = Subst.compose_all [ sub1; sub2; sub3; sub4 ] in
    return (sub, Subst.apply sub et)
  | EIf (i, t, e) ->
    let* sub1, t1 = infer_expression env i in
    let* sub2, t2 = infer_expression (TypeEnv.apply sub1 env) t in
    let* sub3, t3 =
      match e with
      | Some e ->
        let* sub3, t3 = infer_expression (TypeEnv.apply sub2 env) e in
        return (sub3, t3)
      | None -> return (Subst.empty, ty_unit)
    in
    let* sub4 = Subst.unify t1 ty_bool in
    let* sub5 = Subst.unify t2 t3 in
    let* sub = Subst.compose_all [ sub1; sub2; sub3; sub4; sub5 ] in
    return (sub, Subst.apply sub t2)
  | ELet (Nonrecursive, b, bl, e) ->
    let bindings = b :: bl in
    let* env2, s1 = infer_nonrec_bs env bindings in
    let* s2, t = infer_expression env2 e in
    let* sub = Subst.compose s1 s2 in
    return (sub, t)
  | ELet (Recursive, b, bl, e) ->
    let bindings = b :: bl in
    let* env2 = infer_rec_bs env bindings in
    let* s, t = infer_expression env2 e in
    return (s, t)
  | EFun (p, e) ->
    let* _, env, t = infer_pattern env p in
    let* sub, t1 = infer_expression env e in
    return (sub, Subst.apply sub (ty_arrow (t, t1)))
  | ETuple (t1, t2, tl) ->
    let* s1, t1 = infer_expression env t1 in
    let* s2, t2 = infer_expression env t2 in
    let* s3, t3 =
      List.fold_right
        ~f:(fun expr acc ->
          let* sacc, tacc = acc in
          let* s, t = infer_expression env expr in
          let* sacc = Subst.compose sacc s in
          return (sacc, t :: tacc))
        ~init:(return (Subst.empty, []))
        tl
    in
    let* composed_sl = Subst.compose_all [ s1; s2; s3 ] in
    let t1 = Subst.apply composed_sl t1 in
    let t2 = Subst.apply composed_sl t2 in
    let t3 = List.map t3 ~f:(fun t -> Subst.apply s3 t) in
    return (s3, ty_tuple (t1, t2, t3))
  | EApply (e1, e2) ->
    let* fresh = fresh_var in
    let* s1, t1 = infer_expression env e1 in
    let* s2, t2 = infer_expression (TypeEnv.apply s1 env) e2 in
    let* s3 = Subst.unify (ty_arrow (t2, fresh)) (Subst.apply s2 t1) in
    let* sub = Subst.compose_all [ s1; s2; s3 ] in
    let t = Subst.apply sub fresh in
    return (sub, t)
  | EList [] ->
    let* fresh = fresh_var in
    return (Subst.empty, ty_list fresh)
  | EList (l :: ls) ->
    (match l :: ls with
     | [] ->
       let* fresh = fresh_var in
       return (Subst.empty, ty_list fresh)
     | h :: tl ->
       let* sr, tr =
         List.fold_left tl ~init:(infer_expression env h) ~f:(fun acc e ->
           let* sub, t = acc in
           let* s1, t1 = infer_expression env e in
           let* s2 = Subst.unify t t1 in
           let* final_s = Subst.compose_all [ sub; s1; s2 ] in
           let final_t = Subst.apply final_s t in
           return (final_s, final_t))
       in
       return (sr, ty_list tr))
  | EOption (Some eo) ->
    let* s, t = infer_expression env eo in
    return (s, ty_option t)
  | EOption None ->
    let* t = fresh_var in
    return (Subst.empty, ty_option t)
  | EType (e, t) ->
    let* s1, t1 = infer_expression env e in
    let* s2 = Subst.unify t1 t in
    let* s1 = Subst.compose s2 s1 in
    return (s1, t1)

and infer_nonrec_bs env bl =
  let* env2, sub2 =
    Base.List.fold_left
      ~f:(fun acc b ->
        let* env, sub = acc in
        let p, e = b in
        let* s, t = infer_expression env e in
        let* sub = Subst.compose s sub in
        let env = TypeEnv.apply sub env in
        let* env, sub =
          match p with
          | PVar x -> return (TypeEnv.extend env x (generalize env t), sub)
          | _ ->
            let* _, env, t' = infer_pattern env p in
            let* sub' = Subst.unify t' t in
            let* sub = Subst.compose_all [ sub'; sub ] in
            return (TypeEnv.apply sub env, sub)
        in
        return (env, sub))
      ~init:(return (env, Subst.empty))
      bl
  in
  return (env2, sub2)

and infer_rec_bs env bl =
  let* env0 =
    Base.List.fold_left
      ~f:(fun env b ->
        let* env = env in
        let p, _ = b in
        match p with
        | PVar x ->
          let* fresh = fresh_var in
          let sc = Scheme.S (VarSet.empty, fresh) in
          let env = TypeEnv.extend env x sc in
          return env
        | _ -> fail InvalidLeftHandSide)
      ~init:(return env)
      bl
  in
  let* env2 =
    Base.List.fold_left
      ~f:(fun env b ->
        let* env = env in
        let p, e = b in
        match p with
        | PVar x ->
          let* fresh = fresh_var in
          let sc = Scheme.S (VarSet.empty, fresh) in
          let env = TypeEnv.extend env x sc in
          let* s1, t1 = infer_expression env e in
          (match t1 with
           | TArrow _ ->
             let* s2 = Subst.unify t1 fresh in
             let* s3 = Subst.compose s1 s2 in
             let env = TypeEnv.apply s3 env in
             let t2 = Subst.apply s3 t1 in
             let sc = generalize_rec env t2 x in
             let env = TypeEnv.extend env x sc in
             return env
           | _ -> fail InvalidRightHandSide)
        | _ -> fail InvalidLeftHandSide)
      ~init:(return env0)
      bl
  in
  return env2
;;

let infer_structure_item env = function
  | SValue (Nonrecursive, b, bl) ->
    let bindings = b :: bl in
    let* env, _ = infer_nonrec_bs env bindings in
    return env
  | SValue (Recursive, b, bl) ->
    let bindings = b :: bl in
    infer_rec_bs env bindings
;;

let infer_program p =
  let env =
    TypeEnv.extend
      TypeEnv.empty
      "print_int"
      (Scheme.S (VarSet.empty, ty_arrow (ty_int, ty_unit)))
  in
  List.fold_left
    ~f:(fun acc item ->
      let* env = acc in
      let* env = infer_structure_item env item in
      return env)
    ~init:(return env)
    p
;;

(* ========== print ========== *)

let print_env env =
  Base.Map.iteri env ~f:(fun ~key ~data:(Scheme.S (_, ty)) ->
    if String.equal key "print_int"
    then ()
    else Stdlib.Format.printf "val %s : %a\n" key pp_ty ty)
;;

(* inference *)

let inference p = run (infer_program p)
