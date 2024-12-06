(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Template: https://gitlab.com/Kakadu/fp2020course-materials/-/tree/master/code/miniml?ref_type=heads*)

open Typing

module R : sig
  type 'a t

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
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun monad f state ->
    let final_state, result = monad state in
    match result with
    | Error err -> final_state, Error err
    | Ok v -> f v final_state
  ;;

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun v f state ->
    match v state with
    | state, Ok v -> state, Ok (f v)
    | state, Error err -> state, Error err
  ;;

  let return v last = last, Base.Result.return v
  let bind v ~f = v >>= f
  let fail error state = state, Base.Result.fail error

  module Syntax = struct
    let ( let* ) v f = bind v ~f
  end

  module RMap = struct
    let fold_left map ~init ~f =
      Base.Map.fold map ~init ~f:(fun ~key ~data acc -> acc >>= fun acc -> f key data acc)
    ;;
  end

  module RList = struct
    let fold_left lt ~init ~f =
      Base.List.fold_left lt ~init ~f:(fun acc item -> acc >>= fun acc -> f acc item)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

module Type = struct
  let rec occurs_in v = function
    | TyVar x -> x = v
    | TyArrow (l, r) -> occurs_in v l || occurs_in v r
    | TyList ty -> occurs_in v ty
    | TyTuple ty_list -> List.exists (occurs_in v) ty_list
    | TyOption ty -> occurs_in v ty
    | TyPrim _ -> false
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : int -> ty -> t R.t
  val remove : t -> int -> t
  val apply : t -> ty -> ty
  val unify : ty -> ty -> t R.t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
end = struct
  open R
  open R.Syntax

  type t = (int, ty, Base.Int.comparator_witness) Base.Map.t

  let empty = Base.Map.empty (module Base.Int)

  let singleton key v =
    if Type.occurs_in key v
    then fail (OccursCheck (key, v))
    else return (Base.Map.singleton (module Base.Int) key v)
  ;;

  let find sub key = Base.Map.find sub key
  let remove sub key = Base.Map.remove sub key

  let apply sub =
    let rec apply_helper = function
      | TyVar b as ty ->
        (match find sub b with
         | Some x -> x
         | None -> ty)
      | TyArrow (l, r) -> TyArrow (apply_helper l, apply_helper r)
      | TyList ty -> TyList (apply_helper ty)
      | TyTuple t_list -> TyTuple (List.map apply_helper t_list)
      | TyOption ty -> TyOption (apply_helper ty)
      | other -> other
    in
    apply_helper
  ;;

  let rec unify l r =
    match l, r with
    | TyPrim l, TyPrim r when l = r -> return empty
    | TyPrim _, TyPrim _ -> fail (UnificationFailed (l, r))
    | TyVar l, TyVar r when l = r -> return empty
    | TyVar b, t | t, TyVar b -> singleton b t
    | TyArrow (l1, r1), TyArrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | TyTuple ts1, TyTuple ts2 ->
      if List.length ts1 <> List.length ts2
      then fail (UnificationFailed (l, r))
      else (
        let rec unify_tuples subs ts1 ts2 =
          match ts1, ts2 with
          | [], [] -> return subs
          | t1 :: rest1, t2 :: rest2 ->
            let* subs' = unify (apply subs t1) (apply subs t2) in
            let* composed_subs = compose subs subs' in
            unify_tuples composed_subs rest1 rest2
          | _, _ -> failwith "This should not happen"
        in
        unify_tuples empty ts1 ts2)
    | TyList t1, TyList t2 -> unify t1 t2
    | TyOption t1, TyOption t2 -> unify t1 t2
    | _ -> fail (UnificationFailed (l, r))

  and extend k v s =
    match find s k with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      Base.Map.fold s ~init:(return s2) ~f:(fun ~key ~data acc ->
        let* acc = acc in
        let new_data = apply s2 data in
        return (Base.Map.update acc key ~f:(fun _ -> new_data)))
    | Some value ->
      let* s2 = unify v value in
      compose s s2

  and compose s1 s2 = RMap.fold_left s2 ~init:(return s1) ~f:extend

  let compose_all s_list = RList.fold_left s_list ~init:(return empty) ~f:compose
end

module Scheme = struct
  let apply s (Scheme (bind_vars, ty)) =
    let s' = VarSet.fold (fun s k -> Subst.remove k s) bind_vars s in
    Scheme (bind_vars, Subst.apply s' ty)
  ;;
end

module TypeEnv = struct
  type t = (string, scheme, Base.String.comparator_witness) Base.Map.t

  let empty : t = Base.Map.empty (module Base.String)

  let extend : t -> string -> scheme -> t =
    fun env k scheme -> Base.Map.update env k ~f:(fun _ -> scheme)
  ;;

  let apply env s = Base.Map.map env ~f:(Scheme.apply s)
  let find env k = Base.Map.find env k
end

open R
open R.Syntax

let fresh_var = fresh >>| fun n -> TyVar n

let instantiate (Scheme (bind_vars, ty)) =
  let fold_acc var_name acc =
    let* acc = acc in
    let* fv = fresh_var in
    let* sub = Subst.singleton var_name fv in
    return (Subst.apply sub acc)
  in
  VarSet.fold fold_acc bind_vars (return ty)
;;

open Ast

let infer_const = function
  | ConstInt _ -> TyPrim "int"
  | ConstBool _ -> TyPrim "bool"
  | ConstString _ -> TyPrim "string"
;;

let rec infer_pattern env pattern =
  match pattern with
  | PatAny -> fresh_var >>| fun fv -> Subst.empty, fv, env
  | PatConst c ->
    let fv = infer_const c in
    return (Subst.empty, fv, env)
  | PatVariable x ->
    fresh_var
    >>| fun fv ->
    let env' = TypeEnv.extend env x (Scheme (VarSet.empty, fv)) in
    Subst.empty, fv, env'
  | PatTuple (p1, p2, ps) ->
    let patterns = p1 :: p2 :: ps in
    let rec process_patterns patterns subst_acc types_acc env_acc =
      match patterns with
      | [] -> return (subst_acc, types_acc, env_acc)
      | pattern :: rest ->
        let* subst, ty, env' = infer_pattern env_acc pattern in
        let* new_subst = Subst.compose subst_acc subst in
        process_patterns rest new_subst (types_acc @ [ ty ]) env'
    in
    let* subst, types, new_env = process_patterns patterns Subst.empty [] env in
    return (subst, TyTuple types, new_env)
;;

let infer_binop_type = function
  | Equal | NotEqual | GreaterThan | GretestEqual | LowerThan | LowestEqual ->
    fresh_var >>| fun fv -> fv, TyPrim "bool"
  | Plus | Minus | Multiply | Division -> return (TyPrim "int", TyPrim "int")
  | And | Or -> return (TyPrim "bool", TyPrim "bool")
;;

let pattern_to_string = function
  | PatVariable name -> name
  | _ -> failwith "Unsupported pattern: only variables are supported in the environment"
;;

let infer_expr =
  let rec helper env = function
    | ExpConst x -> return (Subst.empty, infer_const x)
    | ExpIdent x ->
      (match TypeEnv.find env x with
       | Some scheme ->
         let* ans = instantiate scheme in
         return (Subst.empty, ans)
       | None -> fail @@ NoVariable x)
    | ExpBinOper (op, e1, e2) ->
      let* args_type, expr_type = infer_binop_type op in
      let* sub_l, ty1 = helper env e1 in
      let* sub_r, ty2 = helper env e2 in
      let* sub1 = Subst.unify ty1 args_type in
      let* sub2 = Subst.unify (Subst.apply sub1 ty2) args_type in
      let* sub = Subst.compose_all [ sub_l; sub_r; sub1; sub2 ] in
      return (sub, expr_type)
    | ExpFunction (e1, e2) ->
      let* sub1, ty1 = helper env e1 in
      let* sub2, ty2 = helper (TypeEnv.apply env sub1) e2 in
      let* fv = fresh_var in
      let* sub3 = Subst.unify (Subst.apply sub2 ty1) (TyArrow (ty2, fv)) in
      let ty = Subst.apply sub3 fv in
      let* sub = Subst.compose_all [ sub1; sub2; sub3 ] in
      return (sub, ty)
    | ExpLambda (patterns, body_expr) ->
      let infer_patterns env patterns =
        let folder acc pattern =
          let* acc_subst, acc_env, acc_types = acc in
          let* subst, ty, new_env = infer_pattern acc_env pattern in
          let* subst = Subst.compose acc_subst subst in
          return (subst, new_env, acc_types @ [ ty ])
        in
        Base.List.fold patterns ~init:(return (Subst.empty, env, [])) ~f:folder
      in
      let* subst, env', param_types = infer_patterns env patterns in
      let* body_subst, body_type = helper env' body_expr in
      let* total_subst = Subst.compose subst body_subst in
      let tarrow l r = TyArrow (l, r) in
      let function_type = Base.List.fold_right param_types ~init:body_type ~f:tarrow in
      return (total_subst, function_type)
    | ExpBranch (e1, e2, e3) ->
      let* sub1, t1 = helper env e1 in
      let* sub2, t2 = helper env e2 in
      let* sub3, t3 =
        match e3 with
        | Some expr -> helper env expr
        | None -> failwith "Expected an expression but got None"
      in
      let* sub_cond = Subst.unify t1 (TyPrim "bool") in
      let* sub_t = Subst.unify t2 t3 in
      let* sub = Subst.compose_all [ sub1; sub2; sub3; sub_cond; sub_t ] in
      return (sub, Subst.apply sub t2)
    | ExpOption opt_expr ->
      (match opt_expr with
       | Some expr ->
         let* sub, ty = helper env expr in
         return (sub, TyOption ty)
       | None ->
         let* fresh_ty = fresh_var in
         return (Subst.empty, TyOption fresh_ty))
    | ExpList elements ->
      let infer_elements env elements =
        let folder acc expr =
          let* acc_subst, acc_types = acc in
          let* sub, ty = helper (TypeEnv.apply env acc_subst) expr in
          let* total_sub = Subst.compose acc_subst sub in
          return (total_sub, acc_types @ [ ty ])
        in
        Base.List.fold elements ~init:(return (Subst.empty, [])) ~f:folder
      in
      let* subst, types = infer_elements env elements in
      let unify_types types =
        match types with
        | [] ->
          let* fresh = fresh_var in
          return (subst, TyList fresh)
        | hd :: tl ->
          let* sub, ty =
            Base.List.fold
              tl
              ~init:(return (Subst.empty, hd))
              ~f:(fun acc ty ->
                let* sub_acc, expected_ty = acc in
                let* sub = Subst.unify ty expected_ty in
                let* total_sub = Subst.compose sub_acc sub in
                return (total_sub, Subst.apply total_sub expected_ty))
          in
          return (sub, TyList ty)
      in
      unify_types types
    | ExpTuple (e1, e2, rest) ->
      let* sub1, t1 = helper env e1 in
      let* sub2, t2 = helper (TypeEnv.apply env sub1) e2 in
      let infer_rest env subs types expr =
        let* sub, ty = helper env expr in
        let* total_sub = Subst.compose subs sub in
        return (total_sub, types @ [ ty ])
      in
      let* sub_rest, types_rest =
        Base.List.fold
          rest
          ~init:(return (sub2, [ t1; t2 ]))
          ~f:(fun acc expr ->
            let* subs, types = acc in
            infer_rest (TypeEnv.apply env subs) subs types expr)
      in
      let tuple_type = TyTuple types_rest in
      return (sub_rest, tuple_type)
    | ExpLet (false, _, e1, None) -> helper env e1
    | ExpLet (true, x, e1, None) ->
      let* fv = fresh_var in
      let pattern_name = pattern_to_string x in
      let env = TypeEnv.extend env pattern_name (Scheme (VarSet.empty, fv)) in
      let* sub1, ty1 = helper env e1 in
      let* sub2 = Subst.unify (Subst.apply sub1 fv) ty1 in
      let* sub = Subst.compose sub1 sub2 in
      return (sub, Subst.apply sub fv)
    | _ -> return (Subst.empty, TyPrim "int")
  in
  helper
;;

let run_inference expr = Result.map snd (run (infer_expr TypeEnv.empty expr))
