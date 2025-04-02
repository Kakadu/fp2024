(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Ast

module State = struct
  open Base
  open Result

  type error =
    | Unification_failed of core_type * core_type
    | Unbound_variable of string
    | Occurs_check of string * core_type
    | Measure_not_implemented
    | TODO

  type 'a t = int -> int * ('a, error) Result.t

  let return x state = state, return x
  let fail e state = state, fail e

  let ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t =
    fun m f state ->
    let state, res = m state in
    match res with
    | Ok r -> f r state
    | Error e -> state, Error e
  ;;

  let bind x ~f = x >>= f
  let ( let* ) x f = bind x ~f

  let ( >>| ) : 'a t -> ('a -> 'b) -> 'b t =
    fun m f state ->
    let state, res = m state in
    match res with
    | Ok r -> return (f r) state
    | Error e -> fail e state
  ;;

  module RList = struct
    let fold_left xs ~init ~f =
      List.fold_left xs ~init ~f:(fun acc x ->
        let* acc = acc in
        f acc x)
    ;;

    let fold_right xs ~init ~f =
      List.fold_right xs ~init ~f:(fun x acc ->
        let* acc = acc in
        f x acc)
    ;;
  end

  module RMap = struct
    let fold map ~init ~f =
      Map.fold map ~init ~f:(fun ~key ~data acc ->
        let* acc = acc in
        f key data acc)
    ;;
  end

  let fresh state = state + 1, Ok state
  let run m = snd (m 0)
end

module VarSet = struct
  include Set.Make (String)

  let pp ppf set =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%s; ") set;
    Format.fprintf ppf "]"
  ;;
end

module Type = struct
  let rec occurs tvar = function
  | Type_var ty -> ty = tvar
  | Type_option ty | Type_list ty -> occurs tvar ty
  | Type_func (ty1, ty2) -> occurs tvar ty1 || occurs tvar ty2
  | Type_tuple (ty1, ty2, tyrest) ->
    List.exists (occurs tvar) (ty1 :: ty2 :: tyrest)
  | _ -> false

  let free_vars =
    let rec helper acc = function
    | Type_var name -> VarSet.add name acc
    | Type_option ty | Type_list ty -> helper acc ty
    | Type_func (ty1, ty2) -> helper (helper acc ty1) ty2
    | Type_tuple (ty1, ty2, tyrest) ->
      List.fold_left helper acc (ty1 :: ty2 :: tyrest)
    | _ -> acc
    in
    helper VarSet.empty
end

module Subst = struct
  open State
  open Base

  let empty = Map.empty (module String)

  let singleton k v =
    if Type.occurs k v then fail (Occurs_check (k, v))
    else return (Map.singleton (module String) k v)

  let remove = Map.remove

  let apply subst =
    let rec helper = function
    | Type_var name ->
      (match Map.find subst name with
      | Some name' -> name'
      | None -> (Type_var name))
    | Type_option ty -> Type_option (helper ty)
    | Type_list ty -> Type_list (helper ty)
    | Type_func (ty1, ty2) -> Type_func (helper ty1, helper ty2)
    | Type_tuple (ty1, ty2, tyrest) ->
      Type_tuple (helper ty1, helper ty2, List.map ~f:helper tyrest)
    | ty -> ty
    in
    helper

  let rec unify l r = match l, r with
  | Type_var a, Type_var b when String.equal a b -> return empty
  | Type_var a, ty | ty, Type_var a -> singleton a ty
  | Type_list ty1, Type_list ty2 -> unify ty1 ty2
  | Type_option ty1, Type_option ty2 -> unify ty1 ty2
  | Type_tuple (ta1, ta2, tarest), Type_tuple (tb1, tb2, tbrest) ->
    (match
    List.fold2
    (ta1 :: ta2 :: tarest)
    (tb1 :: tb2 :: tbrest)
    ~init: (return empty)
    ~f:(fun acc ty1 ty2 ->
      let* subst_acc = acc in
      let* subst' = unify (apply subst_acc ty1) (apply subst_acc ty2) in
      compose subst_acc subst')
    with
    | Ok res -> res
    | _ -> fail (Unification_failed (l, r)))
  | Type_func (ta1, ta2), Type_func (tb1, tb2) ->
    let* subst1 = unify ta1 tb1 in
    let* subst2 = unify (apply subst1 ta2) (apply subst1 tb2) in
    compose subst1 subst2
  | Type_int, Type_int | Type_float, Type_float| Type_bool, Type_bool
  | Type_char, Type_char | Type_string, Type_string | Type_unit, Type_unit -> return empty
  | _ -> fail (Unification_failed (l, r))

    and extend k v subst =
      match Map.find subst k with
      | None ->
        let v' = apply subst v in
        let* subst' = singleton k v' in
        Map.fold subst ~init:(return subst') ~f:(fun ~key ~data acc ->
          let* acc = acc in
          let data' = apply subst' data in
          return (Map.update acc key ~f:(fun _ -> data')))
      | Some old_v ->
        let* subst' = unify v old_v in
        compose subst subst'

    and compose subst1 subst2 = RMap.fold subst2 ~init:(return subst1) ~f:extend

    and compose_all subst_lst = RList.fold_left subst_lst ~init:(return empty) ~f:compose
end

module Scheme = struct
  type scheme = Scheme of VarSet.t * core_type
  
  let free_vars (Scheme (binds, ty)) = VarSet.diff (Type.free_vars ty) binds

  let apply subst (Scheme (binds, ty)) =
    let subst' = VarSet.fold (fun key sub -> Subst.remove sub key) binds subst in
    Scheme (binds, Subst.apply subst' ty)
end

module TypeEnv = struct
  open Base
  open Scheme
  open Subst

  type t = (string, scheme, String.comparator_witness) Map.t

  let empty = Map.empty (module String)
  let extend env name scheme = Map.set env ~key:name ~data:scheme

  let free_vars (env : t) : VarSet.t = Map.fold env ~init:VarSet.empty ~f:(fun ~key:_ ~data acc ->
    VarSet.union acc (Scheme.free_vars data))

  let apply subst env = Map.map env ~f:(Scheme.apply subst)

  let find name env = Map.find env name
end

module Infer = struct
  open Ast
  open State
  open Scheme
  open TypeEnv
  open VarSet

  let unify = Subst.unify
  let fresh_var = fresh >>| fun n -> Type_var (Int.to_string n)

  let instantiate (Scheme (binds, ty)) =
    VarSet.fold
    (fun name ty ->
      let* ty = ty in
      let* fresh = fresh_var in
      let* subst = Subst.singleton name fresh in
      return (Subst.apply subst ty))
      binds
    (return ty)

  let generalize env ty =
    let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
    Scheme (free, ty)

  let rec infer_pat env = function
  | Pattern_wild ->
    let* fresh = fresh_var in
    return (env, fresh)
  | Pattern_ident_or_op id ->
    let* fresh = fresh_var in
    let env' = TypeEnv.extend env id (Scheme (VarSet.empty, fresh)) in
    return (env', fresh)
  | Pattern_const c ->
    (match c with
    | Const_int _ -> return (env, Type_int)
    | Const_float _ -> return (env, Type_float)
    | Const_bool _ -> return (env, Type_bool)
    | Const_char _ -> return (env, Type_char)
    | Const_string _ -> return (env, Type_string)
    | Const_unit -> return (env, Type_unit)
    | Const_unit_of_measure _ -> fail Measure_not_implemented)
  | Pattern_tuple (p1, p2, prest) ->
    let* env1, ty1 = infer_pat env p1 in
    let* env2, ty2 = infer_pat env1 p2 in
    let* env_rest, tyrest =
    RList.fold_right
    ~f:(fun pat acc ->
      let* env_acc, ty_list = return acc in
      let* env', ty = infer_pat env_acc pat in
      return (env', ty :: ty_list))
      ~init:(return (env2, []))
      prest
    in
    return (env_rest, Type_tuple (ty1, ty2, tyrest))
  | Pattern_typed (p, ty) ->
    let* penv, pty = infer_pat env p in
    let* unif_subst = unify ty pty in
    let new_env = TypeEnv.apply unif_subst penv in
    return (new_env, Subst.apply unif_subst pty)
  | Pattern_list [] ->
    let* fresh = fresh_var in
    return (env, Type_list fresh)
  | Pattern_list (p1 :: ptl) ->
    let* fresh = fresh_var in
    let* env', ty = infer_pat env p1 in
    let* unified_sub = unify ty fresh in
    let env'' = TypeEnv.apply unified_sub env' in
    let rec infer_tail env sub_acc cur_pat =
      let helper required_ty pat =
        let* env, type_of_pat = infer_pat env pat in
        let* unified_sub = unify required_ty type_of_pat in
        return (TypeEnv.apply unified_sub env, unified_sub)
      in
      match cur_pat with
      | x :: xs ->
        let* env, sub = helper fresh x in
        let* env, final_sub = infer_tail env (sub :: sub_acc) xs in
        return (env, final_sub)
      | _ -> return (env, sub_acc)
    in
    let* new_env, sub_list = infer_tail env'' [unified_sub] ptl in
    let* final_sub = Subst.compose_all sub_list in 
    return (TypeEnv.apply final_sub new_env, Subst.apply final_sub (Type_list fresh))
  | Pattern_or (p1, p2) ->
    let* fresh = fresh_var in
    let* env1, ty1 = infer_pat env p1 in
    let* subst1 = unify ty1 fresh in
    let env' = TypeEnv.apply subst1 env1 in
    let* env2, ty2 = infer_pat env' p2 in
    let* subst2 = unify ty2 ty1 in
    let* final_sub = Subst.compose subst1 subst2 in
    return (TypeEnv.apply final_sub env2, Subst.apply final_sub fresh)
  | Pattern_cons (p1, p2) ->
    let* env, typ1 = infer_pat env p1 in
    let* env, typ2 = infer_pat env p2  in
    let* subst = Subst.unify typ2 (Type_list typ1) in
    let env = TypeEnv.apply subst env in
    return (env, Subst.apply subst typ2)
  | Pattern_option (Some p) ->
    let* env', ty = infer_pat env p in
    return (env', Type_option ty)
  | Pattern_option (None) ->
    let* fresh = fresh_var in
    return (env, Type_option fresh)


  let rec infer_expr env = function
  | Expr_ident_or_op id ->
    (match TypeEnv.find id env with
    | None -> fail (Unbound_variable id)
    | Some s ->
      let* ty = instantiate s in
      return (Subst.empty, ty))
  | Expr_const c ->
    (match c with
    | Const_int _ -> return (Subst.empty, Type_int)
    | Const_float _ -> return (Subst.empty, Type_float)
    | Const_bool _ -> return (Subst.empty, Type_bool)
    | Const_char _ -> return (Subst.empty, Type_char)
    | Const_string _ -> return (Subst.empty, Type_string)
    | Const_unit -> return (Subst.empty, Type_unit)
    | Const_unit_of_measure _ -> fail Measure_not_implemented)
  | Expr_typed (e, req_ty) ->
    let* sub, ty1 = infer_expr env e in
    let* unif_subst = unify ty1 req_ty in
    let* new_sub = Subst.compose sub unif_subst in
    return (new_sub, ty1)
  | Expr_list [] ->
    let* fresh = fresh_var in
    return (Subst.empty, Type_list fresh)
  | Expr_list (hd :: tl) ->
    let* subst1, ty1 = infer_expr env hd in
    let ty1 = Subst.apply subst1 ty1 in
    let* subst_unify, typ_unified =
      Base.List.fold
        tl
        ~f:(fun acc e ->
          let* subst_acc, typ_acc = acc in
          let* subst, typ = infer_expr env e in
          let* subst_unify = unify typ_acc typ in
          let typ_acc = Subst.apply subst_unify typ_acc in
          let* subst_acc = Subst.compose_all [ subst; subst_acc; subst_unify ] in
          return (subst_acc, typ_acc))
        ~init:(return (subst1, ty1))
    in
    return (subst_unify, Type_list typ_unified)
  | Expr_tuple (e1, e2, el) ->
    let* sub1, ty1 = infer_expr env e1 in
    let* sub2, ty2 = infer_expr (TypeEnv.apply sub1 env) e2 in
    let env = TypeEnv.apply sub2 env in
    let* sub_rest, ty_list =
        RList.fold_right
          ~f:(fun exp acc ->
            let* sub_acc, ty_list = return acc in
            let* sub, ty = infer_expr (TypeEnv.apply sub_acc env) exp in
            let* sub_acc = Subst.compose sub_acc sub in
            return (sub_acc, ty :: ty_list))
          ~init:(return (Subst.empty, []))
          el
      in
      let* sub_result = Subst.compose_all [ sub1; sub2; sub_rest ] in
      let ty1 = Subst.apply sub_result ty1 in
      let ty2 = Subst.apply sub_result ty2 in
      let ty_list = List.map (fun ty -> Subst.apply sub_result ty) ty_list in
      return (sub_result, Type_tuple (ty1, ty2, ty_list))
  | Expr_lam (pat, expr) ->
    let* env, ty1 = infer_pat env pat in
    let* sub, ty2 = infer_expr env expr in
    return (sub, Type_func (Subst.apply sub ty1, ty2))
    | Expr_ifthenelse (eif, ethen, eelse) ->
      let* sub1, ty1 = infer_expr env eif in
      let* uni_sub1 = Subst.unify ty1 (Type_bool) in
    let* sub2, ty2 = infer_expr env ethen in
    (match eelse with
    | None ->
      let* uni_sub2 = Subst.unify ty2 (Type_unit) in
      let* comp_sub = Subst.compose_all [sub1; uni_sub1; sub2; uni_sub2] in
      return (comp_sub, ty2)
    | Some exp ->
      let* sub3, ty3 = infer_expr env exp in
      let* uni_sub2 = Subst.unify ty2 ty3 in
      let* comp_sub = Subst.compose_all [sub1; uni_sub1; sub2; sub3; uni_sub2] in
      return (comp_sub, ty3))
      | Expr_option None ->
        let* fresh = fresh_var in
        return (Subst.empty, Type_option fresh)
  | Expr_option (Some e) ->
    let* sub, ty = infer_expr env e in
    return (sub, Type_option ty)
  | Expr_apply (Expr_apply (Expr_ident_or_op f, e1), e2) when Checks.is_builtin_op f ->
    let* sub1, ty1 = infer_expr env e1 in
    let* sub2, ty2 = infer_expr (TypeEnv.apply sub1 env) e2 in
    let* arg1_ty, arg2_ty, res_ty =
    match f with
    | "+" | "-" | "*" | "/" -> return (Type_int, Type_int, Type_int)
    | "+." | "-." | "*." | "/." -> return (Type_float, Type_float, Type_float)
    | "||" | "&&" -> return (Type_bool, Type_bool, Type_bool)
    | "<=" | "<" | ">=" | ">" | "=" | "<>" ->
      let* fresh = fresh_var in
      return (fresh, fresh, Type_bool)
    | "::" ->
      let* fresh = fresh_var in
      return (fresh, Type_list fresh, Type_list fresh)
    | op -> fail (Unbound_variable op)
    in
    let* unif_subst1 = Subst.unify (Subst.apply sub2 ty1) arg1_ty in
    let* unif_subst2 = Subst.unify (Subst.apply unif_subst1 ty2) arg2_ty in
    let* subst_res = Subst.compose_all [sub1; sub2; unif_subst1; unif_subst2] in
    return (subst_res, Subst.apply subst_res res_ty)
  | Expr_apply (e1, e2) ->
    let* subst1, ty1 = infer_expr env e1 in
    let* subst2, ty2 = infer_expr (TypeEnv.apply subst1 env) e2 in
    let ty1 = Subst.apply subst2 ty1 in
    let* fresh = fresh_var in
    let* subst3 = unify ty1 (Type_func (ty2, fresh)) in
    let* subst_res = Subst.compose_all [subst1; subst2; subst3] in
    return (subst_res, Subst.apply subst3 fresh)
  | Expr_match (e, rl1, rtl) -> fail TODO
  | Expr_function (rl1, rtl) -> fail TODO
  | Expr_let (Nonrecursive, vb1, vbtl, expr) -> fail TODO
  | Expr_let (Recursive, vb1, vbtl, expr) -> fail TODO


  
  let initial_env =
    let prints =
      [ "print_int", Scheme (empty, Type_func (Type_int, Type_unit))
        ; (* etc *)
        ]
      in
      List.fold_left
      (fun env (id, scheme) -> extend env id scheme)
      TypeEnv.empty
      prints
end