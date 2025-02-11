(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Template: https://gitlab.com/Kakadu/fp2020course-materials/-/tree/master/code/miniml?ref_type=heads*)

open Base
open Ast
open Stdlib.Format

type error =
  | OccursCheck of int * ty
  | NoVariable of string
  | UnificationFailed of ty * ty
  | SeveralBounds of string
  | NotImplement

let pp_error fmt = function
  | OccursCheck (id, ty) ->
    fprintf fmt "Occurs check failed. Type variable '%d occurs inside %a." id pp_ty ty
  | NoVariable name -> fprintf fmt "Unbound variable '%s'." name
  | UnificationFailed (ty1, ty2) ->
    fprintf fmt "Failed to unify types: %a and %a." pp_ty ty1 pp_ty ty2
  | SeveralBounds name -> fprintf fmt "Multiple bounds for variable '%s'." name
  | NotImplement -> fprintf fmt "This feature is not implemented yet."
;;

module IntSet = struct
  include Stdlib.Set.Make (Int)
end

module ResultMonad : sig
  type 'a t

  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  val fresh : int t
  val run : 'a t -> ('a, error) Result.t

  module RMap : sig
    val fold : ('a, 'b, 'c) Map.t -> init:'d t -> f:('a -> 'b -> 'd -> 'd t) -> 'd t
  end
end = struct
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) m f state =
    let last, r = m state in
    match r with
    | Result.Error x -> last, Result.fail x
    | Result.Ok a -> f a last
  ;;

  let return x last = last, Result.return x
  let fail e st = st, Result.fail e

  let ( >>| ) m f st =
    match m st with
    | st, Ok x -> st, Result.return (f x)
    | st, Result.Error e -> st, Result.fail e
  ;;

  module Syntax = struct
    let ( let* ) = ( >>= )
  end

  module RMap = struct
    let fold map ~init ~f =
      Map.fold map ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.return last
  let run monad = snd (monad 0)
end

module Type = struct
  let rec occurs_in var = function
    | TyVar b -> b = var
    | TyArrow (left, right) -> occurs_in var left || occurs_in var right
    | TyTuple types -> List.exists types ~f:(occurs_in var)
    | TyList ty -> occurs_in var ty
    | TyOption ty -> occurs_in var ty
    | TyPrim _ -> false
  ;;

  let free_vars =
    let rec helper acc = function
      | TyVar b -> IntSet.add b acc
      | TyArrow (left, right) -> helper (helper acc left) right
      | TyTuple types -> List.fold_left types ~init:acc ~f:helper
      | TyList ty -> helper acc ty
      | TyOption ty -> helper acc ty
      | TyPrim _ -> acc
    in
    helper IntSet.empty
  ;;
end

module Substitution : sig
  type t

  val empty : t
  val singleton : int -> ty -> t ResultMonad.t
  val remove : t -> int -> t
  val apply : t -> ty -> ty
  val unify : ty -> ty -> t ResultMonad.t
  val compose : t -> t -> t ResultMonad.t
  val compose_all : t list -> t ResultMonad.t
end = struct
  open ResultMonad
  open ResultMonad.Syntax

  type t = (int, ty, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)

  let mapping key value =
    if Type.occurs_in key value
    then fail (OccursCheck (key, value))
    else return (key, value)
  ;;

  let singleton key value =
    let* key, value = mapping key value in
    return (Map.singleton (module Int) key value)
  ;;

  let find = Map.find
  let remove = Map.remove

  let apply subst =
    let rec helper = function
      | TyPrim x -> TyPrim x
      | TyVar b as ty ->
        (match find subst b with
         | None -> ty
         | Some x -> x)
      | TyArrow (left, right) -> TyArrow (helper left, helper right)
      | TyList ty -> TyList (helper ty)
      | TyOption ty -> TyOption (helper ty)
      | TyTuple types -> TyTuple (List.map ~f:helper types)
    in
    helper
  ;;

  let rec unify left right =
    match left, right with
    | TyPrim l, TyPrim r when String.equal l r -> return empty
    | TyPrim _, TyPrim _ -> fail (UnificationFailed (left, right))
    | TyVar l, TyVar r when l = r -> return empty
    | TyVar b, ty | ty, TyVar b -> singleton b ty
    | TyArrow (left1, right1), TyArrow (left2, right2) ->
      let* subst1 = unify left1 left2 in
      let* subst2 = unify (apply subst1 right1) (apply subst1 right2) in
      compose subst1 subst2
    | TyTuple types1, TyTuple types2 ->
      if List.length types1 <> List.length types2
      then fail (UnificationFailed (left, right))
      else (
        let rec unify_tuples subst types1 types2 =
          match types1, types2 with
          | [], [] -> return subst
          | t1 :: rest1, t2 :: rest2 ->
            let* subst' = unify (apply subst t1) (apply subst t2) in
            let* composed_subst = compose subst subst' in
            unify_tuples composed_subst rest1 rest2
          | _, _ -> fail (UnificationFailed (left, right))
        in
        unify_tuples empty types1 types2)
    | TyList ty1, TyList ty2 -> unify ty1 ty2
    | TyOption ty1, TyOption ty2 -> unify ty1 ty2
    | _ -> fail (UnificationFailed (left, right))

  and extend key value subst =
    match find subst key with
    | None ->
      let value = apply subst value in
      let* subst2 = singleton key value in
      RMap.fold subst ~init:(return subst2) ~f:(fun key value acc ->
        let value = apply subst2 value in
        let* key, value = mapping key value in
        return (Map.update acc key ~f:(fun _ -> value)))
    | Some value2 ->
      let* subst2 = unify value value2 in
      compose subst subst2

  and compose subst1 subst2 = RMap.fold subst2 ~init:(return subst1) ~f:extend

  let compose_all =
    List.fold_left ~init:(return empty) ~f:(fun acc subst ->
      let* acc = acc in
      compose acc subst)
  ;;
end

module Scheme = struct
  type t = S of IntSet.t * ty

  let free_vars (S (vars, ty)) = IntSet.diff (Type.free_vars ty) vars

  let apply subst (S (vars, ty)) =
    let subst2 =
      IntSet.fold (fun key subst -> Substitution.remove subst key) vars subst
    in
    S (vars, Substitution.apply subst2 ty)
  ;;
end

module TypeEnv = struct
  type t = (ident, Scheme.t, String.comparator_witness) Map.t

  let extend env key value = Map.update env key ~f:(fun _ -> value)
  let remove = Map.remove

  let free_vars : t -> IntSet.t =
    Map.fold ~init:IntSet.empty ~f:(fun ~key:_ ~data:scheme acc ->
      IntSet.union acc (Scheme.free_vars scheme))
  ;;

  let apply subst env = Map.map env ~f:(Scheme.apply subst)
  let find key env = Map.find env key

  let initial_env =
    let open Base.Map in
    empty (module String)
    |> set
         ~key:"print_int"
         ~data:(Scheme.S (IntSet.empty, TyArrow (TyPrim "int", TyPrim "unit")))
    |> set
         ~key:"print_endline"
         ~data:(Scheme.S (IntSet.empty, TyArrow (TyPrim "string", TyPrim "unit")))
    |> set
         ~key:"print_bool"
         ~data:(Scheme.S (IntSet.empty, TyArrow (TyPrim "bool", TyPrim "unit")))
  ;;
end

open ResultMonad
open ResultMonad.Syntax

let fresh_var = fresh >>| fun n -> TyVar n

let instantiate : Scheme.t -> ty ResultMonad.t =
  fun (S (vars, ty)) ->
  IntSet.fold
    (fun var typ ->
      let* typ = typ in
      let* fresh_ty = fresh_var in
      let* subst = Substitution.singleton var fresh_ty in
      return (Substitution.apply subst typ))
    vars
    (return ty)
;;

let generalize env ty =
  let free = IntSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  Scheme.S (free, ty)
;;

let generalize_rec env ty var =
  let env = TypeEnv.remove env var in
  let free = IntSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  Scheme.S (free, ty)
;;

let infer_const = function
  | ConstInt _ -> TyPrim "int"
  | ConstBool _ -> TyPrim "bool"
  | ConstString _ -> TyPrim "string"
  | ConstUnit -> TyPrim "unit"
;;

let rec infer_pattern env = function
  | PatAny ->
    let* fresh = fresh_var in
    return (env, fresh)
  | PatConst const -> return (env, infer_const const)
  | PatVariable var ->
    let* fresh = fresh_var in
    let env = TypeEnv.extend env var (Scheme.S (IntSet.empty, fresh)) in
    return (env, fresh)
  | PatTuple (pat1, pat2, pats) ->
    let* env, types =
      List.fold_left
        ~f:(fun acc pat ->
          let* env1, types = acc in
          let* env2, ty = infer_pattern env1 pat in
          return (env2, ty :: types))
        ~init:(return (env, []))
        (pat1 :: pat2 :: pats)
    in
    return (env, TyTuple (List.rev types))
  | PatType (pat, annot) ->
    let* env1, ty1 = infer_pattern env pat in
    let* subst = Substitution.unify ty1 annot in
    let env = TypeEnv.apply subst env1 in
    return (env, Substitution.apply subst ty1)
  | PatUnit -> return (env, TyPrim "unit")
;;

let infer_binop_type = function
  | Equal | NotEqual | GreaterThan | GretestEqual | LowerThan | LowestEqual ->
    fresh_var >>| fun fresh_ty -> fresh_ty, fresh_ty, TyPrim "bool"
  | Plus | Minus | Multiply | Division -> return (TyPrim "int", TyPrim "int", TyPrim "int")
  | And | Or -> return (TyPrim "bool", TyPrim "bool", TyPrim "bool")
;;

let infer_expr =
  let rec helper env = function
    | ExpConst const -> return (Substitution.empty, infer_const const)
    | ExpIdent var ->
      (match TypeEnv.find var env with
       | Some scheme ->
         let* ty = instantiate scheme in
         return (Substitution.empty, ty)
       | None -> fail (NoVariable var))
    | ExpUnarOper (Negative, expr) ->
      let* subst, ty = helper env expr in
      let* subst' = Substitution.unify ty (TyPrim "int") in
      let* total_subst = Substitution.compose subst subst' in
      return (total_subst, TyPrim "int")
    | ExpUnarOper (Not, expr) ->
      let* subst, ty = helper env expr in
      let* subst' = Substitution.unify ty (TyPrim "bool") in
      let* total_subst = Substitution.compose subst subst' in
      return (total_subst, TyPrim "bool")
    | ExpBinOper (op, expr1, expr2) ->
      let* subst1, ty1 = helper env expr1 in
      let* subst2, ty2 = helper (TypeEnv.apply subst1 env) expr2 in
      let* ty1_op, ty2_op, ty_res = infer_binop_type op in
      let* subst3 = Substitution.unify (Substitution.apply subst2 ty1) ty1_op in
      let* subst4 = Substitution.unify (Substitution.apply subst3 ty2) ty2_op in
      let* total_subst = Substitution.compose_all [ subst1; subst2; subst3; subst4 ] in
      return (total_subst, Substitution.apply total_subst ty_res)
    | ExpBranch (cond, then_expr, else_expr) ->
      let* subst1, ty1 = helper env cond in
      let* subst2, ty2 = helper (TypeEnv.apply subst1 env) then_expr in
      let* subst3, ty3 =
        match else_expr with
        | Some expr -> helper (TypeEnv.apply subst2 env) expr
        | None -> return (Substitution.empty, TyPrim "unit")
      in
      let* subst4 = Substitution.unify ty1 (TyPrim "bool") in
      let* subst5 = Substitution.unify ty2 ty3 in
      let* total_subst =
        Substitution.compose_all [ subst1; subst2; subst3; subst4; subst5 ]
      in
      return (total_subst, Substitution.apply total_subst ty2)
    | ExpTuple (expr1, expr2, exprs) ->
      let* subst, types =
        List.fold_left
          ~f:(fun acc expr ->
            let* subst, types = acc in
            let* subst1, ty = helper env expr in
            let* subst2 = Substitution.compose subst subst1 in
            return (subst2, ty :: types))
          ~init:(return (Substitution.empty, []))
          (expr1 :: expr2 :: exprs)
      in
      return (subst, TyTuple (List.rev_map ~f:(Substitution.apply subst) types))
    | ExpList exprs ->
      (match exprs with
       | [] ->
         let* fresh = fresh_var in
         return (Substitution.empty, TyList fresh)
       | hd :: tl ->
         let* subst1, ty_hd = helper env hd in
         let* subst, ty =
           List.fold_left
             ~f:(fun acc expr ->
               let* subst_acc, ty_acc = acc in
               let* subst_cur, ty_cur = helper env expr in
               let* subst_unify = Substitution.unify ty_acc ty_cur in
               let* subst_combined =
                 Substitution.compose_all [ subst_acc; subst_cur; subst_unify ]
               in
               return (subst_combined, Substitution.apply subst_combined ty_acc))
             ~init:(return (subst1, ty_hd))
             tl
         in
         return (subst, TyList ty))
    | ExpOption opt_expr ->
      (match opt_expr with
       | Some expr ->
         let* subst, ty = helper env expr in
         return (subst, TyOption ty)
       | None ->
         let* fresh_ty = fresh_var in
         return (Substitution.empty, TyOption fresh_ty))
    | ExpFunction (param, body) ->
      let* fresh = fresh_var in
      let* subst1, ty1 = helper env param in
      let* subst2, ty2 = helper (TypeEnv.apply subst1 env) body in
      let* subst3 =
        Substitution.unify (TyArrow (ty2, fresh)) (Substitution.apply subst2 ty1)
      in
      let* total_subst = Substitution.compose_all [ subst1; subst2; subst3 ] in
      let ty = Substitution.apply total_subst fresh in
      return (total_subst, ty)
    | ExpTypeAnnotation (expr, ty_annot) ->
      let* subst, ty = helper env expr in
      let* subst' = Substitution.unify ty ty_annot in
      let* total_subst = Substitution.compose subst subst' in
      return (total_subst, Substitution.apply total_subst ty_annot)
    | ExpLet (_, PatTuple (pat1, pat2, pats), expr1, expr2_opt) ->
      let* env1, ty1 = infer_pattern env (PatTuple (pat1, pat2, pats)) in
      let* subst1, ty1_expr = helper env expr1 in
      let* subst2 = Substitution.unify ty1 (Substitution.apply subst1 ty1_expr) in
      let* total_subst = Substitution.compose subst1 subst2 in
      let env = TypeEnv.apply total_subst env1 in
      (match expr2_opt with
       | None -> return (total_subst, Substitution.apply total_subst ty1_expr)
       | Some expr2 ->
         let* subst3, ty2 = helper env expr2 in
         let* final_subst = Substitution.compose total_subst subst3 in
         return (final_subst, Substitution.apply final_subst ty2))
    | ExpLet (false, PatVariable var, expr1, expr2_opt) ->
      let* subst1, ty1 = helper env expr1 in
      let env = TypeEnv.apply subst1 env in
      let scheme = generalize env ty1 in
      let env = TypeEnv.extend env var scheme in
      (match expr2_opt with
       | None -> return (subst1, ty1)
       | Some expr2 ->
         let* subst2, ty2 = helper env expr2 in
         let* total_subst = Substitution.compose subst1 subst2 in
         return (total_subst, ty2))
    | ExpLet (true, PatVariable var, expr1, expr2_opt) ->
      let* fresh = fresh_var in
      let env1 = TypeEnv.extend env var (Scheme.S (IntSet.empty, fresh)) in
      let* subst, ty = helper env1 expr1 in
      let* subst1 = Substitution.unify ty fresh in
      let* subst2 = Substitution.compose subst subst1 in
      let env = TypeEnv.apply subst2 env in
      let ty = Substitution.apply subst2 ty in
      let scheme = generalize_rec env ty var in
      let env = TypeEnv.extend env var scheme in
      (match expr2_opt with
       | None -> return (subst2, ty)
       | Some expr2 ->
         let* subst3, ty2 = helper env expr2 in
         let* total_subst = Substitution.compose subst2 subst3 in
         return (total_subst, ty2))
    | ExpLet (_, PatType (pat, annot), expr1, expr2_opt) ->
      let* env1, ty1 = infer_pattern env pat in
      let* subst = Substitution.unify ty1 annot in
      let env = TypeEnv.apply subst env1 in
      let* subst1, ty1 = helper env expr1 in
      let env = TypeEnv.apply subst1 env in
      let scheme = generalize env ty1 in
      let env =
        TypeEnv.extend
          env
          (match pat with
           | PatVariable var -> var
           | _ -> "_")
          scheme
      in
      (match expr2_opt with
       | None -> return (subst1, ty1)
       | Some expr2 ->
         let* subst2, ty2 = helper env expr2 in
         let* total_subst = Substitution.compose subst1 subst2 in
         return (total_subst, ty2))
    | ExpLet (_, PatUnit, expr1, expr2_opt) ->
      let* subst1, ty1 = helper env expr1 in
      let env = TypeEnv.apply subst1 env in
      (match expr2_opt with
       | None -> return (subst1, ty1)
       | Some expr2 ->
         let* subst2, ty2 = helper env expr2 in
         let* total_subst = Substitution.compose subst1 subst2 in
         return (total_subst, ty2))
    | ExpLambda (patterns, body) ->
      let init_env = return (env, Substitution.empty, []) in
      let* env', subst_patterns, param_types =
        List.fold_left
          ~f:(fun acc pattern ->
            let* env_acc, subst_acc, types_acc = acc in
            let* env_updated, param_type = infer_pattern env_acc pattern in
            return (env_updated, subst_acc, param_type :: types_acc))
          ~init:init_env
          patterns
      in
      let param_types = List.rev param_types in
      let* subst_body, body_type = helper env' body in
      let* total_subst = Substitution.compose_all [ subst_patterns; subst_body ] in
      let function_type =
        List.fold_right param_types ~init:body_type ~f:(fun l r -> TyArrow (l, r))
      in
      return (total_subst, function_type)
    | ExpLetAnd (_, bindings, expr_opt) ->
      let* env, subst, schemes =
        List.fold_left
          ~f:(fun acc (pat, expr) ->
            let* env_acc, subst_acc, schemes_acc = acc in
            let* env1, ty1 = infer_pattern env_acc pat in
            let* subst1, ty1_expr = helper env1 expr in
            let* subst2 = Substitution.unify ty1 (Substitution.apply subst1 ty1_expr) in
            let* total_subst = Substitution.compose_all [ subst_acc; subst1; subst2 ] in
            let env = TypeEnv.apply total_subst env1 in
            let scheme = generalize env (Substitution.apply total_subst ty1_expr) in
            return (env, total_subst, (pat, scheme) :: schemes_acc))
          ~init:(return (env, Substitution.empty, []))
          bindings
      in
      let env =
        List.fold_left
          ~f:(fun env_acc (pat, scheme) ->
            match pat with
            | PatVariable var -> TypeEnv.extend env_acc var scheme
            | _ -> env_acc)
          ~init:env
          schemes
      in
      (match expr_opt with
       | None -> return (subst, TyPrim "unit")
       | Some expr ->
         let* subst2, ty2 = helper env expr in
         let* total_subst = Substitution.compose subst subst2 in
         return (total_subst, ty2))
    | _ -> fail NotImplement
  in
  helper
;;

let rec infer_structure_item env = function
  | ExpLet (true, PatVariable var, expr1, None) ->
    let* fresh = fresh_var in
    let scheme = Scheme.S (IntSet.empty, fresh) in
    let env = TypeEnv.extend env var scheme in
    let* subst1, ty1 = infer_expr env expr1 in
    let* subst2 = Substitution.unify ty1 fresh in
    let* subst3 = Substitution.compose subst1 subst2 in
    let env = TypeEnv.apply subst3 env in
    let ty2 = Substitution.apply subst3 ty1 in
    let scheme = generalize_rec env ty2 var in
    let env = TypeEnv.extend env var scheme in
    return env
  | ExpLet (false, PatVariable var, expr1, None) ->
    let* subst, ty = infer_expr env expr1 in
    let env = TypeEnv.apply subst env in
    let scheme = generalize env ty in
    let env = TypeEnv.extend env var scheme in
    return env
  | ExpLet (_, PatType (pat, annot), expr1, expr2_opt) ->
    let* env1, ty1 = infer_pattern env pat in
    let* subst = Substitution.unify ty1 annot in
    let env = TypeEnv.apply subst env1 in
    let* subst1, ty1 = infer_expr env expr1 in
    let env = TypeEnv.apply subst1 env in
    let scheme = generalize env ty1 in
    let env =
      TypeEnv.extend
        env
        (match pat with
         | PatVariable var -> var
         | _ -> "_")
        scheme
    in
    (match expr2_opt with
     | None -> return env
     | Some expr2 ->
       let* subst2, _ = infer_expr env expr2 in
       let* total_subst = Substitution.compose subst1 subst2 in
       return (TypeEnv.apply total_subst env))
  | ExpLet (_, PatTuple (pat1, pat2, pats), expr1, expr2_opt) ->
    let* env_pat, ty_pat = infer_pattern env (PatTuple (pat1, pat2, pats)) in
    let* subst_expr, ty_expr = infer_expr env_pat expr1 in
    let* subst_unify =
      Substitution.unify ty_pat (Substitution.apply subst_expr ty_expr)
    in
    let* total_subst = Substitution.compose_all [ subst_expr; subst_unify ] in
    let env_updated = TypeEnv.apply total_subst env_pat in
    (match expr2_opt with
     | Some expr2 ->
       let* subst_expr2, _ = infer_expr env_updated expr2 in
       let* final_subst = Substitution.compose total_subst subst_expr2 in
       return (TypeEnv.apply final_subst env_updated)
     | None -> return env_updated)
  | ExpLet (is_rec, PatVariable var, expr1, Some body) ->
    let* env = infer_structure_item env (ExpLet (is_rec, PatVariable var, expr1, None)) in
    infer_expr env body >>= fun _ -> return env
  | ExpLetAnd (_, bindings, expr_opt) ->
    let* env_with_placeholders =
      List.fold_left
        ~f:(fun acc (pat, _) ->
          let* env_acc = acc in
          match pat with
          | PatVariable var ->
            let* fresh_ty = fresh_var in
            return (TypeEnv.extend env_acc var (Scheme.S (IntSet.empty, fresh_ty)))
          | _ -> fail (NoVariable "Non-variable pattern in recursive binding"))
        ~init:(return env)
        bindings
    in
    let* env_with_inferred_types, subst, schemes =
      List.fold_left
        ~f:(fun acc (pat, expr) ->
          let* env_acc, subst_acc, schemes_acc = acc in
          let* env1, ty1 = infer_pattern env_acc pat in
          let* subst1, ty1_expr = infer_expr env1 expr in
          let* subst2 = Substitution.unify ty1 (Substitution.apply subst1 ty1_expr) in
          let* total_subst = Substitution.compose_all [ subst_acc; subst1; subst2 ] in
          let env = TypeEnv.apply total_subst env1 in
          let scheme = generalize env (Substitution.apply total_subst ty1_expr) in
          return (env, total_subst, (pat, scheme) :: schemes_acc))
        ~init:(return (env_with_placeholders, Substitution.empty, []))
        bindings
    in
    let env_updated =
      List.fold_left
        ~f:(fun env_acc (pat, scheme) ->
          match pat with
          | PatVariable var -> TypeEnv.extend env_acc var scheme
          | _ -> env_acc)
        ~init:env_with_inferred_types
        schemes
    in
    (match expr_opt with
     | None -> return env_updated
     | Some expr ->
       let* subst2, _ = infer_expr env_updated expr in
       let* total_subst = Substitution.compose subst subst2 in
       return (TypeEnv.apply total_subst env_updated))
  | expr ->
    let* _, ty = infer_expr env expr in
    return (TypeEnv.extend env "_" (Scheme.S (IntSet.empty, ty)))
;;

let infer_structure (structure : program) =
  let rec process_items env = function
    | [] -> return env
    | ExpLet (is_rec, pattern, expr, None) :: rest ->
      let* env = infer_structure_item env (ExpLet (is_rec, pattern, expr, None)) in
      process_items env rest
    | ExpLet (is_rec, pattern, expr, Some body) :: rest ->
      let* env = infer_structure_item env (ExpLet (is_rec, pattern, expr, Some body)) in
      process_items env rest
    | ExpLetAnd (is_rec, bindings, expr_opt) :: rest ->
      let* env = infer_structure_item env (ExpLetAnd (is_rec, bindings, expr_opt)) in
      process_items env rest
    | expr :: rest ->
      let* env = infer_structure_item env expr in
      process_items env rest
  in
  process_items TypeEnv.initial_env structure
;;

let run_infer s = run (infer_structure s)
