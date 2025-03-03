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
  | LHS of string
  | RHS of string
  | UnexpectedFunction of ty

let pp_error fmt = function
  | OccursCheck (id, ty) ->
    fprintf fmt "Occurs check failed. Type variable '%d occurs inside %a." id pp_ty ty
  | NoVariable name -> fprintf fmt "Unbound variable '%s'." name
  | UnificationFailed (ty1, ty2) ->
    fprintf fmt "Failed to unify types: %a and %a." pp_ty ty1 pp_ty ty2
  | SeveralBounds name -> fprintf fmt "Multiple bounds for variable '%s'." name
  | LHS msg -> fprintf fmt "Left-hand side error: %s." msg
  | RHS msg -> fprintf fmt "Right-hand side error: %s." msg
  | UnexpectedFunction ty1 -> fprintf fmt "UnexpectedFunction error: %a" pp_ty ty1
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

  let free_vars : t -> IntSet.t =
    Map.fold ~init:IntSet.empty ~f:(fun ~key:_ ~data:scheme acc ->
      IntSet.union acc (Scheme.free_vars scheme))
  ;;

  let apply subst env = Map.map env ~f:(Scheme.apply subst)
  let find = Map.find

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

let infer_const = function
  | ConstInt _ -> TyPrim "int"
  | ConstBool _ -> TyPrim "bool"
  | ConstString _ -> TyPrim "string"
;;

let rec infer_pattern env = function
  | PatAny ->
    let* fresh = fresh_var in
    return (Substitution.empty, fresh, env)
  | PatConst const -> return (Substitution.empty, infer_const const, env)
  | PatVariable var ->
    let* fresh = fresh_var in
    let env = TypeEnv.extend env var (Scheme.S (IntSet.empty, fresh)) in
    return (Substitution.empty, fresh, env)
  | PatTuple (first_pat, second_pat, rest_pats) ->
    let* sub_first, type_first, env_first = infer_pattern env first_pat in
    let updated_env_second = TypeEnv.apply sub_first env_first in
    let* sub_second, type_second, env_second =
      infer_pattern updated_env_second second_pat
    in
    let process_remaining_patterns acc pat =
      let open ResultMonad.Syntax in
      let* current_sub, types, current_env = acc in
      let* sub_new, type_new, env_new = infer_pattern current_env pat in
      let* combined_sub = Substitution.compose current_sub sub_new in
      return (combined_sub, type_new :: types, env_new)
    in
    let initial_state = return (sub_second, [ type_second; type_first ], env_second) in
    let* final_sub, collected_types, final_env =
      List.fold_left rest_pats ~init:initial_state ~f:process_remaining_patterns
    in
    let tuple_type = TyTuple (List.rev collected_types) in
    return (final_sub, tuple_type, final_env)
  | PatList pats ->
    let* fresh_el_type = fresh_var in
    let* final_sub, final_env =
      List.fold_left
        pats
        ~init:(return (Substitution.empty, env))
        ~f:(fun acc pat ->
          let open ResultMonad.Syntax in
          let* sub_acc, env_acc = acc in
          let* sub_cur, el_type, env_cur = infer_pattern env_acc pat in
          let* unified_sub = Substitution.compose sub_acc sub_cur in
          let* final_sub =
            Substitution.unify (Substitution.apply sub_cur fresh_el_type) el_type
          in
          let* combined_sub = Substitution.compose unified_sub final_sub in
          return (combined_sub, TypeEnv.apply final_sub env_cur))
    in
    return (final_sub, TyList (Substitution.apply final_sub fresh_el_type), final_env)
  | PatOption opt ->
    let* sub, typ, env =
      match opt with
      | None ->
        let* fresh = fresh_var in
        return (Substitution.empty, fresh, env)
      | Some p -> infer_pattern env p
    in
    return (sub, TyOption typ, env)
  | PatType (pat, annotated_ty) ->
    let* subst, inferred_ty, env = infer_pattern env pat in
    let* unified_subst = Substitution.unify inferred_ty annotated_ty in
    let* total_subst = Substitution.compose subst unified_subst in
    return
      ( total_subst
      , Substitution.apply total_subst annotated_ty
      , TypeEnv.apply total_subst env )
  | PatUnit -> return (Substitution.empty, TyPrim "unit", env)
;;

let infer_binop_type = function
  | Equal | NotEqual | GreaterThan | GretestEqual | LowerThan | LowestEqual ->
    fresh_var >>| fun fresh_ty -> fresh_ty, fresh_ty, TyPrim "bool"
  | Plus | Minus | Multiply | Division -> return (TyPrim "int", TyPrim "int", TyPrim "int")
  | And | Or -> return (TyPrim "bool", TyPrim "bool", TyPrim "bool")
;;

let rec infer_expr env = function
  | ExpConst const -> return (Substitution.empty, infer_const const)
  | ExpIdent var ->
    (match TypeEnv.find env var with
     | Some scheme ->
       let* ty = instantiate scheme in
       return (Substitution.empty, ty)
     | None -> fail (NoVariable var))
  | ExpUnarOper (operation, expr) ->
    let* subst, ty = infer_expr env expr in
    let* operation_type =
      match operation with
      | Negative -> return (TyArrow (TyPrim "int", TyPrim "int"))
      | Not -> return (TyArrow (TyPrim "bool", TyPrim "bool"))
    in
    let* subst2 =
      match operation_type with
      | TyArrow (arg, _) -> Substitution.unify ty arg
      | ty -> fail (UnexpectedFunction ty)
    in
    let* subst2 = Substitution.compose_all [ subst2; subst ] in
    (match operation_type with
     | TyArrow (_, x) -> return (subst2, Substitution.apply subst2 x)
     | ty -> fail (UnexpectedFunction ty))
  | ExpBinOper (op, expr1, expr2) ->
    let* subst1, ty = infer_expr env expr1 in
    let* subst2, ty' = infer_expr (TypeEnv.apply subst1 env) expr2 in
    let* ty1_op, ty2_op, ty_res = infer_binop_type op in
    let* subst3 = Substitution.unify (Substitution.apply subst2 ty) ty1_op in
    let* subst4 = Substitution.unify (Substitution.apply subst3 ty') ty2_op in
    let* subst = Substitution.compose_all [ subst1; subst2; subst3; subst4 ] in
    return (subst, Substitution.apply subst ty_res)
  | ExpBranch (cond, then_expr, else_expr) ->
    let* subst1, ty1 = infer_expr env cond in
    let* subst2, ty2 = infer_expr (TypeEnv.apply subst1 env) then_expr in
    let* ty3 =
      match else_expr with
      | Some el ->
        let* _, ty3 = infer_expr (TypeEnv.apply subst2 env) el in
        return ty3
      | None -> return (TyPrim "unit")
    in
    let* subst4 = Substitution.unify ty1 (TyPrim "bool") in
    let* subst5 = Substitution.unify ty2 ty3 in
    let* total_subst =
      match else_expr with
      | Some el ->
        let* subst3, _ = infer_expr (TypeEnv.apply subst2 env) el in
        Substitution.compose_all [ subst5; subst4; subst3; subst2; subst1 ]
      | None -> Substitution.compose_all [ subst5; subst4; subst2; subst1 ]
    in
    return (total_subst, Substitution.apply total_subst ty2)
  | ExpTuple (expr1, expr2, exprs) ->
    let* subst1, ty1 = infer_expr env expr1 in
    let* subst2, ty2 = infer_expr (TypeEnv.apply subst1 env) expr2 in
    let infer_tuple_elements env es =
      let rec aux env = function
        | [] -> return ([], [])
        | e :: es' ->
          let* s, t = infer_expr env e in
          let* s', ts = aux (TypeEnv.apply s env) es' in
          return (s' @ [ s ], t :: ts)
      in
      aux env es
    in
    let* subst3, tys = infer_tuple_elements (TypeEnv.apply subst2 env) exprs in
    let* subst = Substitution.compose_all (subst3 @ [ subst2; subst1 ]) in
    return (subst, TyTuple (ty1 :: ty2 :: tys))
  | ExpList exprs ->
    (match exprs with
     | [] ->
       let* fresh = fresh_var in
       return (Substitution.empty, TyList fresh)
     | _ :: _ ->
       let infer_list_elements env es =
         let rec aux env = function
           | [] -> return ([], [])
           | e :: es' ->
             let* s, t = infer_expr env e in
             let* s', ts = aux (TypeEnv.apply s env) es' in
             return (s' @ [ s ], t :: ts)
         in
         aux env es
       in
       let* subst, tys = infer_list_elements env exprs in
       let* total_subst = Substitution.compose_all subst in
       (match tys with
        | [] -> fail (SeveralBounds "inferred empty list type")
        | ty :: _ -> return (total_subst, TyList ty)))
  | ExpLet (false, (PatVariable x, expr1), _, expr2) ->
    let* subst1, ty1 = infer_expr env expr1 in
    let env2 = TypeEnv.apply subst1 env in
    let ty_gen = generalize env2 ty1 in
    let env3 = TypeEnv.extend env x ty_gen in
    let* subst2, ty2 = infer_expr (TypeEnv.apply subst1 env3) expr2 in
    let* total_subst = Substitution.compose subst1 subst2 in
    return (total_subst, ty2)
  | ExpLet (false, (pattern, expr1), bindings, expr2) ->
    let* subst1, ty1 = infer_expr env expr1 in
    let* subst2, ty_pat, env1 = infer_pattern env pattern in
    let* subst = Substitution.compose subst1 subst2 in
    let* unified_subst = Substitution.unify (Substitution.apply subst ty_pat) ty1 in
    let initial_env = TypeEnv.apply unified_subst env1 in
    let* extended_env =
      List.fold_left
        ~f:(fun acc_env (pattern, expr) ->
          let* acc_env = acc_env in
          let* subst_bind, ty_bind = infer_expr acc_env expr in
          let* subst_pattern, _, env_pattern = infer_pattern acc_env pattern in
          let* combined_subst = Substitution.compose subst_bind subst_pattern in
          let* final_subst =
            Substitution.unify (Substitution.apply combined_subst ty_pat) ty_bind
          in
          let updated_env =
            Map.fold
              ~init:(TypeEnv.apply final_subst acc_env)
              ~f:(fun ~key ~data acc_env -> TypeEnv.extend acc_env key data)
              (TypeEnv.apply final_subst env_pattern)
          in
          return updated_env)
        ~init:(return initial_env)
        bindings
    in
    let* subst3, ty2 = infer_expr extended_env expr2 in
    let* total_subst = Substitution.compose_all [ subst3; unified_subst; subst ] in
    return (total_subst, ty2)
  | ExpLet (true, (PatVariable x, expr1), [], expr2) ->
    let* expr1 =
      match expr1 with
      | ExpLambda _ -> return expr1
      | _ -> fail (RHS "Right-hand side of let rec must be a lambda expression")
    in
    let* tv = fresh_var in
    let env2 = TypeEnv.extend env x (S (IntSet.empty, tv)) in
    let* subst1, ty1 = infer_expr env2 expr1 in
    let* subst2 = Substitution.unify (Substitution.apply subst1 tv) ty1 in
    let* subst_total = Substitution.compose subst1 subst2 in
    let env3 = TypeEnv.apply subst_total env in
    let env4 = TypeEnv.apply subst1 env3 in
    let ty_gen = generalize env4 (Substitution.apply subst_total tv) in
    let* subst3, ty2 = infer_expr (TypeEnv.extend env4 x ty_gen) expr2 in
    let* subst_total = Substitution.compose subst_total subst3 in
    return (subst_total, ty2)
  | ExpLet (true, value_binding, value_bindings, expr2) ->
    let* env_ext, subst_acc =
      List.fold_left
        ~f:(fun acc_env (pat, expr) ->
          let* expr =
            match expr with
            | ExpLambda _ -> return expr
            | _ -> fail (RHS "Right-hand side of let rec must be a lambda expression")
          in
          let* pat =
            match pat with
            | PatVariable _ -> return pat
            | _ ->
              fail (LHS "Only variables are allowed on the left-hand side of let rec")
          in
          let* env_acc, _ = acc_env in
          let* subst_expr, ty_expr = infer_expr env_acc expr in
          let* subst_pattern, ty_pat, env_pat = infer_pattern env_acc pat in
          let* subst = Substitution.compose subst_expr subst_pattern in
          let* unified_subst = Substitution.unify ty_expr ty_pat in
          let* combined_subst = Substitution.compose subst unified_subst in
          let extended_env = TypeEnv.apply combined_subst env_pat in
          return (extended_env, combined_subst))
        ~init:(return (env, Substitution.empty))
        (value_binding :: value_bindings)
    in
    let* subst2, ty2 = infer_expr env_ext expr2 in
    let* total_subst = Substitution.compose subst_acc subst2 in
    return (total_subst, ty2)
  | ExpLambda (patterns, body) ->
    let* env, pat_types =
      List.fold_left
        patterns
        ~init:(return (env, []))
        ~f:(fun acc pat ->
          let* env, pat_types = acc in
          let* _, typ, env = infer_pattern env pat in
          return (env, typ :: pat_types))
    in
    let* subst_body, ty_body = infer_expr env body in
    let arrow_type =
      List.fold_right
        ~f:(fun pat_type acc -> TyArrow (Substitution.apply subst_body pat_type, acc))
        ~init:ty_body
        (List.rev pat_types)
    in
    return (subst_body, arrow_type)
  | ExpFunction (param, body) ->
    let* subst1, ty1 = infer_expr env param in
    let* subst2, ty2 = infer_expr (TypeEnv.apply subst1 env) body in
    let* tv = fresh_var in
    let* subst3 =
      Substitution.unify (Substitution.apply subst2 ty1) (TyArrow (ty2, tv))
    in
    let* total_subst = Substitution.compose_all [ subst3; subst2; subst1 ] in
    return (total_subst, Substitution.apply total_subst tv)
  | ExpOption opt_expr ->
    (match opt_expr with
     | Some expr ->
       let* subst, ty = infer_expr env expr in
       return (subst, TyOption ty)
     | None ->
       let* tv = fresh_var in
       return (Substitution.empty, TyOption tv))
  | ExpTypeAnnotation (expr, t) ->
    let* subst1, ty1 = infer_expr env expr in
    let* subst2 = Substitution.unify ty1 (Substitution.apply subst1 t) in
    let* total_subst = Substitution.compose subst1 subst2 in
    return (total_subst, Substitution.apply subst2 ty1)
;;

let infer_structure_item env = function
  | SEval expr ->
    let* subst, _ = infer_expr env expr in
    let updated_env = TypeEnv.apply subst env in
    return (subst, updated_env)
  | SValue (true, (PatVariable x, expr), []) ->
    let* expr =
      match expr with
      | ExpLambda _ -> return expr
      | _ -> fail (RHS "Right-hand side of let rec must be a lambda expression")
    in
    let* tv = fresh_var in
    let env = TypeEnv.extend env x (S (IntSet.empty, tv)) in
    let* subst, ty = infer_expr env expr in
    let* subst2 = Substitution.unify (Substitution.apply subst tv) ty in
    let* composed_subst = Substitution.compose subst subst2 in
    let env2 = TypeEnv.apply composed_subst env in
    let generalized_ty = generalize env2 (Substitution.apply composed_subst ty) in
    let env = TypeEnv.extend env2 x generalized_ty in
    return (composed_subst, env)
  | SValue (true, value_binding, value_bindings) ->
    let all_bindings = value_binding :: value_bindings in
    let* env_with_placeholders =
      List.fold_left
        ~f:(fun acc_env (pat, _) ->
          let* pat =
            match pat with
            | PatVariable _ -> return pat
            | _ ->
              fail (LHS "Only variables are allowed on the left-hand side of let rec")
          in
          let* env_acc = acc_env in
          let* subst_pat, _, env_pat = infer_pattern env_acc pat in
          let extended_env = TypeEnv.apply subst_pat env_pat in
          return extended_env)
        ~init:(return env)
        all_bindings
    in
    let* env_ext, subst_acc =
      List.fold_left
        ~f:(fun acc_env (ty_pattern, expr) ->
          let* expr =
            match expr with
            | ExpLambda _ -> return expr
            | _ -> fail (RHS "Right-hand side of let rec must be a lambda expression")
          in
          let* env_acc, _ = acc_env in
          let* subst_expr, ty_expr = infer_expr env_acc expr in
          let* subst_pat, ty_pat, env_pat = infer_pattern env_acc ty_pattern in
          let* subst = Substitution.compose subst_expr subst_pat in
          let* unified_subst = Substitution.unify ty_expr ty_pat in
          let* combined_subst = Substitution.compose subst unified_subst in
          let extended_env = TypeEnv.apply combined_subst env_pat in
          return (extended_env, combined_subst))
        ~init:(return (env_with_placeholders, Substitution.empty))
        all_bindings
    in
    return (subst_acc, env_ext)
  | SValue (false, (PatVariable x, expr), _) ->
    let* subst, ty = infer_expr env expr in
    let env2 = TypeEnv.apply subst env in
    let generalized_ty = generalize env2 ty in
    let env = TypeEnv.extend (TypeEnv.apply subst env) x generalized_ty in
    return (subst, env)
  | SValue (false, (pattern, expr), _) ->
    let* subst_expr, ty = infer_expr env expr in
    let* subst_pat, ty_pat, env_pat = infer_pattern env pattern in
    let* combined_subst = Substitution.compose subst_expr subst_pat in
    let* unified_subst =
      Substitution.unify (Substitution.apply combined_subst ty_pat) ty
    in
    let updated_env = TypeEnv.apply unified_subst env_pat in
    let* final_subst = Substitution.compose unified_subst combined_subst in
    return (final_subst, updated_env)
;;

let infer_structure env structure =
  let rec process_structure env subst = function
    | [] -> return (subst, env)
    | item :: rest ->
      let* subst1, env1 = infer_structure_item env item in
      let* composed_subst = Substitution.compose subst subst1 in
      process_structure env1 composed_subst rest
  in
  process_structure env Substitution.empty structure
;;

let infer_simple_expression expr =
  Result.map ~f:snd (run (infer_expr TypeEnv.initial_env expr))
;;

let run_infer str = Result.map ~f:snd (run (infer_structure TypeEnv.initial_env str))
