(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open InferCore
open InferCore.Result
open InferCore.Result.Syntax

module Infer = struct
  let fresh_var = fresh >>| fun n -> AVar n

  let instantiate : scheme -> type_annot Result.t =
    fun (S (xs, type_annot)) ->
    VarSet.fold
      (fun name typ ->
        let* typ = typ in
        let* f1 = fresh_var in
        let* s = Subst.singleton name f1 in
        return (Subst.apply s typ))
      xs
      (return type_annot)
  ;;

  let generalize env ty =
    let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
    S (free, ty)
  ;;

  let generalize_rec env ty x =
    let env = TypeEnv.remove env x in
    generalize env ty
  ;;

  let infer_pattern =
    let rec helper env = function
      | PAny ->
        let* fresh = fresh_var in
        return (env, fresh)
      | PConstant c ->
        (match c with
         | CInt _ -> return (env, int_type)
         | CBool _ -> return (env, bool_type)
         | CString _ -> return (env, string_type)
         | CUnit -> return (env, unit_type)
         | CNil ->
           let* fresh = fresh_var in
           return (env, list_type fresh))
      | PVar x ->
        let* fresh = fresh_var in
        let env = TypeEnv.extend env x (S (VarSet.empty, fresh)) in
        return (env, fresh)
      | PCons (p1, p2) ->
        let* env1, t1 = helper env p1 in
        let* env2, t2 = helper env1 p2 in
        let* sub = Subst.unify (list_type t1) t2 in
        let env = TypeEnv.apply sub env2 in
        return (env, Subst.apply sub t2)
      | PTuple (p1, p2, pl) ->
        let* env, tl =
          List.fold_left
            ~f:(fun acc pat ->
              let* env1, tl = acc in
              let* env2, t = helper env1 pat in
              return (env2, t :: tl))
            ~init:(return (env, []))
            (p1 :: p2 :: pl)
        in
        return (env, tuple_type (List.rev tl))
      | PType (pat, an) ->
        let* env1, t1 = helper env pat in
        let* sub = Subst.unify t1 an in
        let env = TypeEnv.apply sub env1 in
        return (env, Subst.apply sub t1)
      | _ -> fail `Pattern_matching_error
    in
    helper
  ;;

  let infer_expression =
    let rec helper env = function
      | ExprConstant c ->
        (match c with
         | CInt _ -> return (Subst.empty, int_type)
         | CBool _ -> return (Subst.empty, bool_type)
         | CString _ -> return (Subst.empty, string_type)
         | CUnit -> return (Subst.empty, unit_type)
         | CNil ->
           let* fresh = fresh_var in
           return (Subst.empty, list_type fresh))
      | ExprVariable x ->
        (match TypeEnv.find x env with
         | Some s ->
           let* t = instantiate s in
           return (Subst.empty, t)
         | None -> fail (`Unbound x))
      | ExprBinOperation (op, e1, e2) ->
        let* sub1, t1 = helper env e1 in
        let* sub2, t2 = helper (TypeEnv.apply sub1 env) e2 in
        let* e1t, e2t, et =
          match op with
          | Mul | Div | Add | Sub -> return (int_type, int_type, int_type)
          | Eq | Neq | Lt | Lte | Gt | Gte ->
            let* fresh = fresh_var in
            return (fresh, fresh, bool_type)
          | And | Or -> return (bool_type, bool_type, bool_type)
        in
        let* sub3 = Subst.unify (Subst.apply sub2 t1) e1t in
        let* sub4 = Subst.unify (Subst.apply sub3 t2) e2t in
        let* sub = Subst.compose_all [ sub1; sub2; sub3; sub4 ] in
        return (sub, Subst.apply sub et)
      | ExprUnOperation (op, e) ->
        let* sub1, t1 = helper env e in
        let* et =
          match op with
          | UnaryPlus | UnaryMinus -> return int_type
          | UnaryNeg -> return bool_type
        in
        let* sub2 = Subst.unify (Subst.apply sub1 t1) et in
        let* sub = Subst.compose_all [ sub1; sub2 ] in
        return (sub, Subst.apply sub et)
      | ExprIf (i, t, e) ->
        let* sub1, t1 = helper env i in
        let* sub2, t2 = helper (TypeEnv.apply sub1 env) t in
        let* sub3, t3 =
          match e with
          | Some e ->
            let* sub3, t3 = helper (TypeEnv.apply sub2 env) e in
            return (sub3, t3)
          | None -> return (Subst.empty, unit_type)
        in
        let* sub4 = Subst.unify t1 bool_type in
        let* sub5 = Subst.unify t2 t3 in
        let* sub = Subst.compose_all [ sub1; sub2; sub3; sub4; sub5 ] in
        return (sub, Subst.apply sub t2)
      | ExprMatch (e, c, cl) ->
        let* sub1, t1 = helper env e in
        let env = TypeEnv.apply sub1 env in
        let* fresh = fresh_var in
        let* sub, t =
          List.fold_left
            ~f:(fun acc (pat, exp) ->
              let* sub1, t = acc in
              let* env1, pt = infer_pattern env pat in
              let* sub2 = Subst.unify t1 pt in
              let env2 = TypeEnv.apply sub2 env1 in
              let* sub3, t' = helper env2 exp in
              let* sub4 = Subst.unify t' t in
              let* sub = Subst.compose_all [ sub1; sub2; sub3; sub4 ] in
              return (sub, Subst.apply sub t))
            ~init:(return (sub1, fresh))
            (c :: cl)
        in
        return (sub, t)
      | ExprLet (NonRec, (PVar x1, e1), [], e) ->
        let* s1, t1 = helper env e1 in
        let env = TypeEnv.apply s1 env in
        let s = generalize env t1 in
        let* s2, t2 = helper (TypeEnv.extend env x1 s) e in
        let* s = Subst.compose s1 s2 in
        return (s, t2)
      | ExprLet (Rec, (PVar x1, e1), [], e) ->
        let* fresh = fresh_var in
        let env1 = TypeEnv.extend env x1 (S (VarSet.empty, fresh)) in
        let* s, t = helper env1 e1 in
        let* s1 = Subst.unify t fresh in
        let* s2 = Subst.compose s s1 in
        let env = TypeEnv.apply s2 env in
        let t = Subst.apply s2 t in
        let s = generalize_rec env t x1 in
        let env = TypeEnv.extend env x1 s in
        let* sub, t = helper env e in
        let* sub = Subst.compose s2 sub in
        return (sub, t)
      | ExprFun (p, e) ->
        let* env, t = infer_pattern env p in
        let* sub, t1 = helper env e in
        return (sub, Subst.apply sub (arrow t t1))
      | ExprTuple (e1, e2, el) ->
        let* sub, t =
          List.fold_left
            ~f:(fun acc e ->
              let* sub, t = acc in
              let* sub1, t1 = helper env e in
              let* sub2 = Subst.compose sub sub1 in
              return (sub2, t1 :: t))
            ~init:(return (Subst.empty, []))
            (e1 :: e2 :: el)
        in
        return (sub, tuple_type (List.rev_map ~f:(Subst.apply sub) t))
      | ExprCons (e1, e2) ->
        let* s1, t1 = helper env e1 in
        let* s2, t2 = helper env e2 in
        let* sub = Subst.unify (list_type t1) t2 in
        let t = Subst.apply sub t2 in
        let* sub = Subst.compose_all [ s1; s2; sub ] in
        return (sub, t)
      | ExprApply (e1, e2) ->
        let* fresh = fresh_var in
        let* s1, t1 = helper env e1 in
        let* s2, t2 = helper (TypeEnv.apply s1 env) e2 in
        let* s3 = Subst.unify (arrow t2 fresh) (Subst.apply s2 t1) in
        let* sub = Subst.compose_all [ s1; s2; s3 ] in
        let t = Subst.apply sub fresh in
        return (sub, t)
      | _ -> fail `Not_implemented
    in
    helper
  ;;

  let infer_structure_item env = function
    | SValue (Rec, (PVar x1, e1), []) ->
      let* fresh = fresh_var in
      let sc = S (VarSet.empty, fresh) in
      let env = TypeEnv.extend env x1 sc in
      let* s1, t1 = infer_expression env e1 in
      let* s2 = Subst.unify t1 fresh in
      let* s3 = Subst.compose s1 s2 in
      let env = TypeEnv.apply s3 env in
      let t2 = Subst.apply s3 t1 in
      let sc = generalize_rec env t2 x1 in
      let env = TypeEnv.extend env x1 sc in
      return env
    | SValue (NonRec, (PVar x1, e1), []) ->
      let* s, t = infer_expression env e1 in
      let env = TypeEnv.apply s env in
      let sc = generalize env t in
      let env = TypeEnv.extend env x1 sc in
      return env
    | SEval e ->
      let* _, _ = infer_expression env e in
      return env
    | _ -> fail `Not_implemented
  ;;

  let infer_structure (structure : structure) =
    List.fold_left
      ~f:(fun acc item ->
        let* env = acc in
        let* env = infer_structure_item env item in
        return env)
      ~init:(return TypeEnv.empty)
      structure
  ;;
end

let run_infer s = run (Infer.infer_structure s)
