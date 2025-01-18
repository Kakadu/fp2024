(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open AstPrinter
open InferencerCore
open InferencerCore.Result
open InferencerCore.Result.Syntax

let fresh_var = fresh >>| fun n -> AVar n

let instantiate : Scheme.t -> type_annot Result.t =
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
  Scheme.S (free, ty)
;;

let generalize_rec env ty x =
  let env = TypeEnv.remove env x in
  generalize env ty
;;

let rec infer_pattern env = function
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
    let env = TypeEnv.extend env x (Scheme.S (VarSet.empty, fresh)) in
    return (env, fresh)
  | PCons (p1, p2) ->
    let* env1, t1 = infer_pattern env p1 in
    let* env2, t2 = infer_pattern env1 p2 in
    let* sub = Subst.unify (list_type t1) t2 in
    let env = TypeEnv.apply sub env2 in
    return (env, Subst.apply sub t2)
  | PTuple (p1, p2, pl) ->
    let* env, tl =
      List.fold_left
        ~f:(fun acc pat ->
          let* env1, tl = acc in
          let* env2, t = infer_pattern env1 pat in
          return (env2, t :: tl))
        ~init:(return (env, []))
        (p1 :: p2 :: pl)
    in
    return (env, tuple_type (List.rev tl))
  | PList (p1, rest) ->
    let* env1, t1 = infer_pattern env p1 in
    let* env2, t_list =
      List.fold_left
        ~f:(fun acc pat ->
          let* env_acc, _ = acc in
          let* env_next, t_next = infer_pattern env_acc pat in
          let* sub = Subst.unify t1 t_next in
          let env_updated = TypeEnv.apply sub env_next in
          return (env_updated, Subst.apply sub t1))
        ~init:(return (env1, list_type t1))
        rest
    in
    return (env2, t_list)
  | POption (Some p) ->
    let* env1, t1 = infer_pattern env p in
    return (env1, AOption t1)
  | POption None ->
    let* fresh = fresh_var in
    return (env, AOption fresh)
  | PType (p, t) ->
    let* env1, t1 = infer_pattern env p in
    let* sub = Subst.unify t1 t in
    let env = TypeEnv.apply sub env1 in
    return (env, Subst.apply sub t1)
;;

let rec infer_expression env = function
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
    let* sub1, t1 = infer_expression env e1 in
    let* sub2, t2 = infer_expression (TypeEnv.apply sub1 env) e2 in
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
    let* sub1, t1 = infer_expression env e in
    let* et =
      match op with
      | UnaryPlus | UnaryMinus -> return int_type
      | UnaryNeg -> return bool_type
    in
    let* sub2 = Subst.unify (Subst.apply sub1 t1) et in
    let* sub = Subst.compose_all [ sub1; sub2 ] in
    return (sub, Subst.apply sub et)
  | ExprIf (i, t, e) ->
    let* sub1, t1 = infer_expression env i in
    let* sub2, t2 = infer_expression (TypeEnv.apply sub1 env) t in
    let* sub3, t3 =
      match e with
      | Some e ->
        let* sub3, t3 = infer_expression (TypeEnv.apply sub2 env) e in
        return (sub3, t3)
      | None -> return (Subst.empty, unit_type)
    in
    let* sub4 = Subst.unify t1 bool_type in
    let* sub5 = Subst.unify t2 t3 in
    let* sub = Subst.compose_all [ sub1; sub2; sub3; sub4; sub5 ] in
    return (sub, Subst.apply sub t2)
  | ExprMatch (e, c, cl) ->
    let* sub1, t1 = infer_expression env e in
    let env = TypeEnv.apply sub1 env in
    let* fresh = fresh_var in
    let* sub, t =
      List.fold_left
        ~f:(fun acc (pat, exp) ->
          let* sub1, t = acc in
          let* env1, pt = infer_pattern env pat in
          let* sub2 = Subst.unify t1 pt in
          let env2 = TypeEnv.apply sub2 env1 in
          let* sub3, t' = infer_expression env2 exp in
          let* sub4 = Subst.unify t' t in
          let* sub = Subst.compose_all [ sub1; sub2; sub3; sub4 ] in
          return (sub, Subst.apply sub t))
        ~init:(return (sub1, fresh))
        (c :: cl)
    in
    return (sub, t)
  | ExprFunction (c, cl) ->
    let p, e = c in
    let* _, t1 = infer_pattern env p in
    let* s1, _ = infer_expression env e in
    let env = TypeEnv.apply s1 env in
    let* fresh = fresh_var in
    let* sub, t =
      List.fold_left
        ~f:(fun acc (pat, exp) ->
          let* sub1, t = acc in
          let* env1, pt = infer_pattern env pat in
          let* sub2 = Subst.unify t1 pt in
          let env2 = TypeEnv.apply sub2 env1 in
          let* sub3, t' = infer_expression env2 exp in
          let* sub4 = Subst.unify t' t in
          let* sub = Subst.compose_all [ sub1; sub2; sub3; sub4 ] in
          return (sub, Subst.apply sub t))
        ~init:(return (s1, fresh))
        (c :: cl)
    in
    return (sub, Subst.apply sub (fun_type t1 t))
  | ExprLet (NonRec, b, bl, e) ->
    let bindings = b :: bl in
    let* env2 = infer_non_rec_binding_list env bindings in
    let* s, t = infer_expression env2 e in
    return (s, t)
  | ExprLet (Rec, b, bl, e) ->
    let bindings = b :: bl in
    let* env2 = infer_rec_binding_list env bindings in
    let* s, t = infer_expression env2 e in
    return (s, t)
  | ExprFun (p, e) ->
    let* env, t = infer_pattern env p in
    let* sub, t1 = infer_expression env e in
    return (sub, Subst.apply sub (fun_type t t1))
  | ExprTuple (e1, e2, el) ->
    let* sub, t =
      List.fold_left
        ~f:(fun acc e ->
          let* sub, t = acc in
          let* sub1, t1 = infer_expression env e in
          let* sub2 = Subst.compose sub sub1 in
          return (sub2, t1 :: t))
        ~init:(return (Subst.empty, []))
        (e1 :: e2 :: el)
    in
    return (sub, tuple_type (List.rev_map ~f:(Subst.apply sub) t))
  | ExprList (l, ls) ->
    (match l :: ls with
     | [] ->
       let* fresh = fresh_var in
       return (Subst.empty, list_type fresh)
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
       return (sr, AList tr))
  | ExprCons (e1, e2) ->
    let* s1, t1 = infer_expression env e1 in
    let* s2, t2 = infer_expression env e2 in
    let* sub = Subst.unify (list_type t1) t2 in
    let t = Subst.apply sub t2 in
    let* sub = Subst.compose_all [ s1; s2; sub ] in
    return (sub, t)
  | ExprApply (e1, e2) ->
    let* fresh = fresh_var in
    let* s1, t1 = infer_expression env e1 in
    let* s2, t2 = infer_expression (TypeEnv.apply s1 env) e2 in
    let* s3 = Subst.unify (fun_type t2 fresh) (Subst.apply s2 t1) in
    let* sub = Subst.compose_all [ s1; s2; s3 ] in
    let t = Subst.apply sub fresh in
    return (sub, t)
  | ExprOption (Some eo) ->
    let* s, t = infer_expression env eo in
    return (s, AOption t)
  | ExprOption None ->
    let* t = fresh_var in
    return (Subst.empty, AOption t)
  | ExprType (e, t) ->
    let* s1, t1 = infer_expression env e in
    let* s2 = Subst.unify t t1 in
    return (s2, Subst.apply s1 t1)

and infer_non_rec_binding_list env (bl : binding list) =
  let* env2 =
    Base.List.fold_left
      ~f:(fun env b ->
        let* env = env in
        let p, e = b in
        match p with
        | PVar x ->
          let* s, t = infer_expression env e in
          let env = TypeEnv.apply s env in
          let sc = generalize env t in
          let env = TypeEnv.extend env x sc in
          return env
        | PConstant CUnit ->
          let* _, t1 = infer_pattern env p in
          let* _, t2 = infer_expression env e in
          let* sub = Subst.unify t1 t2 in
          let env = TypeEnv.apply sub env in
          return env
        | POption (Some (PVar x)) ->
          let* _, t1 = infer_pattern env p in
          let* _, t2 = infer_expression env e in
          let* sub = Subst.unify t1 t2 in
          let env = TypeEnv.apply sub env in
          let sc = generalize env t2 in
          let env = TypeEnv.extend env x sc in
          return env
        | POption None ->
          let* _, t1 = infer_pattern env p in
          let* _, t2 = infer_expression env e in
          let* sub = Subst.unify t1 t2 in
          let env = TypeEnv.apply sub env in
          return env
        | PAny ->
          let* _, _ = infer_expression env e in
          return env
        | PTuple _ ->
          let* _, t1 = infer_pattern env p in
          let* _, t2 = infer_expression env e in
          let* sub = Subst.unify t1 t2 in
          let env = TypeEnv.apply sub env in
          return env
        | PType (_, t) ->
          let* _, t2 = infer_expression env e in
          let* sub = Subst.unify t t2 in
          let env = TypeEnv.apply sub env in
          return env
        | _ -> return env)
      ~init:(return env)
      bl
  in
  return env2

and infer_rec_binding_list env (bl : binding list) =
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
          let* s2 = Subst.unify t1 fresh in
          let* s3 = Subst.compose s1 s2 in
          let env = TypeEnv.apply s3 env in
          let t2 = Subst.apply s3 t1 in
          let sc = generalize_rec env t2 x in
          let env = TypeEnv.extend env x sc in
          return env
        | _ -> fail `LeftHS)
      ~init:(return env)
      bl
  in
  return env2
;;

let infer_structure_item env = function
  | SValue (Rec, b, bl) ->
    let bindings = b :: bl in
    infer_rec_binding_list env bindings
  | SValue (NonRec, b, bl) ->
    let bindings = b :: bl in
    infer_non_rec_binding_list env bindings
  | SEval e ->
    let* _, _ = infer_expression env e in
    return env
;;

let infer_structure (structure : structure) =
  let env =
    TypeEnv.extend TypeEnv.empty "print_int" (Scheme.S (VarSet.empty, AFun (AInt, AUnit)))
  in
  let env =
    TypeEnv.extend env "print_endline" (Scheme.S (VarSet.empty, AFun (AString, AUnit)))
  in
  List.fold_left
    ~f:(fun acc item ->
      let* env = acc in
      let* env = infer_structure_item env item in
      return env)
    ~init:(return env)
    structure
;;

let run_inferencer (s : string) =
  let open Parser in
  match parse s with
  | Ok parsed ->
    let res = run (infer_structure parsed) in
    (match res with
     | Ok env ->
       Base.Map.iteri env ~f:(fun ~key ~data:(S (_, ty)) ->
         if String.equal key "print_int" || String.equal key "print_endline"
         then ()
         else Stdlib.Format.printf "val %s : %a\n" key pp_annot ty)
     | Error e -> Stdlib.Format.printf "Infer error: %a\n" pp_error e)
  | Error e -> Stdlib.Format.printf "Parsing error: %s\n" e
;;
