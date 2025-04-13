(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Forest.Ast
open Forest.TypesTree
open InfAuxilary
open InfAuxilary.FreshResult

let fresh_var =
  let* fresh = fresh in
  return @@ var_typ fresh
;;

(* Makes type scheme copy. *)
let instantiate (Scheme (vars, ty)) =
  VarSet.fold
    (fun old_var ty ->
      let* ty = ty in
      let* fresh_var = fresh_var in
      let* sub = Subst.singleton old_var fresh_var in
      return @@ Subst.apply sub ty)
    vars
    (return ty)
;;

(* Generalizes a type by quantifying over its free variables that are not bound in the environment. *)
let generalize env typ =
  let vars = VarSet.diff (Type.type_vars typ) (TypeEnv.free env) in
  Scheme (vars, typ)
;;

let inf_const = function
  | Int _ -> int_typ
  | Bool _ -> bool_typ
  | Str _ -> string_typ
  | Unit -> unit_typ
;;

let rec inf_pat env = function
  | PatAny ->
    let* ty_var = fresh_var in
    return (Subst.empty, env, ty_var)
  | PatConst pat -> return (Subst.empty, env, inf_const pat)
  | PatListCons (pat1, pat2) ->
    let* sub1, env1, ty1 = inf_pat env pat1 in
    let* _, env2, ty2 = inf_pat (TypeEnv.apply sub1 env1) pat2 in
    let* final_sub = Subst.unify (list_typ ty1) ty2 in
    let env = TypeEnv.apply final_sub env2 in
    return (final_sub, env, Subst.apply final_sub ty2)
  | PatVar name ->
    let* ty_var = fresh_var in
    let sch = Scheme (VarSet.empty, ty_var) in
    let env = TypeEnv.extend env name sch in
    return (Subst.empty, env, ty_var)
  | PatTup (pat1, pat2, pats) ->
    let pats = pat1 :: pat2 :: pats in
    let* sub, env, ty =
      List.fold_left
        (fun acc pat ->
          let* sub1, env, tys = acc in
          let* sub2, env_upd, ty1 = inf_pat env pat in
          let* final_sub = Subst.compose sub1 sub2 in
          return (final_sub, env_upd, ty1 :: tys))
        (return (Subst.empty, env, []))
        pats
    in
    return (sub, env, tup_typ ty)
  | PatList [] ->
    let* ty_var = fresh_var in
    return (Subst.empty, env, list_typ ty_var)
  | PatList pat ->
    let* fresh_typ = fresh_var in
    let* sub, env =
      List.fold_left
        (fun acc pat ->
          let* prev_sub, prev_env = acc in
          let* new_sub, curr_env, el_typ = inf_pat prev_env pat in
          let* unified_sub = Subst.compose prev_sub new_sub in
          let* final_sub = Subst.unify (Subst.apply prev_sub fresh_typ) el_typ in
          let* sub = Subst.compose unified_sub final_sub in
          return (sub, TypeEnv.apply final_sub curr_env))
        (return (Subst.empty, env))
        pat
    in
    return (sub, env, list_typ @@ Subst.apply sub fresh_typ)
  | PatType (pat, ty) ->
    let* sub, env, ty1 = inf_pat env pat in
    let ty = Subst.apply sub ty in
    let* sub = Subst.unify ty1 ty in
    return (sub, TypeEnv.apply sub env, Subst.apply sub ty)
;;

(* Returns operands type and result type *)
let binop_signature = function
  | Eq | Ne | Lt | Le | Gt | Ge ->
    let* ty_var = fresh_var in
    return (ty_var, bool_typ)
  | Mul | Div | Add | Sub -> return (int_typ, int_typ)
  | And | Or -> return (bool_typ, bool_typ)
;;

(* Checks that the left-hand side of a 'let rec' is a variable. *)
let check_rec_lhs pat =
  match pat with
  | PatVar _ -> return pat
  | _ -> fail @@ IllegalLHS
;;

(* Checks that the right-hand side of a 'let rec' is a function. *)
let check_rec_rhs expr =
  match expr with
  | Fun _ -> return expr
  | _ -> fail @@ IllegalRHS
;;

let rec inf_expr env = function
  | Var name ->
    (match TypeEnv.find env name with
     | Some sch ->
       let* ty = instantiate sch in
       return (Subst.empty, ty)
     | None -> fail @@ UnboundVariable name)
  | Const exp -> return (Subst.empty, inf_const exp)
  | Unary (op, exp) ->
    let* sub1, exp_ty = inf_expr env exp in
    let* op_ty =
      match op with
      | Minus | Plus -> return int_typ
      | Not -> return bool_typ
    in
    let* sub2 = Subst.unify exp_ty op_ty in
    let* sub = Subst.compose_many_sub [ sub1; sub2 ] in
    return @@ (sub, Subst.apply sub op_ty)
  | BinOp (op, exp1, exp2) ->
    let* args_ty, res_ty = binop_signature op in
    let* sub1, ty1 = inf_expr env exp1 in
    let* sub2, ty2 = inf_expr (TypeEnv.apply sub1 env) exp2 in
    let* sub3 = Subst.unify (Subst.apply sub2 ty1) args_ty in
    let* sub4 = Subst.unify (Subst.apply sub3 ty2) args_ty in
    let* final_sub = Subst.compose_many_sub [ sub1; sub2; sub3; sub4 ] in
    return (final_sub, Subst.apply final_sub res_ty)
  | Option (Some exp) ->
    let* sub, ty = inf_expr env exp in
    return (sub, option_typ ty)
  | Option None ->
    let* ty_var = fresh_var in
    return (Subst.empty, option_typ ty_var)
  | List exp ->
    (match exp with
     | [] ->
       let* ty_var = fresh_var in
       return (Subst.empty, list_typ ty_var)
     | _ :: _ ->
       let* s, tys =
         List.fold_left
           (fun acc e ->
             let* acc_sub, acc_ty = acc in
             let* inf_sub, inf_ty = inf_expr (TypeEnv.apply acc_sub env) e in
             let* composed_sub = Subst.compose inf_sub acc_sub in
             return (composed_sub, inf_ty :: acc_ty))
           (return (Subst.empty, []))
           exp
       in
       return (s, list_typ (List.hd tys)))
  | Let (NonRec, Binding (PatVar x, exp1), _, exp2) ->
    let* sub1, ty1 = inf_expr env exp1 in
    let ty_copy = generalize (TypeEnv.apply sub1 env) ty1 in
    let env3 = TypeEnv.extend env x ty_copy in
    let* sub2, ty2 = inf_expr (TypeEnv.apply sub1 env3) exp2 in
    let* final_sub = Subst.compose sub1 sub2 in
    return (final_sub, ty2)
  | Let (NonRec, Binding (pat, exp1), bindings, exp2) ->
    let* s1, t1 = inf_expr env exp1 in
    let* s2, env1, ty_pat = inf_pat env pat in
    let* sub1 = Subst.compose s1 s2 in
    let* unified_sub = Subst.unify (Subst.apply sub1 ty_pat) t1 in
    let initial_env = TypeEnv.apply unified_sub env1 in
    let* extended_env =
      List.fold_left
        (fun acc_env (Binding (pat, expr)) ->
          let* acc_env = acc_env in
          let* sub_bind, ty_bind = inf_expr acc_env expr in
          let* sub_pat, env_pat, ty_pat = inf_pat acc_env pat in
          let* combined_subst = Subst.compose sub_bind sub_pat in
          let* final_subst = Subst.unify (Subst.apply combined_subst ty_pat) ty_bind in
          let updated_env = TypeEnv.merge_envs final_subst acc_env env_pat in
          return updated_env)
        (return initial_env)
        bindings
    in
    let* sub3, ty2 = inf_expr extended_env exp2 in
    let* full_subst = Subst.compose_many_sub [ sub3; unified_sub; sub1 ] in
    return (full_subst, ty2)
  | Let (Rec, Binding (PatVar x, exp1), [], exp2) ->
    let* exp1 = check_rec_rhs exp1 in
    let* var_ty = fresh_var in
    let env2 = TypeEnv.extend env x (Scheme (VarSet.empty, var_ty)) in
    let* s1, t1 = inf_expr env2 exp1 in
    let* s2 = Subst.unify (Subst.apply s1 var_ty) t1 in
    let* sub_final = Subst.compose s1 s2 in
    let env3 = TypeEnv.apply sub_final env in
    let env4 = TypeEnv.apply s1 env3 in
    let ty_gen = generalize env4 (Subst.apply sub_final var_ty) in
    let* s3, t2 = inf_expr (TypeEnv.extend env4 x ty_gen) exp2 in
    let* s_final = Subst.compose sub_final s3 in
    return (s_final, t2)
  | Let (Rec, binding, bindings, exp2) ->
    let* env_ext, sub_acc =
      List.fold_left
        (fun acc_env (Binding (pat, expr)) ->
          let* expr = check_rec_rhs expr in
          let* pattern = check_rec_lhs pat in
          let* env_acc, _ = acc_env in
          let* s_expr, ty_expr = inf_expr env_acc expr in
          let* s_pat, env_pat, ty_pat = inf_pat env_acc pattern in
          let* subst = Subst.compose s_expr s_pat in
          let* unified_sub = Subst.unify ty_expr ty_pat in
          let* combined_sub = Subst.compose subst unified_sub in
          let extended_env = TypeEnv.apply combined_sub env_pat in
          return (extended_env, combined_sub))
        (return (env, Subst.empty))
        (binding :: bindings)
    in
    let* sub2, t2 = inf_expr env_ext exp2 in
    let* final_subst = Subst.compose sub_acc sub2 in
    return (final_subst, t2)
  | App (fun_exp, arg_exp) ->
    let* sub1, fun_ty = inf_expr env fun_exp in
    let* sub2, arg_ty = inf_expr (TypeEnv.apply sub1 env) arg_exp in
    let* res_typ = fresh_var in
    let ty1 = Subst.apply sub2 fun_ty in
    let ty2 = arrow_typ arg_ty res_typ in
    let* sub3 = Subst.unify ty1 ty2 in
    let* sub = Subst.compose_many_sub [ sub1; sub2; sub3 ] in
    let ty = Subst.apply sub res_typ in
    return (sub, ty)
  | Fun (pat, pats, exp) ->
    let* env, tys =
      List.fold_left
        (fun acc pat ->
          let* env, pat_types = acc in
          let* _, env, typ = inf_pat env pat in
          return (env, typ :: pat_types))
        (return (env, []))
        (pat :: pats)
    in
    let* sub, ty = inf_expr env exp in
    let arrow_type =
      List.fold_right
        (fun pat_type acc -> TypArrow (Subst.apply sub pat_type, acc))
        (List.rev tys)
        ty
    in
    return (sub, arrow_type)
  | Branch (cond, br1, br2) ->
    let* sub1, ty1 = inf_expr env cond in
    let* sub2, ty2 = inf_expr (TypeEnv.apply sub1 env) br1 in
    let* sub3, ty3 = inf_expr (TypeEnv.apply sub2 env) br2 in
    let* sub4 = Subst.unify ty1 bool_typ in
    let* sub5 = Subst.unify ty2 ty3 in
    let* final_sub = Subst.compose_many_sub [ sub1; sub2; sub3; sub4; sub5 ] in
    let ty = Subst.apply final_sub ty3 in
    return (final_sub, ty)
  | Tup (el1, el2, els) ->
    let* sub, ty =
      List.fold_left
        (fun acc expr ->
          let* sub, ty = acc in
          let* sub1, ty1 = inf_expr (TypeEnv.apply sub env) expr in
          let* sub2 = Subst.compose sub sub1 in
          return (sub2, ty1 :: ty))
        (return (Subst.empty, []))
        (el1 :: el2 :: els)
    in
    return (sub, tup_typ (List.rev_map (Subst.apply sub) ty))
  | Type (el, typ) ->
    let* sub1, typ1 = inf_expr env el in
    let* sub2 = Subst.unify typ1 (Subst.apply sub1 typ) in
    let* final_sub = Subst.compose sub1 sub2 in
    return (final_sub, Subst.apply sub2 typ1)
;;

let inf_struct_item env = function
  | Eval expr ->
    let* subst, _ = inf_expr env expr in
    let updated_env = TypeEnv.apply subst env in
    return (subst, updated_env)
  | Value (Rec, Binding (PatVar x, expr), []) ->
    let* expr = check_rec_rhs expr in
    let* tv = fresh_var in
    let env = TypeEnv.extend env x (Scheme (VarSet.empty, tv)) in
    let* subst, inferred_ty = inf_expr env expr in
    let* subst2 = Subst.unify (Subst.apply subst tv) inferred_ty in
    let* composed_subst = Subst.compose subst subst2 in
    let env2 = TypeEnv.apply composed_subst env in
    let generalized_ty = generalize env2 (Subst.apply composed_subst inferred_ty) in
    let env = TypeEnv.extend env2 x generalized_ty in
    return (composed_subst, env)
  | Value (Rec, binding, bindings) ->
    let all_bindings = binding :: bindings in
    let* env_with_placeholders =
      List.fold_left
        (fun acc_env (Binding (pattern, _)) ->
          let* ty_pattern = check_rec_lhs pattern in
          let* env_acc = acc_env in
          let* s_pat, env_pat, _ = inf_pat env_acc ty_pattern in
          let extended_env = TypeEnv.apply s_pat env_pat in
          return extended_env)
        (return env)
        all_bindings
    in
    let* env_ext, s_acc =
      List.fold_left
        (fun acc_env (Binding (ty_pattern, expr)) ->
          let* expr = check_rec_rhs expr in
          let* env_acc, _ = acc_env in
          let* s_expr, t_expr = inf_expr env_acc expr in
          let* s_pat, env_pat, t_pat = inf_pat env_acc ty_pattern in
          let* subst = Subst.compose s_expr s_pat in
          let* unified_subst = Subst.unify t_expr t_pat in
          let* combined_subst = Subst.compose subst unified_subst in
          let extended_env = TypeEnv.apply combined_subst env_pat in
          return (extended_env, combined_subst))
        (return (env_with_placeholders, Subst.empty))
        all_bindings
    in
    return (s_acc, env_ext)
  | Value (NonRec, Binding (PatVar x, expr), _) ->
    let* subst, inferred_ty = inf_expr env expr in
    let env2 = TypeEnv.apply subst env in
    let generalized_ty = generalize env2 inferred_ty in
    let env = TypeEnv.extend (TypeEnv.apply subst env) x generalized_ty in
    return (subst, env)
  | Value (NonRec, Binding (pattern, expr), _) ->
    let* subst_expr, inferred_ty = inf_expr env expr in
    let* subst_pat, env_pat, t_pat = inf_pat env pattern in
    let* combined_subst =
      let* composed = Subst.compose subst_expr subst_pat in
      return composed
    in
    let* unified_subst = Subst.unify (Subst.apply combined_subst t_pat) inferred_ty in
    let updated_env = TypeEnv.apply unified_subst env_pat in
    let* final_subst = Subst.compose unified_subst combined_subst in
    return (final_subst, updated_env)
;;

let inf_program env structure =
  let rec process_structure env subst = function
    | [] -> return (subst, env)
    | item :: rest ->
      let* subst', env' = inf_struct_item env item in
      let* composed_subst = Subst.compose subst subst' in
      process_structure env' composed_subst rest
  in
  process_structure env Subst.empty structure
;;

let start_env =
  let print_env =
    TypeEnv.extend
      TypeEnv.empty
      "print_int"
      (Scheme (VarSet.empty, arrow_typ int_typ unit_typ))
  in
  let env =
    TypeEnv.extend
      print_env
      "print_endline"
      (Scheme (VarSet.empty, arrow_typ string_typ unit_typ))
  in
  env
;;

let inference tree = run (inf_program start_env tree)
