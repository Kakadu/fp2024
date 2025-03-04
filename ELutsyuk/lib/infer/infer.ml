(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open TypTree
open InfAuxilary
open InfAuxilary.FreshResult
open InfAuxilary.FreshResult.SyntSugar

let fresh_var =
  let* fresh = fresh in
  return @@ var_typ fresh
;;

(* Makes type scheme copy *)
let instantiate (Scheme (sch, ty)) =
  VarSet.fold
    (fun old_var ty ->
      let* ty = ty in
      let* fresh_var = fresh_var in
      let* sub = Subst.singleton old_var fresh_var in
      return @@ Subst.apply sub ty)
    sch
    (return ty)
;;

let generalize env typ =
  let scheme_vars = VarSet.diff (Type.type_vars typ) (TypeEnv.free_in_context env) in
  Scheme (scheme_vars, typ)
;;

let generalize_rec env ty x =
  let env = TypeEnv.remove x env in
  generalize env ty
;;

let inf_constant token =
  return
  @@ ( Subst.empty
     , match token with
       | Ast.Int _ -> int_typ
       | Ast.Bool _ -> bool_typ
       | Ast.Str _ -> string_typ
       | Ast.Unit -> unit_typ )
;;

let lookup_typ env id =
  match TypeEnv.find env id with
  | Some sch -> instantiate sch
  | None -> fail @@ UnboundVariable id
;;

let inf_id env id =
  match id with
  | "_" ->
    let* ty_var = fresh_var in
    return (Subst.empty, ty_var)
  | _ ->
    let* ty = lookup_typ env id in
    return (Subst.empty, ty)
;;

let rec inf_pat env = function
  | Ast.PatAny ->
    let* ty_var = fresh_var in
    return (env, ty_var)
  | Ast.PatConst pat ->
    let* _, ty = inf_constant pat in
    return (env, ty)
  | Ast.PatVar pat ->
    let* ty_var = fresh_var in
    let sch = Scheme (VarSet.empty, ty_var) in
    let env = TypeEnv.extend env pat sch in
    return (env, ty_var)
  | Ast.PatTup (pat1, pat2, pats) ->
    let pats = pat1 :: pat2 :: pats in
    let* env, ty =
      List.fold_left
        (fun acc pat ->
          let* env1, tys = acc in
          let* env_upd, ty1 = inf_pat env1 pat in
          return (env_upd, ty1 :: tys))
        (return (env, []))
        pats
    in
    return (env, tup_typ ty)
  | Ast.PatListCons (pat1, pat2) ->
    let* env1, ty1 = inf_pat env pat1 in
    let* env2, ty2 = inf_pat env1 pat2 in
    let* subst = Subst.unify (list_typ ty1) ty2 in
    let env = TypeEnv.apply subst env2 in
    return (env, Subst.apply subst ty2)
  | Ast.PatList [] ->
    let* ty_var = fresh_var in
    return (env, list_typ ty_var)
  | Ast.PatList (pat1 :: pats) ->
    let* env_start, ty_start = inf_pat env pat1 in
    let* env, ty =
      List.fold_left
        (fun acc pat ->
          let* env_acc, _ = acc in
          let* env_next, t_next = inf_pat env_acc pat in
          let* subst = Subst.unify ty_start t_next in
          let env_updated = TypeEnv.apply subst env_next in
          return (env_updated, Subst.apply subst ty_start))
        (return (env_start, list_typ ty_start))
        pats
    in
    return (env, ty)
;;

(* Returns operands type and result type *)
let binop_signature = function
  | Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge ->
    let* ty_var = fresh_var in
    return (ty_var, bool_typ)
  | Ast.Mul | Ast.Div | Ast.Add | Ast.Sub -> return (int_typ, int_typ)
  | Ast.And | Ast.Or -> return (bool_typ, bool_typ)
;;

let rec inf_expr env = function
  | Ast.Var exp -> inf_id env exp
  | Ast.Const exp -> inf_constant exp
  | Ast.BinOp (op, exp1, exp2) ->
    let* args_ty, res_ty = binop_signature op in
    let* sub1, ty1 = inf_expr env exp1 in
    let* sub2, ty2 = inf_expr env exp2 in
    let* sub3 = Subst.unify ty1 args_ty in
    let* sub4 = Subst.unify (Subst.apply sub1 ty2) args_ty in
    let* final_sub = Subst.compose_many_sub [ sub1; sub2; sub3; sub4 ] in
    return (final_sub, res_ty)
  | Ast.App (fun_exp, arg_exp) ->
    let* sub1, fun_ty = inf_expr env fun_exp in
    let upd_env = TypeEnv.apply sub1 env in
    let* sub2, arg_ty = inf_expr upd_env arg_exp in
    let* res_typ = fresh_var in
    let ty1 = Subst.apply sub2 fun_ty in
    let ty2 = arrow_typ arg_ty res_typ in
    let* sub3 = Subst.unify ty1 ty2 in
    let* subst = Subst.compose_many_sub [ sub1; sub2; sub3 ] in
    let ty = Subst.apply subst res_typ in
    return (subst, ty)
  | Ast.Fun (pat, exp) ->
    let* env1, pat_ty = inf_pat env pat in
    let* subst, exp_ty = inf_expr env1 exp in
    let ty = arrow_typ pat_ty exp_ty in
    let res_ty = Subst.apply subst ty in
    return (subst, res_ty)
  | Ast.Branch (cond, br1, br2) ->
    let* sub1, ty1 = inf_expr env cond in
    let* sub2, ty2 = inf_expr env br1 in
    let* sub3, ty3 = inf_expr env br2 in
    let* sub4 = Subst.unify ty1 bool_typ in
    let* sub5 = Subst.unify ty2 ty3 in
    let* final_sub = Subst.compose_many_sub [ sub1; sub2; sub3; sub4; sub5 ] in
    let ty = Subst.apply final_sub ty3 in
    return (final_sub, ty)
  | Ast.List exp_list ->
    let* ty_var = fresh_var in
    let rec inf_list acc = function
      | [] -> return (acc, list_typ ty_var)
      | exp1 :: rest ->
        let* sub1, ty1 = inf_expr env exp1 in
        let* sub2 = Subst.unify ty1 ty_var in
        let* sub = Subst.compose sub1 sub2 in
        inf_list (sub :: acc) rest
    in
    let* sub, ty = inf_list [] exp_list in
    let* sub = Subst.compose_many_sub sub in
    let ty = Subst.apply sub ty in
    return (sub, ty)
  | Ast.Let (NonRec, bind, binds, exp) ->
    let bindings = bind :: binds in
    let* env2 = inf_non_rec_binding_list env bindings in
    let* subst, ty = inf_expr env2 exp in
    return (subst, ty)
  | Ast.Let (Rec, bind, binds, exp) ->
    let bindings = bind :: binds in
    let* env2 = inf_rec_binding_list env bindings in
    let* subst, ty = inf_expr env2 exp in
    return (subst, ty)
  | Ast.Option (Some exp) ->
    let* subst, ty = inf_expr env exp in
    return (subst, option_typ ty)
  | Ast.Option None ->
    let* ty_var = fresh_var in
    return (Subst.empty, option_typ ty_var)
  | _ -> return (Subst.empty, TypConst TUnit)

and inf_non_rec_binding_list env binding_list =
  let* env2 =
    List.fold_left
      (fun env binding ->
        let* env = env in
        let Ast.{ pat : pat; expr : expr } = binding in
        match pat with
        | Ast.PatVar var ->
          let* subst, ty = inf_expr env expr in
          let env = TypeEnv.apply subst env in
          let sch = generalize env ty in
          let env = TypeEnv.extend env var sch in
          return env
        | Ast.PatConst Unit ->
          let* _, ty1 = inf_pat env pat in
          let* _, ty2 = inf_expr env expr in
          let* sub = Subst.unify ty1 ty2 in
          let env = TypeEnv.apply sub env in
          return env
        | Ast.PatAny ->
          let* _, _ = inf_expr env expr in
          return env
        | Ast.PatTup _ ->
          let* _, ty1 = inf_pat env pat in
          let* _, ty2 = inf_expr env expr in
          let* subst = Subst.unify ty1 ty2 in
          let env = TypeEnv.apply subst env in
          return env
        | _ -> return env)
      (return env)
      binding_list
  in
  return env2

and inf_rec_binding_list env binding_list =
  let* env2 =
    Base.List.fold_left
      binding_list
      ~init:(return env)
      ~f:(fun env Ast.{ pat : pat; expr : expr } ->
        let* env = env in
        match pat with
        | PatVar var ->
          let* ty_var = fresh_var in
          let sch = Scheme (VarSet.empty, ty_var) in
          let env = TypeEnv.extend env var sch in
          let* sub1, ty1 = inf_expr env expr in
          let* sub2 = Subst.unify ty1 ty_var in
          let* sub3 = Subst.compose sub1 sub2 in
          let env = TypeEnv.apply sub3 env in
          let ty2 = Subst.apply sub3 ty1 in
          let sch = generalize_rec var ty2 env in
          let env = TypeEnv.extend env var sch in
          return env
        | _ -> fail InvalidRecursivePattern)
  in
  return env2
;;

let inf_structure_item env = function
  | Ast.EvalExpr exp ->
    let* _ = inf_expr env exp in
    return env
  | Ast.Binding (Rec, bind, binds) ->
    let bindings = bind :: binds in
    inf_rec_binding_list env bindings
  | Ast.Binding (NonRec, bind, binds) ->
    let bindings = bind :: binds in
    inf_non_rec_binding_list env bindings
;;

let inf_program program =
  let env =
    TypeEnv.extend
      TypeEnv.empty
      "print_int"
      (Scheme (VarSet.empty, arrow_typ int_typ unit_typ))
  in
  let env =
    TypeEnv.extend
      env
      "print_endline"
      (Scheme (VarSet.empty, arrow_typ string_typ unit_typ))
  in
  List.fold_left
    (fun acc item ->
      let* env = acc in
      let* env = inf_structure_item env item in
      return env)
    (return env)
    program
;;

let run_infer program =
  match Parse.parse program with
  | Ok tree ->
    let result = run (inf_program tree) in
    (match result with
     | Ok env ->
       Base.Map.iteri env ~f:(fun ~key ~data:(Scheme (_, ty)) ->
         if String.equal key "print_int" || String.equal key "print_endline"
         then ()
         else Stdlib.Format.printf "val %s : %a\n" key pp_typ ty)
     | Error msg -> Stdlib.Format.printf "Infer error: %a\n" pp_error msg)
  | Error msg -> Stdlib.Format.printf "Parsing error: %s\n" msg
;;
