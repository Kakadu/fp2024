(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Ast

module State = struct
  open Base
  open Result

  type error =
    | Unification_failed of core_type * core_type
    | Unbound_variable of string
    | Unequal_list_lengths
    | Let_rec_invalid_rvalue
    | Not_implemented

  let pp_core_type = Pprint.Pprinter.pprint_type

  let pp_error =
    let open Stdlib.Format in
    function
    | Unequal_list_lengths ->
      asprintf "Fresh vars list and value binding list have different length in 'let rec'"
    | Let_rec_invalid_rvalue ->
      asprintf "This kind of expression is not allowed as right-hand side of 'let rec'"
    | Unbound_variable var -> asprintf "Unbound variable: %s" var
    | Not_implemented -> asprintf "Not implemented"
    | Unification_failed (t1, t2) ->
      asprintf
        "Unification failed for types %s and %s"
        (pp_core_type t1)
        (pp_core_type t2)
  ;;

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

    let fold_left2 xs xl ~init ~f =
      Base.List.fold2
        ~f:(fun acc x l ->
          let* acc = acc in
          f acc x l)
        ~init
        xs
        xl
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
end

module Type = struct
  let free_vars =
    let rec helper acc = function
      | Type_var name -> VarSet.add name acc
      | Type_option ty | Type_list ty -> helper acc ty
      | Type_func (ty1, ty2) -> helper (helper acc ty1) ty2
      | Type_tuple (ty1, ty2, tyrest) -> List.fold_left helper acc (ty1 :: ty2 :: tyrest)
      | _ -> acc
    in
    helper VarSet.empty
  ;;
end

module Subst = struct
  open State
  open Base

  let empty = Map.empty (module String)
  let singleton k v = return (Map.singleton (module String) k v)
  let remove = Map.remove

  let apply subst =
    let rec helper = function
      | Type_var name ->
        (match Map.find subst name with
         | Some name' -> name'
         | None -> Type_var name)
      | Type_option ty -> Type_option (helper ty)
      | Type_list ty -> Type_list (helper ty)
      | Type_func (ty1, ty2) -> Type_func (helper ty1, helper ty2)
      | Type_tuple (ty1, ty2, tyrest) ->
        Type_tuple (helper ty1, helper ty2, List.map ~f:helper tyrest)
      | ty -> ty
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | Type_var a, Type_var b when String.equal a b -> return empty
    | Type_var a, ty | ty, Type_var a -> singleton a ty
    | Type_list ty1, Type_list ty2 -> unify ty1 ty2
    | Type_option ty1, Type_option ty2 -> unify ty1 ty2
    | Type_tuple (ta1, ta2, tarest), Type_tuple (tb1, tb2, tbrest) ->
      (match
         List.fold2
           (ta1 :: ta2 :: tarest)
           (tb1 :: tb2 :: tbrest)
           ~init:(return empty)
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
    | Type_int, Type_int
    | Type_float, Type_float
    | Type_bool, Type_bool
    | Type_char, Type_char
    | Type_string, Type_string
    | Type_unit, Type_unit -> return empty
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
  ;;
end

module TypeEnv = struct
  open Base
  open Scheme

  type t = (string, scheme, String.comparator_witness) Map.t

  let empty = Map.empty (module String)
  let extend env name scheme = Map.set env ~key:name ~data:scheme

  let free_vars (env : t) : VarSet.t =
    Map.fold env ~init:VarSet.empty ~f:(fun ~key:_ ~data acc ->
      VarSet.union acc (Scheme.free_vars data))
  ;;

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
  ;;

  let generalize env ty =
    let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
    Scheme (free, ty)
  ;;

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
       | Const_unit_of_measure _ -> fail Not_implemented)
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
      let* new_env, sub_list = infer_tail env'' [ unified_sub ] ptl in
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
      let* env, typ2 = infer_pat env p2 in
      let* subst = Subst.unify typ2 (Type_list typ1) in
      let env = TypeEnv.apply subst env in
      return (env, Subst.apply subst typ2)
    | Pattern_option (Some p) ->
      let* env', ty = infer_pat env p in
      return (env', Type_option ty)
    | Pattern_option None ->
      let* fresh = fresh_var in
      return (env, Type_option fresh)
  ;;

  let rec extend_helper env pat (Scheme (binds, ty) as scheme) =
    match pat, ty with
    | Pattern_ident_or_op name, _ -> TypeEnv.extend env name scheme
    | Pattern_tuple (p1, p2, prest), Type_tuple (t1, t2, trest) ->
      let new_env =
        Base.List.fold2
          ~init:env
          ~f:(fun env pat ty -> extend_helper env pat (Scheme (binds, ty)))
          (p1 :: p2 :: prest)
          (t1 :: t2 :: trest)
      in
      (match new_env with
       | Ok new_env -> new_env
       | _ -> env)
    | _ -> env
  ;;

  let infer_rest_vb env_acc sub_acc sub typ pat =
    let* comp_sub = Subst.compose sub_acc sub in
    let new_env = TypeEnv.apply comp_sub env_acc in
    let new_scheme = generalize new_env (Subst.apply comp_sub typ) in
    let* pat_env, pat_typ = infer_pat new_env pat in
    let new_env = extend_helper pat_env pat new_scheme in
    let* uni_sub = Subst.unify typ pat_typ in
    let* res_sub = Subst.compose comp_sub uni_sub in
    let res_env = TypeEnv.apply res_sub new_env in
    return (res_env, res_sub)
  ;;

  let infer_rec_rest_vb sub_acc env_acc fresh typ name new_sub =
    let* uni_sub = Subst.unify (Subst.apply new_sub fresh) typ in
    let* comp_sub = Subst.compose_all [ new_sub; uni_sub; sub_acc ] in
    let env_acc = TypeEnv.apply comp_sub env_acc in
    let env_rm = Subst.remove env_acc name in
    let new_scheme = generalize env_rm (Subst.apply comp_sub fresh) in
    let env_acc = TypeEnv.extend env_acc name new_scheme in
    return (env_acc, comp_sub)
  ;;

  let rec get_pat_names acc pat =
    match pat with
    | Pattern_ident_or_op id -> id :: acc
    | Pattern_tuple (p1, p2, prest) ->
      Base.List.fold_left ~f:get_pat_names ~init:acc (p1 :: p2 :: prest)
    | Pattern_option (Some pat) -> get_pat_names acc pat
    | Pattern_typed (pat, _) -> get_pat_names acc pat
    | _ -> acc
  ;;

  let add_names_rec env vb_list =
    RList.fold_right
      ~f:(fun vb acc ->
        match vb with
        | Bind (Pattern_ident_or_op name, _)
        | Bind (Pattern_typed (Pattern_ident_or_op name, _), _) ->
          let* env_acc, fresh_acc = return acc in
          let* fresh = fresh_var in
          let env_acc = TypeEnv.extend env_acc name (Scheme (VarSet.empty, fresh)) in
          return (env_acc, fresh :: fresh_acc)
        | _ -> fail Let_rec_invalid_rvalue)
      vb_list
      ~init:(return (env, []))
  ;;

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
       | Const_unit_of_measure _ -> fail Not_implemented)
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
      let* uni_sub1 = Subst.unify ty1 Type_bool in
      let* sub2, ty2 = infer_expr env ethen in
      (match eelse with
       | None ->
         let* uni_sub2 = Subst.unify ty2 Type_unit in
         let* final_sub = Subst.compose_all [ sub1; uni_sub1; sub2; uni_sub2 ] in
         return (final_sub, ty2)
       | Some exp ->
         let* sub3, ty3 = infer_expr env exp in
         let* uni_sub2 = Subst.unify ty2 ty3 in
         let* final_sub = Subst.compose_all [ sub1; uni_sub1; sub2; sub3; uni_sub2 ] in
         return (final_sub, ty3))
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
      let* subst_res = Subst.compose_all [ sub1; sub2; unif_subst1; unif_subst2 ] in
      return (subst_res, Subst.apply subst_res res_ty)
    | Expr_apply (e1, e2) ->
      let* subst1, ty1 = infer_expr env e1 in
      let* subst2, ty2 = infer_expr (TypeEnv.apply subst1 env) e2 in
      let ty1 = Subst.apply subst2 ty1 in
      let* fresh = fresh_var in
      let* subst3 = unify ty1 (Type_func (ty2, fresh)) in
      let* subst_res = Subst.compose_all [ subst1; subst2; subst3 ] in
      return (subst_res, Subst.apply subst3 fresh)
    | Expr_match (e, rl1, rtl) ->
      let* subst, ety = infer_expr env e in
      let env = TypeEnv.apply subst env in
      let* fresh = fresh_var in
      let* res_sub, res_typ =
        RList.fold_left
          (rl1 :: rtl)
          ~init:(return (subst, fresh))
          ~f:(fun acc (Rule (fst, snd)) ->
            let* sub, typ = return acc in
            let pat_names = get_pat_names [] fst in
            let* pat_env, pat_typ = infer_pat env fst in
            let* unif_subst = Subst.unify pat_typ ety in
            let* comp_sub = Subst.compose sub unif_subst in
            let pat_env =
              Base.List.fold_left
                ~f:(fun env name ->
                  match TypeEnv.find name env with
                  | None -> env
                  | Some (Scheme (_, typ)) ->
                    let env = Subst.remove env name in
                    TypeEnv.extend env name (generalize env typ))
                ~init:(TypeEnv.apply unif_subst pat_env)
                pat_names
            in
            let* subexpr, typexpr = infer_expr (TypeEnv.apply comp_sub pat_env) snd in
            let* uni_sub2 = Subst.unify typexpr typ in
            let* res_sub = Subst.compose_all [ uni_sub2; subexpr; comp_sub ] in
            return (res_sub, Subst.apply res_sub typ))
      in
      return (res_sub, res_typ)
    | Expr_function (rl1, rtl) ->
      let* fresh1 = fresh_var in
      let* fresh2 = fresh_var in
      let* res_sub, res_typ =
        RList.fold_left
          (rl1 :: rtl)
          ~init:(return (Subst.empty, fresh2))
          ~f:(fun acc (Rule (fst, snd)) ->
            let* sub, typ = return acc in
            let* pat_env, pat_typ = infer_pat env fst in
            let* uni_sub1 = Subst.unify pat_typ fresh1 in
            let* sub1 = Subst.compose uni_sub1 sub in
            let new_env = TypeEnv.apply sub1 pat_env in
            let* subexpr, typexpr = infer_expr new_env snd in
            let* uni_sub2 = Subst.unify typ typexpr in
            let* comp_sub = Subst.compose_all [ uni_sub2; subexpr; sub1 ] in
            return (comp_sub, Subst.apply comp_sub typ))
      in
      return (res_sub, Type_func (Subst.apply res_sub fresh1, res_typ))
    | Expr_let (Nonrecursive, vb1, vbtl, expr) ->
      let* new_env, sub, _ = infer_vb_list (vb1 :: vbtl) env Subst.empty in
      let* subst, ty = infer_expr new_env expr in
      let* final_sub = Subst.compose sub subst in
      return (final_sub, ty)
    | Expr_let (Recursive, vb1, vbtl, expr) ->
      let* env, fresh_vars = add_names_rec env (vb1 :: vbtl) in
      let* env', subst1, _ = infer_rec_vb_list (vb1 :: vbtl) env Subst.empty fresh_vars in
      let* subst2, ty = infer_expr env' expr in
      let* final_sub = Subst.compose subst1 subst2 in
      return (final_sub, ty)

  and infer_vb_list vb_list env sub =
    let* res_env, res_sub, names =
      RList.fold_left
        vb_list
        ~init:(return (env, sub, []))
        ~f:(fun acc vb ->
          let* env_acc, sub_acc, names = return acc in
          match vb with
          | Bind (Pattern_typed (pat1, ty1), Expr_lam (pat2, expr2)) ->
            let* sub, typ =
              infer_expr env_acc (Expr_lam (pat2, Expr_typed (expr2, ty1)))
            in
            let* res_env, res_sub = infer_rest_vb env_acc sub_acc sub typ pat1 in
            let name = get_pat_names names pat1 in
            return (res_env, res_sub, names @ name)
          | Bind (Pattern_typed (pat1, ty1), Expr_function (rhd, rtl)) ->
            let* sub, typ =
              infer_expr env_acc (Expr_typed (Expr_function (rhd, rtl), ty1))
            in
            let* res_env, res_sub = infer_rest_vb env_acc sub_acc sub typ pat1 in
            let name = get_pat_names names pat1 in
            return (res_env, res_sub, names @ name)
          | Bind (pat, expr) ->
            let* sub, ty = infer_expr env_acc expr in
            let* res_env, res_sub = infer_rest_vb env_acc sub_acc sub ty pat in
            let name = get_pat_names names pat in
            return (res_env, res_sub, names @ name))
    in
    return (res_env, res_sub, names)

  and infer_rec_vb_list vb_list env sub fresh_vars =
    let* res_env, res_sub, names =
      match
        RList.fold_left2
          vb_list
          fresh_vars
          ~init:(return (env, sub, []))
          ~f:(fun acc vb fv ->
            let* env_acc, sub_acc, names = return acc in
            match vb, fv with
            | Bind (Pattern_ident_or_op name, Expr_lam (pat, expr)), fresh ->
              let* subexpr, typexpr = infer_expr env_acc (Expr_lam (pat, expr)) in
              let* res_env, res_sub =
                infer_rec_rest_vb sub_acc env_acc fresh typexpr name subexpr
              in
              return (res_env, res_sub, names @ [ name ])
            | Bind (Pattern_ident_or_op name, Expr_function (rhd, rtl)), fresh ->
              let* subexpr, typexpr = infer_expr env_acc (Expr_function (rhd, rtl)) in
              let* res_env, res_sub =
                infer_rec_rest_vb sub_acc env_acc fresh typexpr name subexpr
              in
              return (res_env, res_sub, names @ [ name ])
            | ( Bind
                  (Pattern_typed (Pattern_ident_or_op name, ty1), Expr_lam (pat2, expr2))
              , fresh ) ->
              let* subexpr, typexpr =
                infer_expr env (Expr_lam (pat2, Expr_typed (expr2, ty1)))
              in
              let* res_env, res_sub =
                infer_rec_rest_vb sub_acc env_acc fresh typexpr name subexpr
              in
              return (res_env, res_sub, names @ [ name ])
            | Bind (Pattern_ident_or_op name, expr), fresh ->
              let* subexpr, typexpr = infer_expr env_acc expr in
              (match typexpr with
               | Type_func (_, _) ->
                 let new_fresh = Subst.apply sub_acc fresh in
                 if typexpr = new_fresh
                 then fail Let_rec_invalid_rvalue
                 else
                   let* res_env, res_sub =
                     infer_rec_rest_vb sub_acc env_acc fresh typexpr name subexpr
                   in
                   return (res_env, res_sub, names @ [ name ])
               | _ -> fail Let_rec_invalid_rvalue)
            | _ -> fail Let_rec_invalid_rvalue)
      with
      | Ok result -> result
      | Unequal_lengths -> fail Unequal_list_lengths
    in
    return (res_env, res_sub, names)
  ;;

  let infer_structure_item env names = function
    | Str_item_eval exp ->
      let* _, typ = infer_expr env exp in
      let new_env = TypeEnv.extend env "-" (Scheme (VarSet.empty, typ)) in
      return (new_env, names @ [ "-" ])
    | Str_item_def (Nonrecursive, vb1, vbtl) ->
      let* env, _, names = infer_vb_list (vb1 :: vbtl) env Subst.empty in
      return (env, names)
    | Str_item_def (Recursive, vb1, vbtl) ->
      let* new_env, fresh_vars = add_names_rec env (vb1 :: vbtl) in
      let* new_env, _, names =
        infer_rec_vb_list (vb1 :: vbtl) new_env Subst.empty fresh_vars
      in
      return (new_env, names)
    | _ -> fail Not_implemented
  ;;

  let infer_program env program =
    let* env, names =
      RList.fold_left
        program
        ~init:(return (env, []))
        ~f:(fun acc item ->
          let* env_acc, names = return acc in
          let* env, name = infer_structure_item env_acc names item in
          return (env, names @ name))
    in
    return (env, names)
  ;;

  let builtin_env =
    let prints =
      [ "print_int", Scheme (empty, Type_func (Type_int, Type_unit))
      ; "print_string", Scheme (empty, Type_func (Type_string, Type_unit))
      ; "print_float", Scheme (empty, Type_func (Type_float, Type_unit))
      ; "print_char", Scheme (empty, Type_func (Type_char, Type_unit))
      ; "print_bool", Scheme (empty, Type_func (Type_bool, Type_unit))
      ; "print_endline", Scheme (empty, Type_func (Type_string, Type_unit))
      ]
    in
    let ops =
      [ "+", Scheme (empty, Type_func (Type_func (Type_int, Type_int), Type_int))
      ; "-", Scheme (empty, Type_func (Type_func (Type_int, Type_int), Type_int))
      ; "*", Scheme (empty, Type_func (Type_func (Type_int, Type_int), Type_int))
      ; "/", Scheme (empty, Type_func (Type_func (Type_int, Type_int), Type_int))
      ; "+.", Scheme (empty, Type_func (Type_func (Type_float, Type_float), Type_float))
      ; "-.", Scheme (empty, Type_func (Type_func (Type_float, Type_float), Type_float))
      ; "*.", Scheme (empty, Type_func (Type_func (Type_float, Type_float), Type_float))
      ; "/.", Scheme (empty, Type_func (Type_func (Type_float, Type_float), Type_float))
      ; ( "<="
        , Scheme
            ( VarSet.singleton "a"
            , Type_func (Type_func (Type_var "a", Type_var "a"), Type_bool) ) )
      ; ( "<"
        , Scheme
            ( VarSet.singleton "a"
            , Type_func (Type_func (Type_var "a", Type_var "a"), Type_bool) ) )
      ; ( ">="
        , Scheme
            ( VarSet.singleton "a"
            , Type_func (Type_func (Type_var "a", Type_var "a"), Type_bool) ) )
      ; ( ">"
        , Scheme
            ( VarSet.singleton "a"
            , Type_func (Type_func (Type_var "a", Type_var "a"), Type_bool) ) )
      ; ( "="
        , Scheme
            ( VarSet.singleton "a"
            , Type_func (Type_func (Type_var "a", Type_var "a"), Type_bool) ) )
      ; ( "<>"
        , Scheme
            ( VarSet.singleton "a"
            , Type_func (Type_func (Type_var "a", Type_var "a"), Type_bool) ) )
      ; "||", Scheme (empty, Type_func (Type_func (Type_bool, Type_bool), Type_bool))
      ; "&&", Scheme (empty, Type_func (Type_func (Type_bool, Type_bool), Type_bool))
      ; ( "::"
        , Scheme
            ( VarSet.singleton "a"
            , Type_func
                ( Type_func (Type_var "a", Type_list (Type_var "a"))
                , Type_list (Type_var "a") ) ) )
      ]
    in
    List.fold_left
      (fun env (id, scheme) -> extend env id scheme)
      TypeEnv.empty
      (prints @ ops)
  ;;
end

let infer env ast = State.run (Infer.infer_program env ast)
