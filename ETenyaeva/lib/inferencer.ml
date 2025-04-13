(** Copyright 2024-2025, Ekaterina Tenyaeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Stdlib.Format

let rec pp_type fmt = function
  | TypInt -> fprintf fmt "int"
  | TypBool -> fprintf fmt "bool"
  | TypUnit -> fprintf fmt "unit"
  | TypStr -> fprintf fmt "string"
  | TypChar -> fprintf fmt "char"
  | TypVar id -> fprintf fmt "%s" id
  | TypArrow (ty1, ty2) -> fprintf fmt "%a -> %a" pp_type ty1 pp_type ty2
  | TypList ty ->
    (match ty with
     | TypTuple _ -> fprintf fmt "(%a) list" pp_type ty
     | _ -> fprintf fmt "%a list" pp_type ty)
  | TypTuple (f, s, xs) ->
    fprintf
      fmt
      "%a"
      (pp_print_list
         ~pp_sep:(fun _ _ -> printf " * ")
         (fun fmt ty ->
           match ty with
           | TypTuple _ -> fprintf fmt "(%a)" pp_type ty
           | _ -> pp_type fmt ty))
      (f :: s :: xs)
  | TypOption TypUnit -> ()
  | TypOption ty ->
    (match ty with
     | TypTuple _ -> fprintf fmt "(%a) option" pp_type ty
     | _ -> fprintf fmt "%a option" pp_type ty)
;;

type error =
  | NoVariableRec
  | NoArgRec
  | SeveralBounds of string
  | OccursCheck of string * typ
  | NoVariable of string
  | UnificationFailed of typ * typ

let pp_error fmt = function
  | OccursCheck (id, typ) ->
    fprintf fmt "Occurs check failed. Type variable %s occurs inside %a\n" id pp_type typ
  | NoVariable name -> fprintf fmt "Unbound variable %s'." name
  | NoVariableRec ->
    fprintf fmt "Only variables are allowed as left-hand side of `let rec'"
  | UnificationFailed (ty1, ty2) ->
    fprintf fmt "Failed to unify types: %a and %a\n" pp_type ty1 pp_type ty2
  | NoArgRec ->
    fprintf fmt "This kind of expression is not allowed as right-hand side of `let rec'"
  | SeveralBounds name -> fprintf fmt "Multiple bounds for variable %s'." name
;;

module State = struct
  open Base

  type 'a t = int -> int * ('a, error) Result.t

  let return x state = state, Result.return x
  let fail e state = state, Result.fail e

  let ( >>= ) (monad : 'a t) (f : 'a -> 'b t) : 'b t =
    fun state ->
    match monad state with
    | state, Result.Ok result -> f result state
    | state, Result.Error e -> fail e state
  ;;

  module Syntax = struct
    let ( let* ) = ( >>= )
  end

  let ( >>| ) (monad : 'a t) (f : 'a -> 'b) : 'b t =
    fun state ->
    match monad state with
    | state, Result.Ok result -> return (f result) state
    | state, Result.Error e -> fail e state
  ;;

  module RList = struct
    let fold_left xs ~init ~f =
      List.fold_left xs ~init ~f:(fun acc x ->
        let open Syntax in
        let* acc = acc in
        f acc x)
    ;;

    let fold_right xs ~init ~f =
      List.fold_right xs ~init ~f:(fun x acc ->
        let open Syntax in
        let* acc = acc in
        f x acc)
    ;;
  end

  module RMap = struct
    let fold map ~init ~f =
      Map.fold map ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end

  let fresh state = state + 1, Result.Ok state
  let run monad = snd (monad 0)
end

module VarSet = struct
  include Stdlib.Set.Make (String)
end

type scheme = Scheme of VarSet.t * typ

module Type = struct
  open Base

  (* gets a variable var and type ty, and checks whether the
     variable is contained in the set of free variables of this type*)
  let occurs_in var ty =
    let rec helper ty =
      match ty with
      | TypOption ty | TypList ty -> helper ty
      | TypVar name -> String.equal name var
      | TypTuple (fst_ty, snd_ty, ty_list) ->
        List.exists ~f:helper (fst_ty :: snd_ty :: ty_list)
      | TypArrow (l, r) -> helper l || helper r
      | _ -> false
    in
    match ty with
    | TypVar _ -> false
    | _ -> helper ty
  ;;

  let free_vars =
    let rec helper acc = function
      | TypOption ty | TypList ty -> helper acc ty
      | TypVar name -> VarSet.add name acc
      | TypTuple (fst_ty, snd_ty, ty_list) ->
        List.fold_left ~f:helper ~init:acc (fst_ty :: snd_ty :: ty_list)
      | TypArrow (l, r) -> helper (helper acc l) r
      | _ -> acc
    in
    helper VarSet.empty
  ;;
end

module Subst = struct
  open State
  open Base
  open State.Syntax

  let empty = Map.empty (module String)
  let singleton1 = Map.singleton (module String)

  let singleton key value =
    if Type.occurs_in key value
    then fail (OccursCheck (key, value))
    else return (Map.singleton (module String) key value)
  ;;

  let remove = Map.remove

  let apply sub =
    let rec helper = function
      | TypVar name as ty ->
        (match Map.find sub name with
         | Some name -> name
         | None -> ty)
      | TypOption ty -> TypOption (helper ty)
      | TypList ty -> TypList (helper ty)
      | TypTuple (fst_ty, snd_ty, ty_list) ->
        TypTuple (helper fst_ty, helper snd_ty, List.map ty_list ~f:helper)
      | TypArrow (l, r) -> TypArrow (helper l, helper r)
      | ty -> ty
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TypUnit, TypUnit
    | TypInt, TypInt
    | TypChar, TypChar
    | TypStr, TypStr
    | TypBool, TypBool -> return empty
    | TypVar l, TypVar r when String.equal l r -> return empty
    | TypVar name, ty | ty, TypVar name -> singleton name ty
    | TypList ty1, TypList ty2 | TypOption ty1, TypOption ty2 -> unify ty1 ty2
    | TypTuple (fst1, snd1, list1), TypTuple (fst2, snd2, list2) ->
      (match
         List.fold2
           (fst1 :: snd1 :: list1)
           (fst2 :: snd2 :: list2)
           ~init:(return empty)
           ~f:(fun acc ty1 ty2 ->
             let* sub_acc = acc in
             let* unified_sub = unify (apply sub_acc ty1) (apply sub_acc ty2) in
             compose sub_acc unified_sub)
       with
       | Ok res -> res
       | _ -> fail (UnificationFailed (l, r)))
    | TypArrow (l1, r1), TypArrow (l2, r2) ->
      let* sub1 = unify l1 l2 in
      let* sub2 = unify (apply sub1 r1) (apply sub1 r2) in
      compose sub1 sub2
    | _ -> fail (UnificationFailed (l, r))

  and extend key value sub =
    match Map.find sub key with
    | None ->
      let value = apply sub value in
      let* new_sub = singleton key value in
      Map.fold sub ~init:(return new_sub) ~f:(fun ~key ~data acc ->
        let* acc = acc in
        let new_data = apply new_sub data in
        return (Map.update acc key ~f:(fun _ -> new_data)))
    | Some existing_value ->
      let* new_sub = unify value existing_value in
      compose sub new_sub

  and compose sub1 sub2 = RMap.fold sub2 ~init:(return sub1) ~f:extend

  let compose_all sub_list = RList.fold_left sub_list ~init:(return empty) ~f:compose
end

module Scheme = struct
  let free_vars (Scheme (bind_set, ty)) = VarSet.diff (Type.free_vars ty) bind_set

  let apply sub (Scheme (bind_set, ty)) =
    let new_sub = VarSet.fold (fun key sub -> Subst.remove sub key) bind_set sub in
    Scheme (bind_set, Subst.apply new_sub ty)
  ;;
end

module TypeEnv = struct
  open Base

  type t = (id, scheme, String.comparator_witness) Map.t

  let empty = Map.empty (module String)
  let extend env key value = Map.update env key ~f:(fun _ -> value)

  let free_vars env =
    Map.fold env ~init:VarSet.empty ~f:(fun ~key:_ ~data acc ->
      VarSet.union acc (Scheme.free_vars data))
  ;;

  let apply sub env = Map.map env ~f:(Scheme.apply sub)
  let find = Map.find

  let rec extend_with_pattern env_acc pat (Scheme (bind_set, ty) as scheme) =
    match pat, ty with
    | PatVar id, _ -> extend env_acc id scheme
    | PatTup (fst_pat, snd_pat, pat_list), TypTuple (fst_ty, snd_ty, ty_list) ->
      let env =
        List.fold2
          ~init:env_acc
          ~f:(fun env pat ty -> extend_with_pattern env pat (Scheme (bind_set, ty)))
          (fst_pat :: snd_pat :: pat_list)
          (fst_ty :: snd_ty :: ty_list)
      in
      (match env with
       | Ok env -> env
       | _ -> env_acc)
    | PatListConstructor pat_list, TypList ty ->
      (match pat_list with
       | single_pat :: [] ->
         extend_with_pattern env_acc single_pat (Scheme (bind_set, TypList ty))
       | first :: rest ->
         extend_with_pattern
           (extend_with_pattern env_acc first (Scheme (bind_set, ty)))
           (PatListConstructor rest)
           (Scheme (bind_set, TypList ty))
       | [] -> env_acc)
    | PatList pat_list, TypList ty ->
      List.fold_left pat_list ~init:env_acc ~f:(fun env_acc pat ->
        extend_with_pattern env_acc pat (Scheme (bind_set, ty)))
    | PatOption (Some pat), TypOption ty ->
      extend_with_pattern env_acc pat (Scheme (bind_set, ty))
    | _ -> env_acc
  ;;

  (** looks for a type by key in the environment and throws an exception if the key is not found*)
  let find_type_exn env key =
    let (Scheme (_, ty)) = Map.find_exn env key in
    ty
  ;;
end

module Infer = struct
  open State
  open State.Syntax

  let unify = Subst.unify
  let fresh_var = fresh >>| fun n -> TypVar ("'ty" ^ Int.to_string n)

  let instantiate (Scheme (bind_set, ty)) =
    VarSet.fold
      (fun name ty ->
        let* ty = ty in
        let* fresh = fresh_var in
        let* sub = Subst.singleton name fresh in
        return (Subst.apply sub ty))
      bind_set
      (return ty)
  ;;

  (** generalizes type ty by removing certain variables from the environment,
      evaluating free variables, creating new names for them, and returning a new generic schema*)
  let generalize env ty ~remove_from_env id =
    let env =
      match remove_from_env, id with
      | true, Some id -> Base.Map.remove env id
      | _ -> env
    in
    let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
    let new_free, new_ty, _ =
      VarSet.fold
        (fun str (temp_free, temp_ty, n) ->
          let degree = n / 26 in
          let new_str =
            Printf.sprintf
              "'%c%s"
              (Stdlib.Char.chr (97 + (n mod 26)))
              (if degree = 0 then "" else Int.to_string degree)
          in
          let sub = Subst.singleton1 str (TypVar new_str) in
          let new_free = VarSet.add new_str temp_free in
          let new_ty = Subst.apply sub temp_ty in
          new_free, new_ty, n + 1)
        free
        (VarSet.empty, ty, 0)
    in
    Scheme (new_free, new_ty)
  ;;

  let lookup_env id env =
    match TypeEnv.find env id with
    | Some scheme ->
      let* ans = instantiate scheme in
      return (Subst.empty, ans)
    | None -> fail (NoVariable id)
  ;;

  let rec infer_pattern env = function
    | PatAny ->
      let* fresh = fresh_var in
      return (env, fresh)
    | PatVar id ->
      let* fresh = fresh_var in
      let env = TypeEnv.extend env id (Scheme (VarSet.empty, fresh)) in
      return (env, fresh)
    | PatConst const ->
      (match const with
       | Int _ -> return (env, TypInt)
       | String _ -> return (env, TypStr)
       | Char _ -> return (env, TypChar)
       | Bool _ -> return (env, TypBool)
       | Unit -> return (env, TypUnit))
    | PatTup (fst_pat, snd_pat, pat_list) ->
      let* env1, ty1 = infer_pattern env fst_pat in
      let* env2, ty2 = infer_pattern env1 snd_pat in
      let* env_rest, ty_list =
        RList.fold_right
          ~f:(fun pat acc ->
            let* env_acc, ty_list = return acc in
            let* env, ty = infer_pattern env_acc pat in
            return (env, ty :: ty_list))
          ~init:(return (env2, []))
          pat_list
      in
      return (env_rest, TypTuple (ty1, ty2, ty_list))
    | PatOption None ->
      let* fresh = fresh_var in
      return (env, TypOption fresh)
    | PatOption (Some pat) ->
      let* env, ty = infer_pattern env pat in
      return (env, TypOption ty)
    | PatListConstructor pat_list ->
      let* fresh = fresh_var in
      let rec helper env sub_acc rest =
        match rest with
        | [] -> return (env, sub_acc)
        | single :: [] ->
          let* env, ty = infer_pattern env single in
          let* unified_sub = unify (TypList fresh) ty in
          let* composed_sub = Subst.compose sub_acc unified_sub in
          helper env composed_sub []
        | first :: rest ->
          let* env, ty = infer_pattern env first in
          let* unified_sub = unify fresh ty in
          let* composed_sub = Subst.compose sub_acc unified_sub in
          helper env composed_sub rest
      in
      let* env, sub = helper env Subst.empty pat_list in
      let result_ty = Subst.apply sub fresh in
      return (env, TypList result_ty)
    | PatList pat_list ->
      let* list_element_type_var = fresh_var in
      let* env, sub =
        RList.fold_left
          pat_list
          ~init:(return (env, Subst.empty))
          ~f:(fun (acc_env, acc_sub) pat ->
            let* env, pat_type = infer_pattern acc_env pat in
            let* unified_sub = unify list_element_type_var pat_type in
            let* composed_sub = Subst.compose unified_sub acc_sub in
            return (env, composed_sub))
      in
      let list_element_type = Subst.apply sub list_element_type_var in
      let env = TypeEnv.apply sub env in
      return (env, TypList list_element_type)
    | PatWithTyp (c_ty, pat) ->
      let* env, ty = infer_pattern env pat in
      let* unified_sub = unify ty c_ty in
      return (TypeEnv.apply unified_sub env, Subst.apply unified_sub ty)
  ;;

  let extend_env_with_bind_names env value_binding_list =
    RList.fold_right
      value_binding_list
      ~init:(return (env, []))
      ~f:(fun let_bind acc ->
        match let_bind with
        | { pat = PatVar id | PatWithTyp (_, PatVar id); _ } ->
          let* env, fresh_acc = return acc in
          let* fresh = fresh_var in
          let env = TypeEnv.extend env id (Scheme (VarSet.empty, fresh)) in
          return (env, fresh :: fresh_acc)
        | _ -> fail NoVariableRec)
  ;;

  (** Recursively traverses patterns by extracting variable names.
      Applies the given func to each identifier found.
      Uses the accumulator acc to store intermediate results.*)
  let rec extract_names_from_pat func acc = function
    | PatVar id -> func acc id
    | PatTup (fst_pat, snd_pat, pat_list) ->
      RList.fold_left
        (fst_pat :: snd_pat :: pat_list)
        ~init:(return acc)
        ~f:(extract_names_from_pat func)
    | PatOption (Some pat) -> extract_names_from_pat func acc pat
    | PatListConstructor pat_list | PatList pat_list ->
      (match pat_list with
       | [] -> return acc
       | first_pat :: rest_pats ->
         let* acc = extract_names_from_pat func acc first_pat in
         extract_names_from_pat func acc (PatList rest_pats))
    | PatWithTyp (_, pat) -> extract_names_from_pat func acc pat
    | _ -> return acc
  ;;

  module StringSet = struct
    include Stdlib.Set.Make (String)

    let add_id set value =
      if mem value set then fail (SeveralBounds value) else return (add value set)
    ;;
  end

  let check_names_from_let_binds =
    RList.fold_left ~init:(return StringSet.empty) ~f:(fun set_acc { pat; _ } ->
      extract_names_from_pat StringSet.add_id set_acc pat)
  ;;

  let infer_binop_type = function
    | Equals | NotEquals | GreaterThan | GreaterEquals | LessThan | LessEquals ->
      fresh_var >>| fun fresh_ty -> fresh_ty, fresh_ty, TypBool
    | Add | Sub | Mult | Div -> return (TypInt, TypInt, TypInt)
    | And | Or -> return (TypBool, TypBool, TypBool)
  ;;

  let infer_unop_type = function
    | Neg -> return (TypInt, TypInt)
    | Not -> return (TypBool, TypBool)
  ;;

  (** takes a type environment and an expression, and then returns the type of that expression*)
  let rec infer_expression env = function
    | ExpVar id -> lookup_env id env
    | ExpConst const ->
      (match const with
       | Int _ -> return (Subst.empty, TypInt)
       | String _ -> return (Subst.empty, TypStr)
       | Bool _ -> return (Subst.empty, TypBool)
       | Char _ -> return (Subst.empty, TypChar)
       | Unit -> return (Subst.empty, TypUnit))
    | ExpTup (fst_exp, snd_exp, exp_list) ->
      let* sub1, ty1 = infer_expression env fst_exp in
      let* sub2, ty2 = infer_expression (TypeEnv.apply sub1 env) snd_exp in
      let env = TypeEnv.apply sub2 env in
      let* sub_rest, ty_list =
        RList.fold_right
          ~f:(fun exp acc ->
            let* sub_acc, ty_list = return acc in
            let* sub, ty = infer_expression (TypeEnv.apply sub_acc env) exp in
            let* sub_acc = Subst.compose sub_acc sub in
            return (sub_acc, ty :: ty_list))
          ~init:(return (Subst.empty, []))
          exp_list
      in
      let* sub_result = Subst.compose_all [ sub1; sub2; sub_rest ] in
      let ty1 = Subst.apply sub_result ty1 in
      let ty2 = Subst.apply sub_result ty2 in
      let ty_list = Base.List.map ~f:(fun ty -> Subst.apply sub_result ty) ty_list in
      return (sub_result, TypTuple (ty1, ty2, ty_list))
    | ExpIfThenElse (if_exp, then_exp, None) ->
      let* sub1, ty1 = infer_expression env if_exp in
      let* sub2, ty2 = infer_expression (TypeEnv.apply sub1 env) then_exp in
      let* sub3 = unify ty1 TypBool in
      let* sub4 = unify ty2 TypUnit in
      let* final_sub = Subst.compose_all [ sub4; sub3; sub2; sub1 ] in
      return (final_sub, Subst.apply final_sub ty2)
    | ExpIfThenElse (if_exp, then_exp, Some else_exp) ->
      let* sub1, ty1 = infer_expression env if_exp in
      let* sub2, ty2 = infer_expression (TypeEnv.apply sub1 env) then_exp in
      let* sub3, ty3 = infer_expression (TypeEnv.apply sub2 env) else_exp in
      let* sub4 = unify ty1 TypBool in
      let* sub5 = unify ty2 ty3 in
      let* final_sub = Subst.compose_all [ sub5; sub4; sub3; sub2; sub1 ] in
      return (final_sub, Subst.apply final_sub ty2)
    | ExpWithTyp (c_ty, exp) ->
      let* sub, ty = infer_expression env exp in
      let* unified_sub = unify ty c_ty in
      let* final_sub = Subst.compose unified_sub sub in
      return (final_sub, Subst.apply unified_sub ty)
    | ExpFunction (case, case_list) ->
      let* fresh_for_matching = fresh_var in
      let* fresh_for_result = fresh_var in
      infer_match_exp
        env
        ~with_exp:false
        Subst.empty
        fresh_for_matching
        fresh_for_result
        (case :: case_list)
    | ExpMatch (exp, case, case_list) ->
      let* exp_sub, exp_ty = infer_expression env exp in
      let env = TypeEnv.apply exp_sub env in
      let* fresh_for_result = fresh_var in
      infer_match_exp
        env
        ~with_exp:true
        exp_sub
        exp_ty
        fresh_for_result
        (case :: case_list)
    | ExpUnOper (operation, expr) ->
      let* subst1, ty = infer_expression env expr in
      let* ty1_op, ty_res = infer_unop_type operation in
      let* subst2 = Subst.unify (Subst.apply subst1 ty) ty1_op in
      let* subst = Subst.compose_all [ subst1; subst2 ] in
      return (subst, Subst.apply subst ty_res)
    | ExpBinOper (op, expr1, expr2) ->
      let* subst1, ty = infer_expression env expr1 in
      let* subst2, ty' = infer_expression (TypeEnv.apply subst1 env) expr2 in
      let* ty1_op, ty2_op, ty_res = infer_binop_type op in
      let* subst3 = Subst.unify (Subst.apply subst2 ty) ty1_op in
      let* subst4 = Subst.unify (Subst.apply subst3 ty') ty2_op in
      let* subst = Subst.compose_all [ subst1; subst2; subst3; subst4 ] in
      return (subst, Subst.apply subst ty_res)
    | ExpApp (exp1, exp2) ->
      let* sub1, ty1 = infer_expression env exp1 in
      let* sub2, ty2 = infer_expression (TypeEnv.apply sub1 env) exp2 in
      let* fresh = fresh_var in
      let* sub3 = unify (Subst.apply sub2 ty1) (TypArrow (ty2, fresh)) in
      let* composed_sub = Subst.compose_all [ sub3; sub2; sub1 ] in
      let final_ty = Subst.apply composed_sub fresh in
      return (composed_sub, final_ty)
    | ExpList exprs ->
      (match exprs with
       | [] ->
         let* fresh = fresh_var in
         return (Subst.empty, TypList fresh)
       | _ :: _ ->
         let infer_list_elements env es =
           let rec aux env = function
             | [] -> return ([], [])
             | e :: es' ->
               let* s, t = infer_expression env e in
               let* s', ts = aux (TypeEnv.apply s env) es' in
               return (s' @ [ s ], t :: ts)
           in
           aux env es
         in
         let* subst, tys = infer_list_elements env exprs in
         let* total_subst = Subst.compose_all subst in
         (match tys with
          | [] -> fail (SeveralBounds "inferred empty list type")
          | ty :: _ -> return (total_subst, TypList ty)))
    | ExpListConstructor expr_list ->
      let* fresh = fresh_var in
      let rec infer_list_constract env acc_sub = function
        | [] ->
          let* fresh1 = fresh_var in
          return (Subst.empty, TypOption fresh1)
        | end_element :: [] ->
          let* expr_sub, expr_ty = infer_expression env end_element in
          let* unified_sub = unify expr_ty (TypList fresh) in
          let* composed_sub = Subst.compose_all [ expr_sub; unified_sub; acc_sub ] in
          return (composed_sub, TypList (Subst.apply composed_sub fresh))
        | expr_element :: expr_rest ->
          let* expr_sub, expr_ty = infer_expression env expr_element in
          let* unified_sub = unify expr_ty fresh in
          let* composed_sub = Subst.compose_all [ expr_sub; unified_sub; acc_sub ] in
          let env = TypeEnv.apply composed_sub env in
          let* sub, ty = infer_list_constract env composed_sub expr_rest in
          return (sub, ty)
      in
      infer_list_constract env Subst.empty expr_list
    | ExpOption None ->
      let* fresh = fresh_var in
      return (Subst.empty, TypOption fresh)
    | ExpOption (Some expr) ->
      let* sub, ty = infer_expression env expr in
      return (sub, TypOption ty)
    | ExpFun (pat, expr) ->
      let* env, ty1 = infer_pattern env pat in
      let* sub, ty2 = infer_expression env expr in
      return (sub, TypArrow (Subst.apply sub ty1, ty2))
    | ExpLet (NonRec, value_binding, value_binding_list, exp) ->
      let* _ = check_names_from_let_binds (value_binding :: value_binding_list) in
      let* env, sub1 =
        infer_value_binding_list env Subst.empty (value_binding :: value_binding_list)
      in
      let* sub2, ty2 = infer_expression env exp in
      let* composed_sub = Subst.compose sub2 sub1 in
      return (composed_sub, ty2)
    | ExpLet (Rec, value_binding, value_binding_list, exp) ->
      let* env, fresh_acc =
        extend_env_with_bind_names env (value_binding :: value_binding_list)
      in
      let* env, sub1 =
        infer_rec_value_binding_list
          env
          fresh_acc
          Subst.empty
          (value_binding :: value_binding_list)
      in
      let* sub2, ty2 = infer_expression env exp in
      let* composed_sub = Subst.compose sub2 sub1 in
      return (composed_sub, ty2)

  and infer_match_exp env ~with_exp match_exp_sub match_exp_ty result_ty case_list =
    let* cases_sub, case_ty =
      RList.fold_left
        case_list
        ~init:(return (match_exp_sub, result_ty))
        ~f:(fun acc { case_pat = pat; case_expr = case_exp } ->
          let* sub_acc, ty_acc = return acc in
          let* env, pat_sub =
            let* env, pat_ty = infer_pattern env pat in
            let* unified_sub1 = unify match_exp_ty pat_ty in
            let* pat_names =
              extract_names_from_pat StringSet.add_id StringSet.empty pat
              >>| StringSet.elements
            in
            if with_exp
            then (
              let env = TypeEnv.apply unified_sub1 env in
              let generalized_schemes =
                Base.List.map pat_names ~f:(fun name ->
                  let ty = TypeEnv.find_type_exn env name in
                  let generalized_ty =
                    generalize env ty ~remove_from_env:true (Some name)
                  in
                  name, generalized_ty)
              in
              let env =
                Base.List.fold generalized_schemes ~init:env ~f:(fun env (key, value) ->
                  TypeEnv.extend env key value)
              in
              return (env, unified_sub1))
            else return (env, unified_sub1)
          in
          let* composed_sub1 = Subst.compose sub_acc pat_sub in
          let* case_exp_sub, case_exp_ty =
            infer_expression (TypeEnv.apply composed_sub1 env) case_exp
          in
          let* unified_sub2 = unify ty_acc case_exp_ty in
          let* composed_sub2 =
            Subst.compose_all [ composed_sub1; case_exp_sub; unified_sub2 ]
          in
          return (composed_sub2, Subst.apply composed_sub2 ty_acc))
    in
    let final_ty =
      if with_exp then case_ty else TypArrow (Subst.apply cases_sub match_exp_ty, case_ty)
    in
    return (cases_sub, final_ty)

  and infer_value_binding_list env sub let_binds =
    let infer_vb new_sub env ty pat rest =
      let* composed_sub = Subst.compose sub new_sub in
      let env = TypeEnv.apply composed_sub env in
      let generalized_ty =
        generalize env (Subst.apply composed_sub ty) ~remove_from_env:false None
      in
      let* env, pat_ty = infer_pattern env pat in
      let env = TypeEnv.extend_with_pattern env pat generalized_ty in
      let* unified_sub = unify ty pat_ty in
      let* final_sub = Subst.compose composed_sub unified_sub in
      let env = TypeEnv.apply final_sub env in
      infer_value_binding_list env final_sub rest
    in
    match let_binds with
    | [] -> return (env, sub)
    | { pat = PatWithTyp (pat_ty, pat); expr = ExpFun (e_pat, expr) } :: rest ->
      let* new_sub, ty =
        infer_expression env (ExpFun (e_pat, ExpWithTyp (pat_ty, expr)))
      in
      infer_vb new_sub env ty pat rest
    | { pat = PatWithTyp (pat_ty, pat); expr = ExpFunction _ as expr } :: rest ->
      let* new_sub, ty = infer_expression env (ExpWithTyp (pat_ty, expr)) in
      infer_vb new_sub env ty pat rest
    | { pat; expr } :: rest ->
      let* new_sub, ty = infer_expression env expr in
      infer_vb new_sub env ty pat rest

  and infer_rec_value_binding_list env fresh_acc sub let_binds =
    let infer_rec_vb new_sub fresh ty id fresh_acc rest ~required_ty =
      let* new_sub =
        match required_ty with
        | Some c_ty ->
          let* unified_sub = unify ty c_ty in
          Subst.compose unified_sub new_sub
        | None -> return new_sub
      in
      let* unified_sub = unify (Subst.apply new_sub fresh) ty in
      let* composed_sub = Subst.compose_all [ new_sub; unified_sub; sub ] in
      let env = TypeEnv.apply composed_sub env in
      let generalized_ty =
        generalize env (Subst.apply composed_sub fresh) ~remove_from_env:true (Some id)
      in
      let env = TypeEnv.extend env id generalized_ty in
      infer_rec_value_binding_list env fresh_acc composed_sub rest
    in
    match let_binds, fresh_acc with
    | [], _ -> return (env, sub)
    | ( { pat = PatVar id; expr = (ExpFun _ | ExpFunction _) as exp } :: rest
      , fresh :: fresh_acc ) ->
      let* new_sub, ty = infer_expression env exp in
      infer_rec_vb new_sub fresh ty id fresh_acc rest ~required_ty:None
    | ( { pat = PatWithTyp (pat_ty, PatVar id); expr = ExpFun (pat, expr) } :: rest
      , fresh :: fresh_acc ) ->
      let* new_sub, ty = infer_expression env (ExpFun (pat, ExpWithTyp (pat_ty, expr))) in
      infer_rec_vb new_sub fresh ty id fresh_acc rest ~required_ty:None
    | ( { pat = PatWithTyp (pat_ty, PatVar id); expr = ExpFunction _ as expr } :: rest
      , fresh :: fresh_acc ) ->
      let* new_sub, ty = infer_expression env (ExpWithTyp (pat_ty, expr)) in
      infer_rec_vb new_sub fresh ty id fresh_acc rest ~required_ty:None
    | { pat = PatVar id; expr } :: rest, fresh :: fresh_acc ->
      let* new_sub, ty = infer_expression env expr in
      let update_fresh = Subst.apply new_sub fresh in
      if ty = update_fresh
      then fail NoArgRec
      else infer_rec_vb new_sub fresh ty id fresh_acc rest ~required_ty:None
    | { pat = PatWithTyp (pat_ty, PatVar id); expr } :: rest, fresh :: fresh_acc ->
      let* new_sub, ty = infer_expression env expr in
      let update_fresh = Subst.apply new_sub fresh in
      if ty = update_fresh
      then fail NoArgRec
      else infer_rec_vb new_sub fresh ty id fresh_acc rest ~required_ty:(Some pat_ty)
    | _ -> fail NoVariableRec
  ;;

  let infer_structure_item (env, out_list) =
    let get_names_from_let_binds env =
      RList.fold_left ~init:(return []) ~f:(fun acc { pat; _ } ->
        extract_names_from_pat
          (fun acc id -> return (acc @ [ Some id, TypeEnv.find_type_exn env id ]))
          acc
          pat)
    in
    function
    | EvalExp exp ->
      let* _, ty = infer_expression env exp in
      return (env, out_list @ [ None, ty ])
    | Binding (NonRec, value_binding, value_binding_list) ->
      let value_binding_list = value_binding :: value_binding_list in
      let* _ = check_names_from_let_binds value_binding_list in
      let* env, _ = infer_value_binding_list env Subst.empty value_binding_list in
      let* id_list = get_names_from_let_binds env value_binding_list in
      return (env, out_list @ id_list)
    | Binding (Rec, value_binding, value_binding_list) ->
      let value_binding_list = value_binding :: value_binding_list in
      let* env, fresh_acc = extend_env_with_bind_names env value_binding_list in
      let* env, _ =
        infer_rec_value_binding_list env fresh_acc Subst.empty value_binding_list
      in
      let* id_list = get_names_from_let_binds env value_binding_list in
      return (env, out_list @ id_list)
  ;;

  let infer_srtucture env ast =
    let* env, out_list =
      RList.fold_left ast ~init:(return (env, [])) ~f:infer_structure_item
    in
    let remove_duplicates =
      let fun_equal el1 el2 =
        match el1, el2 with
        | (Some id1, _), (Some id2, _) -> String.equal id1 id2
        | _ -> false
      in
      function
      | x :: xs when not (Base.List.mem xs x ~equal:fun_equal) -> x :: xs
      | _ :: xs -> xs
      | [] -> []
    in
    return (env, remove_duplicates out_list)
  ;;
end

let empty_env = TypeEnv.empty

let env_with_print_funs =
  let print_fun_list =
    [ "print_int", Scheme (VarSet.empty, TypArrow (TypInt, TypUnit))
    ; "print_endline", Scheme (VarSet.empty, TypArrow (TypStr, TypUnit))
    ]
  in
  Base.List.fold_left
    ~f:(fun env (id, sch) -> TypeEnv.extend env id sch)
    ~init:TypeEnv.empty
    print_fun_list
;;

let run_inferencer env ast = State.run (Infer.infer_srtucture env ast)
