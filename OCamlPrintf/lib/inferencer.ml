(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type error =
  [ `No_variable_rec
  | `No_arg_rec
  | `Bound_several_times of string
  | `Occurs_check of string * core_type
  | `No_variable of string
  | `Unification_failed of core_type * core_type
  ]

let pp_error ppf : error -> _ = function
  | `No_variable_rec ->
    Format.fprintf ppf "Only variables are allowed as left-hand side of `let rec'"
  | `No_arg_rec ->
    Format.fprintf
      ppf
      "This kind of expression is not allowed as right-hand side of `let rec'"
  | `Bound_several_times id ->
    Format.fprintf ppf "Variable '%s' is bound several times in the matching" id
  | `Occurs_check (id, ty) ->
    Format.fprintf
      ppf
      "Occurs check failed: the type variable %s occurs inside %a"
      id
      Pprinter.pp_core_type
      ty
  | `No_variable id -> Format.fprintf ppf "Undefined variable '%s'" id
  | `Unification_failed (l, r) ->
    Format.fprintf
      ppf
      "Unification failed on %a and %a"
      Pprinter.pp_core_type
      l
      Pprinter.pp_core_type
      r
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
  include Set.Make (String)

  let pp ppf set =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%s; ") set;
    Format.fprintf ppf "]"
  ;;
end

type scheme = Scheme of VarSet.t * core_type

let pp_scheme ppf = function
  | Scheme (varset, ty) ->
    Format.fprintf ppf "{ %a : %a }" VarSet.pp varset Pprinter.pp_core_type ty
;;

module Type = struct
  let rec occurs_in var = function
    | Type_option ty -> occurs_in var ty
    | Type_var name -> name = var
    | Type_list ty -> occurs_in var ty
    | Type_tuple (fst_ty, snd_ty, ty_list) ->
      List.exists (occurs_in var) (fst_ty :: snd_ty :: ty_list)
    | Type_arrow (l, r) -> occurs_in var l || occurs_in var r
    | _ -> false
  ;;

  let free_vars =
    let rec helper acc = function
      | Type_option ty -> helper acc ty
      | Type_var name -> VarSet.add name acc
      | Type_tuple (fst_ty, snd_ty, ty_list) ->
        List.fold_left helper acc (fst_ty :: snd_ty :: ty_list)
      | Type_list ty -> helper acc ty
      | Type_arrow (l, r) -> helper (helper acc l) r
      | _ -> acc
    in
    helper VarSet.empty
  ;;
end

module Subst = struct
  open State
  open State.Syntax
  open Base

  let empty = Map.empty (module String)
  let singleton1 = Map.singleton (module String)

  let singleton key value =
    if Type.occurs_in key value
    then fail (`Occurs_check (key, value))
    else return (Map.singleton (module String) key value)
  ;;

  let remove = Map.remove

  let apply sub =
    let rec helper = function
      | Type_var name as ty ->
        (match Map.find sub name with
         | Some name -> name
         | None -> ty)
      | Type_option ty -> Type_option (helper ty)
      | Type_list ty -> Type_list (helper ty)
      | Type_tuple (fst_ty, snd_ty, ty_list) ->
        Type_tuple (helper fst_ty, helper snd_ty, List.map ty_list ~f:helper)
      | Type_arrow (l, r) -> Type_arrow (helper l, helper r)
      | ty -> ty
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | Type_unit, Type_unit
    | Type_int, Type_int
    | Type_char, Type_char
    | Type_string, Type_string
    | Type_bool, Type_bool -> return empty
    | Type_var l, Type_var r when String.equal l r -> return empty
    | Type_var name, ty | ty, Type_var name -> singleton name ty
    | Type_list ty1, Type_list ty2 | Type_option ty1, Type_option ty2 -> unify ty1 ty2
    | Type_tuple (fst1, snd1, list1), Type_tuple (fst2, snd2, list2) ->
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
       | _ -> fail (`Unification_failed (l, r)))
    | Type_arrow (l1, r1), Type_arrow (l2, r2) ->
      let* sub1 = unify l1 l2 in
      let* sub2 = unify (apply sub1 r1) (apply sub1 r2) in
      compose sub1 sub2
    | _ -> fail (`Unification_failed (l, r))

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

  let pp ppf sub =
    Stdlib.Format.fprintf ppf "Subst:\n";
    Map.iteri sub ~f:(fun ~key:str ~data:ty ->
      Stdlib.Format.fprintf ppf "%s <-> %a; " str Pprinter.pp_core_type ty);
    Stdlib.Format.fprintf ppf "\n"
  ;;
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

  type t = (ident, scheme, String.comparator_witness) Map.t

  let empty = Map.empty (module String)
  let extend env key value = Map.update env key ~f:(fun _ -> value)

  let rec extend_with_pattern env_acc pat (Scheme (bind_set, ty) as scheme) =
    match pat, ty with
    | Pat_var id, _ -> extend env_acc id scheme
    | Pat_tuple (fst_pat, snd_pat, pat_list), Type_tuple (fst_ty, snd_ty, ty_list) ->
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
    | Pat_construct ("::", Some pat), Type_list ty ->
      (match pat with
       | Pat_tuple (head, tail, []) ->
         let env_acc = extend_with_pattern env_acc head (Scheme (bind_set, ty)) in
         extend_with_pattern env_acc tail scheme
       | _ -> env_acc)
    | Pat_construct ("Some", Some pat), Type_option ty ->
      extend_with_pattern env_acc pat (Scheme (bind_set, ty))
    | _ -> env_acc
  ;;

  let free_vars env =
    Map.fold env ~init:VarSet.empty ~f:(fun ~key:_ ~data acc ->
      VarSet.union acc (Scheme.free_vars data))
  ;;

  let apply sub env = Map.map env ~f:(Scheme.apply sub)
  let find = Map.find

  let find_type_exn env key =
    let (Scheme (_, ty)) = Map.find_exn env key in
    ty
  ;;

  let pp ppf env =
    Stdlib.Format.fprintf ppf "TypeEnv:\n";
    Map.iteri env ~f:(fun ~key:str ~data:sch ->
      Stdlib.Format.fprintf ppf "%s -> %a; " str pp_scheme sch);
    Stdlib.Format.fprintf ppf "\n"
  ;;
end

module Infer = struct
  open Ast.Expression
  open State
  open State.Syntax

  let unify = Subst.unify
  let fresh_var = fresh >>| fun n -> Type_var ("'ty" ^ Int.to_string n)

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
            (* 97 - is number 'a' in ASCII-table *)
            Printf.sprintf
              "'%c%s"
              (Char.chr (97 + (n mod 26)))
              (if degree = 0 then "" else Int.to_string degree)
          in
          let sub = Subst.singleton1 str (Type_var new_str) in
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
    | None -> fail (`No_variable id)
  ;;

  let rec infer_pattern env = function
    | Pat_any ->
      let* fresh = fresh_var in
      return (env, fresh)
    | Pat_var id ->
      let* fresh = fresh_var in
      let env = TypeEnv.extend env id (Scheme (VarSet.empty, fresh)) in
      return (env, fresh)
    | Pat_constant const ->
      (match const with
       | Const_integer _ -> return (env, Type_int)
       | Const_string _ -> return (env, Type_string)
       | Const_char _ -> return (env, Type_char))
    | Pat_tuple (fst_pat, snd_pat, pat_list) ->
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
      return (env_rest, Type_tuple (ty1, ty2, ty_list))
    | Pat_construct ("[]", None) ->
      let* fresh = fresh_var in
      return (env, Type_list fresh)
    | Pat_construct ("::", Some (Pat_tuple (head, tail, []))) ->
      let* fresh = fresh_var in
      let* env, type_of_head = infer_pattern env head in
      let* unified_sub = unify type_of_head fresh in
      let env = TypeEnv.apply unified_sub env in
      let rec infer_tail env sub_acc cur_pat =
        let helper required_ty pat =
          let* env, type_of_pat = infer_pattern env pat in
          let* unified_sub = unify required_ty type_of_pat in
          return (TypeEnv.apply unified_sub env, unified_sub)
        in
        match cur_pat with
        | Pat_construct (_, None) -> return (env, sub_acc)
        | Pat_construct (_, Some (Pat_tuple (next_head, next_tail, []))) ->
          let* env, sub = helper fresh next_head in
          let* env, final_sub = infer_tail env (sub :: sub_acc) next_tail in
          return (env, final_sub)
        | _ ->
          let* env, sub = helper (Type_list fresh) cur_pat in
          return (env, sub :: sub_acc)
      in
      let* env, sub_list = infer_tail env [ unified_sub ] tail in
      let* final_sub = Subst.compose_all sub_list in
      return (TypeEnv.apply final_sub env, Subst.apply final_sub (Type_list fresh))
    | Pat_construct (id, None) when id = "true" || id = "false" -> return (env, Type_bool)
    | Pat_construct ("()", None) -> return (env, Type_unit)
    | Pat_construct ("Some", Some pat) ->
      let* env, ty = infer_pattern env pat in
      return (env, Type_option ty)
    | Pat_construct _ ->
      let* fresh = fresh_var in
      return (env, fresh)
    | Pat_constraint (pat, c_ty) ->
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
        | { pat = Pat_var id | Pat_constraint (Pat_var id, _); _ } ->
          let* env, fresh_acc = return acc in
          let* fresh = fresh_var in
          let env = TypeEnv.extend env id (Scheme (VarSet.empty, fresh)) in
          return (env, fresh :: fresh_acc)
        | _ -> fail `No_variable_rec)
  ;;

  let rec extract_names_from_pat func acc = function
    | Pat_var id -> func acc id
    | Pat_tuple (fst_pat, snd_pat, pat_list) ->
      RList.fold_left
        (fst_pat :: snd_pat :: pat_list)
        ~init:(return acc)
        ~f:(extract_names_from_pat func)
    | Pat_construct ("::", Some exp) ->
      (match exp with
       | Pat_tuple (head, tail, []) ->
         let* acc = extract_names_from_pat func acc head in
         extract_names_from_pat func acc tail
       | _ -> return acc)
    | Pat_construct ("Some", Some pat) -> extract_names_from_pat func acc pat
    | Pat_constraint (pat, _) -> extract_names_from_pat func acc pat
    | _ -> return acc
  ;;

  module StringSet = struct
    include Set.Make (String)

    let add_id set value =
      if mem value set then fail (`Bound_several_times value) else return (add value set)
    ;;
  end

  let check_names_from_let_binds =
    RList.fold_left ~init:(return StringSet.empty) ~f:(fun set_acc { pat; _ } ->
      extract_names_from_pat StringSet.add_id set_acc pat)
  ;;

  let rec infer_expression env = function
    | Exp_ident id -> lookup_env id env
    | Exp_constant const ->
      (match const with
       | Const_integer _ -> return (Subst.empty, Type_int)
       | Const_string _ -> return (Subst.empty, Type_string)
       | Const_char _ -> return (Subst.empty, Type_char))
    | Exp_let (Nonrecursive, value_binding, value_binding_list, exp) ->
      let* _ = check_names_from_let_binds (value_binding :: value_binding_list) in
      let* env, sub1 =
        infer_value_binding_list env Subst.empty (value_binding :: value_binding_list)
      in
      let* sub2, ty2 = infer_expression env exp in
      let* composed_sub = Subst.compose sub2 sub1 in
      return (composed_sub, ty2)
    | Exp_let (Recursive, value_binding, value_binding_list, exp) ->
      let* env, fresh_acc =
        extend_env_with_bind_names env (value_binding :: value_binding_list)
      in
      let* env, sub1 =
        rec_infer_value_binding_list
          env
          fresh_acc
          Subst.empty
          (value_binding :: value_binding_list)
      in
      let* sub2, ty2 = infer_expression env exp in
      let* composed_sub = Subst.compose sub2 sub1 in
      return (composed_sub, ty2)
    | Exp_fun (pat, pat_list, exp) ->
      let* env, ty1 = infer_pattern env pat in
      let* sub, ty2 =
        match pat_list with
        | [] -> infer_expression env exp
        | hd :: tl -> infer_expression env (Exp_fun (hd, tl, exp))
      in
      return (sub, Type_arrow (Subst.apply sub ty1, ty2))
    | Exp_apply (Exp_ident opr, Exp_apply (exp1, exp2)) when is_operator opr ->
      let* sub1, ty1 = infer_expression env exp1 in
      let* sub2, ty2 = infer_expression (TypeEnv.apply sub1 env) exp2 in
      let* required_arg_ty, required_result_ty =
        match opr with
        | "*" | "/" | "+" | "-" -> return (Type_int, Type_int)
        | ">=" | "<=" | "<>" | "=" | ">" | "<" ->
          let* fresh = fresh_var in
          return (fresh, Type_bool)
        | _ -> return (Type_bool, Type_bool)
      in
      let* unified_sub1 = Subst.unify (Subst.apply sub2 ty1) required_arg_ty in
      let* unified_sub2 = Subst.unify (Subst.apply unified_sub1 ty2) required_arg_ty in
      let* composed_sub = Subst.compose_all [ sub1; sub2; unified_sub1; unified_sub2 ] in
      return (composed_sub, required_result_ty)
    | Exp_apply (exp1, exp2) ->
      (match exp1 with
       | Exp_ident opr when is_negative_op opr ->
         let* sub, ty = infer_expression env exp2 in
         let* unified_sub = Subst.unify ty Type_int in
         let* composed_sub = Subst.compose sub unified_sub in
         return (composed_sub, Type_int)
       | _ ->
         let* sub1, ty1 = infer_expression env exp1 in
         let* sub2, ty2 = infer_expression (TypeEnv.apply sub1 env) exp2 in
         let* fresh = fresh_var in
         let* sub3 = unify (Subst.apply sub2 ty1) (Type_arrow (ty2, fresh)) in
         let* composed_sub = Subst.compose_all [ sub3; sub2; sub1 ] in
         let final_ty = Subst.apply composed_sub fresh in
         return (composed_sub, final_ty))
    | Exp_function (case, case_list) ->
      let* fresh_for_matching = fresh_var in
      let* fresh_for_result = fresh_var in
      infer_match_exp
        env
        ~with_exp:false
        Subst.empty
        fresh_for_matching
        fresh_for_result
        (case :: case_list)
    | Exp_match (exp, case, case_list) ->
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
    | Exp_tuple (fst_exp, snd_exp, exp_list) ->
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
      let ty_list = List.map (fun ty -> Subst.apply sub_result ty) ty_list in
      return (sub_result, Type_tuple (ty1, ty2, ty_list))
    | Exp_construct ("[]", None) ->
      let* fresh = fresh_var in
      return (Subst.empty, Type_list fresh)
    | Exp_construct ("::", Some (Exp_tuple (head, tail, []))) ->
      let* fresh = fresh_var in
      let* sub, ty = infer_expression env head in
      let* unified_sub = unify fresh ty in
      let* sub = Subst.compose sub unified_sub in
      let rec infer_tail sub_acc cur_exp =
        let helper required_ty exp =
          let* sub_of_exp, type_of_exp = infer_expression env exp in
          let* unified_sub = unify required_ty type_of_exp in
          let* sub = Subst.compose sub_of_exp unified_sub in
          return sub
        in
        match cur_exp with
        | Exp_construct (_, None) -> return sub_acc
        | Exp_construct (_, Some (Exp_tuple (next_head, next_tail, []))) ->
          let* sub = helper fresh next_head in
          let* final_sub = infer_tail (sub :: sub_acc) next_tail in
          return final_sub
        | _ ->
          let* sub = helper (Type_list fresh) cur_exp in
          return (sub :: sub_acc)
      in
      let* sub_list = infer_tail [ sub ] tail in
      let* final_sub = Subst.compose_all sub_list in
      return (final_sub, Subst.apply final_sub (Type_list fresh))
    | Exp_construct (id, None) when id = "true" || id = "false" ->
      return (Subst.empty, Type_bool)
    | Exp_construct ("()", None) -> return (Subst.empty, Type_unit)
    | Exp_construct ("Some", Some pat) ->
      let* sub, ty = infer_expression env pat in
      return (sub, Type_option ty)
    | Exp_construct _ ->
      let* fresh = fresh_var in
      return (Subst.empty, fresh)
    | Exp_ifthenelse (if_exp, then_exp, Some else_exp) ->
      let* sub1, ty1 = infer_expression env if_exp in
      let* sub2, ty2 = infer_expression (TypeEnv.apply sub1 env) then_exp in
      let* sub3, ty3 = infer_expression (TypeEnv.apply sub2 env) else_exp in
      let* sub4 = unify ty1 Type_bool in
      let* sub5 = unify ty2 ty3 in
      let* final_sub = Subst.compose_all [ sub5; sub4; sub3; sub2; sub1 ] in
      return (final_sub, Subst.apply final_sub ty2)
    | Exp_ifthenelse (if_exp, then_exp, None) ->
      let* sub1, ty1 = infer_expression env if_exp in
      let* sub2, ty2 = infer_expression (TypeEnv.apply sub1 env) then_exp in
      let* sub3 = unify ty1 Type_bool in
      let* sub4 = unify ty2 Type_unit in
      let* final_sub = Subst.compose_all [ sub4; sub3; sub2; sub1 ] in
      return (final_sub, Subst.apply final_sub ty2)
    | Exp_sequence (exp1, exp2) ->
      let* sub1, ty1 = infer_expression env exp1 in
      let* unified_sub = unify ty1 Type_unit in
      let* sub2, ty2 = infer_expression (TypeEnv.apply sub1 env) exp2 in
      let* final_sub = Subst.compose_all [ unified_sub; sub2; sub1 ] in
      return (final_sub, ty2)
    | Exp_constraint (exp, c_ty) ->
      let* sub, ty = infer_expression env exp in
      let* unified_sub = unify ty c_ty in
      let* final_sub = Subst.compose unified_sub sub in
      return (final_sub, Subst.apply unified_sub ty)

  and infer_match_exp env ~with_exp match_exp_sub match_exp_ty result_ty case_list =
    let* cases_sub, case_ty =
      RList.fold_left
        case_list
        ~init:(return (match_exp_sub, result_ty))
        ~f:(fun acc { left = pat; right = case_exp } ->
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
      if with_exp
      then case_ty
      else Type_arrow (Subst.apply cases_sub match_exp_ty, case_ty)
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
    | { pat = Pat_constraint (pat, pat_ty); exp = Exp_fun (e_pat, e_pat_list, exp) }
      :: rest ->
      let* new_sub, ty =
        infer_expression env (Exp_fun (e_pat, e_pat_list, Exp_constraint (exp, pat_ty)))
      in
      infer_vb new_sub env ty pat rest
    | { pat = Pat_constraint (pat, pat_ty); exp = Exp_function _ as exp } :: rest ->
      let* new_sub, ty = infer_expression env (Exp_constraint (exp, pat_ty)) in
      infer_vb new_sub env ty pat rest
    | { pat; exp } :: rest ->
      let* new_sub, ty = infer_expression env exp in
      infer_vb new_sub env ty pat rest

  and rec_infer_value_binding_list ?(debug = false) env fresh_acc sub let_binds =
    let rec_infer_vb new_sub fresh ty id fresh_acc rest ~required_ty =
      let* new_sub =
        match required_ty with
        | Some c_ty ->
          let* unified_sub = unify ty c_ty in
          Subst.compose unified_sub new_sub
        | None -> return new_sub
      in
      let* unified_sub = unify (Subst.apply new_sub fresh) ty in
      let* composed_sub = Subst.compose_all [ new_sub; unified_sub; sub ] in
      if debug then Subst.pp Format.std_formatter composed_sub;
      let env = TypeEnv.apply composed_sub env in
      let generalized_ty =
        generalize env (Subst.apply composed_sub fresh) ~remove_from_env:true (Some id)
      in
      if debug then pp_scheme Format.std_formatter generalized_ty;
      let env = TypeEnv.extend env id generalized_ty in
      rec_infer_value_binding_list ~debug env fresh_acc composed_sub rest
    in
    match let_binds, fresh_acc with
    | [], _ -> return (env, sub)
    | ( { pat = Pat_var id; exp = (Exp_fun _ | Exp_function _) as exp } :: rest
      , fresh :: fresh_acc ) ->
      let* new_sub, ty = infer_expression env exp in
      rec_infer_vb new_sub fresh ty id fresh_acc rest ~required_ty:None
    | ( { pat = Pat_constraint (Pat_var id, pat_ty); exp = Exp_fun (pat, pat_list, exp) }
        :: rest
      , fresh :: fresh_acc ) ->
      let* new_sub, ty =
        infer_expression env (Exp_fun (pat, pat_list, Exp_constraint (exp, pat_ty)))
      in
      rec_infer_vb new_sub fresh ty id fresh_acc rest ~required_ty:None
    | ( { pat = Pat_constraint (Pat_var id, pat_ty); exp = Exp_function _ as exp } :: rest
      , fresh :: fresh_acc ) ->
      let* new_sub, ty = infer_expression env (Exp_constraint (exp, pat_ty)) in
      rec_infer_vb new_sub fresh ty id fresh_acc rest ~required_ty:None
    | { pat = Pat_var id; exp } :: rest, fresh :: fresh_acc ->
      let* new_sub, ty = infer_expression env exp in
      let update_fresh = Subst.apply new_sub fresh in
      if ty = update_fresh
      then fail `No_arg_rec
      else rec_infer_vb new_sub fresh ty id fresh_acc rest ~required_ty:None
    | { pat = Pat_constraint (Pat_var id, pat_ty); exp } :: rest, fresh :: fresh_acc ->
      let* new_sub, ty = infer_expression env exp in
      let update_fresh = Subst.apply new_sub fresh in
      if ty = update_fresh
      then fail `No_arg_rec
      else rec_infer_vb new_sub fresh ty id fresh_acc rest ~required_ty:(Some pat_ty)
    | _ -> fail `No_variable_rec
  ;;

  let infer_srtucture_item ?(debug = false) env ast =
    let get_names_from_let_binds env =
      RList.fold_left ~init:(return []) ~f:(fun acc { pat; _ } ->
        extract_names_from_pat
          (fun acc id -> return (acc @ [ Some id, TypeEnv.find_type_exn env id ]))
          acc
          pat)
    in
    let* _, out_list =
      RList.fold_left
        ast
        ~init:(return (env, []))
        ~f:(fun (env, out_list) ->
          function
          | Struct_eval exp ->
            let* _, ty = infer_expression env exp in
            return (env, out_list @ [ None, ty ])
          | Struct_value (Nonrecursive, value_binding, value_binding_list) ->
            let value_binding_list = value_binding :: value_binding_list in
            let* _ = check_names_from_let_binds value_binding_list in
            let* env, _ = infer_value_binding_list env Subst.empty value_binding_list in
            let* id_list = get_names_from_let_binds env value_binding_list in
            if debug then TypeEnv.pp Format.std_formatter env;
            return (env, out_list @ id_list)
          | Struct_value (Recursive, value_binding, value_binding_list) ->
            let value_binding_list = value_binding :: value_binding_list in
            let* env, fresh_acc = extend_env_with_bind_names env value_binding_list in
            let* env, _ =
              rec_infer_value_binding_list env fresh_acc Subst.empty value_binding_list
            in
            let* id_list = get_names_from_let_binds env value_binding_list in
            if debug then TypeEnv.pp Format.std_formatter env;
            return (env, out_list @ id_list))
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
    return (remove_duplicates out_list)
  ;;
end

let empty_env = TypeEnv.empty

let env_with_print_int =
  TypeEnv.extend
    TypeEnv.empty
    "print_int"
    (Scheme (VarSet.empty, Type_arrow (Type_int, Type_unit)))
;;

let run_inferencer ?(debug = false) ast env =
  State.run (Infer.infer_srtucture_item ~debug env ast)
;;
