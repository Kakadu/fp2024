(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Pprinter

type error =
  [ `Impossible_error
  | `No_variable_rec
  | `No_arg_rec
  | `Bound_several_times
  | `Occurs_check of string * core_type
  | `No_variable of string
  | `Unification_failed of core_type * core_type
  ]

let pp_error ppf : error -> _ = function
  | `Impossible_error -> Format.fprintf ppf "Something went wrong"
  | `No_variable_rec ->
    Format.fprintf ppf "Only variables are allowed as left-hand side of `let rec'"
  | `No_arg_rec ->
    Format.fprintf
      ppf
      "This kind of expression is not allowed as right-hand side of `let rec'"
  | `Bound_several_times ->
    Format.fprintf ppf "Variable a is bound several times in the matching"
  | `Occurs_check (id, ty) ->
    Format.fprintf
      ppf
      "Occurs check failed: the type variable %s occurs inside %a"
      id
      pp_core_type
      ty
  | `No_variable id -> Format.fprintf ppf "Undefined variable '%s'" id
  | `Unification_failed (l, r) ->
    Format.fprintf ppf "Unification failed on %a and %a" pp_core_type l pp_core_type r
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
    let ( let* ) x f = x >>= f
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

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%s; ") s;
    Format.fprintf ppf "]"
  ;;
end

type scheme = Scheme of VarSet.t * Ast.core_type

let pp_scheme ppf = function
  | Scheme (varset, type') ->
    Format.fprintf ppf "{ %a : %a }" VarSet.pp varset pp_core_type type'
;;

module Type = struct
  type t = core_type

  let rec occurs_in var = function
    | Type_option ty -> occurs_in var ty
    | Type_name name -> name = var
    | Type_list ty -> occurs_in var ty
    | Type_tuple (fst, snd, rest_list) ->
      List.exists (occurs_in var) (fst :: snd :: rest_list)
    | Type_arrow (l, r) -> occurs_in var l || occurs_in var r
    | _ -> false
  ;;

  let free_vars =
    let rec helper acc = function
      | Type_option ty -> helper acc ty
      | Type_name name -> VarSet.add name acc
      | Type_tuple (fst, snd, rest_list) ->
        List.fold_left helper acc (fst :: snd :: rest_list)
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

  type t = (ident, core_type, String.comparator_witness) Map.t

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
      | Type_name name as ty ->
        (match Map.find sub name with
         | Some name -> name
         | None -> ty)
      | Type_option ty -> Type_option (helper ty)
      | Type_list ty -> Type_list (helper ty)
      | Type_tuple (fst, snd, rest_List) ->
        Type_tuple (helper fst, helper snd, List.map rest_List ~f:(fun ty -> helper ty))
      | Type_arrow (l, r) -> Type_arrow (helper l, helper r)
      | ty -> ty
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | Type_any, Type_any
    | Type_unit, Type_unit
    | Type_int, Type_int
    | Type_char, Type_char
    | Type_string, Type_string
    | Type_bool, Type_bool -> return empty
    | Type_name l, Type_name r when String.equal l r -> return empty
    | Type_name name, ty | ty, Type_name name -> singleton name ty
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

  let pp ppf (xs : t) =
    Stdlib.Format.fprintf ppf "Subst:\n";
    Map.iteri xs ~f:(fun ~key:str ~data:ty ->
      Stdlib.Format.fprintf ppf "%s <-> %a; " str pp_core_type ty);
    Stdlib.Format.fprintf ppf "\n"
  ;;
end

module Scheme = struct
  type t = scheme

  let occurs_in var (Scheme (bind_set, ty)) =
    (not (VarSet.mem var bind_set)) && Type.occurs_in var ty
  ;;

  let free_vars (Scheme (bind_set, ty)) = VarSet.diff (Type.free_vars ty) bind_set

  let apply sub (Scheme (bind_set, ty)) =
    let new_sub = VarSet.fold (fun key sub -> Subst.remove sub key) bind_set sub in
    Scheme (bind_set, Subst.apply new_sub ty)
  ;;
end

module TypeEnv = struct
  open Base

  type t = (string, scheme, String.comparator_witness) Map.t

  let empty = Map.empty (module String)
  let extend env key value = Map.update env key ~f:(fun _ -> value)

  let rec extend_with_pattern env_acc pat (Scheme (bind_set, ty) as scheme) =
    match pat, ty with
    | Pat_var value, _ -> extend env_acc value scheme
    | Pat_tuple (fst, snd, rest_list), Type_tuple (fst_ty, snd_ty, rest_list_ty) ->
      let env =
        List.fold2
          ~init:env_acc
          ~f:(fun env pat ty -> extend_with_pattern env pat (Scheme (bind_set, ty)))
          (fst :: snd :: rest_list)
          (fst_ty :: snd_ty :: rest_list_ty)
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
    match Map.find_exn env key with
    | Scheme (_, typ) -> typ
  ;;

  let pp ppf (xs : t) =
    Stdlib.Format.fprintf ppf "TypeEnv:\n";
    Map.iteri xs ~f:(fun ~key:str ~data:sch ->
      Stdlib.Format.fprintf ppf "%s -> %a; " str pp_scheme sch);
    Stdlib.Format.fprintf ppf "\n"
  ;;
end

module Infer = struct
  open State
  open State.Syntax
  open Ast

  let unify = Subst.unify
  let fresh_var = fresh >>| fun n -> Type_name ("'t" ^ Int.to_string n)

  let instantiate (Scheme (bind_set, ty)) =
    VarSet.fold
      (fun name typ ->
        let* typ = typ in
        let* fresh = fresh_var in
        let* sub = Subst.singleton name fresh in
        return (Subst.apply sub typ))
      bind_set
      (return ty)
  ;;

  let generalize env ty ~(remove_from_env : bool) (id : ident option) =
    let env =
      match remove_from_env, id with
      | true, Some ident -> Base.Map.remove env ident
      | _ -> env
    in
    let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
    let new_free, new_type, _ =
      VarSet.fold
        (fun str (temp_free, temp_ty, n) ->
          (* 97 - is number 'a' in ASCII-table *)
          let new_str = "'" ^ String.make 1 (Char.chr (97 + n)) in
          let sub = Subst.singleton1 str (Type_name new_str) in
          let new_free = VarSet.add new_str temp_free in
          let new_type = Subst.apply sub temp_ty in
          new_free, new_type, n + 1)
        free
        (VarSet.empty, ty, 0)
    in
    Scheme (new_free, new_type)
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
    | Pat_tuple (fst, snd, rest_list) ->
      let* env1, type1 = infer_pattern env fst in
      let* env2, type2 = infer_pattern env1 snd in
      let* env_rest, type_list =
        RList.fold_right
          ~f:(fun pat acc ->
            let* env_acc, typ_list = return acc in
            let* env, typ = infer_pattern env_acc pat in
            return (env, typ :: typ_list))
          ~init:(return (env2, []))
          rest_list
      in
      return (env_rest, Type_tuple (type1, type2, type_list))
    | Pat_construct ("[]", None) ->
      let* fresh = fresh_var in
      return (env, Type_list fresh)
    | Pat_construct ("::", Some pat) ->
      (match pat with
       | Pat_tuple (head, tail, []) ->
         let* fresh = fresh_var in
         let* env, type_of_head = infer_pattern env head in
         let* unified_sub = unify type_of_head fresh in
         let env = TypeEnv.apply unified_sub env in
         let rec infer_tail env sub_acc (cur_pat : pattern) =
           let helper required_type pat =
             let* env, type_of_pat = infer_pattern env pat in
             let* unified_sub = unify required_type type_of_pat in
             return (TypeEnv.apply unified_sub env, unified_sub)
           in
           match cur_pat with
           | Pat_construct (_, None) -> return (env, sub_acc)
           | Pat_construct (_, Some pat_tail) ->
             (match pat_tail with
              | Pat_tuple (next_head, next_tail, []) ->
                let* env, sub = helper fresh next_head in
                let* env, final_sub = infer_tail env (sub :: sub_acc) next_tail in
                return (env, final_sub)
              | _ -> fail `Impossible_error)
           | _ ->
             let* env, sub = helper (Type_list fresh) cur_pat in
             return (env, sub :: sub_acc)
         in
         let* env, sub_list = infer_tail env [ unified_sub ] tail in
         let* final_sub = Subst.compose_all sub_list in
         return (TypeEnv.apply final_sub env, Subst.apply final_sub (Type_list fresh))
       | _ -> fail `Impossible_error)
    | Pat_construct (id, None) when id = "true" || id = "false" -> return (env, Type_bool)
    | Pat_construct ("()", None) -> return (env, Type_unit)
    | Pat_construct ("None", None) ->
      let* fresh = fresh_var in
      return (env, fresh)
    | Pat_construct ("Some", Some pat) ->
      let* env, typ = infer_pattern env pat in
      return (env, Type_option typ)
    | Pat_construct (_, _) -> fail `Impossible_error
    | Pat_constraint (pat, c_type) ->
      let* env, typ = infer_pattern env pat in
      let* unified_sub = unify typ c_type in
      let env = TypeEnv.apply unified_sub env in
      return (env, c_type)
  ;;

  let extend_env_with_bind_names env value_binding_list =
    RList.fold_right
      value_binding_list
      ~init:(return (env, []))
      ~f:(fun let_bind acc ->
        match let_bind with
        | { pat = Pat_var pat; _ } ->
          let* env, fresh_acc = return acc in
          let* fresh = fresh_var in
          let env = TypeEnv.extend env pat (Scheme (VarSet.empty, fresh)) in
          return (env, fresh :: fresh_acc)
        | _ -> fail `No_variable_rec)
  ;;

  module StringSet = struct
    include Set.Make (String)

    let add_id set (value : ident) =
      if mem value set then fail `Bound_several_times else return (add value set)
    ;;
  end

  let rec extract_names_from_pat set_acc = function
    | Pat_var id -> StringSet.add_id set_acc id
    | Pat_tuple (fst, snd, rest_list) ->
      RList.fold_left (fst :: snd :: rest_list) ~init:(return set_acc) ~f:(fun acc pat ->
        extract_names_from_pat acc pat)
    | Pat_construct ("::", Some exp) ->
      (match exp with
       | Pat_tuple (head, tail, []) ->
         let* set_acc = extract_names_from_pat set_acc head in
         extract_names_from_pat set_acc tail
       | _ -> return set_acc)
    | Pat_construct ("Some", Some pat) -> extract_names_from_pat set_acc pat
    | Pat_constraint (pat, _) -> extract_names_from_pat set_acc pat
    | _ -> return set_acc
  ;;

  let rec infer_expression env (exp : Expression.t) =
    match exp with
    | Exp_ident id -> lookup_env id env
    | Exp_constant const ->
      (match const with
       | Const_integer _ -> return (Subst.empty, Type_int)
       | Const_string _ -> return (Subst.empty, Type_string)
       | Const_char _ -> return (Subst.empty, Type_char))
    | Exp_let (Nonrecursive, value_binding, value_binding_list, exp) ->
      let* env, sub1 =
        infer_value_binding_list env Subst.empty (value_binding :: value_binding_list)
      in
      let* sub2, type2 = infer_expression env exp in
      let* composed_sub = Subst.compose sub2 sub1 in
      return (composed_sub, type2)
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
      let* sub2, type2 = infer_expression env exp in
      let* composed_sub = Subst.compose sub2 sub1 in
      return (composed_sub, type2)
    | Exp_fun (pat, pat_list, exp) ->
      let* env, type1 = infer_pattern env pat in
      let* sub, type2 =
        match pat_list with
        | [] -> infer_expression env exp
        | hd :: tl -> infer_expression env (Exp_fun (hd, tl, exp))
      in
      return (sub, Type_arrow (Subst.apply sub type1, type2))
    | Exp_apply (e1, e2) ->
      (match e1 with
       | Exp_ident op when is_operator op ->
         let* exp1, exp2 =
           match e2 with
           | Exp_apply (exp1, exp2) -> return (exp1, exp2)
           | _ -> fail `Impossible_error
         in
         let* sub1, type1 = infer_expression env exp1 in
         let* sub2, type2 = infer_expression (TypeEnv.apply sub1 env) exp2 in
         let* required_type1, required_type2, required_result_type =
           match get_priority op with
           | 1 | 2 -> return (Type_int, Type_int, Type_int)
           | 3 ->
             let* fresh = fresh_var in
             return (fresh, fresh, Type_bool)
           | _ -> return (Type_bool, Type_bool, Type_bool)
         in
         let* unified_sub1 = Subst.unify (Subst.apply sub2 type1) required_type1 in
         let* unified_sub2 =
           Subst.unify (Subst.apply unified_sub1 type2) required_type2
         in
         let* composed_sub =
           Subst.compose_all [ sub1; sub2; unified_sub1; unified_sub2 ]
         in
         return (composed_sub, required_result_type)
       | Exp_ident op when is_negative_op op ->
         let* sub, ty = infer_expression env e2 in
         let* unified_sub = Subst.unify ty Type_int in
         let* composed_sub = Subst.compose sub unified_sub in
         return (composed_sub, Type_int)
       | _ ->
         let* sub1, type1 = infer_expression env e1 in
         let* sub2, type2 = infer_expression (TypeEnv.apply sub1 env) e2 in
         let* fresh = fresh_var in
         let* sub3 = unify (Subst.apply sub2 type1) (Type_arrow (type2, fresh)) in
         let* composed_sub = Subst.compose_all [ sub3; sub2; sub1 ] in
         let final_type = Subst.apply composed_sub fresh in
         return (composed_sub, final_type))
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
      let* exp_sub, exp_type = infer_expression env exp in
      let env = TypeEnv.apply exp_sub env in
      let* fresh_for_result = fresh_var in
      infer_match_exp
        env
        ~with_exp:true
        exp_sub
        exp_type
        fresh_for_result
        (case :: case_list)
    | Exp_tuple (fst, snd, rest_list) ->
      let* sub1, type1 = infer_expression env fst in
      let* sub2, type2 = infer_expression (TypeEnv.apply sub1 env) snd in
      let env = TypeEnv.apply sub2 env in
      let* sub_rest, type_list =
        RList.fold_right
          ~f:(fun exp acc ->
            let* sub_acc, typ_list = return acc in
            let* sub, typ = infer_expression (TypeEnv.apply sub_acc env) exp in
            let* sub_acc = Subst.compose sub_acc sub in
            return (sub_acc, typ :: typ_list))
          ~init:(return (Subst.empty, []))
          rest_list
      in
      let* sub_result = Subst.compose_all [ sub1; sub2; sub_rest ] in
      let typ1 = Subst.apply sub_result type1 in
      let typ2 = Subst.apply sub_result type2 in
      let typ_list = List.map (fun typ -> Subst.apply sub_result typ) type_list in
      return (sub_result, Type_tuple (typ1, typ2, typ_list))
    | Exp_construct ("[]", None) ->
      let* fresh = fresh_var in
      return (Subst.empty, Type_list fresh)
    | Exp_construct ("::", Some exp) ->
      (match exp with
       | Exp_tuple (head, tail, []) ->
         let* fresh = fresh_var in
         let* sub1, type1 = infer_expression env head in
         let* unified_sub = unify fresh type1 in
         let* sub1 = Subst.compose sub1 unified_sub in
         let rec infer_tail sub_acc (cur_exp : Expression.t) =
           let helper required_type exp =
             let* sub_of_exp, type_of_exp = infer_expression env exp in
             let* unified_sub = unify required_type type_of_exp in
             let* sub = Subst.compose sub_of_exp unified_sub in
             return sub
           in
           match cur_exp with
           | Exp_construct (_, None) -> return sub_acc
           | Exp_construct (_, Some exp_tail) ->
             (match exp_tail with
              | Exp_tuple (next_head, next_tail, []) ->
                let* sub = helper fresh next_head in
                let* final_sub = infer_tail (sub :: sub_acc) next_tail in
                return final_sub
              | _ -> fail `Impossible_error)
           | _ ->
             let* sub = helper (Type_list fresh) cur_exp in
             return (sub :: sub_acc)
         in
         let* sub_list = infer_tail [ sub1 ] tail in
         let* final_sub = Subst.compose_all sub_list in
         return (final_sub, Subst.apply final_sub (Type_list fresh))
       | _ -> fail `Impossible_error)
    | Exp_construct (id, None) when id = "true" || id = "false" ->
      return (Subst.empty, Type_bool)
    | Exp_construct ("()", None) -> return (Subst.empty, Type_unit)
    | Exp_construct ("None", None) ->
      let* fresh = fresh_var in
      return (Subst.empty, fresh)
    | Exp_construct ("Some", Some pat) ->
      let* sub, typ = infer_expression env pat in
      return (sub, Type_option typ)
    | Exp_construct (_, _) -> fail `Impossible_error
    | Exp_ifthenelse (if_, then_, Some else_) ->
      let* sub1, type1 = infer_expression env if_ in
      let* sub2, type2 = infer_expression (TypeEnv.apply sub1 env) then_ in
      let* sub3, type3 = infer_expression (TypeEnv.apply sub2 env) else_ in
      let* sub4 = unify type1 Type_bool in
      let* sub5 = unify type2 type3 in
      let* final_sub = Subst.compose_all [ sub5; sub4; sub3; sub2; sub1 ] in
      return (final_sub, Subst.apply final_sub type2)
    | Exp_ifthenelse (if_, then_, None) ->
      let* sub1, type1 = infer_expression env if_ in
      let* sub2, type2 = infer_expression (TypeEnv.apply sub1 env) then_ in
      let* sub3 = unify type1 Type_bool in
      let* sub4 = unify type2 Type_unit in
      let* final_sub = Subst.compose_all [ sub4; sub3; sub2; sub1 ] in
      return (final_sub, Subst.apply final_sub type2)
    | Exp_sequence (exp1, exp2) ->
      let* sub1, type1 = infer_expression env exp1 in
      let* unified_sub = unify type1 Type_unit in
      let* sub2, type2 = infer_expression (TypeEnv.apply sub1 env) exp2 in
      let* final_sub = Subst.compose_all [ unified_sub; sub2; sub1 ] in
      return (final_sub, type2)
    | Exp_constraint (exp, c_type) ->
      let* sub, typ = infer_expression env exp in
      let* unified_sub = unify typ c_type in
      let* final_sub = Subst.compose unified_sub sub in
      return (final_sub, typ)

  and infer_match_exp env ~with_exp match_exp_sub match_exp_type result_type case_list =
    let* cases_sub, case_type =
      RList.fold_left
        case_list
        ~init:(return (match_exp_sub, result_type))
        ~f:(fun acc { left = pat; right = case_exp } ->
          let* sub_acc, type_acc = return acc in
          let* env, pat_type =
            let* env, pat_typ = infer_pattern env pat in
            let* unified_sub1 = unify match_exp_type pat_typ in
            if with_exp
            then (
              let env = TypeEnv.apply unified_sub1 env in
              let* pat_names =
                extract_names_from_pat StringSet.empty pat >>| StringSet.elements
              in
              let generalized_schemes =
                Base.List.map pat_names ~f:(fun name ->
                  let typ = TypeEnv.find_type_exn env name in
                  let generalized_typ =
                    generalize env typ ~remove_from_env:true (Some name)
                  in
                  name, generalized_typ)
              in
              let env =
                Base.List.fold generalized_schemes ~init:env ~f:(fun env (key, value) ->
                  TypeEnv.extend env key value)
              in
              return (env, unified_sub1))
            else return (env, unified_sub1)
          in
          let* composed_sub1 = Subst.compose sub_acc pat_type in
          let* case_exp_sub, case_exp_type =
            infer_expression (TypeEnv.apply composed_sub1 env) case_exp
          in
          let* unified_sub2 = unify type_acc case_exp_type in
          let* composed_type2 =
            Subst.compose_all [ composed_sub1; case_exp_sub; unified_sub2 ]
          in
          return (composed_type2, Subst.apply composed_type2 type_acc))
    in
    let final_type =
      if with_exp
      then case_type
      else Type_arrow (Subst.apply cases_sub match_exp_type, case_type)
    in
    return (cases_sub, final_type)

  and infer_value_binding_list env sub = function
    | [] -> return (env, sub)
    | { pat; exp } :: rest ->
      let* new_sub, typ = infer_expression env exp in
      let* composed_sub = Subst.compose sub new_sub in
      let env = TypeEnv.apply composed_sub env in
      let generalized_ty =
        generalize env (Subst.apply composed_sub typ) ~remove_from_env:false None
      in
      let* env, pat_ty = infer_pattern env pat in
      let env = TypeEnv.extend_with_pattern env pat generalized_ty in
      let* unified_sub = unify typ pat_ty in
      let* final_sub = Subst.compose composed_sub unified_sub in
      let env = TypeEnv.apply final_sub env in
      infer_value_binding_list env final_sub rest

  and rec_infer_value_binding_list env fresh_acc sub let_binds =
    match let_binds, fresh_acc with
    | [], _ -> return (env, sub)
    | { pat = Pat_var pat; exp } :: rest, fresh :: fresh_acc ->
      let* new_sub, typ = infer_expression env exp in
      let* unified_sub = unify (Subst.apply new_sub fresh) typ in
      let* composed_sub = Subst.compose_all [ new_sub; unified_sub; sub ] in
      let env = TypeEnv.apply composed_sub env in
      let generalized_ty =
        generalize env (Subst.apply composed_sub fresh) ~remove_from_env:true (Some pat)
      in
      let env = TypeEnv.extend env pat generalized_ty in
      rec_infer_value_binding_list env fresh_acc composed_sub rest
    | _ -> fail `No_variable_rec
  ;;

  let infer_srtucture_item env ast =
    RList.fold_left ast ~init:(return env) ~f:(fun env ->
        function
        | Struct_eval exp ->
          let* _, _ = infer_expression env exp in
          return env
        | Struct_value (Nonrecursive, value_binding, value_binding_list) ->
          let* env, _ =
            infer_value_binding_list env Subst.empty (value_binding :: value_binding_list)
          in
          return env
        | Struct_value (Recursive, value_binding, value_binding_list) ->
          let* env, fresh_acc =
            extend_env_with_bind_names env (value_binding :: value_binding_list)
          in
          let* env, _ =
            rec_infer_value_binding_list
              env
              fresh_acc
              Subst.empty
              (value_binding :: value_binding_list)
          in
          return env)
  ;;
end

let empty_env = TypeEnv.empty

let env_with_print_int =
  TypeEnv.extend
    TypeEnv.empty
    "print_int"
    (Scheme (VarSet.empty, Type_arrow (Type_int, Type_unit)))
;;

let run_inferencer ast env = State.run (Infer.infer_srtucture_item env ast)
