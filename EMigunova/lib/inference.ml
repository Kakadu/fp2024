(** Copyright 2025, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

let rec print_type (ty : ttype) =
  match ty with
  | Type_int -> Printf.printf "int"
  | Type_bool -> Printf.printf "bool"
  | Type_char -> Printf.printf "char"
  | Type_string -> Printf.printf "string"
  | Type_unit -> Printf.printf "unit"
  | Type_var ident -> Printf.printf "%s" ident
  | Type_option (Some ty) ->
    (match ty with
     | Type_tuple _ ->
       Printf.printf "(";
       print_type ty;
       Printf.printf ")"
     | _ -> print_type ty);
    Printf.printf " option"
  | Type_option None -> ()
  | Type_list ty ->
    (match ty with
     | Type_tuple _ ->
       Printf.printf "(";
       print_type ty;
       Printf.printf ")"
     | _ -> print_type ty);
    Printf.printf " list"
  | Type_tuple ty_list ->
    (match ty_list with
     | first :: second :: rest ->
       (match first with
        | Type_arrow _ ->
          Printf.printf "(";
          print_type first;
          Printf.printf ")"
        | _ -> print_type first);
       Printf.printf "*";
       print_type (Type_tuple (second :: rest))
     | single :: [] ->
       (match single with
        | Type_arrow _ ->
          Printf.printf "(";
          print_type single;
          Printf.printf ")"
        | _ -> print_type single)
     | _ -> ())
  | Type_arrow (ty1, ty2) ->
    (match ty1 with
     | Type_arrow _ ->
       Printf.printf "(";
       print_type ty1;
       Printf.printf ")"
     | _ -> print_type ty1);
    Printf.printf "->";
    print_type ty2
;;

type error =
  [ `No_variable_rec
  | `No_arg_rec
  | `Bound_several_times of string
  | `Occurs_check of string * ttype (* * core_type *)
  | `No_variable of string
  | `Unification_failed of string * ttype * ttype (* of core_type * core_type *)
  ]

let print_error (e : error) =
  match e with
  | `No_variable_rec ->
    Printf.printf
      "Recursive binding failed: a function was expected as the RHS. Recursive binding \
       is impossible for that variable. It would lead to infinite recursion."
  | `No_arg_rec ->
    Printf.printf
      "Recursive binding failed: the LHS of the recursive binding must not be a composed \
       pattern (e.g. tuple, list, etc.). A variable is required."
  | `Bound_several_times id ->
    Printf.printf
      "Binding faild: simultaneous binding of one identifier to several values. The \
       identifier that was bound several times: '%s'."
      id
  | `Occurs_check (id, ty) ->
    Printf.printf "Occurs check failed: the type variable %s occurs inside" id;
    print_type ty
  | `No_variable id -> Printf.printf "Undefined variable '%s'" id
  | `Unification_failed (id, ty1, ty2) ->
    Printf.printf "Unification( %s ) failed for following unifiable types: " id;
    print_type ty1;
    Printf.printf " and ";
    print_type ty2
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
end

type scheme = Scheme of VarSet.t * ttype

module Type = struct
  (*gets type_core and returns set of idents of all type_core's type variables *)
  let free_vars =
    let rec helper acc = function
      | Type_option (Some ty) | Type_list ty -> helper acc ty
      | Type_var name -> VarSet.add name acc
      | Type_tuple ty_list -> List.fold_left helper acc ty_list
      | Type_arrow (ty1, ty2) -> VarSet.union (helper acc ty1) (helper acc ty2)
      | _ -> acc
    in
    helper VarSet.empty
  ;;

  (*gets the identifier and core_type and checks whether a type variable with this identifier occurs in the type*)
  let occurs_in var ty = VarSet.mem var (free_vars ty)
end

module Subst = struct
  open State
  open State.Syntax
  open Base

  let empty = Map.empty (module String)
  let singleton1 = Map.singleton (module String)

  (*gets some name of type variable and core_type and checks if an name doesn't occur in a type (bad case of infinite loop)
    and returns wrapped sub*)
  let singleton key value =
    match value with
    | Type_var id when String.equal key id -> return empty
    | _ ->
      if Type.occurs_in key value
      then fail (`Occurs_check (key, value))
      else return (Map.singleton (module String) key value)
  ;;

  let remove = Map.remove

  (*gets substitution and core_type then performs a substitution and returns new concretized core_type*)
  let apply sub =
    let rec helper = function
      | Type_var name as ty ->
        (match Map.find sub name with
         | Some ty -> ty
         | None -> ty)
      | Type_option (Some ty) -> Type_option (Some (helper ty))
      | Type_list ty -> Type_list (helper ty)
      | Type_tuple ty_list -> Type_tuple (List.map ty_list ~f:helper)
      | Type_arrow (ty1, ty2) -> Type_arrow (helper ty1, helper ty2)
      | ty -> ty
    in
    helper
  ;;

  let rec unify (debug_info : string) (l : ttype) (r : ttype) =
    match l, r with
    | Type_unit, Type_unit
    | Type_int, Type_int
    | Type_char, Type_char
    | Type_string, Type_string
    | Type_bool, Type_bool
    | Type_option None, Type_option None
    | Type_option (Some _), Type_option None
    | Type_option None, Type_option (Some _) -> return empty
    | Type_var l, Type_var r when String.equal l r -> return empty
    | Type_var name, ty | ty, Type_var name -> singleton name ty
    | Type_list ty1, Type_list ty2 | Type_option (Some ty1), Type_option (Some ty2) ->
      unify "list_unify" ty1 ty2
    | Type_tuple list1, Type_tuple list2 ->
      let rec helper acc = function
        | first_ty1 :: rest1, first_ty2 :: rest2 ->
          let* acc_sub = acc in
          let* unified_sub =
            unify "tuple_unify" (apply acc_sub first_ty1) (apply acc_sub first_ty2)
          in
          helper (compose acc_sub unified_sub) (rest1, rest2)
        | [], [] -> acc
        | _ -> fail (`Unification_failed (debug_info, l, r))
        (*considering case when list1 and list2 have different lengths*)
      in
      helper (return empty) (list1, list2)
    | Type_arrow (arg1, res1), Type_arrow (arg2, res2) ->
      let* unified_sub1 = unify "([type1] -> [...]) and ([type2] -> [...])" arg1 arg2 in
      let* unified_sub2 =
        unify "arrow_unify_result" (apply unified_sub1 res1) (apply unified_sub1 res2)
      in
      compose unified_sub1 unified_sub2
    | _ -> fail (`Unification_failed (debug_info, l, r))

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
      let* new_sub = unify "extend sub" value existing_value in
      compose sub new_sub

  and compose sub1 sub2 = RMap.fold sub2 ~init:(return sub1) ~f:extend

  let compose_all sub_list = RList.fold_left sub_list ~init:(return empty) ~f:compose
end

module Scheme = struct
  let free_vars (Scheme (bind_set, ty)) = VarSet.diff (Type.free_vars ty) bind_set

  let apply sub (Scheme (bind_set, ty)) =
    let new_sub = VarSet.fold (fun key sub -> Subst.remove sub key) bind_set sub in
    let new_ty = Subst.apply new_sub ty in
    let is_generalized_type_var id = not (Base.String.is_prefix ~prefix:"'ty" id) in
    let rec extract_id_from_ty acc_set = function
      | Type_var id -> VarSet.add id acc_set
      | Type_option (Some ty) -> extract_id_from_ty acc_set ty
      | Type_tuple ty_list ->
        Base.List.fold_left ty_list ~init:acc_set ~f:(fun acc_set ty ->
          extract_id_from_ty acc_set ty)
      | Type_list ty -> extract_id_from_ty acc_set ty
      | Type_arrow (ty1, ty2) -> extract_id_from_ty (extract_id_from_ty acc_set ty1) ty2
      | _ -> acc_set
    in
    let new_bind_set =
      VarSet.fold
        (fun id new_bind_set ->
          if is_generalized_type_var id then VarSet.add id new_bind_set else new_bind_set)
        (extract_id_from_ty VarSet.empty new_ty)
        VarSet.empty
    in
    Scheme (new_bind_set, new_ty)
  ;;
end

module TypeEnv = struct
  open Base

  type t = (ident, scheme, String.comparator_witness) Map.t

  let empty = Map.empty (module String)
  let extend env key value = Map.update env key ~f:(fun _ -> value)

  let rec extend_with_pattern env_acc pat (Scheme (bind_set, ty) as scheme) =
    match pat, ty with
    | Pattern_var id, _ -> extend env_acc id scheme
    | Pattern_tuple pat_list, Type_tuple ty_list ->
      (match pat_list, ty_list with
       | first_pat :: pat_rest, first_ty :: ty_rest ->
         let new_acc_env =
           extend_with_pattern env_acc first_pat (Scheme (bind_set, first_ty))
         in
         extend_with_pattern
           new_acc_env
           (Pattern_tuple pat_rest)
           (Scheme (bind_set, Type_tuple ty_rest))
       | _ -> env_acc)
    | Pattern_list_sugar_case pat_list, Type_list ty ->
      Base.List.fold_left pat_list ~init:env_acc ~f:(fun env_acc pat ->
        extend_with_pattern env_acc pat (Scheme (bind_set, ty)))
    | Pattern_list_constructor_case pat_list, Type_list ty ->
      (match pat_list with
       | single_pat :: [] ->
         extend_with_pattern env_acc single_pat (Scheme (bind_set, Type_list ty))
       | first :: rest ->
         extend_with_pattern
           (extend_with_pattern env_acc first (Scheme (bind_set, ty)))
           (Pattern_list_constructor_case rest)
           (Scheme (bind_set, Type_list ty))
       | [] -> env_acc)
    | Pattern_option (Some pat), Type_option (Some ty) ->
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
end

module Infer = struct
  open Ast
  open State
  open State.Syntax

  let unify = Subst.unify
  let fresh_var = fresh >>| fun n -> Type_var ("'ty" ^ Int.to_string n)

  let fresh_var_instantiate id =
    (*e.g. 'ty3instantiate_'a *)
    fresh >>| fun n -> Type_var (Printf.sprintf "%s%dinstantiate_%s" "'ty" n id)
  ;;

  let instantiate (Scheme (bind_set, ty)) =
    VarSet.fold
      (fun name ty ->
        let* ty = ty in
        let* fresh = fresh_var_instantiate name in
        let* sub = Subst.singleton name fresh in
        return (Subst.apply sub ty))
      bind_set
      (return ty)
  ;;

  let generalize env ty =
    let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
    let new_free, new_ty, _ =
      VarSet.fold
        (fun str (temp_free, temp_ty, n) ->
          let degree = n / 26 in
          let new_str =
            if Base.String.is_substring ~substring:"instantiate" str
            then (
              let index = String.index str '_' in
              String.sub str (index + 1) (String.length str - (index + 1)))
            else
              (* 97 - is number 'a' in ASCII-table *)
              Printf.sprintf
                "'%c%s"
                (Char.chr (97 + (n mod 26)))
                (if degree = 0 then "" else Int.to_string degree)
            (*new_str : 'a, ... ,'z, 'a1, ... ,'z1, ...*)
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

  let rec extract_names_from_pat f acc = function
    | Pattern_var id -> f acc id
    | Pattern_option (Some pat) -> extract_names_from_pat f acc pat
    | Pattern_tuple pat_list
    | Pattern_list_sugar_case pat_list
    | Pattern_list_constructor_case pat_list ->
      (match pat_list with
       | [] -> return acc
       | first_pat :: rest_pats ->
         let* acc = extract_names_from_pat f acc first_pat in
         extract_names_from_pat f acc (Pattern_tuple rest_pats))
    | _ -> return acc
  ;;

  module StringSet = struct
    include Set.Make (String)

    let add_id set value =
      if mem value set then fail (`Bound_several_times value) else return (add value set)
    ;;
  end

  let rec remove_patterns_from_env env = function
    | [] -> return env
    | pat :: rest_pats ->
      let* env =
        extract_names_from_pat (fun env id -> return (Base.Map.remove env id)) env pat
      in
      remove_patterns_from_env env rest_pats
  ;;

  let check_names_from_pat pat =
    extract_names_from_pat StringSet.add_id StringSet.empty pat
  ;;

  let rec infer_pattern env pat =
    let* _ = check_names_from_pat pat in
    match pat with
    | Pattern_any ->
      let* fresh_type_var = fresh_var in
      return (env, fresh_type_var)
    | Pattern_var id ->
      let* fresh_type_var = fresh_var in
      let env = TypeEnv.extend env id (Scheme (VarSet.empty, fresh_type_var)) in
      return (env, fresh_type_var)
    | Pattern_const const ->
      (match const with
       | Const_int _ -> return (env, Type_int)
       | Const_string _ -> return (env, Type_string)
       | Const_char _ -> return (env, Type_char)
       | Const_bool _ -> return (env, Type_bool)
       | Const_unit -> return (env, Type_unit))
    | Pattern_tuple pat_list ->
      let* env, list_of_types =
        RList.fold_right
          pat_list
          ~init:(return (env, []))
          ~f:(fun element (env, list_of_types) ->
            let* new_env, element_type = infer_pattern env element in
            return (new_env, element_type :: list_of_types))
      in
      return (env, Type_tuple list_of_types)
    | Pattern_option None -> return (env, Type_option None)
    | Pattern_option (Some pat) ->
      let* env, type_of_pat = infer_pattern env pat in
      return (env, Type_option (Some type_of_pat))
    | Pattern_list_sugar_case pat_list ->
      let* list_element_type_var = fresh_var in
      let* env, sub =
        RList.fold_left
          pat_list
          ~init:(return (env, Subst.empty))
          ~f:(fun (acc_env, acc_sub) pat ->
            let* env, pat_type = infer_pattern acc_env pat in
            let* unified_sub =
              unify "infer pattern list sugar" list_element_type_var pat_type
            in
            let* composed_sub = Subst.compose unified_sub acc_sub in
            return (env, composed_sub))
      in
      let list_element_type = Subst.apply sub list_element_type_var in
      let env = TypeEnv.apply sub env in
      return (env, Type_list list_element_type)
    | Pattern_list_constructor_case pat_list ->
      let* fresh = fresh_var in
      let rec helper env sub_acc rest =
        match rest with
        | [] -> return (env, sub_acc)
        | single :: [] ->
          let* env, ty = infer_pattern env single in
          let* unified_sub = unify "" (Type_list fresh) ty in
          let* composed_sub = Subst.compose sub_acc unified_sub in
          helper env composed_sub []
        | first :: rest ->
          let* env, ty = infer_pattern env first in
          let* unified_sub = unify "" fresh ty in
          let* composed_sub = Subst.compose sub_acc unified_sub in
          helper env composed_sub rest
      in
      let* env, sub = helper env Subst.empty pat_list in
      let result_ty = Subst.apply sub fresh in
      return (env, Type_list result_ty)
  ;;

  let rec get_pat_type_from_env env = function
    | Pattern_var id -> return (TypeEnv.find_type_exn env id)
    | Pattern_any -> fresh_var
    | Pattern_const const ->
      (match const with
       | Const_int _ -> return Type_int
       | Const_string _ -> return Type_string
       | Const_char _ -> return Type_char
       | Const_bool _ -> return Type_bool
       | Const_unit -> return Type_unit)
    | Pattern_option (Some pat) ->
      let* ty = get_pat_type_from_env env pat in
      return (Type_option (Some ty))
    | Pattern_tuple pat_list ->
      let* ty_list =
        RList.fold_left pat_list ~init:(return []) ~f:(fun acc_list pat ->
          let* ty = get_pat_type_from_env env pat in
          return (acc_list @ [ ty ]))
      in
      return (Type_tuple ty_list)
    | Pattern_list_sugar_case (first_pat :: _)
    | Pattern_list_constructor_case (first_pat :: _) ->
      let* ty = get_pat_type_from_env env first_pat in
      return (Type_list ty)
    | _ -> return (Type_option None)
  ;;

  let extend_env_with_args env args_list =
    RList.fold_right
      args_list
      ~init:(return (env, []))
      ~f:(fun pat (env, pat_ty_list) ->
        let* env, pat_ty = infer_pattern env pat in
        return (env, pat_ty :: pat_ty_list))
  ;;

  (*this function is called when we deal with recursive bindings.
    And our language forbids recursive values*)
  let extend_env_with_bind_names env let_binding_list =
    RList.fold_left let_binding_list ~init:(return env) ~f:(fun env ->
        function
        | Let_binding (_, Let_fun (id, _), _) ->
          let* fresh = fresh_var in
          let env = TypeEnv.extend env id (Scheme (VarSet.empty, fresh)) in
          return env
        | Let_binding (_, Let_pattern (Pattern_var id), Expr_anonym_fun (_, _))
        | Let_binding (_, Let_pattern (Pattern_var id), Expr_function_fun _) ->
          let* fresh = fresh_var in
          let env = TypeEnv.extend env id (Scheme (VarSet.empty, fresh)) in
          return env
        | Let_binding (_, Let_pattern (Pattern_var _), _) -> fail `No_variable_rec
        | _ -> fail `No_arg_rec)
  ;;

  let rec check_names_from_let_binds =
    RList.fold_left ~init:(return StringSet.empty) ~f:(fun set_acc ->
        function
        | Let_binding (_, Let_fun (fun_identifier, _), _) ->
          StringSet.add_id set_acc fun_identifier
        | Let_binding (_, Let_pattern pat, _) ->
          extract_names_from_pat StringSet.add_id set_acc pat
        | Let_rec_and_binding let_binding_list ->
          let* let_rec_and_acc = check_names_from_let_binds let_binding_list in
          return (StringSet.union set_acc let_rec_and_acc))
  ;;

  let rec get_names_from_let_bind env = function
    | Let_binding (_, Let_pattern pat, _) ->
      extract_names_from_pat
        (fun acc id -> return (acc @ [ id, TypeEnv.find_type_exn env id ]))
        []
        pat
    | Let_binding (_, Let_fun (id, _), _) ->
      extract_names_from_pat
        (fun acc id -> return (acc @ [ id, TypeEnv.find_type_exn env id ]))
        []
        (Pattern_var id)
    | Let_rec_and_binding let_binding_list ->
      RList.fold_left
        let_binding_list
        ~init:(return [])
        ~f:(fun result_list let_binding ->
          let* last_element = get_names_from_let_bind env let_binding in
          return (result_list @ last_element))
  ;;

  let lookup_env id env =
    match TypeEnv.find env id with
    | Some scheme ->
      let* ans = instantiate scheme in
      return (Subst.empty, ans)
    | None -> fail (`No_variable id)
  ;;

  let rec infer_expression env = function
    | Expr_var id -> lookup_env id env
    | Expr_const const ->
      (match const with
       | Const_int _ -> return (Subst.empty, Type_int)
       | Const_string _ -> return (Subst.empty, Type_string)
       | Const_char _ -> return (Subst.empty, Type_char)
       | Const_bool _ -> return (Subst.empty, Type_bool)
       | Const_unit -> return (Subst.empty, Type_unit))
    | Expr_construct_in (let_binding, expression) ->
      (match let_binding with
       | Let_binding (Non_recursive, _, _) ->
         let* env, sub = infer_value_non_rec_binding env let_binding in
         let* expr_sub, expr_ty = infer_expression env expression in
         let* composed_sub = Subst.compose sub expr_sub in
         return (composed_sub, expr_ty)
       | Let_binding (Recursive, _, _) | Let_rec_and_binding _ ->
         let let_binding_list =
           match let_binding with
           | Let_rec_and_binding let_binding_list -> let_binding_list
           | _ -> let_binding :: []
         in
         let* env = extend_env_with_bind_names env let_binding_list in
         let* env, sub1 = infer_rec_value_binding_list env Subst.empty let_binding_list in
         let* sub2, ty2 = infer_expression env expression in
         let* composed_sub = Subst.compose sub2 sub1 in
         return (composed_sub, ty2))
    | Expr_anonym_fun (first_pat :: rest_pats, expr) ->
      let* env, ty1 = infer_pattern env first_pat in
      let* sub, ty2 =
        match rest_pats with
        | [] -> infer_expression env expr
        | hd :: tl -> infer_expression env (Expr_anonym_fun (hd :: tl, expr))
      in
      return (sub, Type_arrow (Subst.apply sub ty1, ty2))
    | Expr_binary_op (bin_op, exp1, exp2) ->
      let* sub1, ty1 = infer_expression env exp1 in
      let* sub2, ty2 = infer_expression (TypeEnv.apply sub1 env) exp2 in
      let* required_arg_ty, required_result_ty =
        match bin_op with
        | Plus | Sub | Mul | Div -> return (Type_int, Type_int)
        | Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual ->
          let* fresh = fresh_var in
          return (fresh, Type_bool)
        | _ -> return (Type_bool, Type_bool)
      in
      let* unified_sub1 =
        Subst.unify "expr binary op1" required_arg_ty (Subst.apply sub2 ty1)
      in
      let* unified_sub2 =
        Subst.unify "expr binary op2" required_arg_ty (Subst.apply unified_sub1 ty2)
      in
      let* composed_sub = Subst.compose_all [ sub1; sub2; unified_sub1; unified_sub2 ] in
      let* sub_expr1_type = unify "" ty1 (Subst.apply composed_sub required_arg_ty) in
      let* sub_expr2_type = unify "" ty2 (Subst.apply composed_sub required_arg_ty) in
      let* composed_sub =
        Subst.compose_all [ sub_expr1_type; sub_expr2_type; composed_sub ]
      in
      return (composed_sub, required_result_ty)
    | Expr_application (expr, expr_list) ->
      let* fresh = fresh_var in
      let rec build_arrow_chain expression_list =
        match expression_list with
        | first_expr :: [] ->
          let* sub, ty = infer_expression env first_expr in
          return (sub, Type_arrow (ty, fresh))
        | first_expr :: expression_list ->
          let* sub1, ty1 = build_arrow_chain expression_list in
          let* sub2, ty2 = infer_expression (TypeEnv.apply sub1 env) first_expr in
          let* composed_sub = Subst.compose sub1 sub2 in
          return (composed_sub, Subst.apply composed_sub (Type_arrow (ty2, ty1)))
        | [] -> return (Subst.empty, Type_option None)
      in
      let* args_sub, required_ty = build_arrow_chain expr_list in
      let* expr_sub, expr_ty = infer_expression (TypeEnv.apply args_sub env) expr in
      let* unified_sub =
        unify "fun application " expr_ty (Subst.apply expr_sub required_ty)
      in
      let* composed_sub = Subst.compose_all [ unified_sub; expr_sub; args_sub ] in
      let final_ty = Subst.apply composed_sub fresh in
      return (composed_sub, final_ty)
    | Expr_function_fun case_list ->
      let* fresh_for_matching = fresh_var in
      let* fresh_for_result = fresh_var in
      infer_match_exp
        env
        ~with_exp:false
        Subst.empty
        fresh_for_matching
        fresh_for_result
        case_list
    | Expr_match_with (expr, case_list) ->
      let* expr_sub, expr_ty = infer_expression env expr in
      let env = TypeEnv.apply expr_sub env in
      let* fresh_for_result = fresh_var in
      infer_match_exp env ~with_exp:true expr_sub expr_ty fresh_for_result case_list
    | Expr_tuple expr_list ->
      let* sub, ty_list =
        RList.fold_right
          ~f:(fun expr (sub_acc, ty_list) ->
            let* sub, ty = infer_expression (TypeEnv.apply sub_acc env) expr in
            let* sub_acc = Subst.compose sub_acc sub in
            return (sub_acc, ty :: ty_list))
          ~init:(return (Subst.empty, []))
          expr_list
      in
      let result_ty = Subst.apply sub (Type_tuple ty_list) in
      return (sub, result_ty)
    | Expr_list_construct expr_list ->
      let* fresh = fresh_var in
      let rec infer_list_constract env acc_sub = function
        | [] -> return (Subst.empty, Type_option None)
        | end_element :: [] ->
          let* expr_sub, expr_ty = infer_expression env end_element in
          let* unified_sub = unify "expr list construct end" expr_ty (Type_list fresh) in
          let* composed_sub = Subst.compose_all [ expr_sub; unified_sub; acc_sub ] in
          return (composed_sub, Type_list (Subst.apply composed_sub fresh))
        | expr_element :: expr_rest ->
          let* expr_sub, expr_ty = infer_expression env expr_element in
          let* unified_sub = unify "expr list construct element" expr_ty fresh in
          let* composed_sub = Subst.compose_all [ expr_sub; unified_sub; acc_sub ] in
          let env = TypeEnv.apply composed_sub env in
          let* sub, ty = infer_list_constract env composed_sub expr_rest in
          return (sub, ty)
      in
      infer_list_constract env Subst.empty expr_list
    | Expr_list_sugar expr_list ->
      let* fresh = fresh_var in
      let* sub =
        RList.fold_left expr_list ~init:(return Subst.empty) ~f:(fun acc_sub expr ->
          let* expr_sub, expr_type = infer_expression (TypeEnv.apply acc_sub env) expr in
          let* unified_sub =
            unify
              "expr list sugar"
              expr_type
              (Subst.apply acc_sub (Subst.apply expr_sub fresh))
          in
          let* composed_sub = Subst.compose_all [ acc_sub; unified_sub; expr_sub ] in
          return composed_sub)
      in
      let fresh = Subst.apply sub fresh in
      return (sub, Type_list fresh)
    | Expr_option None -> return (Subst.empty, Type_option None)
    | Expr_option (Some expr) ->
      let* sub, ty = infer_expression env expr in
      return (sub, Type_option (Some ty))
    | Expr_if_then_else (if_expr, then_expr, else_expr) ->
      let* sub1, ty1 = infer_expression env if_expr in
      let* sub2, ty2 = infer_expression (TypeEnv.apply sub1 env) then_expr in
      let* sub3, ty3 =
        infer_expression (TypeEnv.apply sub2 (TypeEnv.apply sub1 env)) else_expr
      in
      let* sub4 = unify "if (here) then else" ty1 Type_bool in
      let* sub5 = unify "if _ then [first type] else [second type]" ty2 ty3 in
      let* final_sub = Subst.compose_all [ sub5; sub4; sub3; sub2; sub1 ] in
      return (final_sub, Subst.apply final_sub ty2)
    | Typed_expression (ty, expr) ->
      let* expr_sub, expr_ty = infer_expression env expr in
      let* unified_sub = unify "typed expression" expr_ty ty in
      let* final_sub = Subst.compose unified_sub expr_sub in
      return (final_sub, Subst.apply unified_sub expr_ty)
    | _ -> return (Subst.empty, Type_option None)

  and infer_match_exp env ~with_exp match_exp_sub match_exp_ty result_ty case_list =
    let* cases_sub, case_ty =
      RList.fold_left
        case_list
        ~init:(return (match_exp_sub, result_ty))
        ~f:(fun (sub_acc, ty_acc) (pat, case_exp) ->
          let* env, pat_sub =
            if with_exp
            then (
              let env = TypeEnv.apply sub_acc env in
              let* _, pat_ty = infer_pattern env pat in
              let* unified_sub1 =
                unify
                  "infer_match_exp, match expression"
                  pat_ty
                  (Subst.apply sub_acc match_exp_ty)
              in
              let gen_pat_ty_sch = generalize env (Subst.apply unified_sub1 pat_ty) in
              let env = TypeEnv.extend_with_pattern env pat gen_pat_ty_sch in
              return (env, unified_sub1))
            else
              let* env, pat_ty = infer_pattern env pat in
              let* unified_sub1 = unify "" pat_ty match_exp_ty in
              let env = TypeEnv.apply unified_sub1 env in
              return (env, unified_sub1)
          in
          let* composed_sub1 = Subst.compose sub_acc pat_sub in
          let* case_exp_sub, case_exp_ty =
            infer_expression (TypeEnv.apply composed_sub1 env) case_exp
          in
          let* unified_sub2 =
            unify "infer_match_exp, result expression" ty_acc case_exp_ty
          in
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

  and infer_value_non_rec_binding env = function
    | Let_binding (Non_recursive, Let_fun (id, pattern_list), expr) ->
      let* env, _ = extend_env_with_args env pattern_list in
      let* expr_sub, expr_ty = infer_expression env expr in
      let env = TypeEnv.apply expr_sub env in
      let* let_bind_ty =
        let rec build_arrow_chain = function
          | single_ty :: [] -> Type_arrow (single_ty, expr_ty)
          | first_ty :: rest -> Type_arrow (first_ty, build_arrow_chain rest)
          | _ -> expr_ty
        in
        let* pat_types =
          RList.fold_left pattern_list ~init:(return []) ~f:(fun acc_list pat ->
            let* pat_ty = get_pat_type_from_env env pat in
            return (acc_list @ [ pat_ty ]))
        in
        return (build_arrow_chain pat_types)
      in
      let* env = remove_patterns_from_env env pattern_list in
      let generalized_let_bind_ty = generalize env let_bind_ty in
      let env = TypeEnv.extend env id generalized_let_bind_ty in
      return (env, expr_sub)
    | Let_binding (Non_recursive, Let_pattern pat, expr) ->
      let* expr_sub, expr_ty = infer_expression env expr in
      let env = TypeEnv.apply expr_sub env in
      let* _, pat_ty = infer_pattern env pat in
      let* unified_sub1 = unify "let binding with pattern" expr_ty pat_ty in
      let env = TypeEnv.apply unified_sub1 env in
      let let_pat_ty_sch = generalize env (Subst.apply unified_sub1 expr_ty) in
      let (Scheme (_, let_pat_ty)) = let_pat_ty_sch in
      let* env, init_pat_ty = infer_pattern env pat in
      let* unified_sub2 = unify "" init_pat_ty let_pat_ty in
      let env = TypeEnv.apply unified_sub1 env in
      let env = TypeEnv.extend_with_pattern env pat let_pat_ty_sch in
      let* composed_sub = Subst.compose_all [ unified_sub1; unified_sub2; expr_sub ] in
      return (env, composed_sub)
    | Let_binding (Recursive, _, _) | Let_rec_and_binding _ -> return (env, Subst.empty)

  and infer_rec_value_binding_list env sub let_binds =
    let infer_rec_vb env new_sub ty id pattern_list rest =
      let env = TypeEnv.apply new_sub env in
      let infered_let_bind_type = TypeEnv.find_type_exn env id in
      (*this type was infered during infering RSH of rec let_biding*)
      let* manual_let_bind_ty =
        (*this type is built by us manually*)
        let rec build_arrow_chain = function
          | single_ty :: [] -> Type_arrow (single_ty, ty)
          | first_ty :: rest -> Type_arrow (first_ty, build_arrow_chain rest)
          | [] -> infered_let_bind_type
        in
        let* pat_types =
          RList.fold_right pattern_list ~init:(return []) ~f:(fun pat pat_ty_list ->
            let* pat_ty = get_pat_type_from_env env pat in
            return (pat_ty :: pat_ty_list))
        in
        return (build_arrow_chain pat_types)
      in
      let* unified_sub =
        unify "rec let binding" manual_let_bind_ty infered_let_bind_type
      in
      let* composed_sub = Subst.compose_all [ new_sub; unified_sub; sub ] in
      let* env = remove_patterns_from_env env (Pattern_var id :: pattern_list) in
      let generalized_let_bind_sch =
        generalize env (Subst.apply composed_sub manual_let_bind_ty)
      in
      let env = TypeEnv.extend env id generalized_let_bind_sch in
      infer_rec_value_binding_list (TypeEnv.apply composed_sub env) composed_sub rest
    in
    match let_binds with
    | [] -> return (env, sub)
    | Let_binding
        ( Recursive
        , Let_fun (id, [])
        , ((Expr_anonym_fun (_, _) | Expr_function_fun _) as expr) )
      :: rest ->
      let* new_sub, ty = infer_expression env expr in
      infer_rec_vb env new_sub ty id [] rest
    | Let_binding (Recursive, Let_fun (_, []), _) :: _ -> fail `No_variable_rec
    | Let_binding (Recursive, Let_fun (id, pattern_list), expr) :: rest ->
      let* env, _ = extend_env_with_args env pattern_list in
      let* expr_sub, ty = infer_expression env expr in
      infer_rec_vb env expr_sub ty id pattern_list rest
    | _ -> fail `No_variable_rec

  and infer_let_biding (env, out_list) let_binding =
    match let_binding with
    | Let_binding (Non_recursive, _, _) ->
      let* env, _ = infer_value_non_rec_binding env let_binding in
      let* id_list = get_names_from_let_bind env let_binding in
      return (env, out_list @ id_list)
    | Let_binding (Recursive, _, _) ->
      let* env = extend_env_with_bind_names env (let_binding :: []) in
      let* env, _ = infer_rec_value_binding_list env Subst.empty (let_binding :: []) in
      let* id_list = get_names_from_let_bind env let_binding in
      return (env, out_list @ id_list)
    | Let_rec_and_binding biding_list ->
      let* _ = check_names_from_let_binds biding_list in
      let* env = extend_env_with_bind_names env biding_list in
      let* env, _ = infer_rec_value_binding_list env Subst.empty biding_list in
      let* id_list =
        RList.fold_left biding_list ~init:(return []) ~f:(fun result_list let_binding ->
          let* last_element = get_names_from_let_bind env let_binding in
          return (result_list @ last_element))
      in
      return (env, out_list @ id_list)
  ;;

  let infer_srtucture env ast =
    let* _, out_list = RList.fold_left ast ~init:(return (env, [])) ~f:infer_let_biding in
    let rec remove_duplicates =
      let fun_equal el1 el2 =
        match el1, el2 with
        | (id1, _), (id2, _) -> String.equal id1 id2
      in
      function
      | x :: xs when not (Base.List.mem xs x ~equal:fun_equal) ->
        x :: remove_duplicates xs
      | _ :: xs -> remove_duplicates xs
      | [] -> []
    in
    return (remove_duplicates out_list)
  ;;
end

let env_with_print_funs =
  let print_fun_list =
    [ "print_int", Scheme (VarSet.empty, Type_arrow (Type_int, Type_unit))
    ; "print_endline", Scheme (VarSet.empty, Type_arrow (Type_string, Type_unit))
    ]
  in
  List.fold_left
    (fun env (id, sch) -> TypeEnv.extend env id sch)
    TypeEnv.empty
    print_fun_list
;;

let run_inferencer ast = State.run (Infer.infer_srtucture env_with_print_funs ast)
