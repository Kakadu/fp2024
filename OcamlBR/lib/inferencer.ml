(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree

module R : sig
  type 'a t

  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
    val fold_right : 'a list -> init:'b t -> f:('a -> 'b -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  ('a, 'b, 'c) Base.Map.t
      -> init:'d t
      -> f:('a -> 'b -> 'd -> 'd t)
      -> 'd t
  end

  val fresh : int t
  val run : 'a t -> ('a, error) Result.t
end = struct
  type 'a t =
    int -> int * ('a, error) Result.t (* a composition of result and state monad *)

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f state ->
    let last, res = m state in
    match res with
    | Result.Error x ->
      last, Error x (* if the first computation (m) fails, propagate the error *)
    | Result.Ok a ->
      f a last (* if it succeeds, pass the result (a) to the next computation (f) *)
  ;;

  (* wraps a value x into the monad without modifying the state. It returns the current state (last) and a successful result (Ok x) *)
  let return x last = last, Base.Result.return x

  (* creates a failed monadic computation, propagating the error (e) while leaving the state unchanged *)
  let fail e st = st, Base.Result.fail e
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Result.Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
  ;;

  (* syntatic sugar *)
  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  (* defines a monadic version of List.fold_left, can handle computations that may fail *)
  (* e.g. for solving multiple type constraints during inference *)
  module RList = struct
    let fold_left xs ~init ~f =
      Base.List.fold_left xs ~init ~f:(fun acc x ->
        let open Syntax in
        let* acc = acc in
        f acc x)
    ;;

    let fold_right xs ~init ~f =
      let open Syntax in
      Base.List.fold_right xs ~init ~f:(fun x acc ->
        let* acc = acc in
        f x acc)
    ;;
  end

  module RMap = struct
    let fold_left map ~init ~f =
      Base.Map.fold map ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end

  (* generates new state for representing a new fresh variable *)
  let fresh last = last + 1, Result.Ok last

  (* runs from initial state 0 and extracts the result from the monadic computation *)
  let run monad = snd (monad 0)
end

module Type = struct
  (* checks if a type variable v occurs anywhere within a given type; primarily used during type unification *)
  let rec occurs_in v = function
    | TVar b -> b = v
    | TPrim _ -> false
    | TArrow (l, r) -> occurs_in v l || occurs_in v r
    | TTuple (fst, snd, rest) ->
      occurs_in v fst || occurs_in v snd || List.exists (occurs_in v) rest
    | TList t -> occurs_in v t
    | TOption t -> occurs_in v t
  ;;

  (* | TRecord _ -> false *)

  (* computes the set of all type variables in a given type; primarily used to generalize types during type inference *)
  let type_vars =
    let rec helper acc = function
      | TVar b -> VarSet.add b acc
      | TPrim _ -> acc
      | TArrow (l, r) -> helper (helper acc l) r
      | TList t -> helper acc t
      | TTuple (fst, snd, rest) -> List.fold_left helper acc (fst :: snd :: rest)
      | TOption t -> helper acc t
      (* | TRecord _ -> acc *)
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val apply : t -> ty -> ty
  val singleton : type_var -> ty -> t R.t
  val unify : ty -> ty -> t R.t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
  val remove : t -> type_var -> t
  (* val pp_subst : Format.formatter -> t -> unit *)
end = struct
  open R
  open R.Syntax
  open Base

  type t = (type_var, ty, Int.comparator_witness) Map.t

  (* let pp_subst ppf sub =
     Base.Map.iteri sub ~f:(fun ~key ~data ->
     Stdlib.Format.fprintf ppf "[%d = %a] " key pp_ty data)
     ;; *)

  let empty = Map.empty (module Int)

  (* creates a substitution for variable [v] with type [ty] if no occurence is found *)
  let mapping v ty =
    if Type.occurs_in v ty
    then fail (`Occurs_check ("type variable " ^ Int.to_string v ^ " inside type", ty))
    else return (v, ty)
  ;;

  let singleton k v =
    let* k, v = mapping k v in
    return (Base.Map.singleton (module Base.Int) k v)
  ;;

  let find = Map.find
  let remove = Map.remove

  (* applies a substitution to a type *)
  let apply subst =
    let rec helper = function
      | TVar v as ty ->
        (match find subst v with
         | Some ty' -> ty'
         | None -> ty)
      | TArrow (l, r) -> TArrow (helper l, helper r)
      | TTuple (f, s, rest) -> TTuple (helper f, helper s, List.map ~f:helper rest)
      | TList t -> TList (helper t)
      | TPrim _ as ty -> ty
      | TOption t -> TOption (helper t)
      (* | TRecord _ as ty -> ty *)
    in
    helper
  ;;

  (* attempts to unify two types [ty1] and [ty2], returning a substitution *)
  let rec unify ty1 ty2 =
    match ty1, ty2 with
    | TPrim l, TPrim r when String.equal l r -> return empty
    | TVar v1, TVar v2 when v1 = v2 -> return empty
    | TVar v, ty | ty, TVar v -> singleton v ty
    | TArrow (l1, r1), TArrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | TTuple (f1, s1, rest1), TTuple (f2, s2, rest2) ->
      let* rest_unified =
        match List.map2 (f1 :: s1 :: rest1) (f2 :: s2 :: rest2) ~f:unify with
        | Unequal_lengths ->
          fail (`Unification_failed (TTuple (f1, s1, rest1), TTuple (f2, s2, rest2)))
        | Ok res -> return res
      in
      List.fold_left rest_unified ~init:(return empty) ~f:(fun acc s ->
        let* s = s in
        let* acc = acc in
        compose acc s)
    | TList t1, TList t2 -> unify t1 t2
    | TOption t1, TOption t2 -> unify t1 t2
    (* | TRecord n1, TRecord n2 when String.equal n1 n2 -> return empty *)
    | _, _ -> fail (`Unification_failed (ty1, ty2))

  (* extends a substitution with a new mapping for variable [v] *)
  and extend v ty subst =
    match Map.find subst v with
    | None ->
      let ty = apply subst ty in
      let* new_subs = singleton v ty in
      let upd ~key ~data acc =
        let* acc = acc in
        let ty = apply new_subs data in
        return (Map.update acc key ~f:(function _ -> ty))
      in
      Map.fold subst ~init:(return new_subs) ~f:upd
    | Some existing_ty ->
      let* new_subs = unify ty existing_ty in
      compose subst new_subs

  and compose s1 s2 = RMap.fold_left s2 ~init:(return s1) ~f:extend

  let compose_all subs_list = RList.fold_left subs_list ~init:(return empty) ~f:compose
end

module VarSet = struct
  include VarSet

  let fold_left_m f acc set =
    fold
      (fun x acc ->
        let open R.Syntax in
        let* acc = acc in
        f acc x)
      acc
      set
  ;;
end

module Scheme = struct
  (* let occurs_in v = function
     | S (xs, t) -> (not (VarSet.mem v xs)) && Type.occurs_in v t
     ;; *)

  let free_vars = function
    | S (bs, t) -> VarSet.diff (Type.type_vars t) bs
  ;;

  let apply sub (S (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    S (names, Subst.apply s2 ty)
  ;;

  (* let pp = pp_scheme *)
end

module RecordEnv = struct
  open Base

  type t = (string, (string * ty) list, String.comparator_witness) Map.t

  let empty : t = Map.empty (module String)
end

module TypeEnv = struct
  open Base

  type t = (string, scheme, String.comparator_witness) Map.t

  let empty = Map.empty (module String)

  let free_vars env =
    Map.fold env ~init:VarSet.empty ~f:(fun ~key:_ ~data:s acc ->
      VarSet.union acc (Scheme.free_vars s))
  ;;

  let apply s env = Map.map env ~f:(Scheme.apply s)
  let extend key s env = Map.update env key ~f:(fun _ -> s)

  let extend_many list env =
    List.fold list ~init:env ~f:(fun env (key, v) -> extend key v env)
  ;;

  let find = Map.find

  let merge_envs subst acc_env env_pat =
    let acc_env = apply subst acc_env in
    let env_pat = apply subst env_pat in
    Map.fold env_pat ~init:acc_env ~f:(fun ~key ~data acc_env -> extend key data acc_env)
  ;;

  let remove = Map.remove

  let find_type_exn env key =
    match Map.find_exn env key with
    | S (_, typ) -> typ
  ;;

  let pp ppf env =
    Stdlib.Format.fprintf ppf "{| ";
    Map.iteri env ~f:(fun ~key:name ~data:scheme ->
      Stdlib.Format.fprintf ppf "%s -> %a; " name pp_scheme scheme);
    Stdlib.Format.fprintf ppf "|}"
  ;;
end

module Infer = struct
  open R
  open R.Syntax

  let unify = Subst.unify
  let fresh_var = fresh >>| fun n -> TVar n

  let instantiate : scheme -> ty R.t =
    fun (S (bs, t)) ->
    VarSet.fold_left_m
      (fun typ name ->
        let* f1 = fresh_var in
        let* s = Subst.singleton name f1 in
        return (Subst.apply s typ))
      bs
      (return t)
  ;;

  let generalize (env : TypeEnv.t) (ty : ty) : scheme =
    let free = VarSet.diff (Type.type_vars ty) (TypeEnv.free_vars env) in
    S (free, ty)
  ;;

  let lookup_env e xs =
    match TypeEnv.find xs e with
    | None -> fail (`Undefined_variable e)
    | Some scheme ->
      let* ans = instantiate scheme in
      return (Subst.empty, ans)
  ;;

  let string_of_id (Ast.Id name) = name

  let infer_const = function
    | Ast.Int _ -> tprim_int
    | Ast.Bool _ -> tprim_bool
    | Ast.String _ -> tprim_string
    | Ast.Unit -> tprim_unit
  ;;

  let rec infer_pattern env = function
    | Ast.PVar id ->
      let var_name = string_of_id id in
      let* fresh = fresh_var in
      let extended_env = TypeEnv.extend var_name (S (VarSet.empty, fresh)) env in
      return (Subst.empty, fresh, extended_env)
    | Ast.PAny ->
      let* fresh = fresh_var in
      return (Subst.empty, fresh, env)
    | Ast.PConst c ->
      let fresh = infer_const c in
      return (Subst.empty, fresh, env)
    | Ast.PTuple (p1, p2, pl) ->
      let* sub1, typ1, env1 = infer_pattern env p1 in
      let* sub2, typ2, env2 = infer_pattern (TypeEnv.apply sub1 env1) p2 in
      let f1 (pat : Ast.pattern) (sub_prev, l, env) =
        let* sub_cur, arg, env = infer_pattern env pat in
        let* sub = Subst.compose sub_prev sub_cur in
        return (sub, arg :: l, env)
      in
      let* sub, arg, env = RList.fold_right pl ~init:(return (sub2, [], env2)) ~f:f1 in
      return (sub, TTuple (typ1, typ2, arg), env)
    | Ast.PList pats ->
      let* fresh_el_type = fresh_var in
      let f1 (sub_acc, env_acc) pat =
        let* sub_cur, el_type, env_cur = infer_pattern env_acc pat in
        let* unified_sub = Subst.compose sub_acc sub_cur in
        let* final_sub = Subst.unify (Subst.apply sub_cur fresh_el_type) el_type in
        let combined_sub = Subst.compose unified_sub final_sub in
        let* combined_sub = combined_sub in
        return (combined_sub, TypeEnv.apply final_sub env_cur)
      in
      let* final_sub, final_env =
        RList.fold_left pats ~init:(return (Subst.empty, env)) ~f:f1
      in
      return (final_sub, TList (Subst.apply final_sub fresh_el_type), final_env)
    | Ast.PCons (p1, p2) ->
      let* sub1, typ1, env1 = infer_pattern env p1 in
      let* _, typ2, env2 = infer_pattern (TypeEnv.apply sub1 env1) p2 in
      let* subst = Subst.unify typ2 (TList typ1) in
      let env = TypeEnv.apply subst env2 in
      return (subst, Subst.apply subst typ2, env)
    | Ast.POption None ->
      let* fresh = fresh_var in
      return (Subst.empty, TOption fresh, env)
    | Ast.POption (Some p) ->
      let* sub, typ, env = infer_pattern env p in
      return (sub, TOption typ, env)
    | Ast.PConstraint (pat, typ) ->
      let* s, t, env = infer_pattern env pat in
      let typ = Subst.apply s typ in
      let* subst = unify typ t in
      return (subst, Subst.apply subst typ, TypeEnv.apply subst env)
  ;;

  let validate_let_rec_lhs pat =
    match pat with
    | Ast.PVar _ -> return pat
    | _ -> fail (`Ill_left_hand_side ": only variables are allowed")
  ;;

  let validate_let_rec_rhs expr =
    match expr with
    | Ast.Efun _ -> return expr
    | _ -> fail (`Ill_right_hand_side "of let rec")
  ;;

  let rec infer env record_env (expr : Ast.expr) : (Subst.t * ty) R.t =
    match expr with
    | Evar (Id x) -> lookup_env x env
    | Econst (Int _) -> return (Subst.empty, tprim_int)
    | Econst (Bool _) -> return (Subst.empty, tprim_bool)
    | Econst (String _) -> return (Subst.empty, tprim_string)
    | Econst Unit -> return (Subst.empty, tprim_unit)
    | Ebin_op (op, e1, e2) ->
      let* s1, t1 = infer env record_env e1 in
      let* s2, t2 = infer (TypeEnv.apply s1 env) record_env e2 in
      let* e1t, e2t, et =
        match op with
        | Mult | Div | Add | Sub -> return (tprim_int, tprim_int, tprim_int)
        | Eq | Neq | Lt | Lte | Gt | Gte ->
          let* fresh = fresh_var in
          return (fresh, fresh, tprim_bool)
        | And | Or -> return (tprim_bool, tprim_bool, tprim_bool)
        | Cons ->
          let* fresh = fresh_var in
          return (fresh, TList fresh, TList fresh)
      in
      let* sub3 = Subst.unify (Subst.apply s2 t1) e1t in
      let* sub4 = Subst.unify (Subst.apply sub3 t2) e2t in
      let* sub = Subst.compose_all [ s1; s2; sub3; sub4 ] in
      return (sub, Subst.apply sub et)
    | Eun_op (op, e) ->
      let* s, t = infer env record_env e in
      let* op_type =
        match op with
        | Negative | Positive -> return (tprim_int @-> tprim_int)
        | Not -> return (tprim_bool @-> tprim_bool)
      in
      let* s2 =
        match op_type with
        | TArrow (arg, _) -> unify t arg
        | ty -> fail (`Unexpected_function_type ty)
      in
      let* s_final = Subst.compose_all [ s2; s ] in
      (match op_type with
       | TArrow (_, ret) -> return (s_final, Subst.apply s_final ret)
       | ty -> fail (`Unexpected_function_type ty))
    | Eif_then_else (c, th, Some el) ->
      let* s1, t1 = infer env record_env c in
      let* s2, t2 = infer (TypeEnv.apply s1 env) record_env th in
      let* s3, t3 = infer (TypeEnv.apply s2 env) record_env el in
      let* s4 = unify t1 tprim_bool in
      let* s5 = unify t2 t3 in
      let* final_subst = Subst.compose_all [ s5; s4; s3; s2; s1 ] in
      return (final_subst, Subst.apply final_subst t2)
    | Eif_then_else (c, th, None) ->
      let* s1, t1 = infer env record_env c in
      let* s2, t2 = infer (TypeEnv.apply s1 env) record_env th in
      let t3 = tprim_unit in
      let* s4 = unify t1 tprim_bool in
      let* s5 = Subst.unify t2 t3 in
      let* final_subst = Subst.compose_all [ s5; s4; s2; s1 ] in
      return (final_subst, Subst.apply final_subst t2)
    | Elet (Non_recursive, Evalue_binding (PVar (Id x), e1), _, e2) ->
      let* s1, t1 = infer env record_env e1 in
      let env2 = TypeEnv.apply s1 env in
      let t_gen = generalize env2 t1 in
      let env3 = TypeEnv.extend x t_gen env in
      let* s2, t2 = infer (TypeEnv.apply s1 env3) record_env e2 in
      let* final_subst = Subst.compose s1 s2 in
      return (final_subst, t2)
    | Elet (Non_recursive, Evalue_binding (pattern, e1), bindings, e2) ->
      let* s1, t1 = infer env record_env e1 in
      let* s2, t_pat, env1 = infer_pattern env pattern in
      let* subst1 = Subst.compose s1 s2 in
      let* unified_subst = unify (Subst.apply subst1 t_pat) t1 in
      let initial_env = TypeEnv.apply unified_subst env1 in
      let* extended_env =
        List.fold_left
          (fun acc_env (Ast.Evalue_binding (p, expr)) ->
            let* acc_env = acc_env in
            let* s_bind, t_bind = infer acc_env record_env expr in
            let* s_pat, t_pat, env_pat = infer_pattern acc_env p in
            let* combined_subst = Subst.compose s_bind s_pat in
            let* final_subst = unify (Subst.apply combined_subst t_pat) t_bind in
            let updated_env = TypeEnv.merge_envs final_subst acc_env env_pat in
            return updated_env)
          (return initial_env)
          bindings
      in
      let* s3, t2 = infer extended_env record_env e2 in
      let* full_subst = Subst.compose_all [ s3; unified_subst; subst1 ] in
      return (full_subst, t2)
    | Elet (Recursive, Evalue_binding (PVar (Id x), e1), [], e2) ->
      let* e1 = validate_let_rec_rhs e1 in
      let* tv = fresh_var in
      let env2 = TypeEnv.extend x (S (VarSet.empty, tv)) env in
      let* s1, t1 = infer env2 record_env e1 in
      let* s2 = unify (Subst.apply s1 tv) t1 in
      let* s_final = Subst.compose s1 s2 in
      let env3 = TypeEnv.apply s_final env in
      let env4 = TypeEnv.apply s1 env3 in
      let t_gen = generalize env4 (Subst.apply s_final tv) in
      let* s3, t2 = infer (TypeEnv.extend x t_gen env4) record_env e2 in
      let* s_final = Subst.compose s_final s3 in
      return (s_final, t2)
    | Elet (Recursive, value_binding, value_bindings, e2) ->
      let* env_ext, s_acc =
        List.fold_left
          (fun acc_env (Ast.Evalue_binding (pattern, expr)) ->
            let* expr = validate_let_rec_rhs expr in
            let* pattern = validate_let_rec_lhs pattern in
            let* env_acc, _ = acc_env in
            let* s_expr, t_expr = infer env_acc record_env expr in
            let* s_pat, t_pat, env_pat = infer_pattern env_acc pattern in
            let* subst = Subst.compose s_expr s_pat in
            let* unified_subst = unify t_expr t_pat in
            let* combined_subst = Subst.compose subst unified_subst in
            let extended_env = TypeEnv.apply combined_subst env_pat in
            return (extended_env, combined_subst))
          (return (env, Subst.empty))
          (value_binding :: value_bindings)
      in
      let* s2, t2 = infer env_ext record_env e2 in
      let* final_subst = Subst.compose s_acc s2 in
      return (final_subst, t2)
    | Efun (pattern, pattern_list, body) ->
      let* env, pat_types =
        RList.fold_left
          (pattern :: pattern_list)
          ~init:(return (env, []))
          ~f:(fun (env, pat_types) pat ->
            let* _, typ, new_env = infer_pattern env pat in
            return (new_env, typ :: pat_types))
      in
      let* s_body, t_body = infer env record_env body in
      let arrow_type =
        List.fold_right
          (fun pat_type acc -> TArrow (Subst.apply s_body pat_type, acc))
          (List.rev pat_types)
          t_body
      in
      return (s_body, arrow_type)
    | Efun_application (e1, e2) ->
      let* s1, t1 = infer env record_env e1 in
      let* s2, t2 = infer (TypeEnv.apply s1 env) record_env e2 in
      let* tv = fresh_var in
      let* s3 = unify (Subst.apply s2 t1) (TArrow (t2, tv)) in
      let* s_final = Subst.compose_all [ s3; s2; s1 ] in
      return (s_final, Subst.apply s_final tv)
    | Eoption (Some e) ->
      let* s, t = infer env record_env e in
      return (s, TOption t)
    | Eoption None ->
      let* tv = fresh_var in
      return (Subst.empty, TOption tv)
    | Ematch (e, c, cl) ->
      let* sub1, t1 = infer env record_env e in
      let env = TypeEnv.apply sub1 env in
      let* tv = fresh_var in
      infer_match env record_env (c :: cl) sub1 t1 tv ~with_expr:true
    | Efunction (c, cl) ->
      let* t1 = fresh_var in
      let* tv = fresh_var in
      infer_match env record_env (c :: cl) Subst.empty t1 tv ~with_expr:false
    | Etuple (e1, e2, es) ->
      let* s1, t1 = infer env record_env e1 in
      let* s2, t2 = infer (TypeEnv.apply s1 env) record_env e2 in
      let infer_tuple_elements env es =
        let rec aux env = function
          | [] -> return ([], [])
          | e :: es' ->
            let* s, t = infer env record_env e in
            let* s', ts = aux (TypeEnv.apply s env) es' in
            return (s' @ [ s ], t :: ts)
        in
        aux env es
      in
      let* s3, ts = infer_tuple_elements (TypeEnv.apply s2 env) es in
      let* s_final = Subst.compose_all (s3 @ [ s2; s1 ]) in
      return (s_final, TTuple (t1, t2, ts))
    | Elist es ->
      (match es with
       | [] ->
         let* fresh = fresh_var in
         return (Subst.empty, tlist fresh)
       | _ :: _ ->
         let infer_list_elements env es =
           let rec aux env = function
             | [] -> return ([], [])
             | e :: es' ->
               let* s, t = infer env record_env e in
               let* s', ts = aux (TypeEnv.apply s env) es' in
               return (s' @ [ s ], t :: ts)
           in
           aux env es
         in
         let* s, ts = infer_list_elements env es in
         let* s_final = Subst.compose_all s in
         return (s_final, TList (List.hd ts)))
    | Econstraint (e, t) ->
      let* s1, t1 = infer env record_env e in
      let* s2 = unify t1 (Subst.apply s1 t) in
      let* s_final = Subst.compose s1 s2 in
      return (s_final, Subst.apply s2 t1)
  (* | Erecord (record_field, record_fields) ->
     let* inferred_record_fields =
     RList.fold_right
     (record_field :: record_fields)
     ~init:(return [])
     ~f:(fun (Ast.Erecord_field (Label name, expr)) acc ->
     let* _, t = infer env record_env expr in
     Format.printf " %s: " name;
     Format.printf " %a; \n " pp_ty t;
     return ((name, t) :: acc))
     in
     let* t = RecordEnv.find_record_name record_env inferred_record_fields in
     Format.printf "t: %a; \n " pp_ty t;
     return (Subst.empty, t)
     | _ -> fail `Occurs_check *)

  and infer_match env record_env cases inferred_sub inferred_t ty_var ~with_expr =
    let* s, final_t =
      let f1 acc (Ast.Ecase (pat, expr)) =
        let* s1, t = acc in
        let f_with_expr =
          let* _, pat_t, env = infer_pattern env pat in
          let* subst = unify pat_t inferred_t in
          let env = TypeEnv.apply subst env in
          let name =
            match pat with
            | PVar (Id name) | POption (Some (PVar (Id name))) -> Some name
            | _ -> None
          in
          let env =
            match name with
            | Some name ->
              let found_t = TypeEnv.find_type_exn env name in
              let env = TypeEnv.remove env name in
              let t_gen = generalize env found_t in
              TypeEnv.extend name t_gen env
            | None -> env
          in
          return (env, subst)
        in
        let f_no_expr =
          let* _, pat, env = infer_pattern env pat in
          let* s2 = unify inferred_t pat in
          return (env, s2)
        in
        let* env, s2 = if with_expr then f_with_expr else f_no_expr in
        let* s3 = Subst.compose s1 s2 in
        let* s4, t4 = infer (TypeEnv.apply s3 env) record_env expr in
        let* s5 = unify t t4 in
        let* subst = Subst.compose_all [ s3; s4; s5 ] in
        return (subst, Subst.apply subst t)
      in
      Base.List.fold cases ~init:(return (inferred_sub, ty_var)) ~f:f1
    in
    let final_t =
      if with_expr then final_t else TArrow (Subst.apply s inferred_t, final_t)
    in
    return (s, final_t)
  ;;

  (* and infer_typed_record env record_env expected_type = function
     | Ast.Erecord (record_field, record_fields) ->
     Format.printf "hiii \n ";
     let* inferred_record_fields =
     RList.fold_right
     (record_field :: record_fields)
     ~init:(return [])
     ~f:(fun (Ast.Erecord_field (Label name, expr)) acc ->
     let* _, t = infer env record_env expr in
     Format.printf " %s: " name;
     Format.printf " %a; \n " pp_ty t;
     return ((name, t) :: acc))
     in
     let* t =
     RecordEnv.find_record_by_name record_env expected_type inferred_record_fields
     in
     Format.printf "t: %a; \n " pp_ty t;
     return (Subst.empty, t)
     | _ -> return (Subst.empty, TRecord expected_type) *)

  (* let infer_ty_opt env record_env t_opt expr =
     match t_opt with
     | Some expected_type ->
     (match expected_type with
     | TRecord t -> infer_typed_record env record_env t expr
     | _ -> infer env record_env expr)
     | None -> infer env record_env expr *)
  let w expr = Result.map snd (run (infer TypeEnv.empty RecordEnv.empty expr))

  let infer_structure_item env record_env = function
    | Ast.SEval expr ->
      let* subst, _ = infer env record_env expr in
      let updated_env = TypeEnv.apply subst env in
      return (subst, updated_env, record_env)
    | Ast.SValue (Recursive, Evalue_binding (PVar (Id x), expr), []) ->
      let* expr = validate_let_rec_rhs expr in
      let* tv = fresh_var in
      let env = TypeEnv.extend x (S (VarSet.empty, tv)) env in
      let* subst, inferred_ty = infer env record_env expr in
      let* subst2 = unify (Subst.apply subst tv) inferred_ty in
      let* composed_subst = Subst.compose subst subst2 in
      let env2 = TypeEnv.apply composed_subst env in
      let generalized_ty = generalize env2 (Subst.apply composed_subst inferred_ty) in
      let env = TypeEnv.extend x generalized_ty env2 in
      (* Format.printf "composed_subst: %a\n" Subst.pp_subst composed_subst; *)
      return (composed_subst, env, record_env)
    | Ast.SValue (Recursive, value_binding, value_bindings) ->
      let all_bindings = value_binding :: value_bindings in
      let* env_with_placeholders =
        List.fold_left
          (fun acc_env (Ast.Evalue_binding (pattern, _)) ->
            let* ty_pattern = validate_let_rec_lhs pattern in
            let* env_acc = acc_env in
            let* s_pat, _, env_pat = infer_pattern env_acc ty_pattern in
            let extended_env = TypeEnv.apply s_pat env_pat in
            return extended_env)
          (return env)
          all_bindings
      in
      let* env_ext, s_acc =
        List.fold_left
          (fun acc_env (Ast.Evalue_binding (ty_pattern, expr)) ->
            let* expr = validate_let_rec_rhs expr in
            let* env_acc, _ = acc_env in
            let* s_expr, t_expr = infer env_acc record_env expr in
            let* s_pat, t_pat, env_pat = infer_pattern env_acc ty_pattern in
            let* subst = Subst.compose s_expr s_pat in
            let* unified_subst = unify t_expr t_pat in
            let* combined_subst = Subst.compose subst unified_subst in
            let extended_env = TypeEnv.apply combined_subst env_pat in
            return (extended_env, combined_subst))
          (return (env_with_placeholders, Subst.empty))
          all_bindings
      in
      return (s_acc, env_ext, record_env)
    | Ast.SValue (Non_recursive, Evalue_binding (PVar (Id x), expr), _) ->
      let* subst, inferred_ty = infer env record_env expr in
      let env2 = TypeEnv.apply subst env in
      let generalized_ty = generalize env2 inferred_ty in
      let env = TypeEnv.extend x generalized_ty (TypeEnv.apply subst env) in
      return (subst, env, record_env)
    | Ast.SValue (Non_recursive, Evalue_binding (pattern, expr), _) ->
      let* subst_expr, inferred_ty = infer env record_env expr in
      let* subst_pat, t_pat, env_pat = infer_pattern env pattern in
      let* combined_subst =
        let* composed = Subst.compose subst_expr subst_pat in
        return composed
      in
      let* unified_subst = unify (Subst.apply combined_subst t_pat) inferred_ty in
      let updated_env = TypeEnv.apply unified_subst env_pat in
      let* final_subst = Subst.compose unified_subst combined_subst in
      return (final_subst, updated_env, record_env)
  ;;

  (* | Ast.SType (record, field_decl, field_decls) ->
     let fields =
     List.map
     (fun (Ast.Sfield_decl (Label name, t)) -> name, t)
     (field_decl :: field_decls)
     in
     let* record_env = RecordEnv.add_record record_env record fields in
     return (Subst.empty, env, record_env) *)

  let infer_structure env record_env structure =
    let rec process_structure env record_env subst = function
      | [] -> return (subst, env)
      | item :: rest ->
        let* subst', env', record_env' = infer_structure_item env record_env item in
        let* composed_subst = Subst.compose subst subst' in
        process_structure env' record_env' composed_subst rest
    in
    process_structure env record_env Subst.empty structure
  ;;

  let env =
    TypeEnv.extend_many
      [ "print_int", S (VarSet.empty, TArrow (tprim_int, tprim_unit))
      ; "print_endline", S (VarSet.empty, TArrow (tprim_string, tprim_unit))
      ]
      TypeEnv.empty
  ;;

  let record_env = RecordEnv.empty
  let infer_program str = Result.map snd (run (infer_structure env record_env str))
end
