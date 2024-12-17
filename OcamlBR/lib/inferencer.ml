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

  (* computes the set of all type variables in a given type; primarily used to generalize types during type inference *)
  let type_vars =
    let rec helper acc = function
      | TVar b -> VarSet.add b acc
      | TPrim _ -> acc
      | TArrow (l, r) -> helper (helper acc l) r
      | TList t -> helper acc t
      | TTuple (fst, snd, rest) -> List.fold_left helper acc (fst :: snd :: rest)
      | TOption t -> helper acc t
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

  (* val pp
     :  Format.formatter
     -> (type_var, ty, Base.Int.comparator_witness) Base.Map.t
     -> unit *)
end = struct
  open R
  open R.Syntax
  open Base

  type t = (type_var, ty, Int.comparator_witness) Map.t

  (* let pp ppf subst =
     let open Stdlib.Format in
     fprintf ppf "[ ";
    Base.Map.iteri subst ~f:(fun ~key ~data ->
      fprintf ppf "%a -> %a" pp_type_var key pp_ty data;
      fprintf ppf "; ");
    fprintf ppf "]"
     ;; *)

  let empty = Map.empty (module Int)

  (* creates a substitution for variable [v] with type [ty] if no occurence is found *)
  let mapping v ty = if Type.occurs_in v ty then fail `Occurs_check else return (v, ty)

  let singleton k v =
    let* k, v = mapping k v in
    return (Base.Map.singleton (module Base.Int) k v)
  ;;

  let find subst v = Map.find subst v
  let remove subst v = Map.remove subst v

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
        match List.map2 rest1 rest2 ~f:(fun t1 t2 -> unify t1 t2) with
        | Unequal_lengths ->
          fail (`Unification_failed (TTuple (f1, s1, rest1), TTuple (f2, s2, rest2)))
        | Ok res -> return res
      in
      let* fst_unified = unify f1 f2 in
      let* snd_unified = unify (apply fst_unified s1) (apply fst_unified s2) in
      List.fold_left rest_unified ~init:(compose fst_unified snd_unified) ~f:(fun acc s ->
        let* s = s in
        let* acc = acc in
        compose acc s)
    | TList t1, TList t2 -> unify t1 t2
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
  let find env key = Map.find env key

  (* let pp ppf xs =
    Stdlib.Format.fprintf ppf "{| ";
    Base.Map.iter xs ~f:(fun (n, s) ->
      Stdlib.Format.fprintf ppf "%s -> %a; " n pp_scheme s);
    Stdlib.Format.fprintf ppf "|}%!"
  ;; *)
  (* let pp ppf xs =
    Stdlib.Format.fprintf ppf "{| ";
    Base.Map.iteri xs ~f:(fun ~key:n ~data:s ->
      Stdlib.Format.fprintf ppf "%s -> %a; " n pp_scheme s);
    Stdlib.Format.fprintf ppf "|}%!"
  ;; *)
  let pp ppf env =
    Stdlib.Format.fprintf ppf "{| ";
    Base.Map.iteri env ~f:(fun ~key:name ~data:scheme ->
      Stdlib.Format.fprintf ppf "%s -> %a; " name pp_scheme scheme);
    Stdlib.Format.fprintf ppf "|}"
  ;;

  (* let find_exn name xs =
     match Map.find xs name with
     | None -> R.fail (`Undefined_variable name)
     | Some scheme -> scheme
     ;; *)
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

  (* let pp ppf env =
    Stdlib.Format.fprintf ppf "{| ";
    Base.Map.iteri env ~f:(fun ~key:name ~data:s ->
      Stdlib.Format.fprintf ppf "%s -> %a; " name pp_scheme s);
    Stdlib.Format.fprintf ppf "|}%!"
  ;; *)

  let fst_arrow = function
    | TArrow (arg, _) -> arg
    | ty -> failwith (Format.asprintf "Expected function type, got: %a" pp_ty ty)
  ;;

  let snd_arrow = function
    | TArrow (_, ret) -> ret
    | ty -> failwith (Format.asprintf "Expected function type, got: %a" pp_ty ty)
  ;;

  open R

  let string_of_id (Ast.Id (name, _)) = name

  let infer_const c =
    match c with
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
      return (Subst.empty, fresh, env)
    | Ast.POption (Some p) -> infer_pattern env p
  ;;

  (*
     let rec id_type_to_ty (t : Ast.id_type) : ty =
     match t with
     | Ast.TInt -> tprim_int
     | Ast.TString -> tprim_string
     | Ast.TBool -> tprim_bool
     | Ast.Tlist t -> tlist (id_type_to_ty t)
     | Ast.TTuple (t1, t2, ts) ->
     ttuple (id_type_to_ty t1) (id_type_to_ty t2) (List.map id_type_to_ty ts)
     | Ast.TFun (t1, t2) -> tarrow (id_type_to_ty t1) (id_type_to_ty t2)
     ;; *)

  let rec infer (env : TypeEnv.t) (expr : Ast.expr) : (Subst.t * ty) R.t =
    match expr with
    | Evar (Id (x, _)) -> lookup_env x env
    | Econst (Int _) -> return (Subst.empty, tprim_int)
    | Econst (Bool _) -> return (Subst.empty, tprim_bool)
    | Econst (String _) -> return (Subst.empty, tprim_string)
    | Econst Unit -> return (Subst.empty, tprim_unit)
    | Ebin_op (op, e1, e2) ->
      let* s1, t1 = infer env e1 in
      let* s2, t2 = infer (TypeEnv.apply s1 env) e2 in
      (* let* op_type =
          match op with
          | Add | Sub | Mult | Div -> return (tprim_int @-> tprim_int @-> tprim_int)
          | Gt | Lt | Eq | Neq | Gte | Lte ->
            return (tprim_int @-> tprim_int @-> tprim_bool)
          | And | Or -> return (tprim_bool @-> tprim_bool @-> tprim_bool)
        in *)
      let* e1t, e2t, et =
        match op with
        | Mult | Div | Add | Sub -> return (tprim_int, tprim_int, tprim_int)
        | Eq | Neq | Lt | Lte | Gt | Gte ->
          let* fresh = fresh_var in
          return (fresh, fresh, tprim_bool)
        | And | Or -> return (tprim_bool, tprim_bool, tprim_bool)
      in
      let* sub3 = Subst.unify (Subst.apply s2 t1) e1t in
      let* sub4 = Subst.unify (Subst.apply sub3 t2) e2t in
      let* sub = Subst.compose_all [ s1; s2; sub3; sub4 ] in
      return (sub, Subst.apply sub et)
      (* let* s3 = unify (Subst.apply s2 t1) (fst_arrow op_type) in
         let* s4 = unify (Subst.apply s3 t2) (snd_arrow op_type) in
         let* s_final = Subst.compose_all [ s4; s3; s2; s1 ] in
         return (s_final, Subst.apply s_final (snd_arrow op_type)) *)
    | Eun_op (op, e) ->
      let* s, t = infer env e in
      let* op_type =
        match op with
        | Negative | Positive -> return (tprim_int @-> tprim_int)
        | Not -> return (tprim_bool @-> tprim_bool)
      in
      let* s2 = unify t (fst_arrow op_type) in
      let* s_final = Subst.compose_all [ s2; s ] in
      return (s_final, Subst.apply s_final (snd_arrow op_type))
    | Eif_then_else (c, th, Some el) ->
      let* s1, t1 = infer env c in
      let* s2, t2 = infer (TypeEnv.apply s1 env) th in
      let* s3, t3 = infer (TypeEnv.apply s2 env) el in
      let* s4 = unify t1 tprim_bool in
      let* s5 = unify t2 t3 in
      let* final_subst = Subst.compose_all [ s5; s4; s3; s2; s1 ] in
      return (final_subst, Subst.apply final_subst t2)
    | Eif_then_else (c, th, None) ->
      let* s1, t1 = infer env c in
      let* s2, t2 = infer (TypeEnv.apply s1 env) th in
      let* s3 = unify t1 tprim_bool in
      let* final_subst = Subst.compose_all [ s3; s2; s1 ] in
      return (final_subst, TOption (Subst.apply final_subst t2))
    | Elet (Non_recursive, Evalue_binding (Id (x, t_opt), e1), [], e2) ->
      (match t_opt with
       | Some expected_type ->
         let* s1, t1 = infer env e1 in
         let expected_type = Subst.apply s1 expected_type in
         let* sub1 = Subst.unify t1 expected_type in
         let env2 = TypeEnv.apply sub1 env in
         let t_gen = generalize env2 t1 in
         let env3 = TypeEnv.extend x t_gen env in
         let* s2, t2 = infer (TypeEnv.apply s1 env3) e2 in
         let* final_subst = Subst.compose s1 s2 in
         return (final_subst, t2)
       | None ->
         let* s1, t1 = infer env e1 in
         let env2 = TypeEnv.apply s1 env in
         let t_gen = generalize env2 t1 in
         let env3 = TypeEnv.extend x t_gen env in
         let* s2, t2 = infer (TypeEnv.apply s1 env3) e2 in
         let* final_subst = Subst.compose s1 s2 in
         return (final_subst, t2))
    | Elet (Recursive, value_binding, value_bindings, e2) ->
      let* s', env' = infer_value_bindings env (value_binding :: value_bindings) in
      let* s3, t2 = infer env' e2 in
      let* s_final =
        let rec combined_s acc = function
          | [] -> return acc
          | s :: substs ->
            let* acc = Subst.compose acc s in
            combined_s acc substs
        in
        let* sub = combined_s Subst.empty s' in
        Subst.compose sub s3
      in
      return (s_final, t2)
    | Efun (pattern, pattern_list, body) ->
      let* env, pat_types =
        RList.fold_left
          (pattern :: pattern_list)
          ~init:(return (env, []))
          ~f:(fun (env, pat_types) pat ->
            let* _, typ, new_env = infer_pattern env pat in
            return (new_env, typ :: pat_types))
      in
      let* s_body, t_body = infer env body in
      let arrow_type =
        List.fold_right
          (fun pat_type acc -> TArrow (Subst.apply s_body pat_type, acc))
          (List.rev pat_types)
          t_body
      in
      return (s_body, arrow_type)
    | Efun_application (e1, e2) ->
      let* s1, t1 = infer env e1 in
      let* s2, t2 = infer (TypeEnv.apply s1 env) e2 in
      let* tv = fresh_var in
      let* s3 = unify (Subst.apply s2 t1) (TArrow (t2, tv)) in
      let* s_final = Subst.compose_all [ s3; s2; s1 ] in
      return (s_final, Subst.apply s_final tv)
    | Eoption (Some e) ->
      let* s, t = infer env e in
      return (s, TOption t)
    | Ematch (e, c, cl) ->
      let* sub1, t1 = infer env e in
      let env = TypeEnv.apply sub1 env in
      let* fresh = fresh_var in
      let cases = c :: cl in
      let* sub, t =
        List.fold_left
          (fun acc case ->
            let* _, t = acc in
            match case with
            | Ast.Ecase (pat, exp) ->
              let* sub_pat, pt, env1 = infer_pattern env pat in
              let* sub2 = Subst.unify t1 pt in
              let env2 = TypeEnv.apply sub2 env1 in
              let* sub3, t' = infer env2 exp in
              let* sub4 = Subst.unify t' t in
              let* sub = Subst.compose_all [ sub_pat; sub2; sub3; sub4 ] in
              return (sub, Subst.apply sub t))
          (return (sub1, fresh))
          cases
      in
      return (sub, t)
    | Etuple (e1, e2, es) ->
      let* s1, t1 = infer env e1 in
      let* s2, t2 = infer (TypeEnv.apply s1 env) e2 in
      let infer_tuple_elements env es =
        let rec aux env = function
          | [] -> return ([], [])
          | e :: es' ->
            let* s, t = infer env e in
            let* s', ts = aux (TypeEnv.apply s env) es' in
            return (s' @ [ s ], t :: ts)
        in
        aux env es
      in
      let* s3, ts = infer_tuple_elements (TypeEnv.apply s2 env) es in
      let* s_final = Subst.compose_all (s3 @ [ s2; s1 ]) in
      return (s_final, TTuple (t1, t2, ts))
    | Elist es ->
      let infer_list_elements env es =
        let rec aux env = function
          | [] -> return ([], [])
          | e :: es' ->
            let* s, t = infer env e in
            let* s', ts = aux (TypeEnv.apply s env) es' in
            return (s' @ [ s ], t :: ts)
        in
        aux env es
      in
      let* s, ts = infer_list_elements env es in
      let* s_final = Subst.compose_all s in
      return (s_final, TList (List.hd ts))
    | Eprint_int e ->
      let* s, t = infer env e in
      if t = TPrim "int"
      then return (s, TPrim "unit")
      else (
        let expected_type = TPrim "int" in
        fail (`Unification_failed (expected_type, t)))
    | _ -> fail (`Undefined_variable "Unhandled case in `infer`")

  and infer_value_bindings env value_bindings =
    let* tv', env' =
      RList.fold_left
        value_bindings
        ~init:(return ([], env))
        ~f:(fun (types, env) (Ast.Evalue_binding (Id (x, _), _)) ->
          let* tv = fresh_var in
          let new_env = TypeEnv.extend x (S (VarSet.empty, tv)) env in
          return (tv :: types, new_env))
    in
    let* s', env' =
      RList.fold_left
        (List.map2 (fun x y -> x, y) value_bindings (List.rev tv'))
        ~init:(return ([], env'))
        ~f:(fun (substs, env) (Ast.Evalue_binding (Id (x, _), e), tv) ->
          let* s1, t1 = infer env e in
          let* s2 = unify (Subst.apply s1 tv) t1 in
          let* s = Subst.compose s1 s2 in
          let new_env = TypeEnv.apply s env in
          let t_gen = generalize new_env (Subst.apply s tv) in
          let new_env = TypeEnv.extend x t_gen new_env in
          return (s :: substs, new_env))
    in
    return (List.rev s', env')
  ;;

  let w expr = Result.map snd (run (infer TypeEnv.empty expr))

  let rec infer_structure_item env = function
    | Ast.SEval expr ->
      let* subst, _ = infer env expr in
      return (subst, env)
    | Ast.SValue (Recursive, value_binding, value_bindings) ->
      let* s', env' = infer_value_bindings env (value_binding :: value_bindings) in
      let* s_final =
        let rec combined_s acc = function
          | [] -> return acc
          | s :: substs ->
            let* acc = Subst.compose acc s in
            combined_s acc substs
        in
        let* sub = combined_s Subst.empty s' in
        Subst.compose sub Subst.empty
      in
      return (s_final, env')
    | Ast.SValue (Non_recursive, Evalue_binding (id, expr), other_bindings) ->
      let id_str = (fun (Ast.Id (name, _)) -> name) id in
      let* subst, inferred_ty = infer env expr in
      let generalized_ty =
        generalize (TypeEnv.apply subst env) (Subst.apply subst inferred_ty)
      in
      let env = TypeEnv.extend id_str generalized_ty (TypeEnv.apply subst env) in
      infer_remaining_bindings env subst other_bindings

  and infer_remaining_bindings env subst = function
    | [] -> return (subst, env)
    | Evalue_binding (id, expr) :: rest ->
      let id_str = (fun (Ast.Id (name, _)) -> name) id in
      let* subst', inferred_ty = infer env expr in
      let* composed_subst = Subst.compose subst subst' in
      let generalized_ty =
        generalize
          (TypeEnv.apply composed_subst env)
          (Subst.apply composed_subst inferred_ty)
      in
      let env = TypeEnv.extend id_str generalized_ty (TypeEnv.apply composed_subst env) in
      infer_remaining_bindings env composed_subst rest
  ;;

  let infer_structure env structure =
    let rec process_structure env subst = function
      | [] -> return (subst, env)
      | item :: rest ->
        let* subst', env' = infer_structure_item env item in
        let* composed_subst = Subst.compose subst subst' in
        process_structure env' composed_subst rest
    in
    process_structure env Subst.empty structure
  ;;

  let infer_program str = Result.map snd (run (infer_structure TypeEnv.empty str))

  let infer_program_test s =
    let open Stdlib.Format in
    match Parser.parse_expr s with
    | Ok parsed ->
      (match infer_program parsed with
       | Ok env ->
         Base.Map.iteri env ~f:(fun ~key ~data:(S (_, ty)) ->
           printf "val %s : %a\n" key pp_ty ty)
       | Error e -> printf "Infer error: %a\n" pp_error e)
    | Error e -> printf "Parsing error: %s\n" e
  ;;
end
