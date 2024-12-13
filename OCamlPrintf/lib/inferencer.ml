(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Pprinter

type error =
  [ `Occurs_check
  | `Not_implemented
  | `No_variable of string
  | `Unification_failed of core_type * core_type
  ]

let pp_error ppf : error -> _ = function
  | `Occurs_check -> Format.fprintf ppf "Occurs check failed"
  | `Not_implemented -> Format.fprintf ppf "Not_implemented"
  | `No_variable s -> Format.fprintf ppf "Undefined variable '%s'" s
  | `Unification_failed (l, r) ->
    Format.fprintf ppf "unification failed on %a and %a" pp_core_type l pp_core_type r
;;

module R : sig
  type 'a t

  val return : 'a -> 'a t
  val fail : error -> 'a t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
    val fold_right : 'a list -> init:'b t -> f:('a -> 'b -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold : ('a, 'b, 'c) Base.Map.t -> init:'d t -> f:('a -> 'b -> 'd -> 'd t) -> 'd t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t
end = struct
  open Base

  (* A compositon: State monad after Result monad *)
  type 'a t = int -> int * ('a, error) Result.t

  let return x state = state, Result.return x
  let fail e state = state, Result.fail e

  let ( >>= ) (monad : 'a t) (f : 'a -> 'b t) : 'b t =
    fun state ->
    match monad state with
    | state, Result.Ok result -> f result state
    | state, Result.Error e -> fail e state
  ;;

  let bind x ~f = x >>= f

  module Syntax = struct
    let ( let* ) x f = bind x ~f
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

  let fold_left_m f acc set =
    fold
      (fun x acc ->
        let open R.Syntax in
        let* acc = acc in
        f acc x)
      acc
      set
  ;;

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%s; ") s;
    Format.fprintf ppf "]"
  ;;
end

type scheme = Scheme of VarSet.t * Ast.core_type [@@deriving show { with_path = false }]

module Type = struct
  type t = core_type

  let rec occurs_in var = function
    | Type_any | Type_unit | Type_char | Type_int | Type_string | Type_bool -> false
    | Type_name name -> name = var
    | Type_list ty -> occurs_in var ty
    | Type_tuple (first, second, list) ->
      List.exists (occurs_in var) (first :: second :: list)
    | Type_arrow (l, r) -> occurs_in var l || occurs_in var r
  ;;

  let free_vars =
    let rec helper acc = function
      | Type_any | Type_unit | Type_char | Type_int | Type_string | Type_bool -> acc
      | Type_name name -> VarSet.add name acc
      | Type_tuple (first, second, list) ->
        List.fold_left helper acc (first :: second :: list)
      | Type_list ty -> helper acc ty
      | Type_arrow (l, r) -> helper (helper acc l) r
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : ident -> core_type -> t R.t
  val apply : t -> core_type -> core_type
  val unify : core_type -> core_type -> t R.t

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : t -> ident -> t
end = struct
  open R
  open R.Syntax
  open Base

  type t = (ident, core_type, String.comparator_witness) Map.t

  let empty = Map.empty (module String)

  let singleton key value =
    if Type.occurs_in key value
    then fail `Occurs_check
    else return (Map.singleton (module String) key value)
  ;;

  let find sub value = Map.find sub value
  let remove sub value = Map.remove sub value

  let apply sub =
    let rec helper = function
      | Type_name name as ty ->
        (match find sub name with
         | Some name -> name
         | None -> ty)
      | Type_list t -> Type_list (helper t)
      | Type_tuple (first, second, list) ->
        Type_tuple
          (helper first, helper second, List.map list ~f:(fun item -> helper item))
      | Type_arrow (l, r) -> Type_arrow (helper l, helper r)
      | ty -> ty
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | Type_any, Type_any
    | Type_int, Type_int
    | Type_char, Type_char
    | Type_string, Type_string
    | Type_bool, Type_bool -> return empty
    | Type_name l, Type_name r when String.equal l r -> return empty
    | Type_name a, t | t, Type_name a -> singleton a t
    | Type_list t1, Type_list t2 -> unify t1 t2
    | Type_tuple (fst1, snd1, list1), Type_tuple (fst2, snd2, list2) ->
      (match
         Base.List.fold2
           (fst1 :: snd1 :: list1)
           (fst2 :: snd2 :: list2)
           ~init:(return empty)
           ~f:(fun acc it1 it2 ->
             let* sub1 = acc in
             let* sub2 = unify (apply sub1 it1) (apply sub1 it2) in
             compose sub1 sub2)
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

  let pp = pp_scheme
end

module TypeEnv = struct
  open Base

  type t = (string, scheme, String.comparator_witness) Map.t

  let empty = Map.empty (module String)
  let extend env key value = Map.update env key ~f:(fun _ -> value)

  let free_vars env =
    Map.fold env ~init:VarSet.empty ~f:(fun ~key:_ ~data acc ->
      VarSet.union acc (Scheme.free_vars data))
  ;;

  let apply sub env = Map.map env ~f:(Scheme.apply sub)

  let pp ppf xs =
    Stdlib.Format.fprintf ppf "{| ";
    Map.iter xs ~f:(fun (n, s) -> Stdlib.Format.fprintf ppf "%s -> %a; " n pp_scheme s);
    Stdlib.Format.fprintf ppf "|}%!"
  ;;

  let find_exn env name =
    match Map.find env name with
    | Some scheme -> scheme
    | None -> R.fail (`No_variable name)
  ;;
end

module Infer = struct
  open R
  open R.Syntax
  open Ast

  let unify = Subst.unify

  (** [TODO] Maybe rewrite *)
  let fresh_var =
    (* 98 - is number 'a' in ASCII-table *)
    fresh >>| fun n -> Type_name ("'" ^ String.make 1 (Char.chr (98 + n - 1)))
  ;;

  let instantiate : scheme -> core_type R.t =
    fun (Scheme (bs, t)) ->
    VarSet.fold
      (fun name typ ->
        let* typ = typ in
        let* f1 = fresh_var in
        let* s = Subst.singleton name f1 in
        return (Subst.apply s typ))
      bs
      (return t)
  ;;

  let generalize (env : TypeEnv.t) (ty : Type.t) : Scheme.t =
    let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
    Scheme (free, ty)
  ;;

  let lookup_env e env =
    match Base.Map.find env e with
    | Some scheme ->
      let* ans = instantiate scheme in
      return (Subst.empty, ans)
    | None -> fail (`No_variable e)
  ;;

  let infer_pattern =
    let rec helper (env : TypeEnv.t) = function
      | Pat_any ->
        let* fresh = fresh_var in
        return (env, fresh)
      | Pat_var x ->
        let* fresh = fresh_var in
        let env = TypeEnv.extend env x (Scheme (VarSet.empty, fresh)) in
        return (env, fresh)
      | Pat_constant const ->
        (match const with
         | Const_integer _ -> return (env, Type_int)
         | Const_string _ -> return (env, Type_string)
         | Const_char _ -> return (env, Type_char))
      | Pat_construct (id, None) when id = "true" || id = "false" ->
        return (env, Type_bool)
      | Pat_construct ("[]", None) ->
        let* fresh = fresh_var in
        return (env, Type_list fresh)
      | Pat_tuple (fst, snd, rest_list) ->
        let* env1, t1 = helper env fst in
        let* env2, t2 = helper env1 snd in
        let* env_rest, t_list =
          RList.fold_right
            ~f:(fun pat acc ->
              let* env_acc, typ_list = return acc in
              let* env, typ = helper env_acc pat in
              return (env, typ :: typ_list))
            ~init:(return (env2, []))
            rest_list
        in
        return (env_rest, Type_tuple (t1, t2, t_list))
      | Pat_constraint (pat, c_type) ->
        let* env, typ = helper env pat in
        let* unified_sub = unify typ c_type in
        let env = TypeEnv.apply unified_sub env in
        return (env, c_type)
      | _ -> fail `Not_implemented
    in
    helper
  ;;

  let infer_expression =
    let rec helper (env : TypeEnv.t) (exp : Expression.t) : (Subst.t * core_type) R.t =
      match exp with
      | Exp_ident x -> lookup_env x env
      | Exp_constant const ->
        (match const with
         | Const_integer _ -> return (Subst.empty, Type_int)
         | Const_string _ -> return (Subst.empty, Type_string)
         | Const_char _ -> return (Subst.empty, Type_char))
      | Exp_let (Nonrecursive, { pat = Pat_var pat; exp }, [], exp1) ->
        let* s1, t1 = helper env exp in
        let env2 = TypeEnv.apply s1 env in
        let t2 = generalize env2 t1 in
        let* s2, t3 = helper (TypeEnv.extend env2 pat t2) exp1 in
        let* final_subst = Subst.compose s1 s2 in
        return (final_subst, t3)
      | Exp_fun (pat, pat_list, exp) ->
        let* env, t1 = infer_pattern env pat in
        let* sub, t2 =
          match pat_list with
          | [] -> helper env exp
          | hd :: tl -> helper env (Exp_fun (hd, tl, exp))
        in
        return (sub, Type_arrow (Subst.apply sub t1, t2))
      | Exp_apply (e1, e2) ->
        let* s1, t1 = helper env e1 in
        let* s2, t2 = helper (TypeEnv.apply s1 env) e2 in
        let* fresh = fresh_var in
        let* s3 = unify (Subst.apply s2 t1) (Type_arrow (t2, fresh)) in
        let* composed_sub = Subst.compose_all [ s3; s2; s1 ] in
        let final_type = Subst.apply composed_sub fresh in
        return (composed_sub, final_type)
      | Exp_match (exp, case, case_list) ->
        let* exp_sub, exp_type = helper env exp in
        let* fresh = fresh_var in
        let* cases_sub, case_type =
          RList.fold_left
            ~f:(fun acc { left = pat; right = case_exp } ->
              let* sub_acc, type_acc = return acc in
              let* env, pat_type = infer_pattern env pat in
              let* unified_sub1 = unify exp_type pat_type in
              let* case_exp_sub, case_exp_type = helper env case_exp in
              let* unified_sub2 = unify case_exp_type type_acc in
              let* composed_type =
                Subst.compose_all [ sub_acc; unified_sub1; unified_sub2; case_exp_sub ]
              in
              return (composed_type, case_exp_type))
            ~init:(return (Subst.empty, fresh))
            (case :: case_list)
        in
        let* final_sub = Subst.compose cases_sub exp_sub in
        return (final_sub, Subst.apply final_sub case_type)
      | Exp_tuple (fst, snd, rest_list) ->
        let* s1, t1 = helper env fst in
        let* s2, t2 = helper (TypeEnv.apply s1 env) snd in
        let* sub_rest, t_list =
          RList.fold_right
            ~f:(fun exp acc ->
              let* sub_acc, typs = return acc in
              let* sub, typ = helper (TypeEnv.apply sub_acc env) exp in
              let* sub_acc = Subst.compose sub_acc sub in
              return (sub_acc, typ :: typs))
            ~init:(return (Subst.empty, []))
            rest_list
        in
        let* sub_result = Subst.compose_all [ s1; s2; sub_rest ] in
        let typ1 = Subst.apply sub_result t1 in
        let typ2 = Subst.apply sub_result t2 in
        let typ_list_rest = List.map (fun typ -> Subst.apply sub_result typ) t_list in
        return (sub_result, Type_tuple (typ1, typ2, typ_list_rest))
      | Exp_construct (id, None) when id = "true" || id = "false" ->
        return (Subst.empty, Type_bool)
      | Exp_construct ("[]", None) ->
        let* fresh = fresh_var in
        return (Subst.empty, Type_list fresh)
      | Exp_ifthenelse (if_, then_, Some else_) ->
        let* s1, t1 = helper env if_ in
        let* s2, t2 = helper (TypeEnv.apply s1 env) then_ in
        let* s3, t3 = helper (TypeEnv.apply s2 env) else_ in
        let* s4 = unify t1 Type_bool in
        let* s5 = unify t2 t3 in
        let* final_sub = Subst.compose_all [ s5; s4; s3; s2; s1 ] in
        return (final_sub, Subst.apply s5 t2)
      | Exp_sequence (exp1, exp2) ->
        let* sub1, typ1 = helper env exp1 in
        let* unified_sub = unify typ1 Type_unit in
        let* sub2, typ2 = helper (TypeEnv.apply sub1 env) exp2 in
        let* final_sub = Subst.compose_all [ unified_sub; sub2; sub1 ] in
        return (final_sub, typ2)
      | Exp_constraint (exp, c_type) ->
        let* sub, typ = helper env exp in
        let* unified_sub = unify typ c_type in
        let* final_sub = Subst.compose unified_sub sub in
        return (final_sub, typ)
      | _ -> fail `Not_implemented
    in
    helper
  ;;

  let rec infer_srtucture_item env ast =
    RList.fold_left ast ~init:(return env) ~f:(fun env ->
        function
        | Struct_value (Nonrecursive, value_binding, value_binding_list) ->
          infer_value_binding_list env Subst.empty (value_binding :: value_binding_list)
        | Struct_eval exp ->
          let* _, _ = infer_expression env exp in
          return env
        | _ -> fail `Not_implemented)

  and infer_value_binding_list env sub = function
    | [] -> return env
    | { pat = Pat_var pat; exp } :: rest ->
      let* new_sub, typ = infer_expression env exp in
      let* composed_sub = Subst.compose sub new_sub in
      let env = TypeEnv.apply composed_sub env in
      let generalized_ty = generalize env (Subst.apply composed_sub typ) in
      let env = TypeEnv.extend env pat generalized_ty in
      infer_value_binding_list env composed_sub rest
    | _ -> fail `Not_implemented
  ;;
end

let run_inferencer ast = R.run (Infer.infer_srtucture_item TypeEnv.empty ast)
