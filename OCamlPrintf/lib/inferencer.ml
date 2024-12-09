(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open TypedTree
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

module Type = struct
  type t = core_type

  let rec occurs_in var = function
    | Type_unit | Type_any | Type_char | Type_int | Type_string | Type_bool -> false
    | Type_name name -> name = var
    | Type_list ty -> occurs_in var ty
    | Type_tuple (first, second, list) ->
      List.exists (occurs_in var) (first :: second :: list)
    | Type_arrow (l, r) -> occurs_in var l || occurs_in var r
  ;;

  let free_vars =
    let rec helper acc = function
      | Type_unit | Type_any | Type_char | Type_int | Type_string | Type_bool -> acc
      | Type_name name -> VarSet.add name acc
      | Type_tuple (first, second, list) ->
        List.fold_left helper acc (first :: second :: list)
      | Type_list ty -> helper acc ty
      | Type_arrow (l, r) -> helper (helper acc l) r
    in
    helper VarSet.empty
  ;;
end

module Subst = struct
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
    | Type_unit, Type_unit
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
    Base.Map.iter xs ~f:(fun (n, s) ->
      Stdlib.Format.fprintf ppf "%s -> %a; " n pp_scheme s);
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
    (* 97 - is number 'a' in ASCII-table *)
    fresh >>| fun n -> Type_name ("'" ^ String.make 1 (Char.chr (97 + n - 1)))
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

  (* let pp_env subst ppf env =
     let env : TypeEnv.t =
     List.map (fun (k, Scheme (args, v)) -> k, Scheme (args, Subst.apply subst v)) env
     in
     TypeEnv.pp ppf env
     ;; *)

  let infer_pattern =
    let rec helper env = function
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
         | Const_char _ -> return (env, Type_char)
         | Const_string _ -> return (env, Type_string))
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
         | Const_char _ -> return (Subst.empty, Type_char)
         | Const_string _ -> return (Subst.empty, Type_string))
      | Exp_let (Nonrecursive, { pat = Pat_var pat; exp }, [], exp1) ->
        let* s1, t1 = helper env exp in
        let env2 = TypeEnv.apply s1 env in
        let t2 = generalize env2 t1 in
        let* s2, t3 = helper (TypeEnv.extend env2 pat t2) exp1 in
        let* final_subst = Subst.compose s1 s2 in
        return (Subst.(final_subst), t3)
      | Exp_fun (pat, [], exp) ->
        let* env, t1 = infer_pattern env pat in
        let* sub, t2 = helper env exp in
        return (sub, Subst.apply sub (Type_arrow (t1, t2)))
      | Exp_apply (e1, e2, []) ->
        let* s1, t1 = helper env e1 in
        let* s2, t2 = helper (TypeEnv.apply s1 env) e2 in
        let* fresh = fresh_var in
        let* s3 = unify (Subst.apply s2 t1) (Type_arrow (t2, fresh)) in
        let* composed_sub = Subst.compose_all [ s3; s2; s1 ] in
        let sub = Subst.apply composed_sub fresh in
        return (composed_sub, sub)
      | Exp_ifthenelse (if_, then_, Some else_) ->
        let* s1, t1 = helper env if_ in
        let* s2, t2 = helper (TypeEnv.apply s1 env) then_ in
        let* s3, t3 = helper (TypeEnv.apply s2 env) else_ in
        let* s4 = unify t1 Type_bool in
        let* s5 = unify t2 t3 in
        let* final_sub = Subst.compose_all [ s5; s4; s3; s2; s1 ] in
        return (final_sub, Subst.apply s5 t2)
      | _ -> fail `Not_implemented
    in
    helper
  ;;

  let rec infer_srtucture_item env ast =
    RList.fold_left ast ~init:(return env) ~f:(fun env ->
        function
        | Struct_value (Nonrecursive, value_binding, value_binding_list) ->
          infer_value_binding_list env Subst.empty (value_binding :: value_binding_list)
        | Struct_eval e ->
          let* _, _ = infer_expression env e in
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

let () =
  let open Format in
  match Parser.parse {|  |} with
  | Ok ast ->
    (match run_inferencer ast with
     | Ok env ->
       Base.Map.iteri env ~f:(fun ~key ~data:(Scheme (_, ty)) ->
         printf "val %s : %a\n" key pp_core_type ty)
     | Error e -> printf "Infer error: %a\n" pp_error e)
  | Error _ -> printf "Parsing error\n"
;;
