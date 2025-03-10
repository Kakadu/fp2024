(** Copyright 2024-2025, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp fmt s =
    Format.fprintf fmt "[ ";
    iter (Format.fprintf fmt "%d; ") s;
    Format.fprintf fmt "]"
  ;;
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]
type scheme = S of binder_set * core_type [@@deriving show { with_path = false }]

let ty_int = TypeIdentifier "int"
let ty_bool = TypeIdentifier "bool"
let ty_unit = TypeIdentifier "unit"
let ty_arrow (l, r) = ArrowType (l, r)
let ty_var v = TypeVariable v
let ty_tuple (t1, t2, tl) = TupleType (t1, t2, tl)
let ty_constructor (n, t) = TypeConstructor (n, t)

let rec pp_ty fmt = function
  | TypeIdentifier s -> Format.fprintf fmt "%s" s
  | TypeVariable v -> Format.fprintf fmt "'%d" v
  | ArrowType (l, r) -> Format.fprintf fmt "(%a -> %a)" pp_ty l pp_ty r
  | TupleType (t1, t2, tl) ->
    Format.fprintf
      fmt
      "(%a * %a%a)"
      pp_ty
      t1
      pp_ty
      t2
      (Format.pp_print_list ~pp_sep:(fun _ _ -> Format.fprintf fmt " * ") pp_ty)
      tl
  | TypeConstructor (n, t) ->
    Format.fprintf
      fmt
      "%a %s"
      (fun ppf ty ->
        match ty with
        | ArrowType _ -> Format.fprintf ppf "(%a)" pp_ty ty
        | _ -> Format.fprintf ppf "%a" pp_ty ty)
      t
      n
;;

type error =
  | OccursCheck of binder * core_type
  | NoVariable of id
  | UnificationFailed of core_type * core_type

let pp_error ppf : error -> _ =
  let open Stdlib.Format in
  function
  | OccursCheck v, t -> printf ppf ("Occurs check: " ^ Int.to_string v ^ pp_ty t)
  | `No_variable s -> fprintf ppf "Undefined variable '%s'" s
  | `Unification_failed (l, r) ->
    fprintf ppf "unification failed on %a and %a" pp_ty l pp_ty r
  | `Not_implemented -> fprintf ppf "Not implemented"
;;

module Result : sig
  type 'a t

  (* val bind : 'a t -> f:('a -> 'b t) -> 'b t *)
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold
      :  ('k, 'v, 'cmp) Base.Map.t
      -> init:'b t
      -> f:('k -> 'v -> 'b -> 'b t)
      -> 'b t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t
end = struct
  (* A compositon: State monad after Result monad *)
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Result.fail x
    | Result.Ok a -> f a last
  ;;

  let fail e st = st, Base.Result.fail e
  let return x last = last, Base.Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Ok x -> st, Result.return (f x)
    | st, Result.Error e -> st, Result.fail e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  module RMap = struct
    let fold xs ~init ~f =
      Map.fold xs ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module Type = struct
  let rec occurs_in v = function
    | TypeVariable x -> x = v
    | ArrowType (l, r) -> occurs_in v l || occurs_in v r
    | TypeConstructor (_, typ) -> occurs_in v typ
    | TupleType (t1, t2, tl) ->
      occurs_in v t1 || occurs_in v t2 || Base.List.exists tl ~f:(occurs_in v)
    | TypeIdentifier _ -> false
  ;;

  let free_vars =
    let rec helper acc = function
      | TypeVariable x -> VarSet.add x acc
      | ArrowType (l, r) -> helper (helper acc l) r
      | TypeConstructor (_, t) -> helper acc t
      | TupleType (t1, t2, tl) ->
        let acc' = helper (helper acc t1) t2 in
        List.fold_left ~f:(fun acc t -> helper acc t) ~init:acc' tl
      | TypeIdentifier _ -> acc
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> core_type -> t Result.t

  (* val find : t -> fresh -> core_type option *)
  val remove : t -> fresh -> t
  val apply : t -> core_type -> core_type
  val unify : core_type -> core_type -> t Result.t
  val compose : t -> t -> t Result.t
  val compose_all : t list -> t Result.t
  (* val pp : Format.formatter -> (ty, ty, Base.Int.comparator_witness) Base.Map.t -> unit *)
end = struct
  open Result
  open Result.Syntax

  type t = (fresh, core_type, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)
  let mapping k v = if Type.occurs_in k v then fail `Occurs_check else return (k, v)

  let singleton k v =
    let* k, v = mapping k v in
    return (Map.singleton (module Int) k v)
  ;;

  let find s k = Map.find s k
  let remove s k = Map.remove s k

  let apply s =
    let rec helper = function
      | TypeVariable v as ty ->
        (match find s v with
         | Some ty' -> helper ty'
         | None -> ty)
      | ArrowType (l, r) -> ty_arrow (helper l, helper r)
      | TupleType (t1, t2, tl) -> ty_tuple (helper t1, helper t2, List.map ~f:helper tl)
      | TypeConstructor (n, r) -> ty_constructor (n, helper r)
      | TypeIdentifier _ as ty -> ty
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TypeIdentifier l, TypeIdentifier r when String.equal l r -> return empty
    | TypeVariable l, TypeVariable r when l = r -> return empty
    | TypeVariable v, t | t, TypeVariable v -> singleton v t
    | ArrowType (l1, r1), ArrowType (l2, r2) ->
      let* s1 = unify l1 l2 in
      let* s2 = unify (apply s1 r1) (apply s1 r2) in
      compose s1 s2
    | TupleType (t11, t12, t1l), TupleType (t21, t22, t2l) ->
      if List.length t1l <> List.length t2l
      then fail (`Unification_failed (l, r))
      else (
        let tls = List.zip_exn (t11 :: t12 :: t1l) (t21 :: t22 :: t2l) in
        let* s =
          List.fold
            tls
            ~f:(fun acc (l, r) ->
              let* acc = acc in
              let* u = unify l r in
              return (u :: acc))
            ~init:(return [])
        in
        let composed_tl = compose_all s in
        composed_tl)
    | TypeConstructor (n1, l1), TypeConstructor (n2, l2) ->
      if String.equal n1 n2 then unify l1 l2 else fail (`Unification_failed (l, r))
    | _, _ -> fail (`Unification_failed (l, r))

  and extend k v s =
    match find s k with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RMap.fold s ~init:(return s2) ~f:(fun k v acc ->
        let v = apply s2 v in
        let* k, v = mapping k v in
        return (Map.update acc k ~f:(fun _ -> v)))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = RMap.fold s2 ~init:(return s1) ~f:extend

  and compose_all ss =
    List.fold_left ss ~init:(return empty) ~f:(fun acc s ->
      let* acc = acc in
      compose acc s)
  ;;
end

module Scheme = struct
  (* let occurs_in v (S (xs, t)) = (not (VarSet.mem v xs)) && Type.occurs_in v t *)
  let free_vars (S (xs, t)) = VarSet.diff (Type.free_vars t) xs

  let apply s (S (xs, t)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) xs s in
    S (xs, Subst.apply s2 t)
  ;;

  let pp fmt =
    let open Stdlib.Format in
    function
    | S (st, typ) ->
      if VarSet.is_empty st
      then fprintf fmt "%a" pp_ty typ
      else fprintf fmt "%a. %a" VarSet.pp st pp_ty typ
  ;;
end

module TypeEnv = struct
  type t = (identifier, scheme, String.comparator_witness) Map.t

  let extend e k v = Map.update e k ~f:(fun _ -> v)
  let remove e k = Map.remove e k
  let empty = Map.empty (module String)

  let free_vars : t -> VarSet.t =
    Map.fold ~init:VarSet.empty ~f:(fun ~key:_ ~data:s acc ->
      VarSet.union acc (Scheme.free_vars s))
  ;;

  let apply s env = Map.map env ~f:(Scheme.apply s)
  let find x env = Map.find env x

  let pp_env fmt env =
    Map.iteri env ~f:(fun ~key ~data ->
      Stdlib.Format.fprintf fmt "%S : %a\n" key Scheme.pp data)
  ;;
end

open Result
open Result.Syntax

let fresh_var = fresh >>| fun n -> TypeVariable n

let instantiate : scheme -> core_type Result.t =
  fun (S (bs, t)) ->
  VarSet.fold
    (fun name typ ->
      let* typ = typ in
      let* f1 = fresh_var in
      let* s = Subst.singleton name f1 in
      return (Subst.apply s typ))
    bs
    (return t)
;;

let generalize env ty =
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  S (free, ty)
;;

let generalize_rec env ty x =
  let env = TypeEnv.remove env x in
  generalize env ty
;;

let infer_pattern =
  let rec helper env = function
    (* | PAny ->
       let* fresh = fresh_var in
       return (env, fresh) *)
    | PVar x ->
      let* fresh = fresh_var in
      let env = TypeEnv.extend env x (S (VarSet.empty, fresh)) in
      return (env, fresh)
    | PTuple (t1, t2, tl) ->
      let* _, t1' = helper env t1 in
      let* _, t2' = helper env t2 in
      let* tl' =
        List.fold_right
          ~f:(fun p acc ->
            let* acc = acc in
            let* _, t = helper env p in
            return (t :: acc))
          ~init:(return [])
          tl
      in
      return (env, ty_tuple (t1', t2', tl'))
    | PUnit -> return (env, ty_unit)
    | PConstrain (p, t) ->
      let* _, t_p = helper env p in
      let* s = Subst.unify t_p t in
      let env = TypeEnv.apply s env in
      let applied_t = Subst.apply s t in
      return (env, applied_t)
  in
  helper
;;

let infer_expression =
  let rec helper env = function
    | Const c ->
      (match c with
       | IntLiteral _ -> return (Subst.empty, ty_int)
       | BoolLiteral _ -> return (Subst.empty, ty_bool)
       | UnitLiteral -> return (Subst.empty, ty_unit))
    | Variable x ->
      (match TypeEnv.find x env with
       | Some s ->
         let* t = instantiate s in
         return (Subst.empty, t)
       | None -> fail (`No_variable x))
    | Unary (_, e) ->
      let* sub1, t1 = helper env e in
      let* sub2 = Subst.unify (Subst.apply sub1 t1) ty_int in
      let* sub = Subst.compose_all [ sub1; sub2 ] in
      return (sub, Subst.apply sub ty_int)
    | Binary (e1, op, e2) ->
      let* sub1, t1 = helper env e1 in
      let* sub2, t2 = helper (TypeEnv.apply sub1 env) e2 in
      let* e1t, e2t, et =
        match op with
        | Multiply | Division | Add | Subtract -> return (ty_int, ty_int, ty_int)
        | Equals | Unequals | Lt | Lte | Gt | Gte ->
          let* fresh = fresh_var in
          return (fresh, fresh, ty_bool)
        | And | Or -> return (ty_bool, ty_bool, ty_bool)
      in
      let* sub3 = Subst.unify (Subst.apply sub2 t1) e1t in
      let* sub4 = Subst.unify (Subst.apply sub3 t2) e2t in
      let* sub = Subst.compose_all [ sub1; sub2; sub3; sub4 ] in
      return (sub, Subst.apply sub et)
    | If (i, t, e) ->
      let* sub1, t1 = helper env i in
      let* sub2, t2 = helper (TypeEnv.apply sub1 env) t in
      let* sub3, t3 =
        match e with
        | Some e ->
          let* sub3, t3 = helper (TypeEnv.apply sub2 env) e in
          return (sub3, t3)
        | None -> return (Subst.empty, ty_unit)
      in
      let* sub4 = Subst.unify t1 ty_bool in
      let* sub5 = Subst.unify t2 t3 in
      let* sub = Subst.compose_all [ sub1; sub2; sub3; sub4; sub5 ] in
      return (sub, Subst.apply sub t2)
    | Match (e, c, c_rest) ->
      let* sub1, t1 = helper env e in
      let env = TypeEnv.apply sub1 env in
      let* fresh = fresh_var in
      let* sub, t =
        List.fold_left
          (c :: c_rest)
          ~init:(return (sub1, fresh))
          ~f:(fun acc case ->
            let* sub1, t = acc in
            let* env1, pt = infer_pattern env case.pattern in
            let* sub2 = Subst.unify t1 pt in
            let env2 = TypeEnv.apply sub2 env1 in
            let* sub_filter =
              match case.filter with
              | Some filter_expr ->
                let* sub, _ = helper env2 filter_expr in
                return sub
              | None -> return Subst.empty
            in
            let env2 = TypeEnv.apply sub_filter env2 in
            let* sub3, t' = helper env2 case.result in
            let* sub4 = Subst.unify t' t in
            let* sub = Subst.compose_all [ sub1; sub2; sub_filter; sub3; sub4 ] in
            return (sub, Subst.apply sub t))
      in
      return (sub, t)
    | Define ((Nonrecursive, (p, e) :: _), expr) ->
      (match p with
       | PVar x ->
         let* s1, t1 = helper env e in
         let env = TypeEnv.apply s1 env in
         let s = generalize env t1 in
         let* s2, t2 = helper (TypeEnv.extend env x s) expr in
         let* s = Subst.compose s1 s2 in
         return (s, t2)
       | _ -> fail `Not_implemented)
    | Define ((Recursive, (p, e) :: _), expr) ->
      (match p with
       | PVar x ->
         let* fresh = fresh_var in
         let env1 = TypeEnv.extend env x (S (VarSet.empty, fresh)) in
         let* s, t = helper env1 e in
         let* s1 = Subst.unify t fresh in
         let* s2 = Subst.compose s s1 in
         let env = TypeEnv.apply s2 env in
         let t = Subst.apply s2 t in
         let s = generalize_rec env t x in
         let env = TypeEnv.extend env x s in
         let* sub, t = helper env expr in
         let* sub = Subst.compose s2 sub in
         return (sub, t)
       | _ -> fail `Not_implemented)
    | Tuple (t1, t2, tl) ->
      let* s1, t1 = helper env t1 in
      let* s2, t2 = helper env t2 in
      let* s3, t3 =
        List.fold_right
          ~f:(fun expr acc ->
            let* sacc, tacc = acc in
            let* s, t = helper env expr in
            let* sacc = Subst.compose sacc s in
            return (sacc, t :: tacc))
          ~init:(return (Subst.empty, []))
          tl
      in
      let* composed_sl = Subst.compose_all [ s1; s2; s3 ] in
      let t1 = Subst.apply composed_sl t1 in
      let t2 = Subst.apply composed_sl t2 in
      let t3 = List.map t3 ~f:(fun t -> Subst.apply s3 t) in
      return (s3, ty_tuple (t1, t2, t3))
    | Apply (e1, e2, _) ->
      let* fresh = fresh_var in
      let* s1, t1 = helper env e1 in
      let* s2, t2 = helper (TypeEnv.apply s1 env) e2 in
      let* s3 = Subst.unify (ty_arrow (t2, fresh)) (Subst.apply s2 t1) in
      let* sub = Subst.compose_all [ s1; s2; s3 ] in
      let t = Subst.apply sub fresh in
      return (sub, t)
    | ExpressionsList l ->
      (match l with
       | [] ->
         let* fresh = fresh_var in
         return (Subst.empty, ty_constructor ("list", fresh))
       | h :: tl ->
         let* sr, tr =
           Base.List.fold_left tl ~init:(helper env h) ~f:(fun acc e ->
             let* sacc, tacc = acc in
             let* s1, t1 = helper env e in
             let* s2 = Subst.unify tacc t1 in
             let* final_s = Subst.compose_all [ sacc; s1; s2 ] in
             let final_t = Subst.apply final_s tacc in
             return (final_s, final_t))
         in
         return (sr, ty_constructor ("list", tr)))
    | Lambda (pl, e) ->
      let* env, t1s =
        List.fold_right
          pl
          ~init:(return (env, []))
          ~f:(fun p acc ->
            let* old_env, ts = acc in
            let* new_env, t = infer_pattern old_env p in
            return (new_env, t :: ts))
      in
      let* sub, t2 = helper env e in
      return
        ( sub
        , Subst.apply
            sub
            (List.fold_right t1s ~init:t2 ~f:(fun tl tr -> ty_arrow (tl, tr))) )
    | Construct (n, Some e) ->
      let* sub1, t1 = helper env e in
      let ty_constructor = ty_constructor (n, t1) in
      return (sub1, ty_constructor)
    | Construct (n, None) ->
      let* fresh = fresh_var in
      let ty_constructor = ty_constructor (n, fresh) in
      return (Subst.empty, ty_constructor)
    | Func (c, c_rest) ->
      let rec infer_cases env cases =
        match cases with
        | [] -> fail `Not_implemented
        | [ case ] ->
          let* env1, pt = infer_pattern env case.pattern in
          let* sub1, rt = helper env1 case.result in
          return (sub1, ty_arrow (pt, rt))
        | case :: tl ->
          let* sub1, ty1 = infer_cases env [ case ] in
          let* sub2, ty2 = infer_cases env tl in
          let* sub3 = Subst.unify ty1 ty2 in
          let* sub = Subst.compose_all [ sub1; sub2; sub3 ] in
          return (sub, ty1)
      in
      infer_cases env (c :: c_rest)
    | ExpressionBlock (e1, e2, el) ->
      let* sub1, _ = helper env e1 in
      let* sub2, t2 = helper (TypeEnv.apply sub1 env) e2 in
      let* sub3, t3 =
        List.fold_left
          el
          ~init:(return (sub2, t2))
          ~f:(fun acc e ->
            let* sub_acc, _ = acc in
            let* sub, t = helper (TypeEnv.apply sub_acc env) e in
            let* sub_composed = Subst.compose sub_acc sub in
            return (sub_composed, t))
      in
      return (sub3, t3)
    | Define ((_, []), _) -> fail `Not_implemented
  in
  helper
;;

let infer_structure_item env = function
  | DefineItem def ->
    (match def with
     | Recursive, bs ->
       (match bs with
        | [] -> fail `Not_implemented
        | (p, e1) :: _ ->
          (match p with
           | PVar x1 ->
             let* fresh = fresh_var in
             let sc = S (VarSet.empty, fresh) in
             let env = TypeEnv.extend env x1 sc in
             let* s1, t1 = infer_expression env e1 in
             let* s2 = Subst.unify t1 fresh in
             let* s3 = Subst.compose s1 s2 in
             let env = TypeEnv.apply s3 env in
             let t2 = Subst.apply s3 t1 in
             let sc = generalize_rec env t2 x1 in
             let env = TypeEnv.extend env x1 sc in
             return env
           | _ -> fail `Not_implemented))
     | Nonrecursive, bs ->
       (match bs with
        | [] -> fail `Not_implemented
        | (p, e1) :: _ ->
          (match p with
           | PVar x1 ->
             let* s, t = infer_expression env e1 in
             let env = TypeEnv.apply s env in
             let sc = generalize env t in
             let env = TypeEnv.extend env x1 sc in
             return env
           | _ -> fail `Not_implemented)))
  | EvalItem e ->
    let* _, _ = infer_expression env e in
    return env
;;

let infer_program p =
  List.fold_left
    ~f:(fun acc item ->
      let* env = acc in
      let* env = infer_structure_item env item in
      return env)
    ~init:(return TypeEnv.empty)
    p
;;

let run_infer p = run (infer_program p)
