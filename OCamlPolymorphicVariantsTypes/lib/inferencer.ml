(** Copyright 2024-2027, Ilia Suponev, Chirkov Dmitri *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Parser
open Ast
open Typedtree
open Base
open Format

type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of ty * ty
  ]

let pp_error ppf : error -> _ = function
  | `Occurs_check -> Format.fprintf ppf "Occurs check failed"
  | `No_variable s -> Format.fprintf ppf "Undefined variable '%s'" s
  | `Unification_failed (l, r) ->
    Format.fprintf ppf "unification failed on %a and %a" pp_ty l pp_ty r
;;

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
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

  module RList = struct
    let fold_left xs ~init ~f =
      Base.List.fold_left xs ~init ~f:(fun acc x ->
        let open Syntax in
        let* acc = acc in
        f acc x)
    ;;
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
  type t = ty

  let rec occurs_in v = function
    | TVar x -> x = v
    | TArrow (l, r) -> occurs_in v l || occurs_in v r
    | TList typ -> occurs_in v typ
    | TTuple (t1, t2, tl) ->
      occurs_in v t1 || occurs_in v t2 || Base.List.exists tl ~f:(occurs_in v)
    | TPrim _ -> false
  ;;

  let free_vars =
    let rec helper acc = function
      | TVar x -> VarSet.add x acc
      | TArrow (l, r) -> helper (helper acc l) r
      | TList t -> helper acc t
      | TTuple (t1, t2, tl) ->
        let acc' = helper (helper acc t1) t2 in
        List.fold_left ~f:(fun acc t -> helper acc t) ~init:acc' tl
      | TPrim _ -> acc
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : fresh -> ty -> t R.t
  val find : t -> fresh -> ty option
  val remove : t -> fresh -> t
  val apply : t -> ty -> ty
  val unify : ty -> ty -> t R.t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
  (* val pp : Format.formatter -> (ty, ty, Base.Int.comparator_witness) Base.Map.t -> unit *)
end = struct
  open R
  open R.Syntax

  type t = (fresh, ty, Int.comparator_witness) Map.t

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
      | TVar v as ty ->
        (match find s v with
         | Some ty' -> helper ty'
         | None -> ty)
      | TArrow (l, r) -> TArrow (helper l, helper r)
      | TTuple (t1, t2, tl) -> ty_tuple (helper t1, helper t2, List.map ~f:helper tl)
      | TList l -> ty_list (helper l)
      | TPrim _ as ty -> ty
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TPrim l, TPrim r when String.equal l r -> return empty
    | TVar l, TVar r when l = r -> return empty
    | TVar v, t | t, TVar v -> singleton v t
    | TArrow (l1, r1), TArrow (l2, r2) ->
      let* s1 = unify l1 l2 in
      let* s2 = unify (apply s1 r1) (apply s1 r2) in
      compose s1 s2
    | TTuple (t11, t12, t1l), TTuple (t21, t22, t2l) ->
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
    | TList l1, TList l2 -> unify l1 l2
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
  type t = scheme

  let occurs_in v (S (xs, t)) = (not (VarSet.mem v xs)) && Type.occurs_in v t
  let free_vars (S (xs, t)) = VarSet.diff (Type.free_vars t) xs

  let apply s (S (xs, t)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) xs s in
    S (xs, Subst.apply s2 t)
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
end

open R
open R.Syntax

let fresh_var = fresh >>| fun n -> TVar n

let instantiate : scheme -> ty R.t =
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
    | PAny ->
      let* fresh = fresh_var in
      return (env, fresh)
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
      return (env, TTuple (t1', t2', tl'))
    | _ -> failwith "TODO"
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
    | Unary _ -> failwith "TODO"
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
    | Match (e, cl) -> failwith "TODO"
    | Define ((Nonrecursive, bindings), e) -> failwith "TODO"
    | Define ((Recursive, bindings), e) -> failwith "TODO"
    | Func _ -> failwith "TODO"
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
      return (s3, TTuple (t1, t2, t3))
    | _ -> failwith "TODO"
  in
  helper
;;

let infer_structure_item env = function
  | EvalItem e ->
    let* _, _ = infer_expression env e in
    return env
  | _ -> failwith "TODO"
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

let code_infer s =
  let open Stdlib.Format in
  match Parser.program_parser s with
  | ParseSuccess (p, parser_state) ->
    (match run_infer p with
     | Ok env ->
       Base.Map.iteri env ~f:(fun ~key ~data:(S (_, ty)) ->
         printf "val %s : %a\n" key pp_ty ty)
     | Error e -> printf "Inferencer error: %a\n" pp_error e)
  | ParseError (e, parser_state) -> printf "Parser error: %s\n" e
  | ParseFail -> printf "Parser failed" (*TODO*)
;;
