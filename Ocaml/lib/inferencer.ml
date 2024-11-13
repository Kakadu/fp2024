(** Copyright 2021-2023, Daniil Kadochnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree
open Ast

let use_logging = false

let log fmt =
  if use_logging
  then Format.kasprintf (fun s -> Format.printf "%s\n%!" s) fmt
  else Format.ifprintf Format.std_formatter fmt
;;

type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of ty * ty
  | `Unapplicable_type of type_expr option * ty
  | `Unexpected_error
  ]

let pp_error ppf : error -> _ = function
  | `Occurs_check -> Format.fprintf ppf "Type Checker Error: occurs check failed"
  | `No_variable s -> Format.fprintf ppf "Type Checker Error: undefined variable '%s'" s
  | `Unification_failed (l, r) ->
    Format.fprintf
      ppf
      "Type Checker Error: unification failed on %a and %a"
      Pprint.pp_typ
      l
      Pprint.pp_typ
      r
  | `Unapplicable_type (par, inf) ->
    Format.fprintf
      ppf
      "Type Checker Error: expression has type %a but expected %a"
      Pprint.pp_typ
      inf
      Pprint.pp_typ_expr
      par
  | `Unexpected_error -> Format.fprintf ppf "Type Checker Error: unexpected Error"
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
    | Result.Error x -> last, Error x
    | Ok a -> f a last
  ;;

  let fail e st = st, Base.Result.fail e
  let return x last = last, Base.Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
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

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

module Type = struct
  type t = ty

  let rec occurs_in v = function
    | Ty_var b -> b = v
    | Arrow (l, r) -> occurs_in v l || occurs_in v r
    | TyList t -> occurs_in v t
    | TyOption t -> occurs_in v t
    | TyTuple tl -> List.exists (occurs_in v) tl
    | TyInt | TyBool | TyString -> false
  ;;

  let free_vars =
    let rec helper acc = function
      | Ty_var b -> VarSet.add b acc
      | Arrow (l, r) -> helper (helper acc l) r
      | TyList t -> helper acc t
      | TyOption t -> helper acc t
      | TyTuple tl -> List.fold_left helper acc tl
      | TyInt | TyBool | TyString -> acc
    in
    helper VarSet.empty
  ;;
end

module Subst : sig
  type t

  val pp : Stdlib.Format.formatter -> t -> unit
  val empty : t
  val singleton : fresh -> ty -> t R.t

  (** Getting value from substitution. May raise [Not_found] *)
  val find_exn : fresh -> t -> ty

  val find : fresh -> t -> ty option
  val apply : t -> ty -> ty
  val unify : ty -> ty -> t R.t
  val unify_with_tyexpr : type_expr option -> ty -> t R.t

  (** Compositon of substitutions *)
  val compose : t -> t -> t R.t

  val compose_all : t list -> t R.t
  val remove : t -> fresh -> t
end = struct
  open R
  open R.Syntax
  open Base
  open Ast

  (* an association list. In real world replace it by a finite map *)
  type t = (fresh * ty) list

  let pp ppf subst =
    let open Stdlib.Format in
    fprintf
      ppf
      "[ %a ]"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf ", ")
         (fun ppf (k, v) -> fprintf ppf "%d -> %a" k Pprint.pp_typ v))
      subst
  ;;

  let empty = []
  let mapping k v = if Type.occurs_in k v then fail `Occurs_check else return (k, v)

  let singleton k v =
    let* mapping = mapping k v in
    return [ mapping ]
  ;;

  let find_exn k xs = List.Assoc.find_exn xs k ~equal:Int.equal
  let find k xs = List.Assoc.find xs k ~equal:Int.equal
  let remove xs k = List.Assoc.remove xs k ~equal:Int.equal

  let apply s =
    let rec helper = function
      | Ty_var b as ty ->
        (match find_exn b s with
         | exception Not_found_s _ -> ty
         | x -> x)
      | Arrow (l, r) -> Arrow (helper l, helper r)
      | TyList t -> TyList (helper t)
      | TyOption t -> TyOption (helper t)
      | TyTuple tl -> TyTuple (Stdlib.List.map helper tl)
      | other -> other
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TyInt, TyInt | TyBool, TyBool | TyString, TyString -> return empty
    | Ty_var a, Ty_var b when Int.equal a b -> return empty
    | Ty_var b, t | t, Ty_var b -> singleton b t
    | Arrow (l1, r1), Arrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | TyTuple tl1, TyTuple tl2 ->
      if List.length tl1 <> List.length tl2
      then fail (`Unification_failed (l, r))
      else (
        let unify_pairs acc (t1, t2) =
          let* s = acc in
          let* s' = unify (apply s t1) (apply s t2) in
          compose s s'
        in
        try
          Stdlib.List.fold_left unify_pairs (return empty) (Stdlib.List.combine tl1 tl2)
        with
        | Invalid_argument _ -> fail (`Unification_failed (l, r)))
    | TyList t1, TyList t2 -> unify t1 t2
    | TyOption t1, TyOption t2 -> unify t1 t2
    | l, r -> fail (`Unification_failed (l, r))

  and extend s (k, v) =
    match List.Assoc.find s ~equal:Int.equal k with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      RList.fold_left s ~init:(return s2) ~f:(fun acc (k, v) ->
        let v = apply s2 v in
        let* mapping = mapping k v in
        return (mapping :: acc))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = RList.fold_left s2 ~init:(return s1) ~f:extend

  let rec type_transform = function
    | TInt -> TyInt
    | TBool -> TyBool
    | TString -> TyString
    | TTuple l ->
      TyTuple
        (List.rev (Stdlib.List.fold_left (fun acc t -> type_transform t :: acc) [] l))
    | TList t -> TyList (type_transform t)
    | TOption t -> TyOption (type_transform t)
    | TFun (l, r) -> Arrow (type_transform l, type_transform r)
  ;;

  let rec unify_with_tyexpr (l : type_expr option) (r : ty) =
    match l, r with
    | Some TInt, TyInt | Some TBool, TyBool | Some TString, TyString -> return empty
    | None, _ -> return empty
    | Some t, Ty_var b -> return [ b, type_transform t ]
    | Some (TFun (l1, r1)), Arrow (l2, r2) ->
      let* subs1 = unify_with_tyexpr (Some l1) l2 in
      let* subs2 = unify_with_tyexpr (Some r1) (apply subs1 r2) in
      compose subs1 subs2
    | Some (TTuple tl1), TyTuple tl2 ->
      if List.length tl1 <> List.length tl2
      then fail (`Unapplicable_type (l, r))
      else (
        let unify_pairs acc (t1, t2) =
          let* s = acc in
          let* s' = unify_with_tyexpr (Some t1) (apply s t2) in
          compose s s'
        in
        try
          Stdlib.List.fold_left unify_pairs (return empty) (Stdlib.List.combine tl1 tl2)
        with
        | Invalid_argument _ -> fail (`Unapplicable_type (l, r)))
    | Some (TList t1), TyList t2 -> unify_with_tyexpr (Some t1) t2
    | Some (TOption t1), TyOption t2 -> unify_with_tyexpr (Some t1) t2
    | l, r -> fail (`Unapplicable_type (l, r))
  ;;

  let compose_all ss = RList.fold_left ss ~init:(return empty) ~f:compose
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

  let occurs_in v = function
    | S (xs, t) -> (not (VarSet.mem v xs)) && Type.occurs_in v t
  ;;

  let free_vars = function
    | S (bs, t) -> VarSet.diff (Type.free_vars t) bs
  ;;

  let apply sub (S (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove s k) names sub in
    S (names, Subst.apply s2 ty)
  ;;

  let pp = Pprint.pp_scheme
end

module TypeEnv = struct
  open Base

  type t = (string * scheme) list

  let extend e h = h :: e
  let empty = []

  let free_vars : t -> VarSet.t =
    List.fold_left ~init:VarSet.empty ~f:(fun acc (_, s) ->
      VarSet.union acc (Scheme.free_vars s))
  ;;

  let apply s env = List.Assoc.map env ~f:(Scheme.apply s)

  let pp ppf xs =
    Stdlib.Format.fprintf ppf "{| ";
    List.iter xs ~f:(fun (n, s) ->
      Stdlib.Format.fprintf ppf "%s -> %a; " n Pprint.pp_scheme s);
    Stdlib.Format.fprintf ppf "|}%!"
  ;;

  let find_exn name xs = List.Assoc.find_exn ~equal:String.equal xs name
end

open R
open R.Syntax
open Ast

let unify = Subst.unify
let unify_with_tyexpr = Subst.unify_with_tyexpr
let fresh_var = fresh >>| fun n -> Ty_var n

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

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
  fun env ty ->
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  S (free, ty)
;;

let lookup_env e xs =
  match Base.List.Assoc.find_exn xs ~equal:String.equal e with
  | (exception Stdlib.Not_found) | (exception Base.Not_found_s _) -> fail (`No_variable e)
  | scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

let pp_env subst ppf env =
  let env : TypeEnv.t =
    List.map (fun (k, S (args, v)) -> k, S (args, Subst.apply subst v)) env
  in
  TypeEnv.pp ppf env
;;

let uncover_item = function
  | t :: [] -> return t
  | _ -> fail `Unexpected_error
;;

let infer =
  let rec (helper : TypeEnv.t -> Ast.expr -> (Subst.t * ty list) R.t) =
    fun env -> function
    | EInt _ -> return (Subst.empty, [ TyInt ])
    | EBool _ -> return (Subst.empty, [ TyBool ])
    | EString _ -> return (Subst.empty, [ TyString ])
    | EVar (name, ex_type) ->
      let* s, t = lookup_env name env in
      let* s' =
        match run (unify_with_tyexpr ex_type t) with
        | Result.Error _ -> fail (`Unapplicable_type (ex_type, t))
        | Ok subst -> return subst
      in
      let* s = Subst.compose s s' in
      return (s, [ t ])
    | EBinOp (op, e1, e2) ->
      let* s1, t1 = helper env e1 in
      let* t1 = uncover_item t1 in
      let env' = TypeEnv.apply s1 env in
      let* s2, t2 = helper env' e2 in
      let* t2 = uncover_item t2 in
      let* s3, result_type =
        match op with
        | Add | Sub | Mul | Div ->
          let* s1 = unify t1 TyInt in
          let* s2 = unify t2 TyInt in
          let* final_subst = Subst.compose_all [ s1; s2 ] in
          return (final_subst, TyInt)
        | And | Or ->
          let* s1 = unify t1 TyBool in
          let* s2 = unify t2 TyBool in
          let* final_subst = Subst.compose_all [ s1; s2 ] in
          return (final_subst, TyBool)
        | Eq | Neq | Lt | Gt | Le | Ge ->
          let* s = unify t1 t2 in
          return (s, TyBool)
      in
      let* final_subst = Subst.compose_all [ s3; s2; s1 ] in
      return (final_subst, [ Subst.apply final_subst result_type ])
    | EFun (PVar (x, _), e1) ->
      let* tv = fresh_var in
      let env2 = TypeEnv.extend env (x, S (VarSet.empty, tv)) in
      let* s, ty = helper env2 e1 in
      let* ty = uncover_item ty in
      let trez = Arrow (Subst.apply s tv, ty) in
      return (s, [ trez ])
    | EApp (e1, e2) ->
      let* s1, t1 = helper env e1 in
      let* t1 = uncover_item t1 in
      let* s2, t2 = helper (TypeEnv.apply s1 env) e2 in
      let* t2 = uncover_item t2 in
      let* tv = fresh_var in
      let* s3 = unify (Subst.apply s2 t1) (Arrow (t2, tv)) in
      let trez = Subst.apply s3 tv in
      let* final_subst = Subst.compose_all [ s3; s2; s1 ] in
      return (final_subst, [ trez ])
    | ETuple exprs ->
      let infer_list =
        List.fold_left
          (fun acc t ->
            let* sub, typ = acc in
            let* s, t = helper env t in
            let* t = uncover_item t in
            return (s :: sub, t :: typ))
          (return ([], []))
          exprs
      in
      let* s, t = infer_list in
      let* final_subst = Subst.compose_all s in
      return (final_subst, [ TyTuple (List.rev t) ])
    | EList exprs ->
      let* fresh_tv = fresh_var in
      let rec infer_list acc = function
        | [] -> return (acc, TyList fresh_tv)
        | hd :: tl ->
          let* sub, ty = helper env hd in
          let* ty = uncover_item ty in
          let* sub2 = Subst.unify ty fresh_tv in
          let* sub = Subst.compose sub sub2 in
          infer_list (sub :: acc) tl
      in
      let* sub, ty = infer_list [] exprs in
      let* sub = Subst.compose_all sub in
      let ty = Subst.apply sub ty in
      return (sub, [ ty ])
    | ESome e ->
      let* s, t = helper env e in
      let* t = uncover_item t in
      return (s, [ TyOption t ])
    | ENone ->
      let* tv = fresh_var in
      return (Subst.empty, [ TyOption tv ])
    | ELet (Recursive, bindings, body) ->
      let extend_env env (PVar (x, _)) =
        let* fresh_tv = fresh_var in
        return (TypeEnv.extend env (x, S (VarSet.empty, fresh_tv)), fresh_tv)
      in
      let* initial_env, fresh_types =
        List.fold_left
          (fun acc (p, _) ->
            let* env, fresh_tvs = acc in
            let* env', fresh_tv = extend_env env p in
            return (env', fresh_tv :: fresh_tvs))
          (return (env, []))
          bindings
      in
      let infer_bindings env bindings fresh_types =
        List.fold_left2
          (fun acc (pattern, expr) fresh_type ->
            let* env', s_acc = acc in
            let* s, t = helper env' expr in
            let* t = uncover_item t in
            let* s = Subst.compose s_acc s in
            match pattern with
            | PVar (name, pat_type) ->
              let* s' =
                match run (unify_with_tyexpr pat_type t) with
                | Result.Error _ -> fail (`Unapplicable_type (pat_type, t))
                | Ok subst -> return subst
              in
              let* s = Subst.compose s s' in
              let* s' = unify (Subst.apply s fresh_type) t in
              let* s = Subst.compose s' s in
              let env = TypeEnv.apply s env' in
              let generalized_type = generalize env (Subst.apply s fresh_type) in
              let env = TypeEnv.(extend (apply s env) (name, generalized_type)) in
              return (env, s))
          (return (env, Subst.empty))
          bindings
          (List.rev fresh_types)
      in
      let* extended_env, final_subst = infer_bindings initial_env bindings fresh_types in
      let* s, t =
        match body with
        | Some expr_body ->
          let* s_body, t_body = helper extended_env expr_body in
          let* t_body = uncover_item t_body in
          let* final_subst' = Subst.compose s_body final_subst in
          return (final_subst', [ t_body ])
        | None ->
          let find_type env bindings =
            List.fold_left
              (fun acc (pattern, _) ->
                let* acc' = acc in
                match pattern with
                | PVar (name, _) ->
                  let* _, t = lookup_env name env in
                  return (t :: acc'))
              (return [])
              bindings
          in
          let* types = find_type extended_env bindings in
          return (final_subst, List.rev types)
      in
      return (s, t)
    | ELet (NonRecursive, bindings, body) ->
      let infer_bindings env bindings =
        List.fold_left
          (fun acc (pattern, expr) ->
            let* env', acc' = acc in
            let* s, t = helper env' expr in
            let* t = uncover_item t in
            let env' = TypeEnv.apply s env' in
            match pattern with
            | PVar (name, pat_type) ->
              let* s' =
                match run (unify_with_tyexpr pat_type t) with
                | Result.Error _ -> fail (`Unapplicable_type (pat_type, t))
                | Ok subst -> return subst
              in
              let* s'' = Subst.compose s s' in
              let env'' = TypeEnv.apply s'' env' in
              let generalized_type = generalize env'' t in
              let* composed_s = Subst.compose s'' acc' in
              return (TypeEnv.extend env'' (name, generalized_type), composed_s))
          (return (env, Subst.empty))
          bindings
      in
      let* extended_env, substs = infer_bindings env bindings in
      let* s, t =
        match body with
        | Some expr_body ->
          let* s_body, t_body = helper extended_env expr_body in
          let* t_body = uncover_item t_body in
          let* final_subst' = Subst.compose s_body substs in
          return (final_subst', [ t_body ])
        | None ->
          let find_type env bindings =
            List.fold_left
              (fun acc (pattern, _) ->
                let* acc' = acc in
                match pattern with
                | PVar (name, _) ->
                  let* _, t = lookup_env name env in
                  return (t :: acc'))
              (return [])
              bindings
          in
          let* types = find_type extended_env bindings in
          return (substs, List.rev types)
      in
      return (s, t)
    | EIf (c, th, el) ->
      let* s1, t1 = helper env c in
      let* t1 = uncover_item t1 in
      let* s2, t2 = helper env th in
      let* t2 = uncover_item t2 in
      let* s3, t3 = helper env el in
      let* t3 = uncover_item t3 in
      let* s4 = unify t1 TyBool in
      let* s5 = unify t2 t3 in
      let* final_subst = Subst.compose_all [ s5; s4; s3; s2; s1 ] in
      return (final_subst, [ Subst.apply s5 t2 ])
  in
  helper
;;

let w e = Result.map snd (run (infer TypeEnv.empty e))
let show_ty_list l = List.fold_left (fun acc t -> acc ^ show_ty t) "" l

let test_infer subst =
  match w subst with
  | Result.Error er -> pp_error Format.std_formatter er
  | Ok subst -> print_endline (show_ty_list subst)
;;

let%expect_test "infer: int" =
  test_infer (EInt 5);
  [%expect {| TyInt |}]
;;

let%expect_test "infer: bool" =
  test_infer (EBool true);
  [%expect {| TyBool |}]
;;

let%expect_test "infer: bool" =
  test_infer (EString "Hi!");
  [%expect {| TyString |}]
;;

(* Test EVar *)
let test_var str env =
  match Result.map snd (run (lookup_env str env)) with
  | Result.Error er -> pp_error Format.std_formatter er
  | Ok subst -> print_endline (show_ty subst)
;;

let%expect_test "infer: lookup_env: var" =
  let env = TypeEnv.extend TypeEnv.empty ("x", S (VarSet.empty, TyBool)) in
  test_var "x" env;
  [%expect {| TyBool |}]
;;

let%expect_test "infer: lookup_env: var" =
  test_var "x" TypeEnv.empty;
  [%expect {| Type Checker Error: undefined variable 'x' |}]
;;

(* Test EBinOp *)
let%expect_test "infer: binOp int int" =
  test_infer (EBinOp (Add, EInt 1, EInt 2));
  [%expect {| TyInt |}]
;;

let%expect_test "infer: binOp bool bool" =
  test_infer (EBinOp (And, EBool true, EBool false));
  [%expect {| TyBool |}]
;;

let%expect_test "infer: binOp a a bool" =
  test_infer (EBinOp (Eq, EBool true, EBool false));
  [%expect {| TyBool |}]
;;

let%expect_test "infer: binOp a a int" =
  test_infer (EBinOp (Eq, EInt 5, EInt 4));
  [%expect {| TyBool |}]
;;

let%expect_test "infer: binOp dif types fail" =
  test_infer (EBinOp (Eq, EBool true, EInt 4));
  [%expect {| Type Checker Error: unification failed on bool and int |}]
;;

let%expect_test "infer: binOp wrong types fail" =
  test_infer (EBinOp (Add, EBool true, EBool false));
  [%expect {| Type Checker Error: unification failed on bool and int |}]
;;

let%expect_test "infer: binOp wrong types fail" =
  test_infer (EBinOp (And, EInt 4, EInt 7));
  [%expect {| Type Checker Error: unification failed on int and bool |}]
;;

(* Test EFun *)
let%expect_test "infer: fun basic" =
  test_infer (EFun (PVar ("x", None), EBinOp (Add, EVar ("x", None), EInt 1)));
  [%expect {| (Arrow (TyInt, TyInt)) |}]
;;

let%expect_test "infer: fun multiple" =
  test_infer
    (EFun
       ( PVar ("x", None)
       , EFun (PVar ("y", None), EBinOp (Add, EVar ("x", None), EVar ("y", None))) ));
  [%expect {| (Arrow (TyInt, (Arrow (TyInt, TyInt)))) |}]
;;

(* Test ELet NonRec *)
let%expect_test "infer: binOp let basic" =
  test_infer (ELet (NonRecursive, [ PVar ("x", None), EInt 5 ], Some (EVar ("x", None))));
  [%expect {| TyInt |}]
;;

let%expect_test "infer: binOp multi let" =
  test_infer
    (ELet
       ( NonRecursive
       , [ PVar ("x", None), EBinOp (Add, EInt 5, EInt 1)
         ; PVar ("y", None), EBinOp (Add, EInt 6, EInt 2)
         ]
       , Some (EBinOp (Add, EVar ("x", None), EVar ("y", None))) ));
  [%expect {| TyInt |}]
;;

let%expect_test "infer: binOp included let" =
  test_infer
    (ELet
       ( NonRecursive
       , [ PVar ("x", None), EInt 5 ]
       , Some
           (ELet
              ( NonRecursive
              , [ PVar ("y", None), EBinOp (Add, EVar ("x", None), EInt 5) ]
              , Some (EVar ("y", None)) )) ));
  [%expect {| TyInt |}]
;;

let%expect_test "infer: binOp let basic" =
  test_infer (ELet (Recursive, [ PVar ("x", None), EInt 5 ], Some (EVar ("x", None))));
  [%expect {| TyInt |}]
;;

(* Test ELet Rec *)
let%expect_test "infer: binOp let basic" =
  test_infer (ELet (Recursive, [ PVar ("x", None), EInt 5 ], Some (EVar ("x", None))));
  [%expect {| TyInt |}]
;;

let%expect_test "infer: binOp included let" =
  test_infer
    (ELet
       ( Recursive
       , [ PVar ("x", None), EInt 5 ]
       , Some
           (ELet
              ( NonRecursive
              , [ PVar ("y", None), EBinOp (Add, EVar ("x", None), EInt 5) ]
              , Some (EVar ("y", None)) )) ));
  [%expect {| TyInt |}]
;;

let%expect_test "infer: binOp let basic" =
  test_infer (ELet (Recursive, [ PVar ("x", None), EInt 5 ], Some (EVar ("x", None))));
  [%expect {| TyInt |}]
;;

let%expect_test "infer: let recursive" =
  test_infer
    (ELet
       ( Recursive
       , [ ( PVar ("fact", None)
           , EFun
               ( PVar ("n", None)
               , EBinOp (Add, EVar ("n", None), EApp (EVar ("fact", None), EInt 4)) ) )
         ]
       , Some (EApp (EVar ("fact", None), EInt 5)) ));
  [%expect {| TyInt |}]
;;

(* Test ETuple *)
let%expect_test "infer: tuple with int and bool" =
  test_infer (ETuple [ EInt 42; EBool true ]);
  [%expect {| (TyTuple [TyInt; TyBool]) |}]
;;

let%expect_test "infer: tuple with two ints" =
  test_infer (ETuple [ EInt 1; EInt 2 ]);
  [%expect {| (TyTuple [TyInt; TyInt]) |}]
;;

let%expect_test "infer: nested tuple" =
  test_infer (ETuple [ EInt 1; ETuple [ EBool true; EString "Hello" ] ]);
  [%expect {| (TyTuple [TyInt; (TyTuple [TyBool; TyString])]) |}]
;;

(* Test EList *)
let%expect_test "infer: list of ints" =
  test_infer (EList [ EInt 1; EInt 2; EInt 3 ]);
  [%expect {| (TyList TyInt) |}]
;;

let%expect_test "infer: empty list" =
  test_infer (EList []);
  [%expect {| (TyList (Ty_var 0)) |}]
;;

let%expect_test "infer: list with mixed types" =
  test_infer (EList [ EInt 1; EBool true; EString "Test" ]);
  [%expect {| Type Checker Error: unification failed on bool and string |}]
;;

let%expect_test "infer: nested list" =
  test_infer (EList [ EList [ EInt 1; EInt 2 ]; EList [ EInt 3; EInt 4 ] ]);
  [%expect {| (TyList (TyList TyInt)) |}]
;;

(* Test ESome ENone *)
let%expect_test "infer: Some with bool" =
  test_infer (ESome (EBool true));
  [%expect {| (TyOption TyBool) |}]
;;

let%expect_test "infer: Some with None" =
  test_infer (ESome ENone);
  [%expect {| (TyOption (TyOption (Ty_var 0))) |}]
;;

let%expect_test "infer: None" =
  test_infer ENone;
  [%expect {| (TyOption (Ty_var 0)) |}]
;;

(* Parsed type testing *)
let%expect_test "infer: non-recursive let with TInt" =
  test_infer
    (ELet (NonRecursive, [ PVar ("x", Some TInt), EInt 42 ], Some (EVar ("x", None))));
  [%expect {| TyInt |}]
;;

let%expect_test "infer: non-recursive let type mismatch TInt vs TBool" =
  test_infer
    (ELet (NonRecursive, [ PVar ("x", Some TBool), EInt 42 ], Some (EVar ("x", None))));
  [%expect {| Type Checker Error: expression has type int but expected bool |}]
;;

let%expect_test "infer: non-recursive let with tuple type (TInt * TBool)" =
  test_infer
    (ELet
       ( NonRecursive
       , [ PVar ("y", Some (TTuple [ TInt; TBool ])), ETuple [ EInt 1; EBool true ] ]
       , Some (EVar ("y", None)) ));
  [%expect {| (TyTuple [TyInt; TyBool]) |}]
;;

let%expect_test "infer: non-recursive let with incorrect tuple type (TInt * TString)" =
  test_infer
    (ELet
       ( NonRecursive
       , [ PVar ("y", Some (TTuple [ TInt; TString ])), ETuple [ EInt 1; EBool true ] ]
       , Some (EVar ("y", None)) ));
  [%expect
    {| Type Checker Error: expression has type (int * bool) but expected (int * string) |}]
;;

let%expect_test "infer: recursive let with TFun TInt -> TInt" =
  test_infer
    (ELet
       ( Recursive
       , [ ( PVar ("f", Some (TFun (TInt, TInt)))
           , EFun (PVar ("x", Some TInt), EBinOp (Add, EVar ("x", None), EInt 1)) )
         ]
       , Some (EVar ("f", None)) ));
  [%expect {| (Arrow (TyInt, TyInt)) |}]
;;

let%expect_test "infer: recursive let with incorrect TFun TString -> TInt" =
  test_infer
    (ELet
       ( Recursive
       , [ ( PVar ("f", Some (TFun (TString, TInt)))
           , EFun (PVar ("x", Some TInt), EBinOp (Add, EVar ("x", None), EInt 1)) )
         ]
       , Some (EVar ("f", None)) ));
  [%expect
    {| Type Checker Error: expression has type (int -> int) but expected (string -> int) |}]
;;

let%expect_test "infer: recursive let with TFun TOption TInt -> TInt" =
  test_infer
    (ELet
       ( Recursive
       , [ ( PVar ("f", Some (TFun (TOption TInt, TInt)))
           , EFun
               (PVar ("x", Some (TOption TInt)), EApp (EVar ("f", None), ESome (EInt 5)))
           )
         ]
       , Some (EVar ("f", None)) ));
  [%expect {| (Arrow ((TyOption TyInt), TyInt)) |}]
;;

(* Test EIf *)

let%expect_test "infer: if-then-else with TInt branches" =
  test_infer (EIf (EBool true, EInt 1, EInt 0));
  [%expect {| TyInt |}]
;;

let%expect_test "infer: if-then-else with mismatched branch types" =
  test_infer (EIf (EBool true, EInt 1, EBool false));
  [%expect {| Type Checker Error: unification failed on int and bool |}]
;;

let%expect_test "infer: if-then-else with variable condition" =
  test_infer
    (ELet
       ( NonRecursive
       , [ PVar ("x", Some TInt), EInt 10 ]
       , Some (EIf (EBinOp (Gt, EVar ("x", None), EInt 5), EInt 1, EInt 0)) ));
  [%expect {| TyInt |}]
;;

let%expect_test "infer: nested if-then-else" =
  test_infer (EIf (EBool true, EIf (EBool false, EInt 1, EInt 2), EInt 3));
  [%expect {| TyInt |}]
;;

let%expect_test "infer: if-then-else with function branches" =
  test_infer
    (EIf
       ( EBool true
       , EFun (PVar ("x", Some TInt), EBinOp (Add, EVar ("x", None), EInt 1))
       , EFun (PVar ("y", Some TInt), EBinOp (Sub, EVar ("y", None), EInt 1)) ));
  [%expect {| (Arrow (TyInt, TyInt)) |}]
;;
