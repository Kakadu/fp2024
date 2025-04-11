[@@@ocaml.text "/*"]

(** Copyright 2024-2025, Damir Yunusov and Ilhom Kombaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Types
open Config

module R : sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t

    (* Map functions by Homka122 😼😼😼 *)
    val map : 'a list -> f:('a -> 'b t) -> 'b list t
    val fmap : 'a list -> f:('a -> 'b) -> 'b list t
    val map2 : 'a list -> 'b list -> f:('a -> 'b -> 'c t) -> 'c list t
    val fmap2 : 'a list -> 'b list -> f:('a -> 'b -> 'c) -> 'c list t
  end

  module RMap : sig
    val fold_left
      :  ('a, 'b, 'c) Base.Map.t
      -> init:'d t
      -> f:('a -> 'b -> 'd -> 'd t)
      -> 'd t
  end

  val fresh : int t
  val run : 'a t -> int -> ('a, error) Result.t
end = struct
  type 'a t = int -> int * ('a, error) Result.t (* State and Result monad composition *)

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f s ->
    match m s with
    | s, Result.Error e -> s, Error e
    | s, Result.Ok v -> f v s
  ;;

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun m f s ->
    match m s with
    | s, Result.Error e -> s, Error e
    | s, Result.Ok v -> s, Base.Result.return @@ f v
  ;;

  let ( <$> ) : ('a -> 'b) -> 'a t -> 'b t = fun f m -> m >>| f
  let return v last = last, Base.Result.return v
  let fail e state = state, Base.Result.fail e
  let bind x ~f = x >>= f
  let fresh last = last + 1, Result.Ok last

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  module RMap = struct
    let fold_left mp ~init ~f =
      let open Syntax in
      Base.Map.fold mp ~init ~f:(fun ~key ~data acc ->
        let* acc = acc in
        f key data acc)
    ;;
  end

  module RList = struct
    let fold_left lt ~init ~f =
      let open Syntax in
      Base.List.fold_left lt ~init ~f:(fun acc item ->
        let* acc = acc in
        f acc item)
    ;;

    (* Map functions by Homka122 😼😼😼 (I'm proud of these) *)
    let map xs ~f =
      let open Syntax in
      let rec helper acc = function
        | x :: t ->
          let* res = f x in
          helper (res :: acc) t
        | [] -> return (List.rev acc)
      in
      helper [] xs
    ;;

    let fmap xs ~f = map xs ~f:(fun x -> return (f x))

    let map2 xs ys ~f =
      let open Syntax in
      let rec helper acc = function
        | [], [] -> return (List.rev acc)
        | [], _ :: _ | _ :: _, [] -> fail (SomeError "two lists must be have equal size")
        | hx :: tx, hy :: ty ->
          let* res = f hx hy in
          helper (res :: acc) (tx, ty)
      in
      helper [] (xs, ys)
    ;;

    let fmap2 xs ys ~f = map2 xs ys ~f:(fun x y -> return (f x y))
  end

  let run m init = snd (m init)
end

module Type = struct
  type t = typ

  let rec occurs_in v = function
    | TVar b -> b = v
    | TArrow (left, right) -> occurs_in v left || occurs_in v right
    | TList typ -> occurs_in v typ
    | TOption typ -> occurs_in v typ
    | TTuple (a, b, typ_list) ->
      List.fold_left (fun acc item -> acc || occurs_in v item) false (a :: b :: typ_list)
    | TBase _ -> false
  ;;

  let type_vars =
    let rec helper acc = function
      | TVar n -> TVarSet.add n acc
      | TArrow (left, right) -> helper (helper acc left) right
      | TList typ -> helper acc typ
      | TOption typ -> helper acc typ
      | TTuple (a, b, typ_list) -> List.fold_left helper acc (a :: b :: typ_list)
      | TBase _ -> acc
    in
    helper TVarSet.empty
  ;;
end

module Subst : sig
  type t

  val empty : t
  val singleton : int -> typ -> t R.t
  val remove : t -> int -> t
  val apply : t -> typ -> typ
  val unify : typ -> typ -> t R.t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
  val pp : Format.formatter -> t -> unit
end = struct
  open R
  open R.Syntax

  type t = (type_var, typ, Base.Int.comparator_witness) Base.Map.t

  let empty = Base.Map.empty (module Base.Int)

  let mapping k v =
    (* if debug then Format.printf "Mapping %d to %s\n" k @@ show_typ v; *)
    if Type.occurs_in k v then fail (OccursCheckFailed (k, v)) else return (k, v)
  ;;

  let singleton k v =
    let* k, v = mapping k v in
    return (Base.Map.singleton (module Base.Int) k v)
  ;;

  let find (sub : t) k = Base.Map.find sub k
  let remove (sub : t) k : t = Base.Map.remove sub k

  let apply sub t =
    let rec helper t =
      match t with
      | TVar n ->
        (match find sub n with
         | None -> TVar n
         | Some (TVar v) when v = n -> TVar n
         | Some v -> helper v)
      | TArrow (left, right) -> TArrow (helper left, helper right)
      | TList typ -> TList (helper typ)
      | TOption typ -> TOption (helper typ)
      | TTuple (a, b, t_list) ->
        TTuple (helper a, helper b, Base.List.map t_list ~f:helper)
      | TBase t -> TBase t
    in
    helper t
  ;;

  let rec unify l r =
    (* if debug then Format.printf "Unify: %s AND %s\n" (show_typ l) (show_typ r); *)
    match l, r with
    | TBase l, TBase r when l = r -> return empty
    | TVar a, TVar b when a = b -> return empty
    | TVar a, t | t, TVar a -> singleton a t
    | TArrow (left1, right1), TArrow (left2, right2) ->
      let* sub1 = unify left1 left2 in
      let* sub2 = unify (apply sub1 right1) (apply sub1 right2) in
      compose sub1 sub2
    | TList typ1, TList typ2 -> unify typ1 typ2
    | TOption typ1, TOption typ2 -> unify typ1 typ2
    | TTuple (a, b, t_list1), TTuple (c, d, t_list2) ->
      (match
         Base.List.fold2
           (a :: b :: t_list1)
           (c :: d :: t_list2)
           ~init:(return empty)
           ~f:(fun acc it1 it2 ->
             let* sub1 = acc in
             let* sub2 = unify (apply sub1 it1) (apply sub1 it2) in
             compose sub1 sub2)
       with
       | Ok r -> r
       | _ -> fail (UnificationFailed (l, r)))
    | _ -> fail (UnificationFailed (l, r))

  and compose sub1 sub2 =
    (* RMap.fold_left sub2 ~init:(return sub1) ~f:extend *)
    let sub2 = Base.Map.map sub2 ~f:(fun s -> apply sub1 s) in
    let* sub =
      Base.Map.fold sub1 ~init:(return sub2) ~f:(fun ~key ~data sub ->
        let* sub = sub in
        match Base.Map.find sub key with
        | None -> return @@ Base.Map.add_exn sub ~key ~data
        | Some v ->
          let* s = unify v data in
          compose s sub)
    in
    return sub
  ;;

  let compose_all sub_list =
    RList.fold_left (List.rev sub_list) ~init:(return empty) ~f:compose
  ;;

  let pp fmt sub =
    if Base.Map.is_empty sub
    then Format.fprintf fmt "empty"
    else (
      Format.fprintf fmt "{";
      Base.Map.iteri sub ~f:(fun ~key ~data ->
        Format.fprintf
          fmt
          "%a : %a; "
          Infer_print.pp_typ_my
          (TVar key)
          Infer_print.pp_typ_my
          data);
      Format.fprintf fmt "}")
  ;;
end

let print_sub ?(name = "Sub") sub = Format.printf "%s: %a\n" name Subst.pp sub

module Scheme = struct
  let free_vars (Scheme (bind_vars, ty)) = TVarSet.diff (Type.type_vars ty) bind_vars

  let apply sub (Scheme (bind_vars, ty)) =
    let sub2 = TVarSet.fold (fun sub key -> Subst.remove key sub) bind_vars sub in
    Scheme (bind_vars, Subst.apply sub2 ty)
  ;;
end

module TypeEnv : sig
  type t

  val empty : t
  val free_vars : t -> TVarSet.t
  val extend : t -> string -> scheme -> t
  val find : t -> string -> scheme option
  val find_exn : t -> string -> scheme
  val remove : t -> string -> t
  val apply : Subst.t -> t -> t
  val operators : (id list * typ) list
  val pp : Format.formatter -> t -> unit
  val pp_names : id list -> Format.formatter -> t -> unit
  val print : ?name:string -> t -> unit
end = struct
  type t = (string, scheme, Base.String.comparator_witness) Base.Map.t

  let empty : t = Base.Map.empty (module Base.String)

  let free_vars (env : t) =
    Base.Map.fold
      ~init:TVarSet.empty
      ~f:(fun ~key:_ ~data acc -> TVarSet.union acc (Scheme.free_vars data))
      env
  ;;

  let apply sub env = Base.Map.map env ~f:(Scheme.apply sub)
  let extend env key schema = Base.Map.update env key ~f:(fun _ -> schema)
  let find = Base.Map.find
  let find_exn = Base.Map.find_exn
  let remove = Base.Map.remove

  let operators =
    [ [ "+"; "-"; "*"; "/" ], TBase BInt @-> TBase BInt @-> TBase BInt
    ; [ "print_int" ], TBase BInt @-> TBase BUnit
    ; [ "<="; "<"; ">"; ">="; "="; "<>" ], TVar 0 @-> TVar 0 @-> TBase BBool
    ; [ "Some" ], TVar 0 @-> TOption (TVar 0)
    ; [ "None" ], TOption (TVar 0)
    ; [ "()" ], TBase BUnit
    ; [ "[]" ], TList (TVar 0)
    ; [ "::" ], TTuple (TVar 0, TList (TVar 0), []) @-> TList (TVar 0)
    ]
  ;;

  let pp_key_value fmt key (Scheme (s, t)) =
    if not Config.show_scheme_vars
    then Format.fprintf fmt "val %s : %a\n" key Infer_print.pp_typ_my t
    else (
      Format.fprintf fmt "val %s: " key;
      TVarSet.iter (fun t -> Format.fprintf fmt "%a " Infer_print.pp_typ_my (TVar t)) s;
      Format.fprintf fmt ". %a\n" Infer_print.pp_typ_my t)
  ;;

  let pp fmt env = Base.Map.iteri env ~f:(fun ~key ~data -> pp_key_value fmt key data)

  (* Print types of specific variables *)
  let pp_names names fmt env =
    List.iter
      (fun key ->
        let value = find_exn env key in
        pp_key_value fmt key value)
      names
  ;;

  let print ?(name = "Env") env = Format.printf "%s: %a" name pp env
end

let ( >- ) = TypeEnv.apply
let ( |- ) = Subst.apply

module DebugLog = struct
  let log (f : unit -> unit) =
    if debug
    then (
      f ();
      print_newline ())
  ;;

  module Aux = struct
    let non_rec_vb env sub =
      (fun () ->
        TypeEnv.print env;
        Format.printf "sub: %a\n" Subst.pp sub)
      |> log
    ;;

    let rec_vb env' subs ts sub env'' =
      (fun () ->
        Format.printf "Env: \n%a\n" TypeEnv.pp env';
        List.iter (fun sub -> Format.printf "Sub: %a\n" Subst.pp sub) subs;
        List.iter (fun t -> Format.printf "Type: %a\n" Infer_print.pp_typ_my t) ts;
        Format.printf "Sub: %a\n" Subst.pp sub;
        Format.printf "Env: %a\n" TypeEnv.pp env'')
      |> log
    ;;
  end

  module Expr = struct
    let apply env e0 =
      (fun () ->
        Format.printf "Env: %a\n" TypeEnv.pp env;
        Format.printf "e0: %a\n" pp_expression e0)
      |> log
    ;;

    let apply_helper_1 e =
      (fun () ->
        Format.printf "APPLY\n";
        Format.printf "e1: %a\n" pp_expression e)
      |> log
    ;;

    let apply_helper_2 t0 sub0 t1 sub1 sub2 sub3 =
      (fun () ->
        Infer_print.(
          Format.printf "t0: %a sub0: %a\n" pp_typ_my t0 Subst.pp sub0;
          Format.printf "t1: %a sub1: %a\n" pp_typ_my t1 Subst.pp sub1;
          Format.printf "sub2: %a\n" Subst.pp sub2;
          Format.printf "sub3: %a\n" Subst.pp sub3;
          print_newline ()))
      |> log
    ;;

    let non_rec_let let_expr t sub1 sub =
      (fun () ->
        Format.printf "Non rec let %a\n" pp_expression let_expr;
        Format.printf "t: %a\n" Infer_print.pp_typ_my t;
        Format.printf "sub1: %a\n" Subst.pp sub1;
        Format.printf "sub: %a\n" Subst.pp sub)
      |> log
    ;;

    let match_expr env_pat ty_expr t_pat sub_expr sub_un_exprs sub =
      (fun () ->
        TypeEnv.print env_pat;
        Infer_print.print_typ ~name:"ty_expr" ty_expr;
        Infer_print.print_typ ~name:"t_pat" t_pat;
        print_sub ~name:"sub_expr" sub_expr;
        print_sub ~name:"sub_un_expr" sub_un_exprs;
        print_sub ~name:"sub" sub)
      |> log
    ;;
  end
end

open R
open R.Syntax

let fresh_var = fresh >>| fun name -> TVar name

let instantiate : scheme -> typ R.t =
  fun (Scheme (bind_var, ty)) ->
  TVarSet.fold
    (fun var_name acc ->
      let* acc = acc in
      let* fv = fresh_var in
      let* sub = Subst.singleton var_name fv in
      return (sub |- acc))
    bind_var
    (return ty)
;;

let generalize : TypeEnv.t -> Type.t -> scheme =
  fun env ty ->
  let free = TVarSet.diff (Type.type_vars ty) (TypeEnv.free_vars env) in
  Scheme (free, ty)
;;

let lookup_env env name =
  match TypeEnv.find env name with
  | Some scheme ->
    let* ty = instantiate scheme in
    return (ty, Subst.empty)
  | None -> fail (Unbound name)
;;

let infer_const c =
  let ty =
    match c with
    | Pconst_int _ -> TBase BInt
    | Pconst_boolean _ -> TBase BBool
    | Pconst_string _ -> TBase BString
  in
  return (ty, Subst.empty)
;;

let rec infer_pattern env ?ty =
  let names = [] in
  function
  | Ppat_var v ->
    let* fv = fresh_var in
    let schema =
      match ty with
      | Some t -> generalize env t
      | None -> Scheme (TVarSet.empty, fv)
    in
    let env = TypeEnv.extend env v schema in
    return (fv, env, v :: names)
  | Ppat_constant c ->
    let* ty, _ = infer_const c in
    return (ty, env, [])
  | Ppat_tuple [] | Ppat_tuple [ _ ] ->
    fail (SomeError "Pattern tuple must contain greather or equal two elements")
  | Ppat_tuple (first :: second :: pats) ->
    let* fv1, env1, names1 = infer_pattern env first in
    let* fv2, env2, names2 = infer_pattern env1 second in
    let infer_pats_acc acc pat =
      let fvs, env, names = acc in
      let* fv, env, new_names = infer_pattern env pat in
      return (fv :: fvs, env, names @ new_names)
    in
    let* fvs, env, names =
      RList.fold_left ~init:(return ([], env2, [])) ~f:infer_pats_acc pats
    in
    (* not effective list concat but this is for mother of readability *)
    return (TTuple (fv1, fv2, List.rev fvs), env, names1 @ names2 @ names)
  | Ppat_construct (name, None) ->
    let* ty, _ = lookup_env env name in
    return (ty, env, [])
  | Ppat_construct (name, Some pat) ->
    let* ty, _ = lookup_env env name in
    (match ty with
     | TArrow (f, s) ->
       let* ty_pat, env, names = infer_pattern env pat in
       let* sub_un = Subst.unify f ty_pat in
       return (sub_un |- s, sub_un >- env, names)
     | _ -> fail (SomeError "Constructor don't accept arguments"))
  | Ppat_any ->
    let* fv = fresh_var in
    return (fv, env, names)
;;

let infer_non_rec_value_bindings infer_expr env vbs =
  (* Example: let homka = fun x y -> let z = x y in z + 2 *)
  let* env, sub, names =
    RList.fold_left
      ~f:(fun (env, sub, names) vb ->
        (* Env: {x: 'a, y: 'b} *)
        (* vb: {pat: z, expr: x y} *)
        (* So t0 will be 'c and sub0 will be '{a: 'b -> 'c} *)
        let* t0, sub0 = infer_expr env vb.pvb_expr in
        let* sub = Subst.compose sub0 sub in
        let env = TypeEnv.apply sub env in
        (* With type (x y): 'c we append new variable z with type 'c *)
        let* env, sub, new_names =
          match vb.pvb_pat with
          | Ppat_var v -> (TypeEnv.extend env v (generalize env t0), sub, [ v ]) |> return
          | _ ->
            let* t, env, new_names = infer_pattern env vb.pvb_pat in
            let* sub_un = Subst.unify t t0 in
            let* sub = Subst.compose_all [ sub_un; sub ] in
            return (TypeEnv.apply sub env, sub, new_names)
        in
        let repeated_name = List.find_opt (fun name -> List.mem name names) new_names in
        match repeated_name with
        | Some name -> fail (PatternNameTwice name)
        | None -> return (env, sub, List.append (List.rev new_names) names))
      ~init:(return (env, Subst.empty, []))
      vbs
  in
  DebugLog.Aux.non_rec_vb env sub;
  return (env, sub, names)
;;

let rec get_expr_names = function
  | Pexp_ident i -> [ i ]
  | Pexp_constant _ -> []
  | Pexp_apply (e0, es) ->
    List.fold_left (fun acc e -> get_expr_names e @ acc) [] (e0 :: es)
  | Pexp_constraint (e, _) -> get_expr_names e
  | Pexp_construct (e, None) -> [ e ]
  | Pexp_construct (e, Some c) -> e :: get_expr_names c
  | Pexp_fun (_, e) -> get_expr_names e
  | Pexp_function cs ->
    List.fold_left (fun acc case -> get_expr_names case.pc_rhs @ acc) [] cs
  | Pexp_ifthenelse (e0, e1, None) -> get_expr_names e0 @ get_expr_names e1
  | Pexp_ifthenelse (e0, e1, Some e2) ->
    get_expr_names e0 @ get_expr_names e1 @ get_expr_names e2
  | Pexp_let (_, _, e) -> get_expr_names e
  | Pexp_match (e, cs) -> get_expr_names e @ get_expr_names (Pexp_function cs)
  | Pexp_tuple es -> List.fold_left (fun acc e -> get_expr_names e @ acc) [] es
;;

let infer_rec_value_bindings infer_expr env vbs =
  let exprs, patterns = List.split @@ List.map (fun x -> x.pvb_expr, x.pvb_pat) vbs in
  (* New type variables to all names in patterns *)
  let* env', fvs, names =
    RList.fold_left
      patterns
      ~init:(return (env, [], []))
      ~f:(fun (env, fvs, names) pat ->
        let* fv, env, new_names = infer_pattern env pat in
        let repeated_name = List.find_opt (fun name -> List.mem name new_names) names in
        match repeated_name with
        | Some name -> fail (PatternNameTwice name)
        | None -> return (env, fv :: fvs, List.append (List.rev new_names) names))
  in
  (* We get types of e0, e1, ... en and additional type info about type of variables outside of ei expression for all i *)
  let* ts, subs = List.split <$> RList.map exprs ~f:(fun expr -> infer_expr env' expr) in
  (* Combine all information about variables *)
  let* sub = Subst.compose_all subs in
  (* Apply all gotten types to out new names *)
  let* env'' =
    RList.fold_left
      (List.combine (List.combine ts fvs) patterns)
      ~init:(return env)
      ~f:(fun env ((ty, fv), pat) ->
        match pat with
        | Ppat_var v ->
          let* sub_un = Subst.unify ty (sub |- fv) in
          let env =
            TypeEnv.extend (TypeEnv.apply sub env) v (generalize env (sub_un |- ty))
          in
          return env
        | _ ->
          SomeError "Only variables are allowed as left-hand side of `let rec`" |> fail)
  in
  DebugLog.Aux.rec_vb env' subs ts sub env'';
  let validate_expr expr =
    (* List.iter (Format.printf "%s ") (get_expr_names expr); *)
    (* List.iter (Format.printf "%s ") names; *)
    let has_no_free_occurence =
      List.for_all (fun name -> not (List.mem name names)) (get_expr_names expr)
    in
    let is_variable_fun_function =
      match expr with
      | Pexp_ident _ | Pexp_fun _ | Pexp_function _ -> true
      | _ -> false
    in
    has_no_free_occurence || is_variable_fun_function
  in
  let result_expr_check = List.for_all validate_expr exprs in
  if not result_expr_check
  then
    fail
      (SomeError "This kind of expression is not allowed as right-hand side of 'let rec'")
  else return (env'', sub, names)
;;

(* https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_W *)
let infer_expr =
  let rec helper : TypeEnv.t -> expression -> (typ * Subst.t) t =
    fun env expr ->
    (* if debug then Format.printf "Infer expression: \n\t%s\n" (show_expression expr); *)
    match expr with
    | Pexp_constant c -> infer_const c
    | Pexp_ident id -> lookup_env env id
    | Pexp_fun (pattern, expr) ->
      let* t, env', _ = infer_pattern env pattern in
      let* t', sub = helper env' expr in
      return (sub |- t @-> t', sub)
      (* Constraint type inference by Homka122 😼😼😼 *)
    | Pexp_constraint (expr, ty) ->
      let* t, sub = helper env expr in
      let* sub0 = Subst.unify t ty in
      let* sub = Subst.compose sub0 sub in
      return (t, sub)
    (* Recursive apply type inference by Homka122 😼😼😼 *)
    | Pexp_apply (e0, es) ->
      DebugLog.Expr.apply env e0;
      let rec helper_apply init = function
        | [] -> return init
        | e1 :: tl ->
          DebugLog.Expr.apply_helper_1 e1;
          let* t' = fresh_var in
          let t0, sub0 = init in
          let* t1, sub1 = helper (sub0 >- env) e1 in
          let* sub2 = Subst.unify (sub1 |- t0) (t1 @-> t') in
          let* sub3 = Subst.compose_all [ sub0; sub1; sub2 ] in
          DebugLog.Expr.apply_helper_2 t0 sub0 t1 sub1 sub2 sub3;
          helper_apply (sub2 |- t', sub3) tl
      in
      let* init = helper env e0 in
      helper_apply init es
    (* Recursive ifthenelse type inference with option else by Homka122 😼😼😼 *)
    | Pexp_ifthenelse (e0, e1, None) ->
      let* t0, sub0 = helper env e0 in
      let* t1, sub1 = helper env e1 in
      let* sub_bool = Subst.unify t0 (TBase BBool) in
      let* sub = Subst.compose_all [ sub_bool; sub1; sub0 ] in
      return (sub |- t1, sub)
    | Pexp_ifthenelse (e0, e1, Some e2) ->
      let* t0, sub0 = helper env e0 in
      let* t1, sub1 = helper env e1 in
      let* t2, sub2 = helper env e2 in
      let* sub_bool = Subst.unify t0 (TBase BBool) in
      let* sub_eq = Subst.unify t1 t2 in
      let* sub = Subst.compose_all [ sub_bool; sub_eq; sub2; sub1; sub0 ] in
      return (sub |- t2, sub)
    (* let x0 = e0 and x1 = e1 and ... xn = en in e_f *)
    (* each xN = eN generate type tN of xN, type kN of eN, substitution S0 and envN with xN: tN *)
    (* So I think i can just generate substitution with unify tN and kN *)
    (* i want to die after three hours of attempting implemented this 😿😿😿 *)
    (* Recursive multiple let definitions type inference by Homka122 😼😼😼 (it took 4 hours) *)
    | Pexp_let (NonRecursive, vb, e1) as let_expr ->
      let* env, sub0, _ = infer_non_rec_value_bindings helper env vb in
      let* t, sub1 = helper (sub0 >- env) e1 in
      let* sub = Subst.compose sub1 sub0 in
      DebugLog.Expr.non_rec_let let_expr t sub1 sub;
      return (t, sub)
      (* https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Typing_rule *)
    | Pexp_let (Recursive, vbs, e1) ->
      let* env, sub0, _ = infer_rec_value_bindings helper env vbs in
      let* t, sub1 = helper env e1 in
      let* sub = Subst.compose sub0 sub1 in
      return (t, sub)
    | Pexp_tuple [] | Pexp_tuple [ _ ] ->
      fail (SomeError "Tuple expression must contain two or more expressions")
    | Pexp_tuple (e0 :: e1 :: exps) ->
      let* t0, sub0 = helper env e0 in
      let* t1, sub1 = helper env e1 in
      let* ts, subs = List.split <$> RList.map exps ~f:(fun e -> helper env e) in
      let* sub = Subst.compose_all (sub0 :: sub1 :: subs) in
      return (TTuple (t0, t1, ts), sub)
    | Pexp_construct (name, expr) ->
      let list =
        match expr with
        | None -> []
        | Some x -> [ x ]
      in
      let* ty, sub = helper env (Pexp_apply (Pexp_ident name, list)) in
      return (ty, sub)
    | Pexp_match (e, cases) ->
      let* t0, sub0 = helper env e in
      let env = sub0 >- env in
      let* fv = fresh_var in
      RList.fold_left
        cases
        ~init:(return (fv, sub0))
        ~f:(fun (ty, sub) case ->
          let* t_pat, env_pat, names = infer_pattern env case.pc_lhs in
          let* sub_un_pat = Subst.unify t_pat t0 in
          let* sub1 = Subst.compose sub_un_pat sub in
          let env_pat =
            List.fold_left
              (fun env name ->
                let (Scheme (_, t)) = TypeEnv.find_exn env name in
                let env = TypeEnv.remove env name in
                TypeEnv.extend env name (generalize env t))
              (sub_un_pat >- env_pat)
              names
          in
          let* ty_expr, sub_expr = helper (sub1 >- env_pat) case.pc_rhs in
          let* sub_un_exprs = Subst.unify ty_expr ty in
          let* sub = Subst.compose_all [ sub_un_exprs; sub_expr; sub1 ] in
          DebugLog.Expr.match_expr env_pat ty_expr t_pat sub_expr sub_un_exprs sub;
          return (sub |- ty, sub))
    | Pexp_function cases ->
      let* fv_match = fresh_var in
      let* fv_result = fresh_var in
      let* ty, sub =
        RList.fold_left
          cases
          ~init:(return (fv_result, Subst.empty))
          ~f:(fun (ty, sub) case ->
            let* t_pat, env_pat, _ = infer_pattern env case.pc_lhs in
            let* sub_un_pat = Subst.unify t_pat fv_match in
            let* sub1 = Subst.compose sub_un_pat sub in
            let* ty_expr, sub_expr = helper (sub1 >- env_pat) case.pc_rhs in
            let* sub_un_exprs = Subst.unify ty ty_expr in
            let* sub = Subst.compose_all [ sub_un_exprs; sub_expr; sub1 ] in
            return (sub |- ty, sub))
      in
      return (sub |- (sub |- fv_match) @-> ty, sub)
  in
  helper
;;

let infer_structure =
  let helper env = function
    | Pstr_eval expr ->
      let* _, _ = infer_expr env expr in
      return (env, [])
    | Pstr_value (NonRecursive, vbs) ->
      let* env, _, names = infer_non_rec_value_bindings infer_expr env vbs in
      return (env, List.rev names)
    | Pstr_value (Recursive, vbs) ->
      let* env, _, names = infer_rec_value_bindings infer_expr env vbs in
      return (env, names)
  in
  helper
;;

let infer_program env program =
  RList.fold_left
    program
    ~init:(return (env, []))
    ~f:(fun (env, names) structure ->
      let* new_env, new_names = infer_structure env structure in
      return (new_env, names @ new_names))
;;

let defaultEnv =
  List.fold_left
    (fun env (names, typ) ->
      List.fold_left
        (fun env_in name -> TypeEnv.extend env_in name (generalize env_in typ))
        env
        names)
    TypeEnv.empty
    TypeEnv.operators
;;

let run_expr_inferencer expr =
  Result.map fst (run (infer_expr defaultEnv expr) (List.length TypeEnv.operators))
;;

let run_structure_inferencer ?(env = defaultEnv) structure =
  run (infer_structure env structure) (List.length TypeEnv.operators)
;;

let run_structure_inferencer_exn ?(env = defaultEnv) structure =
  match run (infer_structure env structure) (List.length TypeEnv.operators) with
  | Error e -> Error (Format.asprintf "%a" Infer_print.pp_error_my e)
  | Ok x -> Ok x
;;

let run_program_inferencer ?(env = defaultEnv) program =
  run (infer_program env program) (List.length TypeEnv.operators)
;;

let run_program_inferencer_exn ?(env = defaultEnv) program =
  match run (infer_program env program) (List.length TypeEnv.operators) with
  | Error e -> Error (Format.asprintf "%a" Infer_print.pp_error_my e)
  | Ok x -> Ok x
;;
