(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.TypeExpr
open InferTypes

module MInfer = struct
  open Base

  type 'a t = int -> int * ('a, InferTypes.error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
  ;;

  let fail e st = st, Result.fail e
  let return x last = last, Result.return x
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

    let fold_left2 xs xl ~init ~f =
      Base.List.fold2
        ~f:(fun acc x l ->
          let open Syntax in
          let* acc = acc in
          f acc x l)
        ~init
        xs
        xl
    ;;

    let fold_right xs ~init ~f =
      Base.List.fold_right xs ~init ~f:(fun x acc ->
        let open Syntax in
        let* acc = acc in
        f x acc)
    ;;
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

module Type = struct
  type t = Ast.TypeExpr.t

  let rec occurs_check tvar = function
    | Type_var binder -> binder = tvar
    | Type_arrow (l, r) -> occurs_check tvar l || occurs_check tvar r
    | Type_tuple (t1, t2, t) ->
      List.fold_left (fun acc h -> acc || occurs_check tvar h) false (t1 :: t2 :: t)
    | Type_construct (_, ty) ->
      List.fold_left (fun acc h -> acc || occurs_check tvar h) false ty
  ;;

  let free_vars =
    let rec helper acc = function
      | Type_var binder -> VarSet.add binder acc
      | Type_arrow (l, r) -> helper (helper acc l) r
      | Type_tuple (t1, t2, t) ->
        List.fold_left (fun acc h -> helper acc h) acc (t1 :: t2 :: t)
      | Type_construct (_, ty) -> List.fold_left (fun acc h -> helper acc h) acc ty
    in
    helper VarSet.empty
  ;;
end

module Substitution = struct
  open MInfer
  open MInfer.Syntax
  open Base

  type t = (string, Type.t, Base.String.comparator_witness) Base.Map.t

  let empty = Map.empty (module Base.String)

  let singleton k v =
    match k, v with
    | a, Type_var b when String.equal a b -> return (Base.Map.empty (module Base.String))
    | _ ->
      if Type.occurs_check k v
      then fail (Occurs_check (k, v))
      else return (Base.Map.singleton (module Base.String) k v)
  ;;

  let remove = Map.remove

  let apply sub =
    let rec helper = function
      | Type_var b as typ ->
        (match Map.find sub b with
         | Some b -> b
         | None -> typ)
      | Type_arrow (l, r) -> Type_arrow (helper l, helper r)
      | Type_tuple (t1, t2, t) -> Type_tuple (helper t1, helper t2, List.map t ~f:helper)
      | Type_construct (id, ty) -> Type_construct (id, List.map ty ~f:helper)
    in
    helper
  ;;

  let fold mp init f =
    Map.fold mp ~init ~f:(fun ~key:k ~data:vm acc ->
      let* acc = acc in
      f k vm acc)
  ;;

  let rec unify l r =
    match l, r with
    | Type_var a, Type_var b when String.equal a b -> return empty
    | Type_var b, t | t, Type_var b -> singleton b t
    | Type_arrow (l1, r1), Type_arrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | Type_tuple (l11, l12, l1), Type_tuple (l21, l22, l2) ->
      (match
         Base.List.fold2
           (l11 :: l12 :: l1)
           (l21 :: l22 :: l2)
           ~init:(return empty)
           ~f:(fun acc t1 t2 ->
             let* sub1 = acc in
             let* sub2 = unify (apply sub1 t1) (apply sub1 t2) in
             compose sub1 sub2)
       with
       | Ok sub -> sub
       | _ -> fail (Unification_failed (l, r)))
    | Type_construct (id1, ty1), Type_construct (id2, ty2) when String.equal id1 id2 ->
      let* subs =
        match
          Base.List.fold2 ty1 ty2 ~init:(return empty) ~f:(fun acc t1 t2 ->
            let* sub1 = acc in
            let* sub2 = unify (apply sub1 t1) (apply sub1 t2) in
            compose sub1 sub2)
        with
        | Ok sub -> sub
        | _ -> fail (Unification_failed (l, r))
      in
      return subs
    | _ -> fail (Unification_failed (l, r))

  and extend k v s =
    match Map.find s k with
    | None ->
      let v = apply s v in
      let* s2 = singleton k v in
      fold s (return s2) (fun k v acc ->
        let* acc = return acc in
        let v = apply s2 v in
        return (Map.update acc k ~f:(fun _ -> v)))
    | Some v2 ->
      let* s2 = unify v v2 in
      compose s s2

  and compose s1 s2 = fold s2 (return s1) extend
  and compose_all ss = RList.fold_left ss ~init:(return empty) ~f:compose
end

module Scheme = struct
  type t = scheme

  let free_vars = function
    | Forall (bs, t) -> VarSet.diff (Type.free_vars t) bs
  ;;

  let apply subst (Forall (binder_set, typ)) =
    let s2 = VarSet.fold (fun k s -> Substitution.remove s k) binder_set subst in
    Forall (binder_set, Substitution.apply s2 typ)
  ;;

  let pp_scheme fmt = function
    | Forall (st, typ) ->
      if VarSet.is_empty st
      then
        Format.fprintf
          fmt
          "%a"
          (pprint_type ~poly_names_map:(Base.Map.empty (module Base.String)))
          typ
      else
        Format.fprintf
          fmt
          "%a. %a"
          VarSet.pp
          st
          (pprint_type ~poly_names_map:(Base.Map.empty (module Base.String)))
          typ
  ;;
end

module TypeEnv = struct
  open Base

  type t = (string, scheme, String.comparator_witness) Map.t

  let extend env name scheme = Map.set env ~key:name ~data:scheme
  let empty = Map.empty (module String)
  let fold f init mp = Map.fold mp ~init ~f:(fun ~key:k ~data:v acc -> f k v acc)

  let free_vars : t -> VarSet.t =
    fold (fun _ s acc -> VarSet.union acc (Scheme.free_vars s)) VarSet.empty
  ;;

  let apply s env = Map.map env ~f:(Scheme.apply s)
  let find name xs = Map.find xs name
  let find_exn name xs = Map.find_exn xs name
  let remove sub k = Base.Map.remove sub k

  let pp_env fmt environment =
    Map.iteri environment ~f:(fun ~key ~data ->
      Stdlib.Format.fprintf fmt "%S: %a\n" key Scheme.pp_scheme data)
  ;;
end

open MInfer
open MInfer.Syntax

let fresh_var = fresh >>| fun n -> Type_var (Int.to_string n)

let instantiate : scheme -> Ast.TypeExpr.t MInfer.t =
  fun (Forall (bs, t)) ->
  VarSet.fold
    (fun name typ ->
      let* typ = typ in
      let* f1 = fresh_var in
      let* s = Substitution.singleton name f1 in
      return (Substitution.apply s typ))
    bs
    (return t)
;;

let generalize : TypeEnv.t -> Type.t -> Scheme.t =
  fun env ty ->
  let free = VarSet.diff (Type.free_vars ty) (TypeEnv.free_vars env) in
  Forall (free, ty)
;;

open Ast.Constant
open Ast.Expression
open Ast.Pattern

let rec infer_pat ~debug pat env =
  match pat with
  | Pat_any ->
    let* fresh = fresh_var in
    return (env, fresh)
  | Pat_var ident ->
    let* fresh = fresh_var in
    let new_env = TypeEnv.extend env ident (Forall (VarSet.empty, fresh)) in
    return (new_env, fresh)
  | Pat_constant const ->
    (match const with
     | Const_char _ -> return (env, Type_construct ("char", []))
     | Const_integer _ -> return (env, Type_construct ("int", []))
     | Const_string _ -> return (env, Type_construct ("string", [])))
  | Pat_tuple (pat1, pat2, rest) ->
    let* env1, typ1 = infer_pat ~debug pat1 env in
    let* env2, typ2 = infer_pat ~debug pat2 env1 in
    let* env3, typ3 =
      RList.fold_right
        ~f:(fun pat acc ->
          let* env_acc, typ_list = return acc in
          let* env, typ = infer_pat ~debug pat env_acc in
          return (env, typ :: typ_list))
        ~init:(return (env2, []))
        rest
    in
    return (env3, Type_tuple (typ1, typ2, typ3))
  | Pat_construct (name, pat) ->
    (match TypeEnv.find name env with
     | None -> fail (Unbound_variable name)
     | Some (Forall (x, Type_arrow (arg, adt))) ->
       let* typ = instantiate (Forall (x, Type_arrow (arg, adt))) in
       (match pat with
        | Some const_pat ->
          let* patenv, typepat = infer_pat ~debug const_pat env in
          let* uni_sub = Substitution.unify arg typepat in
          let new_env = TypeEnv.apply uni_sub patenv in
          return (new_env, Substitution.apply uni_sub adt)
        | None -> return (env, typ))
     | Some el ->
       let* typ = instantiate el in
       return (env, typ))
  | Pat_constraint (pat, typ) ->
    let* pat_env, pat_typ = infer_pat ~debug pat env in
    let* uni_sub = Substitution.unify pat_typ typ in
    let new_env = TypeEnv.apply uni_sub pat_env in
    return (new_env, Substitution.apply uni_sub pat_typ)
;;

let rec extend_helper env pat (Forall (binder_set, typ) as scheme) =
  match pat, typ with
  | Pat_var name, _ -> TypeEnv.extend env name scheme
  | Pat_tuple (p1, p2, prest), Type_tuple (t1, t2, trest) ->
    let new_env =
      Base.List.fold2
        ~init:env
        ~f:(fun env pat typ -> extend_helper env pat (Forall (binder_set, typ)))
        (p1 :: p2 :: prest)
        (t1 :: t2 :: trest)
    in
    (match new_env with
     | Ok new_env -> new_env
     | _ -> env)
  | _ -> env
;;

let add_names_rec env vb_list =
  RList.fold_right
    ~f:(fun vb acc ->
      match vb with
      | { pat = Pat_var name; _ } | { pat = Pat_constraint (Pat_var name, _); _ } ->
        let* env_acc, fresh_acc = return acc in
        let* fresh = fresh_var in
        let env_acc = TypeEnv.extend env_acc name (Forall (VarSet.empty, fresh)) in
        return (env_acc, fresh :: fresh_acc)
      | _ -> fail Wrong_rec)
    vb_list
    ~init:(return (env, []))
;;

let infer_rest_vb ~debug env_acc sub_acc sub typ pat =
  let* comp_sub = Substitution.compose sub_acc sub in
  let new_env = TypeEnv.apply comp_sub env_acc in
  let new_scheme = generalize new_env (Substitution.apply comp_sub typ) in
  let* pat_env, pat_typ = infer_pat ~debug pat new_env in
  let new_env = extend_helper pat_env pat new_scheme in
  let* uni_sub = Substitution.unify typ pat_typ in
  let* res_sub = Substitution.compose comp_sub uni_sub in
  let res_env = TypeEnv.apply res_sub new_env in
  return (res_env, res_sub)
;;

let infer_rec_rest_vb sub_acc env_acc fresh typ name new_sub =
  let* uni_sub = Substitution.unify (Substitution.apply new_sub fresh) typ in
  let* comp_sub = Substitution.compose_all [ new_sub; uni_sub; sub_acc ] in
  let env_acc = TypeEnv.apply comp_sub env_acc in
  let env_rm = TypeEnv.remove env_acc name in
  let new_scheme = generalize env_rm (Substitution.apply comp_sub fresh) in
  let env_acc = TypeEnv.extend env_acc name new_scheme in
  return (env_acc, comp_sub)
;;

let rec get_pat_names acc pat =
  match pat with
  | Pat_var id -> id :: acc
  | Pat_tuple (pat1, pat2, rest) ->
    Base.List.fold_left ~f:get_pat_names ~init:acc (pat1 :: pat2 :: rest)
  | Pat_construct ("Some", Some pat) -> get_pat_names acc pat
  | Pat_constraint (pat, _) -> get_pat_names acc pat
  | _ -> acc
;;

let rec infer_exp ~debug exp env =
  match exp with
  | Exp_ident varname ->
    (match TypeEnv.find varname env with
     | None -> fail (Unbound_variable varname)
     | Some x ->
       let* typ = instantiate x in
       return (Substitution.empty, typ))
  | Exp_constant const ->
    (match const with
     | Const_char _ -> return (Substitution.empty, Type_construct ("char", []))
     | Const_integer _ -> return (Substitution.empty, Type_construct ("int", []))
     | Const_string _ -> return (Substitution.empty, Type_construct ("string", [])))
  | Exp_apply (Exp_ident op, Exp_tuple (exp1, exp2, [])) ->
    (match op with
     | "*" | "/" | "+" | "-" | "<" | ">" | "=" | "<>" | "<=" | ">=" | "&&" | "||" ->
       let* sub1, typ1 = infer_exp ~debug exp1 env in
       let* sub2, typ2 = infer_exp ~debug exp2 (TypeEnv.apply sub1 env) in
       let* arg_typ, res_typ =
         match TypeEnv.find op env with
         | Some (Forall (_, Type_arrow (Type_arrow (arg, _), res))) -> return (arg, res)
         | _ -> fail @@ Unsupported_operator op
       in
       let* unif_sub1 = Substitution.unify (Substitution.apply sub2 typ1) arg_typ in
       let* unif_sub2 = Substitution.unify (Substitution.apply unif_sub1 typ2) arg_typ in
       let* comp_sub = Substitution.compose_all [ sub1; sub2; unif_sub1; unif_sub2 ] in
       return (comp_sub, res_typ)
     | _ ->
       let* sub1, typ1 = infer_exp ~debug (Exp_ident op) env in
       let* sub2, typ2 =
         infer_exp ~debug (Exp_tuple (exp1, exp2, [])) (TypeEnv.apply sub1 env)
       in
       let* fresh = fresh_var in
       let* unif_sub =
         Substitution.unify (Substitution.apply sub2 typ1) (Type_arrow (typ2, fresh))
       in
       let* comp_sub = Substitution.compose_all [ unif_sub; sub2; sub1 ] in
       let res_typ = Substitution.apply comp_sub fresh in
       return (comp_sub, res_typ))
  | Exp_apply (exp1, exp2) ->
    (match exp1 with
     | Exp_ident op when op = "+" || op = "-" ->
       let* sub1, typ1 = infer_exp ~debug exp2 env in
       let* unif_sub = Substitution.unify typ1 (Type_construct ("int", [])) in
       let* comp_sub = Substitution.compose sub1 unif_sub in
       return (comp_sub, Type_construct ("int", []))
     | _ ->
       let* sub1, typ1 = infer_exp ~debug exp1 env in
       let* sub2, typ2 = infer_exp ~debug exp2 (TypeEnv.apply sub1 env) in
       let* fresh = fresh_var in
       let* unif_sub =
         Substitution.unify (Substitution.apply sub2 typ1) (Type_arrow (typ2, fresh))
       in
       let* comp_sub = Substitution.compose_all [ unif_sub; sub2; sub1 ] in
       let res_typ = Substitution.apply comp_sub fresh in
       return (comp_sub, res_typ))
  | Exp_fun ((pattern, patterns), expr) ->
    let* new_env, typ1 = infer_pat ~debug pattern env in
    let* sub1, typ2 =
      match patterns with
      | hd :: tl -> infer_exp ~debug (Exp_fun ((hd, tl), expr)) new_env
      | [] -> infer_exp ~debug expr new_env
    in
    return (sub1, Type_arrow (Substitution.apply sub1 typ1, typ2))
  | Exp_construct (name, Some expr) ->
    let* ty, sub = infer_exp ~debug (Exp_apply (Exp_ident name, expr)) env in
    return (ty, sub)
  | Exp_construct (name, None) ->
    let* ty, sub = infer_exp ~debug (Exp_ident name) env in
    return (ty, sub)
  | Exp_tuple (exp1, exp2, rest) ->
    let* sub1, typ1 = infer_exp ~debug exp1 env in
    let new_env = TypeEnv.apply sub1 env in
    let* sub2, typ2 = infer_exp ~debug exp2 new_env in
    let new_env = TypeEnv.apply sub2 new_env in
    let* sub3, typ3 =
      RList.fold_right
        ~f:(fun exp acc ->
          let* sub_acc, typ_list = return acc in
          let new_env = TypeEnv.apply sub_acc new_env in
          let* sub, typ = infer_exp ~debug exp new_env in
          let* sub_acc = Substitution.compose sub_acc sub in
          return (sub_acc, typ :: typ_list))
        ~init:(return (Substitution.empty, []))
        rest
    in
    let* fin_sub = Substitution.compose_all [ sub1; sub2; sub3 ] in
    let typ1 = Substitution.apply fin_sub typ1 in
    let typ2 = Substitution.apply fin_sub typ2 in
    let typ3 = List.map (fun typ -> Substitution.apply fin_sub typ) typ3 in
    return (fin_sub, Type_tuple (typ1, typ2, typ3))
  | Exp_if (ifexp, thenexp, Some elseexp) ->
    let* sub1, typ1 = infer_exp ~debug ifexp env in
    let* uni_sub1 = Substitution.unify typ1 (Type_construct ("bool", [])) in
    let* sub2, typ2 = infer_exp ~debug thenexp env in
    let* sub3, typ3 = infer_exp ~debug elseexp env in
    let* uni_sub2 = Substitution.unify typ2 typ3 in
    let* comp_sub = Substitution.compose_all [ sub1; uni_sub1; sub2; sub3; uni_sub2 ] in
    return (comp_sub, typ3)
  | Exp_if (ifexp, thenexp, None) ->
    let* sub1, typ1 = infer_exp ~debug ifexp env in
    let* uni_sub1 = Substitution.unify typ1 (Type_construct ("bool", [])) in
    let* sub2, typ2 = infer_exp ~debug thenexp env in
    let* comp_sub = Substitution.compose_all [ sub1; uni_sub1; sub2 ] in
    return (comp_sub, typ2)
  | Exp_match (expr, (case, rest)) ->
    let* subexpr, typexpr = infer_exp ~debug expr env in
    let new_env = TypeEnv.apply subexpr env in
    let* fresh = fresh_var in
    let* res_sub, res_typ =
      RList.fold_left
        (case :: rest)
        ~init:(return (subexpr, fresh))
        ~f:(fun acc case ->
          let* sub, typ = return acc in
          let pat_names = get_pat_names [] case.first in
          let* pat_env, pat_typ = infer_pat ~debug case.first new_env in
          let* uni_sub = Substitution.unify pat_typ typexpr in
          let* comp_sub = Substitution.compose sub uni_sub in
          let pat_env =
            Base.List.fold_left
              ~f:(fun env name ->
                let (Forall (_, typ)) = TypeEnv.find_exn name env in
                let env = TypeEnv.remove env name in
                TypeEnv.extend env name (generalize env typ))
              ~init:(TypeEnv.apply uni_sub pat_env)
              pat_names
          in
          let* subexpr, typexpr =
            infer_exp ~debug case.second (TypeEnv.apply comp_sub pat_env)
          in
          let* uni_sub2 = Substitution.unify typexpr typ in
          let* res_sub = Substitution.compose_all [ uni_sub2; subexpr; comp_sub ] in
          return (res_sub, Substitution.apply res_sub typ))
    in
    return (res_sub, res_typ)
  | Exp_function (case, rest) ->
    let* fresh1 = fresh_var in
    let* fresh2 = fresh_var in
    let* res_sub, res_typ =
      RList.fold_left
        (case :: rest)
        ~init:(return (Substitution.empty, fresh2))
        ~f:(fun acc case ->
          let* sub, typ = return acc in
          let* pat_env, pat_typ = infer_pat ~debug case.first env in
          let* uni_sub1 = Substitution.unify pat_typ fresh1 in
          let* sub1 = Substitution.compose uni_sub1 sub in
          let new_env = TypeEnv.apply sub1 pat_env in
          let* subexpr, typexpr = infer_exp ~debug case.second new_env in
          let* uni_sub2 = Substitution.unify typ typexpr in
          let* comp_sub = Substitution.compose_all [ uni_sub2; subexpr; sub1 ] in
          return (comp_sub, Substitution.apply comp_sub typ))
    in
    return (res_sub, Type_arrow (Substitution.apply res_sub fresh1, res_typ))
  | Exp_let (Nonrecursive, (value_binding, rest), exp) ->
    let* new_env, sub, _ =
      infer_value_binding_list ~debug (value_binding :: rest) env Substitution.empty
    in
    let* subb, typp = infer_exp ~debug exp new_env in
    let* comp_sub = Substitution.compose sub subb in
    return (comp_sub, typp)
  | Exp_let (Recursive, (value_binding, rest), exp) ->
    let* new_env, fresh_vars = add_names_rec env (value_binding :: rest) in
    let* new_env, sub, _ =
      infer_rec_value_binding_list
        ~debug
        (value_binding :: rest)
        new_env
        Substitution.empty
        fresh_vars
    in
    let* subb, typp = infer_exp ~debug exp new_env in
    let* comp_sub = Substitution.compose subb sub in
    return (comp_sub, typp)
  | Exp_constraint (expr, typ) ->
    let* sub, typ1 = infer_exp ~debug expr env in
    let* uni_sub = Substitution.unify typ1 typ in
    let* comp_sub = Substitution.compose sub uni_sub in
    return (comp_sub, typ1)

and infer_value_binding_list ~debug vb_list env sub =
  let* res_env, res_sub, names =
    RList.fold_left
      vb_list
      ~init:(return (env, sub, []))
      ~f:(fun acc vb ->
        let* env_acc, sub_acc, names = return acc in
        match vb with
        | { pat = Pat_constraint (pat, pat_typ); expr = Exp_fun ((fpat, fpatrest), exp) }
          ->
          let* sub, typ =
            infer_exp
              ~debug
              (Exp_fun ((fpat, fpatrest), Exp_constraint (exp, pat_typ)))
              env_acc
          in
          let* res_env, res_sub = infer_rest_vb ~debug env_acc sub_acc sub typ pat in
          let name = get_pat_names names pat in
          return (res_env, res_sub, names @ name)
        | { pat = Pat_constraint (pat, pat_typ); expr = Exp_function _ as exp } ->
          let* sub, typ = infer_exp ~debug (Exp_constraint (exp, pat_typ)) env_acc in
          let* res_env, res_sub = infer_rest_vb ~debug env_acc sub_acc sub typ pat in
          let name = get_pat_names names pat in
          return (res_env, res_sub, names @ name)
        | { pat; expr } ->
          let* sub, typ = infer_exp ~debug expr env_acc in
          let* res_env, res_sub = infer_rest_vb ~debug env_acc sub_acc sub typ pat in
          let name = get_pat_names names pat in
          return (res_env, res_sub, names @ name))
  in
  return (res_env, res_sub, names)

and infer_rec_value_binding_list ~debug vb_list env sub fresh_vars =
  let* res_env, res_sub, names =
    match
      RList.fold_left2
        vb_list
        fresh_vars
        ~init:(return (env, sub, []))
        ~f:(fun acc vb fv ->
          let* env_acc, sub_acc, names = return acc in
          match vb, fv with
          | ( ( { pat = Pat_var name; expr = Exp_fun _ as exp }
              | { pat = Pat_var name; expr = Exp_function _ as exp } )
            , fresh ) ->
            let* subexpr, typexpr = infer_exp ~debug exp env_acc in
            let* res_env, res_sub =
              infer_rec_rest_vb sub_acc env_acc fresh typexpr name subexpr
            in
            return (res_env, res_sub, names @ [ name ])
          | ( { pat = Pat_constraint (Pat_var name, pat_typ)
              ; expr = Exp_fun ((pat, pat_list), expr)
              }
            , fresh ) ->
            let* subexpr, typexpr =
              infer_exp
                ~debug
                (Exp_fun ((pat, pat_list), Exp_constraint (expr, pat_typ)))
                env
            in
            let* res_env, res_sub =
              infer_rec_rest_vb sub_acc env_acc fresh typexpr name subexpr
            in
            return (res_env, res_sub, names @ [ name ])
          | { pat = Pat_var name; expr }, fresh ->
            let* subexpr, typexpr = infer_exp ~debug expr env_acc in
            (match typexpr with
             | Type_arrow (_, _) ->
               let new_fresh = Substitution.apply sub_acc fresh in
               if typexpr = new_fresh
               then fail Wrong_rec
               else
                 let* res_env, res_sub =
                   infer_rec_rest_vb sub_acc env_acc fresh typexpr name subexpr
                 in
                 return (res_env, res_sub, names @ [ name ])
             | _ -> fail Wrong_rec)
          | _ -> fail Wrong_rec)
    with
    | Ok result -> result
    | Unequal_lengths -> fail Incorrect_list_lengths
  in
  return (res_env, res_sub, names)
;;

open Ast.Structure

let rec check_poly_types ~debug typ_list marity = function
  | Type_var var when Base.List.mem typ_list var ~equal:String.equal -> return ()
  | Type_var name -> fail (Unbound_variable name)
  | Type_construct (name, args) ->
    let* arity =
      Base.Map.find marity name
      |> Base.Option.value_map ~f:return ~default:(fail (Undeclared_type name))
    in
    if arity = Base.List.length args
    then check_many ~debug typ_list marity args
    else fail Arity_mismatch
  | Type_arrow (l, r) ->
    let* () = check_poly_types ~debug typ_list marity l in
    check_poly_types ~debug typ_list marity r
  | Type_tuple (t1, t2, rest) ->
    let* () = check_poly_types ~debug typ_list marity t1 in
    let* () = check_poly_types ~debug typ_list marity t2 in
    check_many ~debug typ_list marity rest

and check_many ~debug typ_list marity args =
  let rec iter = function
    | [] -> return ()
    | arg :: rest ->
      let* () = check_poly_types ~debug typ_list marity arg in
      iter rest
  in
  iter args
;;

let ( ! ) fresh = Type_var fresh

let infer_structure_item ~debug env item marity names =
  match item with
  | Str_eval exp ->
    let* _, typ = infer_exp ~debug exp env in
    let new_env = TypeEnv.extend env "-" (Forall (VarSet.empty, typ)) in
    return (new_env, marity, names @ [ "-" ])
  | Str_value (Nonrecursive, (value_binding, rest)) ->
    let* env, _, names =
      infer_value_binding_list ~debug (value_binding :: rest) env Substitution.empty
    in
    return (env, marity, names)
  | Str_value (Recursive, (value_binding, rest)) ->
    let* new_env, fresh_vars = add_names_rec env (value_binding :: rest) in
    let* new_env, _, names =
      infer_rec_value_binding_list
        ~debug
        (value_binding :: rest)
        new_env
        Substitution.empty
        fresh_vars
    in
    return (new_env, marity, names)
  | Str_adt (poly, name, (variant, rest)) ->
    let adt_type = Type_construct (name, Base.List.map poly ~f:( ! )) in
    let type_arity = List.length poly in
    let arity_map = Base.Map.set marity ~key:name ~data:type_arity in
    let* constrs =
      RList.fold_left
        (variant :: rest)
        ~init:(return env)
        ~f:(fun acc (constr_name, constr_types) ->
          let* env_acc = return acc in
          let* fresh = fresh in
          let* new_env =
            match constr_types with
            | None ->
              return
                (TypeEnv.extend
                   env_acc
                   constr_name
                   (Forall (VarSet.singleton (Int.to_string fresh), adt_type)))
            | Some typ ->
              let* () = check_poly_types ~debug poly arity_map typ in
              return
                (TypeEnv.extend
                   env_acc
                   constr_name
                   (Forall (VarSet.of_list poly, Type_arrow (typ, adt_type))))
          in
          return new_env)
    in
    return (constrs, arity_map, names)
;;

let infer_program ~debug program env =
  let marity = Base.Map.empty (module Base.String) in
  let marity = Base.Map.add_exn marity ~key:"int" ~data:0 in
  let marity = Base.Map.add_exn marity ~key:"char" ~data:0 in
  let marity = Base.Map.add_exn marity ~key:"string" ~data:0 in
  let marity = Base.Map.add_exn marity ~key:"bool" ~data:0 in
  let marity = Base.Map.add_exn marity ~key:"unit" ~data:0 in
  let* env, _, names =
    RList.fold_left
      program
      ~init:(return (env, marity, []))
      ~f:(fun acc item ->
        let* env_acc, arr_acc, names = return acc in
        let* env, arr, name = infer_structure_item ~debug env_acc item arr_acc names in
        return (env, arr, names @ name))
  in
  return (env, names)
;;

let env_with_things =
  let things_list =
    [ ( "+"
      , Forall
          ( VarSet.empty
          , Type_arrow
              ( Type_arrow (Type_construct ("int", []), Type_construct ("int", []))
              , Type_construct ("int", []) ) ) )
    ; ( "-"
      , Forall
          ( VarSet.empty
          , Type_arrow
              ( Type_arrow (Type_construct ("int", []), Type_construct ("int", []))
              , Type_construct ("int", []) ) ) )
    ; ( "*"
      , Forall
          ( VarSet.empty
          , Type_arrow
              ( Type_arrow (Type_construct ("int", []), Type_construct ("int", []))
              , Type_construct ("int", []) ) ) )
    ; ( "/"
      , Forall
          ( VarSet.empty
          , Type_arrow
              ( Type_arrow (Type_construct ("int", []), Type_construct ("int", []))
              , Type_construct ("int", []) ) ) )
    ; ( "<"
      , Forall
          ( VarSet.singleton "a"
          , Type_arrow
              (Type_arrow (Type_var "a", Type_var "a"), Type_construct ("bool", [])) ) )
    ; ( ">"
      , Forall
          ( VarSet.singleton "a"
          , Type_arrow
              (Type_arrow (Type_var "a", Type_var "a"), Type_construct ("bool", [])) ) )
    ; ( "<>"
      , Forall
          ( VarSet.singleton "a"
          , Type_arrow
              (Type_arrow (Type_var "a", Type_var "a"), Type_construct ("bool", [])) ) )
    ; ( "<="
      , Forall
          ( VarSet.singleton "a"
          , Type_arrow
              (Type_arrow (Type_var "a", Type_var "a"), Type_construct ("bool", [])) ) )
    ; ( ">="
      , Forall
          ( VarSet.singleton "a"
          , Type_arrow
              (Type_arrow (Type_var "a", Type_var "a"), Type_construct ("bool", [])) ) )
    ; ( "="
      , Forall
          ( VarSet.singleton "a"
          , Type_arrow
              (Type_arrow (Type_var "a", Type_var "a"), Type_construct ("bool", [])) ) )
    ; ( "||"
      , Forall
          ( VarSet.empty
          , Type_arrow
              ( Type_arrow (Type_construct ("bool", []), Type_construct ("bool", []))
              , Type_construct ("bool", []) ) ) )
    ; ( "&&"
      , Forall
          ( VarSet.empty
          , Type_arrow
              ( Type_arrow (Type_construct ("bool", []), Type_construct ("bool", []))
              , Type_construct ("bool", []) ) ) )
    ; ( "print_int"
      , Forall
          ( VarSet.empty
          , Type_arrow (Type_construct ("int", []), Type_construct ("unit", [])) ) )
    ; ( "print_endline"
      , Forall
          ( VarSet.empty
          , Type_arrow (Type_construct ("string", []), Type_construct ("unit", [])) ) )
    ; ( "print_char"
      , Forall
          ( VarSet.empty
          , Type_arrow (Type_construct ("char", []), Type_construct ("unit", [])) ) )
    ; ( "print_bool"
      , Forall
          ( VarSet.empty
          , Type_arrow (Type_construct ("bool", []), Type_construct ("unit", [])) ) )
    ; ( "Some"
      , Forall
          ( VarSet.singleton "a"
          , Type_arrow (Type_var "a", Type_construct ("option", [ Type_var "a" ])) ) )
    ; "None", Forall (VarSet.singleton "a", Type_construct ("option", [ Type_var "a" ]))
    ; ( "::"
      , Forall
          ( VarSet.singleton "a"
          , Type_arrow
              ( Type_tuple (Type_var "a", Type_construct ("list", [ Type_var "a" ]), [])
              , Type_construct ("list", [ Type_var "a" ]) ) ) )
    ; "[]", Forall (VarSet.singleton "a", Type_construct ("list", [ Type_var "a" ]))
    ; "()", Forall (VarSet.empty, Type_construct ("unit", []))
    ; "true", Forall (VarSet.empty, Type_construct ("bool", []))
    ; "false", Forall (VarSet.empty, Type_construct ("bool", []))
    ]
  in
  List.fold_left
    (fun env (id, sch) -> TypeEnv.extend env id sch)
    TypeEnv.empty
    things_list
;;

let run_infer_program ?(debug = false) (program : Ast.program) env =
  run (infer_program ~debug program env)
;;
