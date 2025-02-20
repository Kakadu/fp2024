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

type fresh_var = int

module Type = struct
  type t = Ast.TypeExpr.t

  let rec occurs_check tvar = function
    | Type_var binder -> binder = tvar
    | Type_arrow (l, r) -> occurs_check tvar l || occurs_check tvar r
    | Type_tuple (t1, t2, t) ->
      List.fold_left (fun acc h -> acc || occurs_check tvar h) false (t1 :: t2 :: t)
    | Type_construct (id, ty) ->
      List.fold_left (fun acc h -> acc || occurs_check tvar h) false ty (*maybe rework*)
  ;;

  let free_vars =
    let rec helper acc = function
      | Type_var binder -> VarSet.add binder acc
      | Type_arrow (l, r) -> helper (helper acc l) r
      | Type_tuple (t1, t2, t) ->
        List.fold_left (fun acc h -> helper acc h) acc (t1 :: t2 :: t)
      | Type_construct (id, ty) ->
        List.fold_left (fun acc h -> helper acc h) acc ty (*maybe rework*)
    in
    helper VarSet.empty
  ;;
end

module Substitution = struct
  open MInfer
  open MInfer.Syntax
  open Base

  (* type t = (binder, Ast.TypeExpr.t, Int.comparator_witness) Map.t *)

  let pp_sub ppf (sub : (string, Type.t, Base.String.comparator_witness) Base.Map.t) =
    Stdlib.Format.fprintf ppf "\nSubst:\n";
    Map.iteri sub ~f:(fun ~key:str ~data:ty ->
      Stdlib.Format.fprintf ppf "%s <-> %a @@ " str pprint_type ty);
    Stdlib.Format.fprintf ppf "\n"
  ;;

  let empty = Map.empty (module Base.String)
  let mapping k v = if Type.occurs_check k v then fail `Occurs_check else return (k, v)
  (*i need this?*)

  let singleton k vm =
    if Type.occurs_check k vm
    then fail (`Occurs_check (k, vm))
    else return (Base.Map.singleton (module Base.String) k vm)
  ;;

  (* let find_exn (map : t) (k : binder) : Ast.TypeExpr.t = Map.find_exn map k
     let find (map : t) (k : binder) : Ast.TypeExpr.t option = Map.find map k *)
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
      (*maybe rework*)
    in
    helper
  ;;

  let fold mp init f =
    Map.fold mp ~init ~f:(fun ~key:k ~data:vm acc ->
      let* acc = acc in
      f k vm acc)
  ;;

  let rec unify l r =
    (* let _ = Stdlib.Format.printf "UNIFY: %a ||||| %a\n" pprint_type l pprint_type r in *)
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
       | _ -> fail (`Unification_failed (l, r)))
    | Type_construct (id1, ty1), Type_construct (id2, ty2) when String.equal id1 id2 ->
      let* subs =
        match
          Base.List.fold2 ty1 ty2 ~init:(return empty) ~f:(fun acc t1 t2 ->
            let* sub1 = acc in
            let* sub2 = unify (apply sub1 t1) (apply sub1 t2) in
            compose sub1 sub2)
        with
        | Ok sub -> sub
        | _ -> fail (`Unification_failed (l, r))
      in
      return subs
    | _ -> fail (`Unification_failed (l, r))

  and extend k v s =
    (*maybe rework*)
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

  let occurs_check v = function
    | Forall (xs, t) -> (not (VarSet.mem v xs)) && Type.occurs_check v t
  ;;

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
      then Format.fprintf fmt "%a" pprint_type typ
      else Format.fprintf fmt "%a. %a" VarSet.pp st pprint_type typ
  ;;
end

module TypeEnv = struct
  open Base

  type t = (string, scheme, String.comparator_witness) Map.t

  let extend env name scheme =
    (* let () = print_endline name in *)
    Map.set env ~key:name ~data:scheme
  ;;

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
open Scheme

let rec infer_pat ~debug pat env =
  match pat with
  | Pat_any ->
    let* fresh = fresh_var in
    return (env, fresh)
  | Pat_var ident ->
    let* fresh = fresh_var in
    let new_env = TypeEnv.extend env ident (Forall (VarSet.empty, fresh)) in
    (* let _ = Stdlib.Format.printf "DEBUG: env in Pat_var:%a" TypeEnv.pp_env new_env in *)
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
  | Pat_construct ("()", None) -> return (env, Type_construct ("unit", []))
  | Pat_construct ("Some", Some pat) ->
    let* env, typ = infer_pat ~debug pat env in
    return (env, Type_construct ("option", [ typ ]))
  | Pat_construct (id, None) when id = "false" || id = "true" ->
    return (env, Type_construct ("bool", []))
  | Pat_construct (name, None) ->
    let* sub, typ =
      match TypeEnv.find name env with
      | Some el ->
        let* typ = instantiate el in
        return (Substitution.empty, typ)
      | None -> failwith "noname"
    in
    return (env, typ)
  | Pat_construct (name, Some pat) ->
    let* sub, typ =
      match TypeEnv.find name env with
      | Some el ->
        let* typ = instantiate el in
        return (Substitution.empty, typ)
      | None -> failwith "noname"
    in
    let* env, typ =
      match typ with
      | Type_arrow (arg, adt) ->
        let* patenv, typepat = infer_pat ~debug pat env in
        let* uni_sub = Substitution.unify arg typepat in
        let new_env = TypeEnv.apply uni_sub patenv in
        return (new_env, Substitution.apply uni_sub adt)
      | _ -> failwith "shit"
    in
    return (env, typ)
  | Pat_constraint (pat, typ) ->
    let* pat_env, pat_typ = infer_pat ~debug pat env in
    let* uni_sub = Substitution.unify pat_typ typ in
    let new_env = TypeEnv.apply uni_sub pat_env in
    return (new_env, Substitution.apply uni_sub pat_typ)
;;

(*remove?*)

(*remove?^^^*)

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
      | _ -> failwith "abobi")
    vb_list
    ~init:(return (env, []))
;;

let infer_rest_vb ~debug env_acc sub_acc sub typ pat =
  let* comp_sub = Substitution.compose sub_acc sub in
  (* if debug
     then Stdlib.Format.printf "DEBUG: comp_sub in vb_rest:%a\n" Substitution.pp_sub comp_sub; *)
  let new_env = TypeEnv.apply comp_sub env_acc in
  (* if debug
  then Stdlib.Format.printf "DEBUG: first env in vb_rest:{%a}\n" TypeEnv.pp_env new_env;
  let typ = Substitution.apply comp_sub typ in
  if debug
  then
    Stdlib.Format.printf
      "DEBUG: type of expr in vb_rest after comp_sub apply:%a\n"
      pprint_type 
      typ; *)
  let new_scheme = generalize new_env (Substitution.apply comp_sub typ) in
  let* pat_env, pat_typ = infer_pat ~debug pat new_env in
  let new_env = extend_helper pat_env pat new_scheme in
  let* uni_sub = Substitution.unify typ pat_typ in
  let* res_sub = Substitution.compose comp_sub uni_sub in
  let res_env = TypeEnv.apply res_sub new_env in
  return (res_env, res_sub)
;;

let infer_rec_rest_vb ~debug sub_acc env_acc fresh typ name new_sub =
  (*constraint there*)
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
     | None -> fail (`Unbound_variable varname)
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
         match op with
         | "*" | "/" | "+" | "-" ->
           return (Type_construct ("int", []), Type_construct ("int", []))
         | "<" | ">" | "=" | "<>" | "<=" | ">=" ->
           let* fresh = fresh_var in
           return (fresh, Type_construct ("bool", []))
         | "&&" | "||" -> return (Type_construct ("bool", []), Type_construct ("bool", []))
         | _ -> failwith "aboba"
       in
       let* unif_sub1 = Substitution.unify (Substitution.apply sub2 typ1) arg_typ in
       let* unif_sub2 = Substitution.unify (Substitution.apply unif_sub1 typ2) arg_typ in
       let* comp_sub = Substitution.compose_all [ sub1; sub2; unif_sub1; unif_sub2 ] in
       return (comp_sub, res_typ)
     | _ ->
       if debug then Stdlib.Format.printf "DEBUG: IN APPLY\n";
       let* sub1, typ1 = infer_exp ~debug (Exp_ident op) env in
       if debug then Stdlib.Format.printf "DEBUG: IN APPLY AFTER 1 exp\n";
       let* sub2, typ2 =
         infer_exp ~debug (Exp_tuple (exp1, exp2, [])) (TypeEnv.apply sub1 env)
       in
       if debug then Stdlib.Format.printf "DEBUG: IN APPLY AFTER 2 exp\n";
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
         if debug then Stdlib.Format.printf "DEBUG: &&&&&&&%a\n" pprint_type fresh;
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
  | Exp_construct ("()", None) -> return (Substitution.empty, Type_construct ("unit", []))
  | Exp_construct ("Some", Some expr) ->
    let* sub, typ = infer_exp ~debug expr env in
    return (sub, Type_construct ("option", [ typ ]))
  | Exp_construct (id, None) when id = "false" || id = "true" ->
    return (Substitution.empty, Type_construct ("bool", []))
  | Exp_construct (name, None) ->
      let* ty, sub = infer_exp ~debug (Exp_ident name) env in
      return (ty, sub)
  | Exp_construct (name, Some expr) ->
      let* ty, sub = infer_exp ~debug  (Exp_apply (Exp_ident name, expr)) env in
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
    let new_env = TypeEnv.apply uni_sub1 env in
    let* sub2, typ2 = infer_exp ~debug thenexp new_env in
    let new_env = TypeEnv.apply sub2 new_env in
    let* sub3, typ3 = infer_exp ~debug elseexp new_env in
    let* uni_sub2 = Substitution.unify typ2 typ3 in
    let new_env = TypeEnv.apply uni_sub2 new_env in
    let* comp_sub = Substitution.compose_all [ sub1; uni_sub1; sub2; sub3; uni_sub2 ] in
    return (comp_sub, typ3)
  | Exp_if (ifexp, thenexp, None) ->
    let* sub1, typ1 = infer_exp ~debug ifexp env in
    let* uni_sub1 = Substitution.unify typ1 (Type_construct ("bool", [])) in
    let new_env = TypeEnv.apply uni_sub1 env in
    let* sub2, typ2 = infer_exp ~debug thenexp new_env in
    let new_env = TypeEnv.apply sub2 new_env in
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
    return (res_sub, res_typ)
  | Exp_let (Nonrecursive, (value_binding, rest), exp) ->
    let* new_env, sub =
      infer_value_binding_list ~debug (value_binding :: rest) env Substitution.empty
    in
    (* let new_env = TypeEnv.apply sub new_env in *)
    if debug then Stdlib.Format.printf "DEBUG: Before EXPR\n";
    let* subb, typp = infer_exp ~debug exp new_env in
    if debug then Stdlib.Format.printf "DEBUG: AFTER EXPR%a\n" pprint_type typp;
    let* comp_sub = Substitution.compose sub subb in
    return (comp_sub, typp)
  | Exp_let (Recursive, (value_binding, rest), exp) ->
    let* new_env, fresh_vars = add_names_rec env (value_binding :: rest) in
    let* new_env, sub =
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
    (*remove?*)
  | _ -> failwith "unlucky"

and infer_cases ~debug env cases tyexp typat subst =
  let* env, sub, typexp, typpat =
    RList.fold_right
      ~f:(fun case acc ->
        let* env_acc, sub_acc, tyexp, typat = return acc in
        let* new_env, typepat = infer_pat ~debug case.first env_acc in
        let* new_sub, typeexp = infer_exp ~debug case.second new_env in
        let* uni_sub_exp = Substitution.unify tyexp typeexp in
        let* uni_sub_pat = Substitution.unify typat typepat in
        let* comp_sub =
          Substitution.compose_all [ sub_acc; new_sub; uni_sub_exp; uni_sub_pat ]
        in
        let new_env = TypeEnv.apply comp_sub new_env in
        return (new_env, comp_sub, typeexp, typepat))
      ~init:(return (env, subst, tyexp, typat))
      cases
  in
  return (env, sub, typexp, typpat)

and infer_value_binding_list ~debug vb_list env sub =
  let* res_env, res_sub =
    RList.fold_left
      vb_list
      ~init:(return (env, sub))
      ~f:(fun acc vb ->
        let* env_acc, sub_acc = return acc in
        match vb with
        | { pat = Pat_constraint (pat, pat_typ); expr = Exp_fun ((fpat, fpatrest), exp) }
          ->
          let* sub, typ =
            infer_exp
              ~debug
              (Exp_fun ((fpat, fpatrest), Exp_constraint (exp, pat_typ)))
              env_acc
          in
          if debug
          then
            Stdlib.Format.printf "DEBUG: sub of expr in vb:%a\n" Substitution.pp_sub sub;
          if debug
          then Stdlib.Format.printf "DEBUG: type of expr in vb:%a\n" pprint_type typ;
          let* res_env, res_sub = infer_rest_vb ~debug env_acc sub_acc sub typ pat in
          if debug
          then
            Stdlib.Format.printf
              "DEBUG: env after rest_vb in vb :{{%a}}\n\n"
              TypeEnv.pp_env
              res_env;
          return (res_env, res_sub)
        | { pat = Pat_constraint (pat, pat_typ); expr = Exp_function _ as exp } ->
          let* sub, typ = infer_exp ~debug (Exp_constraint (exp, pat_typ)) env_acc in
          let* res_env, res_sub = infer_rest_vb ~debug env_acc sub_acc sub typ pat in
          return (res_env, res_sub)
        | { pat; expr } ->
          if debug then Stdlib.Format.printf "DEBUG: VB BEFORE INFER EXP\n";
          let* sub, typ = infer_exp ~debug expr env_acc in
          if debug
          then
            Stdlib.Format.printf
              "DEBUG:!!!!!!! sub of expr in vb:%a\n"
              Substitution.pp_sub
              sub;
          if debug
          then Stdlib.Format.printf "DEBUG: type of expr in vb:%a\n" pprint_type typ;
          let* res_env, res_sub = infer_rest_vb ~debug env_acc sub_acc sub typ pat in
          if debug
          then
            Stdlib.Format.printf
              "DEBUG: env after rest_vb in vb :{{%a}}\n\n"
              TypeEnv.pp_env
              res_env;
          return (res_env, res_sub))
  in
  return (res_env, res_sub)

and infer_rec_value_binding_list ~debug vb_list env sub fresh_vars =
  let* res_env, res_sub =
    match
      RList.fold_left2
        vb_list
        fresh_vars
        ~init:(return (env, sub))
        ~f:(fun acc vb fv ->
          let* env_acc, sub_acc = return acc in
          match vb, fv with
          | ( ( { pat = Pat_var name; expr = Exp_fun _ as exp }
              | { pat = Pat_var name; expr = Exp_function _ as exp } )
            , fresh ) ->
            let* subexpr, typexpr = infer_exp ~debug exp env_acc in
            let* res_env, res_sub =
              infer_rec_rest_vb ~debug sub_acc env_acc fresh typexpr name subexpr
            in
            return (res_env, res_sub)
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
              infer_rec_rest_vb ~debug sub_acc env_acc fresh typexpr name subexpr
            in
            return (res_env, res_sub)
          | { pat = Pat_var name; expr }, fresh ->
            let* subexpr, typexpr = infer_exp ~debug expr env_acc in
            (match typexpr with
             | Type_arrow (_, _) ->
               let new_fresh = Substitution.apply sub_acc fresh in
               if typexpr = new_fresh
               then failwith "abobiks"
               else
                 let* res_env, res_sub =
                   infer_rec_rest_vb ~debug sub_acc env_acc fresh typexpr name subexpr
                 in
                 return (res_env, res_sub)
             | _ -> failwith "wrong rec")
          | _ -> failwith "rest")
    with
    | Ok result -> result
    | Unequal_lengths -> failwith "Lists have unequal lengths"
  in
  return (res_env, res_sub)
;;

(* let get_pattern_names = *)

(* let infer_rec_value_binding_list *)

open Ast.Pattern
open Ast.Structure
let check_poly_types typ typ_list =
  match typ with
  | Type_var x -> 
    (match Base.List.find ~f:(fun y -> String.equal x y) typ_list with
      | Some _ -> typ
      | None -> failwith "wrong poly types"
    )
  | _ -> typ
  ;;
let get_names_adt env poly_list =
  RList.fold_right
    ~f:(fun poly acc ->
      let* env_acc, fresh_acc = return acc in
      let* fresh = fresh_var in
      let env_acc = TypeEnv.extend env_acc poly (Forall (VarSet.empty, fresh)) in
      return (env_acc, fresh :: fresh_acc))
    poly_list
    ~init:(return (env, []))
;;

let infer_structure_item ~debug env item marity =
  match item with
  | Str_eval exp ->
    let* _, typ = infer_exp ~debug exp env in
    let new_env = TypeEnv.extend env "-" (Forall (VarSet.empty, typ)) in
    return (new_env, marity)
  | Str_value (Nonrecursive, (value_binding, rest)) ->
    let* env, sub =
      infer_value_binding_list ~debug (value_binding :: rest) env Substitution.empty
    in
    if debug then Stdlib.Format.printf "DEBUG: AFTER LKet\n";
    (* if debug then TypeEnv.pp_env Format.std_formatter env; *)
    return (env, marity)
  | Str_value (Recursive, (value_binding, rest)) ->
    let* new_env, fresh_vars = add_names_rec env (value_binding :: rest) in
    let* new_env, sub =
      infer_rec_value_binding_list
        ~debug
        (value_binding :: rest)
        new_env
        Substitution.empty
        fresh_vars
    in
    if debug then Stdlib.Format.printf "DEBUG: AFTER LKeREC\n";
    return (new_env, marity)
  | Str_adt (poly, name, (variant, rest)) ->
    if debug then Format.printf "DEBUG: In ADT\n";
    let* env, poly_types = get_names_adt env poly in
    let adt_type = Type_construct (name, poly_types) in
    let type_arity = List.length poly in
    let arity_map = Base.Map.set marity ~key:name ~data:type_arity in
    (* if debug then (
       Format.printf "Marity map:\n";
       Base.Map.iteri arity_map ~f:(fun ~key ~data ->
       Format.printf "Key: %s, Value: %d\n" key data
       )
       ); *)
    (* let env = TypeEnv.extend env name (Forall (VarSet.empty, adt_type)) in *)
    let* constrs =
      RList.fold_left
        (variant :: rest)
        ~init:(return env)
        ~f:(fun acc (constr_name, constr_types) ->
          let* env_acc = return acc in
          let* fresh = fresh_var in
          let new_env =
            match constr_types with
            | [] -> TypeEnv.extend env_acc constr_name (Forall (VarSet.empty, adt_type))
            | hd :: [] ->
              let _ = check_poly_types hd poly in
              TypeEnv.extend
                env_acc
                constr_name
                (Forall (VarSet.empty, Type_arrow (hd, adt_type)))
            | [ hd; tl ] ->
              let _ = Base.List.map [hd;tl] ~f:(fun typ -> check_poly_types typ poly) in
              TypeEnv.extend
                env_acc
                constr_name
                (Forall (VarSet.empty, Type_arrow (Type_tuple (hd, tl, []), adt_type)))
            | hd :: tl1 :: tl2 ->
              let _ = Base.List.map (hd::tl1::tl2) ~f:(fun typ -> check_poly_types typ poly) in
              TypeEnv.extend
                env_acc
                constr_name
                (Forall (VarSet.empty, Type_arrow (Type_tuple (hd, tl1, tl2), adt_type)))
          in
          (* let new_env = TypeEnv.extend env constr_name (Forall (VarSet.empty, fresh)) in *)
          return new_env)
    in
    return (constrs, arity_map)
;;

let infer_program ~debug program env =
  let marity = Base.Map.empty (module Base.String) in
  let* env, arr =
    RList.fold_left
      program
      ~init:(return (env, marity))
      ~f:(fun acc item ->
        let* env_acc, arr_acc = return acc in
        infer_structure_item ~debug env_acc item arr_acc)
  in
  if debug
  then (
    Format.printf "Marity map:\n";
    Base.Map.iteri arr ~f:(fun ~key ~data ->
      Format.printf "Key: %s, Value: %d\n" key data));
  return env
;;

let empty_env = TypeEnv.empty

let env_with_print_funs =
  let print_fun_list =
    [ ( "print_int"
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
    ]
  in
  List.fold_left
    (fun env (id, sch) -> TypeEnv.extend env id sch)
    TypeEnv.empty
    print_fun_list
;;

(*for expr test*)
(* let run_infer_expr (program : Ast.Expression.t) env = run (infer_exp program env) *)

(*for str item test*)
let run_infer_program ?(debug = false) (program : Ast.program) env =
  run (infer_program ~debug program env)
;;
