open InferTypes

(*Type*)
module Type = struct
  type t = typchik

  let rec occurs_check tvar = function
    | Typ_prim _ -> false
    | Typ_var binder -> binder = tvar
    | Typ_arrow (l, r) -> occurs_check tvar l || occurs_check tvar r
    | Typ_tuple t -> List.fold_left (fun acc h -> acc || occurs_check tvar h) false t
    | Typ_list l -> occurs_check tvar l
  ;;

  (* let free_vars =
     let rec helper acc = function
     | Typ_prim _ -> acc
     | Typ_var b -> VarSet.add b acc
     | Typ_arrow (l, r) -> helper (helper acc l) r
     | Typ_tuple t -> List.fold_left (fun acc h -> helper acc h) acc t
     | Typ_list l -> helper acc l
     in
     helper VarSet.empty
     ;; *)
end

(*Scheme*)
module Scheme = struct
  type t = scheme

  let occurs_check v = function
    | Forall (xs, t) -> (not (VarSet.mem v xs)) && Type.occurs_check v t
  ;;

  let free_vars = function
    | Forall (bs, t) -> VarSet.diff (Type.free_vars t) bs
  ;;

  let apply subst (Scheme (names, ty)) =
    let s2 = VarSet.fold (fun k s -> Subst.remove k s) names subst in
    Scheme (names, Subst.apply s2 ty)
  ;;

  let pp_scheme fmt = function
    | Forall (st, typ) ->
      if VarSet.is_empty st
      then fprintf fmt "%a" pp_ty typ
      else fprintf fmt "%a. %a" VarSet.pp_varset st pp_ty typ
  ;;
end

(*Context / Environment*)
module TypeEnv = struct
  open Base

  type t = scheme Map.M(String).t

  let extend key value = Map.add ~key value
  let empty = Map.empty (module String)
  let fold f init mp = Map.fold mp ~init ~f:(fun ~key:k ~data:v acc -> f k v acc)

  let free_vars : t -> VarSet.t =
    fold (fun _ s acc -> VarSet.union acc (Scheme.free_vars s)) VarSet.empty
  ;;
end

(*Substitution*)
module Subst = struct
  open Base

  type t = (string, typchik, String.comparator_witness) Map.t
end
(*Unification*)

(*Infer monad*)

(*Generalization and Instantiation*)
