open InferTypes

(*Infer monad*)
module MInfer = struct
  open Base

  type 'a t = int -> int * ('a, error) Result.t

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
  end

  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

(*Type*)
type fresh_var = int

module Type = struct
  type t = typchik

  let rec occurs_check tvar = function
    | Typ_prim _ -> false
    | Typ_var binder -> binder = tvar
    | Typ_arrow (l, r) -> occurs_check tvar l || occurs_check tvar r
    | Typ_tuple t -> List.fold_left (fun acc h -> acc || occurs_check tvar h) false t
    | Typ_list l -> occurs_check tvar l
  ;;

  let free_vars =
    let rec helper acc = function
      | Typ_prim _ -> acc
      | Typ_var binder -> VarSet.add binder acc
      | Typ_arrow (l, r) -> helper (helper acc l) r
      | Typ_tuple t -> List.fold_left (fun acc h -> helper acc h) acc t
      | Typ_list l -> helper acc l
    in
    helper VarSet.empty
  ;;
end



(*Substitution*)
module Substitution = struct
  open MInfer
  open MInfer.Syntax
  open Base

  type t = (binder,typchik,Int.comparator_witness) Map.t

  let pp ppf subst =
    let open Stdlib.Format in
    fprintf
      ppf
      "[ %a ]"
      (pp_print_list
         ~pp_sep:(fun ppf () -> fprintf ppf ", ")
         (fun ppf (k, v) -> fprintf ppf "%d -> %a" k pprint_type v))
      subst
  ;;

  let empty = Map.empty (module Int)
  let mapping k v = if Type.occurs_check k v then fail `Occurs_check else return (k, v)

  let singleton k vm =
    let* k, vm = mapping k vm in
    return (Base.Map.singleton (module Base.Int) k vm)
  ;;

  let find_exn (map: t) (k: binder) : typchik =
    Map.find_exn map k
    
  let find (map: t) (k: binder) : typchik option =
    Map.find map k
  
  let remove (map: t) (k: binder) : t =
    Map.remove map k

  let apply sub =
    let rec helper = function
      | Typ_prim _ as typchik -> typchik
      | Typ_var b as typchik ->
        (match find_exn sub b with 
         | exception Not_found_s _ -> typchik
         | _ -> typchik) (*refactor?*)
      | Typ_arrow (l, r) -> Typ_arrow (helper l, helper r)
      | Typ_tuple t -> Typ_tuple (List.map t ~f:(fun el -> helper el))
      | Typ_list l -> Typ_list (helper l)
    in
    helper
  ;;

  let fold mp init f =
    Map.fold mp ~init
      ~f:(fun ~key:k ~data:vm acc ->
        let* acc = acc in
        f k vm acc)
  ;;

  let rec unify l r =
    match l, r with
    | Typ_prim l, Typ_prim r when String.equal l r -> return empty
    | Typ_prim _, Typ_prim _ -> fail (`Unification_failed (l, r))
    | Typ_var a, Typ_var b when Int.equal a b -> return empty
    | Typ_var b, t | t, Typ_var b -> singleton b t
    | Typ_arrow (l1, r1), Typ_arrow (l2, r2) ->
      let* subs1 = unify l1 l2 in
      let* subs2 = unify (apply subs1 r1) (apply subs1 r2) in
      compose subs1 subs2
    | _ -> fail (`Unification_failed (l, r))

    and extend k v s =
      match Map.find s k with
      | None ->
        let v = apply s v in
        let* s2 = singleton k v in  
        fold s (return s2) (fun k v acc ->
          let v = apply s2 v in
          let* k, v = mapping k v in
          return (Map.set acc ~key:k ~data:v))
      | Some v2 ->
        let* s2 = unify v v2 in
        compose s s2

  and compose s1 s2 = fold s2 (return s1) extend

  let compose_all ss = RList.fold_left ss ~init:(return empty) ~f:compose
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

  let apply subst (Forall (binder_set, typchik)) =
    let s2 = VarSet.fold (fun k s -> Substitution.remove s k) binder_set subst in
    Forall (binder_set, Substitution.apply s2 typchik)
  ;;

  let pp_scheme fmt = function
    | Forall (st, typ) ->
      if VarSet.is_empty st
      then Format.fprintf fmt "%a" pprint_type typ
      else Format.fprintf fmt "%a. %a" VarSet.pp st pprint_type typ
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

  let apply s env = Map.map env ~f:(Scheme.apply s)
  let find name xs = Map.find xs name

  let pp_env fmt environment =
    Map.iteri environment
      ~f:(fun ~key:key ~data:data -> Format.fprintf fmt "%S: %a\n" key Scheme.pp_scheme data)
  ;;
end

(*Generalization and Instantiation*)
open MInfer
open MInfer.Syntax

let fresh_var = fresh >>| fun n -> Typ_var n

let instantiate : scheme -> typchik MInfer.t =
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

