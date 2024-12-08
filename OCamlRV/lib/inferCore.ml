open Ast
open Typedtree
open Base

module VarSet = struct
  include Stdlib.Set.Make (Int)
end

type scheme = S of VarSet.t * ty

module R : sig
  type 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Monad.Infix with type 'a t := 'a t

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  (** Creation of a fresh name from internal state *)
  val fresh : int t

  (** Running a transformer: getting the inner result value *)
  val run : 'a t -> ('a, error) Result.t

  module RMap : sig
    val fold : ('a, 'b, 'c) Map.t -> init:'d t -> f:('a -> 'b -> 'd -> 'd t) -> 'd t
  end
end = struct
  (** A composition: State monad after Result monad *)
  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Result.fail x
    | Result.Ok a -> f a last
  ;;

  let fail e st = st, Result.fail e
  let return x last = last, Result.return x
  let bind x ~f = x >>= f

  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun m f st ->
    match m st with
    | st, Ok x -> st, Result.return (f x)
    | st, Result.Error e -> st, Result.fail e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  let fresh : int t = fun last -> last + 1, Result.return last
  let run m = snd (m 0)

  module RMap = struct
    let fold m ~init ~f =
      Map.fold m ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end
end

type fresh = int

module Type = struct
  type t = ty

  let rec occurs_in v = function
    | TVar b -> b = v
    | TArrow (l, r) -> occurs_in v l || occurs_in v r
    | TTuple tl -> Base.List.exists tl ~f:(occurs_in v)
    | TList t -> occurs_in v t
    | TPrim _ -> false
  ;;

  let free_vars =
    let rec helper acc = function
      | TVar b -> VarSet.add b acc
      | TArrow (l, r) -> helper (helper acc l) r
      | TTuple tl -> List.fold_left tl ~init:acc ~f:helper
      | TList t -> helper acc t
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
      | TVar b as ty ->
        (match find s b with
         | None -> ty
         | Some x -> x)
      | TArrow (l, r) -> arrow (helper l) (helper r)
      | TList t -> list_type (helper t)
      | TTuple ts -> tuple_type (List.map ~f:helper ts)
      | other -> other
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TPrim l, TPrim r when String.equal l r -> return empty
    | TVar a, TVar b when Int.equal a b -> return empty
    | TVar b, t | t, TVar b -> singleton b t
    | TArrow (l1, r1), TArrow (l2, r2) ->
      let* s1 = unify l1 l2 in
      let* s2 = unify (apply s1 r1) (apply s1 r2) in
      compose s1 s2
    | TList t1, TList t2 -> unify t1 t2
    | TTuple ts1, TTuple ts2 ->
      (match
         List.fold2
           ts1
           ts2
           ~f:(fun acc t1 t2 ->
             let* acc = acc in
             let* s = unify (apply acc t1) (apply acc t2) in
             compose acc s)
           ~init:(return empty)
       with
       | Unequal_lengths -> fail (`Unification_failed (l, r))
       | Ok s -> s)
    | _ -> fail (`Unification_failed (l, r))

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

  let compose_all ss =
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
