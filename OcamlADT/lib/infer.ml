open InferTypes

(*Infer monad*)
(*
   module MInfer : sig
   open Base
   type 'a t

   val bind : 'a t -> f:('a -> 'b t) -> 'b t
   val return : 'a -> 'a t
   val fail : error -> 'a t *)

(* include Monad.Infix with type 'a t := 'a t

   module Syntax : sig
   val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
   end

   module RList : sig
   val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
   end

   (** Creation of a fresh name from internal state *)
   val fresh : int t

   (** Running a transformer: getting the inner result value *)
   val run : 'a t -> ('a, error) Result.t *)
module MInfer = struct
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

(*Scheme*)
module Scheme = struct
  type t = scheme

  let occurs_check v = function
    | Forall (xs, t) -> (not (VarSet.mem v xs)) && Type.occurs_check v t
  ;;

  let free_vars = function
    | Forall (bs, t) -> VarSet.diff (Type.free_vars t) bs
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

(*Unification*)

(*Generalization and Instantiation*)
