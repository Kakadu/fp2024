(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open TypedTree
open TypesPp
open Format
open Base

type error =
  [ `Occurs_check
  | `Undef_var of string
  | `Unification_failed of typ * typ
  ]

let pp_error fmt : error -> _ = function
  | `Occurs_check -> fprintf fmt "Occurs check failed"
  | `Undef_var s -> fprintf fmt "Undefined variable '%s'" s
  | `Unification_failed (fst, snd) ->
    fprintf fmt "unification failed on %a and %a" pp_typ fst pp_typ snd
;;

(* for treating result of type inference *)
module R : sig
  (* signature, smth like interface before realization *)
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

  val fresh : int t
  val run : 'a t -> ('a, error) Result.t

  module RMap : sig
    val fold : ('a, 'b, 'c) Map.t -> init:'d t -> f:('a -> 'b -> 'd -> 'd t) -> 'd t
  end
end = struct
  (* takes current state, runs smth, outputs new state and success / error *)
  type 'a t = int -> int * ('a, error) Result.t

  (* bind -- if applying new state to first arg is correct, then apply f to
     new argument and new state, else output error and state that caused it *)
  let ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
    fun m f st ->
    let last, r = m st in
    match r with
    | Result.Error x -> last, Error x
    | Ok a -> f a last
  ;;

  (* is called to cover result in fail or ok constructions *)
  let fail e st = st, Base.Result.fail e
  let return x last = last, Base.Result.return x
  let bind x ~f = x >>= f

  (* is called from x, function and state. if applying state to x is correct,
     then output applying f to x in constructor Ok, otherwise output error and
     state that caused it *)
  let ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t =
    fun x f st ->
    match x st with
    | st, Ok x -> st, Ok (f x)
    | st, Result.Error e -> st, Result.Error e
  ;;

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end

  (* for applying f to all elements x of list xs with check that everything is
     correct. If it is, outputs accumulator of all applyings *)
  module RList = struct
    let fold_left xs ~init ~f =
      Base.List.fold_left xs ~init ~f:(fun acc x ->
        let open Syntax in
        let* acc = acc in
        f acc x)
    ;;
  end

  (* analogically to list. let* acc = acc is to extract value from type t *)
  module RMap = struct
    let fold map ~init ~f =
      Map.fold map ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end

  (* takes current state, returns state + 1 *)
  let fresh : int t = fun last -> last + 1, Result.Ok last
  let run m = snd (m 0)
end

type fresh = int

(* module with all type methods *)
module Type = struct
  type t = typ

  (* check that type of the arg is not inside of type v.
     Runs during substitution to ensure that there are no cycles*)
  let rec occurs_in v = function
    | Primary _ -> false
    | Type_var b -> b = v
    | Arrow (fst, snd) -> occurs_in v fst || occurs_in v snd
  ;;

  (* collects all type variables *)
  let free_vars =
    let rec helper acc = function
      | Primary _ -> acc
      | Type_var b -> VarSet.add b acc
      | Arrow (fst, snd) -> helper (helper acc fst) snd
    in
    helper VarSet.empty
  ;;
end

(* module of substitution *)

module Substitution : sig
  type t

  val empty : t
  val mapping : fresh -> typ -> (fresh * typ) R.t
  val singleton : fresh -> typ -> t R.t
  val find : t -> fresh -> typ option
  val remove : t -> fresh -> t
  val apply : t -> typ -> typ
  val unify : typ -> typ -> t R.t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
end = struct
  open R
  open R.Syntax

  (* t in this module is map of key fresh to value typ. last arg specifies
     keys as int values (see fresh def) *)
  type t = (fresh, typ, Int.comparator_witness) Map.t

  (* empty map *)
  let empty = Map.empty (module Int)

  (* perform mapping of fresh var to typ with occurs check, if correct,
     output new pair *)
  let mapping k v = if Type.occurs_in k v then fail `Occurs_check else return (k, v)

  (* perform mapping, if correct, create map w 1 element as described in type t *)
  let singleton k v =
    let* k, v = mapping k v in
    return (Map.singleton (module Int) k v)
  ;;

  (* aliases for Map actions *)
  let find map key = Map.find map key
  let remove map key = Map.remove map key

  (* search for input in given map, if there is no match, output
     input type, else output found typ value associated w this key.
     Basically narrow given type to conditions given in substitution *)
  let apply map =
    let rec helper = function
      | Type_var b as typ ->
        (match find map b with
         | None -> typ
         | Some x -> x)
      | Arrow (fst, snd) -> Arrow (helper fst, helper snd)
      | other -> other
    in
    helper
  ;;

  (* check that two types are compatible. in third case put new pair of type_var
     and type into context (map) *)
  let rec unify fst snd =
    match fst, snd with
    | Primary fst, Primary snd when String.equal fst snd -> return empty
    | Primary _, Primary _ -> fail (`Unification_failed (fst, snd))
    | Type_var f, Type_var s when Int.equal f s -> return empty
    | Type_var b, t | t, Type_var b -> singleton b t
    | Arrow (f1, s1), Arrow (f2, s2) ->
      let* subst1 = unify f1 f2 in
      let* subst2 = unify s1 s2 in
      compose subst1 subst2
    | _ -> fail (`Unification_failed (fst, snd))

  (* if value associated w this key exists in map, try to unify them, otherwise
     get old substitution, form new singleton, update map so in contains new info *)
  and extend key value map =
    match find map key with
    | Some value2 ->
      let* map2 = unify value value2 in
      compose map map2
    | None ->
      let value = apply map value in
      let* map2 = singleton key value in
      RMap.fold map ~init:(return map2) ~f:(fun key value acc ->
        let value = apply map2 value in
        let* key, value = mapping key value in
        return (Map.update acc key ~f:(fun _ -> value)))

  (* compose two maps together *)
  and compose map1 map2 = RMap.fold map2 ~init:(return map1) ~f:extend

  (* compose list of maps together *)
  let compose_all maps = RList.fold_left maps ~init:(return empty) ~f:compose
end

(* module for scheme treatment *)
module Scheme = struct
  type t = scheme

  (* occurs check for both type vars set and typ in sheme *)
  let occurs_in value = function
    | S (vars, t) -> (not (VarSet.mem value vars)) && Type.occurs_in value t
  ;;

  (* take all vars that are not bound in typ *)
  let free_vars = function
    | S (vars, t) -> VarSet.diff (Type.free_vars t) vars
  ;;

  (* take substitution and scheme, remove its free vars from substitution,
     form new scheme according to substitution (apply it to typ) *)
  let apply subst (S (vars, t)) =
    let subst2 = VarSet.fold (fun key s -> Substitution.remove s key) vars subst in
    S (vars, Substitution.apply subst2 t)
  ;;

  let pp = pp_scheme
end

module TypeEnvironment = struct
  open Base

  (* environment (context?) -- pairs of names and their types list *)
  type t = (ident, scheme, String.comparator_witness) Map.t

  (* if pair (key, some old value) exists in map env, then replace old value
     with new, else add pair (key, value) into map *)
  let extend env key value = Map.update env key ~f:(fun _ -> value)
  let remove env key = Map.remove env key
  let empty = Map.empty (module String)

  (* apply given substitution to all elements of environment *)
  let apply subst env = Map.map env ~f:(Scheme.apply subst)
  let find key env = Map.find env key

  (* collect all free vars from environment *)
  let free_vars : t -> VarSet.t =
    Map.fold ~init:VarSet.empty ~f:(fun ~key:_ ~data:s acc ->
      VarSet.union acc (Scheme.free_vars s))
  ;;

  (* TODO: custom pp_scheme? not from deriving *)
  let pp fmt map =
    Stdlib.Format.fprintf fmt "{| ";
    Map.iteri map ~f:(fun ~key:n ~data:s ->
      Stdlib.Format.fprintf fmt "%s -> %a; " n pp_scheme s);
    Stdlib.Format.fprintf fmt "|}%!"
  ;;
end

open R
open R.Syntax

let unify = Substitution.unify
let make_fresh_var = fresh >>| fun n -> Type_var n

(* replace all type vars with fresh ones *)
let instantiate : scheme -> typ R.t =
  fun (S (vars, t)) ->
  VarSet.fold
    (fun name ty ->
      let* ty = ty in
      let* fr_var = make_fresh_var in
      let* subst = Substitution.singleton name fr_var in
      return (Substitution.apply subst ty))
    vars
    (return t)
;;

(* take free vars of type t and environment, put difference between them
   in S constructor so all vars are context independent *)
let generalize : TypeEnvironment.t -> Type.t -> Scheme.t =
  fun env t ->
  let free = VarSet.diff (Type.free_vars t) (TypeEnvironment.free_vars env) in
  S (free, t)
;;

let infer_expr =
  let rec helper env = function
    | Const const ->
      (match const with
       | Int_lt _ -> return (Substitution.empty, int_typ)
       | Bool_lt _ -> return (Substitution.empty, bool_typ)
       | String_lt _ -> return (Substitution.empty, string_typ)
       | Unit_lt -> return (Substitution.empty, unit_typ))
    | Bin_expr (op, e1, e2) ->
      let* subst1, typ1 = helper env e1 in
      let* subst2, typ2 = helper (TypeEnvironment.apply subst1 env) e2 in
      let* e1typ, e2typ, etyp =
        match op with
        | Logical_and | Logical_or -> return (bool_typ, bool_typ, bool_typ)
        | Binary_add
        | Binary_subtract
        | Binary_multiply
        | Binary_divide
        | Binary_and_bitwise
        | Binary_or_bitwise
        | Binary_xor_bitwise -> return (int_typ, int_typ, int_typ)
        | Binary_greater | Binary_greater_or_equal | Binary_less | Binary_less_or_equal ->
          return (int_typ, int_typ, bool_typ)
        | Binary_equal | Binary_unequal ->
          let* fr = make_fresh_var in
          return (fr, fr, bool_typ)
        | _ -> failwith "WIP"
      in
      let* subst3 = Substitution.unify (Substitution.apply subst2 typ1) e1typ in
      (*Format.printf "Checking types: res_typ1 = %a\n" pp_typ (Substitution.apply subst2 typ1);
        Format.printf "Checking types: res_typ2 = %a\n" pp_typ (Substitution.apply subst3 typ2);*)
      let* subst4 = Substitution.unify (Substitution.apply subst3 typ2) e2typ in
      let* subst_res = Substitution.compose_all [ subst1; subst2; subst3; subst4 ] in
      return (subst_res, Substitution.apply subst_res etyp)
    | If_then_else (c, th, Some el) ->
      let* subst1, typ1 = helper env c in
      let* subst2, typ2 = helper (TypeEnvironment.apply subst1 env) th in
      let* subst3, typ3 = helper (TypeEnvironment.apply subst2 env) el in
      let* subst4 = unify typ1 bool_typ in
      let* subst5 = unify typ2 typ3 in
      let* subst_result =
        Substitution.compose_all [ subst1; subst2; subst3; subst4; subst5 ]
      in
      return (subst_result, Substitution.apply subst5 typ2)
    | If_then_else (c, th, None) ->
      let* subst1, typ1 = helper env c in
      let* subst2, typ2 = helper (TypeEnvironment.apply subst1 env) th in
      let* subst3 = unify typ1 bool_typ in
      let* subst_result = Substitution.compose_all [ subst1; subst2; subst3 ] in
      return (subst_result, Substitution.apply subst2 typ2)
    | _ -> failwith "WIP"
  in
  helper
;;

let infer_construction env = function
  | Expr exp -> infer_expr env exp
  | _ -> failwith "WIP"
;;

let infer e = run (infer_construction TypeEnvironment.empty e)
