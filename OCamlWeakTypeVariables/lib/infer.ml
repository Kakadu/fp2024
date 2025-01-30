open Ast
open Types

let debug = true

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
        | [], _ :: _ | _ :: _, [] -> failwith "two lists must be have equal size"
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
    | TTuple (a, b, typ_list) ->
      List.fold_left (fun acc item -> acc || occurs_in v item) false (a :: b :: typ_list)
    | TBase _ -> false
  ;;

  let type_vars =
    let rec helper acc = function
      | TVar n -> TVarSet.add n acc
      | TArrow (left, right) -> helper (helper acc left) right
      | TList typ -> helper acc typ
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
  val find : t -> int -> typ option
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
    if Type.occurs_in k v then fail OccursCheckFailed else return (k, v)
  ;;

  let singleton k v =
    let* k, v = mapping k v in
    return (Base.Map.singleton (module Base.Int) k v)
  ;;

  let find (sub : t) k = Base.Map.find sub k
  let remove (sub : t) k : t = Base.Map.remove sub k

  let apply sub =
    let rec helper = function
      | TVar n ->
        (match find sub n with
         | None -> TVar n
         | Some v -> v)
      | TArrow (left, right) -> TArrow (helper left, helper right)
      | TList typ -> TList (helper typ)
      | TTuple (a, b, t_list) ->
        TTuple (helper a, helper b, Base.List.map t_list ~f:helper)
      | other -> other
    in
    helper
  ;;

  let rec unify l r =
    if debug then Format.printf "Unify: %s AND %s\n" (show_typ l) (show_typ r);
    match l, r with
    | TBase l, TBase r when l = r -> return empty
    | TVar a, TVar b when a = b -> return empty
    | TVar a, t | t, TVar a -> singleton a t
    | TArrow (left1, right1), TArrow (left2, right2) ->
      let* sub1 = unify left1 left2 in
      let* sub2 = unify (apply sub1 right1) (apply sub1 right2) in
      compose sub1 sub2
    | TList typ1, TList typ2 -> unify typ1 typ2
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

  and extend k v sub =
    match find sub k with
    | None ->
      let v = apply sub v in
      let* new_sub = singleton k v in
      let f1 ~key ~data acc =
        let* acc = acc in
        let new_data = apply new_sub data in
        return (Base.Map.update acc key ~f:(fun _ -> new_data))
      in
      Base.Map.fold sub ~init:(return new_sub) ~f:f1
    | Some vl ->
      let* new_sub = unify v vl in
      compose sub new_sub

  and compose sub1 sub2 = RMap.fold_left sub2 ~init:(return sub1) ~f:extend

  let compose_all sub_list = RList.fold_left sub_list ~init:(return empty) ~f:compose

  let pp fmt sub =
    Base.Map.iteri sub ~f:(fun ~key ~data ->
      Format.fprintf fmt "val %d : %a\n" key Infer_print.pp_typ_my data)
  ;;
end

module Scheme = struct
  let free_vars = function
    | Scheme (bind_vars, ty) -> TVarSet.diff (Type.type_vars ty) bind_vars
  ;;

  let apply sub = function
    | Scheme (bind_vars, ty) ->
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
  val apply : t -> Subst.t -> t
  val operators : (id list * typ) list
  val pp : Format.formatter -> t -> unit
end = struct
  type t = (string, scheme, Base.String.comparator_witness) Base.Map.t

  let empty : t = Base.Map.empty (module Base.String)

  let free_vars (env : t) =
    Base.Map.fold
      ~init:TVarSet.empty
      ~f:(fun ~key:_ ~data acc -> TVarSet.union acc (Scheme.free_vars data))
      env
  ;;

  let apply env sub = Base.Map.map env ~f:(Scheme.apply sub)
  let extend env key schema = Base.Map.update env key ~f:(fun _ -> schema)
  let find env key = Base.Map.find env key
  let find_exn env key = Base.Map.find_exn env key

  let operators =
    [ [ "+"; "-"; "*"; "/" ], TBase BInt @-> TBase BInt @-> TBase BInt
    ; [ "print_int" ], TBase BInt @-> TBase BUnit
    ; [ "<="; "<"; ">"; ">="; "="; "<>" ], TVar 0 @-> TVar 0 @-> TBase BBool
    ]
  ;;

  let pp fmt env =
    Base.Map.iteri env ~f:(fun ~key ~data:(Scheme (_, t)) ->
      Format.fprintf fmt "val %s : %a\n" key Infer_print.pp_typ_my t)
  ;;
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
      return (Subst.apply sub acc))
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

let infer_id env id =
  match id with
  | "_" ->
    let* fv = fresh_var in
    return (fv, Subst.empty)
  | _ -> lookup_env env id
;;

let infer_pattern env ?ty =
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
;;

(* [@@@warning "-8"] *)

(* https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_W *)
let infer_expr =
  let rec helper : TypeEnv.t -> expression -> (typ * Subst.t) t =
    fun env expr ->
    (* if debug then Format.printf "Infer expression: \n\t%s\n" (show_expression expr); *)
    match expr with
    | Pexp_constant c -> infer_const c
    | Pexp_ident id -> infer_id env id
    | Pexp_fun (pattern, expr) ->
      let* t, env', _ = infer_pattern env pattern in
      let* t', sub = helper env' expr in
      return (Subst.apply sub (t @-> t'), sub)
      (* Recursive apply type inference by Homka122 😼😼😼 *)
    | Pexp_apply (e0, es) ->
      let rec helper_apply init es =
        match es with
        | [] -> return init
        | e1 :: tl ->
          let* t' = fresh_var in
          let t0, sub0 = init in
          let* t1, sub1 = helper (TypeEnv.apply env sub0) e1 in
          let* sub2 = Subst.unify (Subst.apply sub1 t0) (t1 @-> t') in
          let* sub3 = Subst.compose_all [ sub0; sub1; sub2 ] in
          helper_apply (Subst.apply sub2 t', sub3) tl
      in
      let* init = helper env e0 in
      helper_apply init es
    (* Recursive ifthenelse type inference with option else by Homka122 😼😼😼 *)
    | Pexp_ifthenelse (e0, e1, e2) ->
      let* t0, sub0 = helper env e0 in
      let* t1, sub1 = helper env e1 in
      let* sub_bool = Subst.unify t0 (TBase BBool) in
      let result =
        match e2 with
        | None ->
          let* sub = Subst.compose_all [ sub0; sub1; sub_bool ] in
          return (Subst.apply sub t1, sub)
        | Some e2 ->
          let* t2, sub2 = helper env e2 in
          let* sub_eq = Subst.unify t1 t2 in
          let* sub = Subst.compose_all [ sub0; sub1; sub2; sub_eq; sub_bool ] in
          return (Subst.apply sub t2, sub)
      in
      result
    (* let x0 = e0 and x1 = e1 and ... xn = en in e_f *)
    (* each xN = eN generate type tN of xN, type kN of eN, substitution S0 and envN with xN: tN *)
    (* So I think i can just generate substitution with unify tN and kN *)
    (* i want to die after three hours of attempting implemented this 😿😿😿 *)
    (* Recursive multiple let definitions type inference by Homka122 😼😼😼 (it took 4 hours) *)
    | Pexp_let (NonRecursive, vb, e1) ->
      let rec helper_let env pattern expr vbs =
        (* Upd: it's wrong *)
        (* let x0 = e0 in e1. x0: k0, e0: t0, x0 \in env0 *)
        (* Let S0(env0) = env1. x0: k0 \in env0  *)
        (* Let tk = gen(env1, t0) *)
        (* (unify tk k0)env1 = env2. Where is x: tk \in env2 *)
        (* So we have env with gen type tk and x0: t0 *)
        (* And that is without knowing name of variable *)
        (* It took 2.5 hours for invented this *)
        let* t0, sub0 = helper env expr in
        let* _, env0, _ = infer_pattern ~ty:t0 env pattern in
        (* let x0 = e0 in e1 if vbs is empty OR let x0 = e0 and x1 = e1 and ... xn = en in E otherwise *)
        let* t1, sub1 =
          match vbs with
          | h :: t -> helper_let env0 h.pvb_pat h.pvb_expr t
          | _ -> helper env0 e1
        in
        let* sub = Subst.compose sub0 sub1 in
        return (t1, sub)
      in
      let result =
        match vb with
        | h :: t -> helper_let env h.pvb_pat h.pvb_expr t
        | [] -> failwith "Parser error"
      in
      result
      (* https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Typing_rule *)
    | Pexp_let (Recursive, vbs, e1) ->
      let exprs, patterns = List.split @@ List.map (fun x -> x.pvb_expr, x.pvb_pat) vbs in
      (* New type variables to all names in patterns *)
      let* env' =
        RList.fold_left patterns ~init:(return env) ~f:(fun env pat ->
          let* _, env, _ = infer_pattern env pat in
          return env)
      in
      (* We get types of e0, e1, ... en and additional type info about type of variables outside of ei expression for all i *)
      let* ts, subs = List.split <$> RList.map exprs ~f:(fun expr -> helper env' expr) in
      (* Combine all information about variables *)
      let* sub = Subst.compose_all subs in
      (* Apply all gotten types to out new names *)
      let* env' =
        RList.fold_left
          (List.combine ts patterns)
          ~init:(return env)
          ~f:(fun env (ty, pat) ->
            let* _, env, _ = infer_pattern ~ty:(Subst.apply sub ty) env pat in
            return env)
      in
      let* t, sub = helper env' e1 in
      return (t, sub)
    | Pexp_tuple e ->
      (match e with
       | [] | [ _ ] -> failwith "Tuple parser error"
       | e0 :: e1 :: exps ->
         let* t0, sub0 = helper env e0 in
         let* t1, sub1 = helper env e1 in
         let* ts, subs = List.split <$> RList.map exps ~f:(fun e -> helper env e) in
         let* sub = Subst.compose_all (sub0 :: sub1 :: subs) in
         return (TTuple (t0, t1, ts), sub))
  in
  helper
;;

let infer_structure =
  let helper env = function
    | Pstr_eval expr ->
      let* _, _ = infer_expr env expr in
      return (env, [])
    | Pstr_value (NonRecursive, vbs) ->
      let* env, names =
        RList.fold_left
          vbs
          ~init:(return (env, []))
          ~f:(fun (env, names) vb ->
            let* t0, _ = infer_expr env vb.pvb_expr in
            let* _, env1, new_names = infer_pattern ~ty:t0 env vb.pvb_pat in
            match List.exists (fun name -> List.mem name new_names) names with
            | true -> fail (PatternNameTwice vb.pvb_pat)
            | false -> return (env1, List.append (List.rev new_names) names))
      in
      return (env, List.rev names)
    | _ -> failwith "not implemented"
  in
  helper
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

let run_structure_inferencer structure =
  match run (infer_structure defaultEnv structure) (List.length TypeEnv.operators) with
  | Result.Ok typ -> typ
  | Result.Error e -> failwith (show_error e)
;;
