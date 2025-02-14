(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open TypedTree
open TypesPp
open Format
open Base

module InferenceError = struct
  type t =
    [ `Occurs_check
    | `Undef_var of string
    | `Unification_failed of typ * typ
    | `Not_allowed_right_hand_side_let_rec
    | `Not_allowed_left_hand_side_let_rec
    | `Args_after_not_variable_let
    | `Bound_several_times
    | `Active_pattern_are_not_determined_by_input
    ]

  let bound_error = `Bound_several_times

  let pp_error fmt : t -> _ = function
    | `Occurs_check -> fprintf fmt "Occurs check failed"
    | `Undef_var s -> fprintf fmt "Undefined variable '%s'" s
    | `Unification_failed (fst, snd) ->
      fprintf fmt "unification failed on %a and %a\n" pp_typ fst pp_typ snd
    | `Not_allowed_right_hand_side_let_rec ->
      fprintf fmt "This kind of expression is not allowed as right-hand side of `let rec'"
    | `Not_allowed_left_hand_side_let_rec ->
      fprintf fmt "Only variables are allowed as left-hand side of `let rec'"
    | `Args_after_not_variable_let ->
      fprintf fmt "Arguments in let allowed only after variable"
    | `Bound_several_times -> fprintf fmt "Variable is bound several times"
    | `Active_pattern_are_not_determined_by_input ->
      fprintf fmt "Some active pattern are not determined by input"
  ;;
end

(* for treating result of type inference *)
module R : sig
  type 'a t
  type error = InferenceError.t

  val bound_error : error
  val pp_error : formatter -> error -> unit

  (* val bind : 'a t -> f:('a -> 'b t) -> 'b t *)
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
  val run : 'a t -> int -> int * ('a, error) Result.t

  module RMap : sig
    val fold : ('a, 'b, 'c) Map.t -> init:'d t -> f:('a -> 'b -> 'd -> 'd t) -> 'd t
  end
end = struct
  include InferenceError

  type error = InferenceError.t

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
  let run m = m
end

type fresh = int

(* module with all type methods *)
module Type : sig
  type t = typ

  val occurs_in : fresh -> t -> bool
  val free_vars : t -> binder_set
end = struct
  type t = typ

  (* check that v is not inside of second type.
     Runs during substitution to ensure that there are no cycles*)
  let rec occurs_in v = function
    | Primitive _ -> false
    | Type_var b -> b = v
    | Arrow (fst, snd) -> occurs_in v fst || occurs_in v snd
    | Type_list typ -> occurs_in v typ
    | Type_tuple (fst, snd, rest) -> List.exists (fst :: snd :: rest) ~f:(occurs_in v)
    | Choice map -> List.exists ~f:(occurs_in v) (choice_to_list map)
    | TOption t -> occurs_in v t
    | TActPat (_, typ) -> occurs_in v typ
  ;;

  (* collects all type variables *)
  let free_vars =
    let rec helper acc = function
      | Primitive _ -> acc
      | Type_var b -> VarSet.add b acc
      | Arrow (fst, snd) -> helper (helper acc fst) snd
      | Type_list typ -> helper acc typ
      | Type_tuple (fst, snd, rest) -> List.fold (fst :: snd :: rest) ~init:acc ~f:helper
      | Choice map -> List.fold (choice_to_list map) ~init:acc ~f:helper
      | TOption t -> helper acc t
      | TActPat (_, t) -> helper acc t
    in
    helper VarSet.empty
  ;;
end

(* module of substitution *)

module Substitution : sig
  type t

  val empty : t

  (* val mapping : fresh -> typ -> (fresh * typ) R.t *)
  val singleton : fresh -> typ -> t R.t

  (* val find : t -> fresh -> typ option *)
  val remove : t -> fresh -> t
  val apply : t -> typ -> typ
  val unify : typ -> typ -> t R.t
  val compose : t -> t -> t R.t
  val compose_all : t list -> t R.t
  (* val pp : formatter -> t -> unit *)
end = struct
  open R
  open R.Syntax

  (* t in this module is map of key fresh to value typ. last arg specifies
     keys as int values (see fresh def) *)
  type t = (fresh, typ, Int.comparator_witness) Map.t

  (* empty map *)
  let empty = Map.empty (module Int)
  (* let pp fmt s = Map.iteri s ~f:(fun ~key ~data -> fprintf fmt "%d: %a" key pp_typ data) *)

  (* perform mapping of fresh var to typ with occurs check, if correct,
     output new pair *)
  let mapping k v = if Type.occurs_in k v then fail `Occurs_check else return (k, v)

  (* perform mapping, if correct, create map w 1 element as described in type t *)
  let singleton k v =
    let* k, v = mapping k v in
    return (Map.singleton (module Int) k v)
  ;;

  (* aliases for Map actions *)
  let find = Map.find
  let remove = Map.remove

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
      | Type_list t -> Type_list (helper t)
      | Type_tuple (fst, snd, rest) ->
        Type_tuple (helper fst, helper snd, List.map rest ~f:helper)
      | Primitive t -> Primitive t
      | TOption t -> TOption (helper t)
      | TActPat (name, t) -> TActPat (name, helper t)
      | Choice map -> Choice (Map.map map ~f:helper)
    in
    helper
  ;;

  (* check that two types are compatible. in third case put new pair of type_var
     and type into context (map) *)
  let rec unify fst snd =
    match fst, snd with
    | Primitive fst, Primitive snd when String.equal fst snd -> return empty
    | Type_var f, Type_var s when Int.equal f s -> return empty
    | Type_var b, t | t, Type_var b -> singleton b t
    | Arrow (f1, s1), Arrow (f2, s2) ->
      let* subst1 = unify f1 f2 in
      let* subst2 = unify s1 s2 in
      compose subst1 subst2
    | Type_list t1, Type_list t2 -> unify t1 t2
    | TOption t1, TOption t2 -> unify t1 t2
    | Type_tuple (t1_1, t1_2, t1_rest), Type_tuple (t2_1, t2_2, t2_rest)
      when List.length t1_rest = List.length t2_rest ->
      let type_pairs = List.zip_exn (t1_1 :: t1_2 :: t1_rest) (t2_1 :: t2_2 :: t2_rest) in
      let* substitutions =
        List.fold type_pairs ~init:(return []) ~f:(fun acc (t1, t2) ->
          let* acc = acc in
          let* subst = unify t1 t2 in
          return (subst :: acc))
      in
      let substitution_result = compose_all substitutions in
      substitution_result
    | TActPat (name1, _), TActPat (name2, _) when not (phys_equal name1 name2) ->
      return empty
    | TActPat (name1, t1), TActPat (name2, t2) when not (phys_equal name1 name2) ->
      unify t1 t2
    | Choice map, TActPat (name, t) | TActPat (name, t), Choice map ->
      unify (Choice map) (Choice (Map.singleton (module String) name t))
    | Choice map1, Choice map2 ->
      Map.fold map1 ~init:(return empty) ~f:(fun ~key ~data subst ->
        let* subst = subst in
        let* new_subst =
          match Map.find map2 key with
          | Some t -> unify data t
          | None -> return empty
        in
        compose subst new_subst)
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
  and compose_all maps = RList.fold_left maps ~init:(return empty) ~f:compose
end

(* module for scheme treatment *)
module Scheme : sig
  type t = scheme

  (* val occurs_in : fresh -> t -> bool *)
  val apply : Substitution.t -> t -> t
  val free_vars : t -> binder_set

  (* val pp : formatter -> t -> unit *)
end = struct
  type t = scheme

  (* take all vars that are not bound in typ *)
  let free_vars = function
    | Scheme (vars, t) -> VarSet.diff (Type.free_vars t) vars
  ;;

  (* take substitution and scheme, remove its free vars from substitution,
     form new scheme according to substitution (apply it to typ) *)
  let apply subst (Scheme (vars, t)) =
    let subst2 = VarSet.fold (fun key s -> Substitution.remove s key) vars subst in
    Scheme (vars, Substitution.apply subst2 t)
  ;;

  (* let pp = pp_scheme *)
end

module TypeEnv : sig
  type t

  val free_vars : t -> VarSet.t
  val extend : t -> string -> scheme -> t
  val extend_many : t -> (string * scheme) list -> t
  val apply : Substitution.t -> t -> t
  val default : t
  val find : t -> string -> scheme option
  val find_exn : t -> string -> scheme
  val find_typ_exn : t -> string -> typ
  val find_typ : t -> string -> typ option
  val remove : t -> string -> t
  val remove_many : t -> string list -> t
  val iteri : t -> f:(name:string -> typ:typ -> unit) -> unit
  (* val pp : formatter -> t -> unit *)
end = struct
  open Base

  (* environment (context?) -- pairs of names and their types list *)
  type t = (string, scheme, String.comparator_witness) Map.t

  (* if pair (key, some old value) exists in map env, then replace old value
     with new, else add pair (key, value) into map *)
  let extend env key value = Map.update env key ~f:(fun _ -> value)

  let extend_many env list =
    List.fold list ~init:env ~f:(fun env (k, v) -> extend env k v)
  ;;

  let remove = Map.remove
  let remove_many t keys = List.fold ~init:t keys ~f:remove

  let default =
    Map.set
      (Map.empty (module String))
      ~key:"print_int"
      ~data:(Scheme (VarSet.empty, Arrow (int_typ, unit_typ)))
  ;;

  (* apply given substitution to all elements of environment *)
  let apply subst env = Map.map env ~f:(Scheme.apply subst)
  let find = Map.find
  let find_exn = Map.find_exn

  let find_typ env key =
    match find env key with
    | Some (Scheme (_, typ)) -> Some typ
    | None -> None
  ;;

  let find_typ_exn env key =
    match find_exn env key with
    | Scheme (_, typ) -> typ
  ;;

  let iteri env ~f =
    Map.iteri env ~f:(fun ~key ~data ->
      (function
        | Scheme (_, typ) -> f ~name:key ~typ)
        data)
  ;;

  (* collect all free vars from environment *)
  let free_vars : t -> VarSet.t =
    Map.fold ~init:VarSet.empty ~f:(fun ~key:_ ~data:s acc ->
      VarSet.union acc (Scheme.free_vars s))
  ;;
end

include R
open R.Syntax
module ExtractIdents = ExtractIdents.Make (R)
open ExtractIdents

let unify = Substitution.unify
let make_fresh_var = fresh >>| fun n -> Type_var n

(* replace all type vars with fresh ones *)
let instantiate : scheme -> typ R.t =
  fun (Scheme (vars, t)) ->
  VarSet.fold
    (fun name typ ->
      let* typ = typ in
      let* fr_var = make_fresh_var in
      let* subst = Substitution.singleton name fr_var in
      return (Substitution.apply subst typ))
    vars
    (return t)
;;

(* take free vars of type t and environment, put difference between them
   in S constructor so all vars are context independent *)
let generalize env typ =
  let free = VarSet.diff (Type.free_vars typ) (TypeEnv.free_vars env) in
  Scheme (free, typ)
;;

let rec find_args_type = function
  | Arrow (a, Arrow (b, rest)) -> Arrow (a, find_args_type (Arrow (b, rest)))
  | Arrow (a, _) -> a
  | t -> t
;;

let rec decompose_arrow typ =
  match typ with
  | Arrow (fst, snd) ->
    let args, final_return_type = decompose_arrow snd in
    fst :: args, final_return_type
  | _ -> [], typ
;;

let find_return_type typ =
  let _, return_t = decompose_arrow typ in
  return_t
;;

let infer_lt = function
  | Int_lt _ -> return int_typ
  | Bool_lt _ -> return bool_typ
  | String_lt _ -> return string_typ
  | Unit_lt -> return unit_typ
;;

let rec infer_pattern env ~shadow = function
  | Wild ->
    let* fresh_var = make_fresh_var in
    return (env, fresh_var)
  | PConst lt ->
    let* t = infer_lt lt in
    return (env, t)
  | PVar (Ident name) ->
    let* fresh = make_fresh_var in
    let scheme = Scheme (VarSet.empty, fresh) in
    let env, typ =
      if shadow
      then TypeEnv.extend env name scheme, fresh
      else (
        let typ = TypeEnv.find_typ env name in
        env, Option.value typ ~default:fresh)
    in
    return (env, typ)
  | POption None ->
    let* fresh_var = make_fresh_var in
    return (env, TOption fresh_var)
  | POption (Some p) ->
    let* env, typ = infer_pattern env ~shadow p in
    return (env, TOption typ)
  | PList [] ->
    let* fresh_var = make_fresh_var in
    return (env, Type_list fresh_var)
  | PList (hd :: tl) ->
    let* env, typ1 = infer_pattern env ~shadow hd in
    let* env, typ2 = infer_pattern env ~shadow (PList tl) in
    let* subst = Substitution.unify typ2 (Type_list typ1) in
    let env = TypeEnv.apply subst env in
    return (env, Substitution.apply subst typ2)
  | PCons (hd, tl) ->
    let* env, typ1 = infer_pattern env ~shadow hd in
    let* env, typ2 = infer_pattern env ~shadow tl in
    let* subst = Substitution.unify typ2 (Type_list typ1) in
    let env = TypeEnv.apply subst env in
    return (env, Substitution.apply subst typ2)
  | PTuple (fst, snd, rest) ->
    let* env, typ1 = infer_pattern env ~shadow fst in
    let* env, typ2 = infer_pattern env ~shadow snd in
    let* env, typs_rest =
      List.fold_right
        rest
        ~f:(fun p acc ->
          let* env, types = acc in
          let* env, typ = infer_pattern env ~shadow p in
          return (env, typ :: types))
        ~init:(return (env, []))
    in
    return (env, Type_tuple (typ1, typ2, typs_rest))
  | PConstraint (p, t) ->
    let* env, inferred_typ = infer_pattern env ~shadow p in
    let* subst = unify t inferred_typ in
    return (TypeEnv.apply subst env, Substitution.apply subst t)
  | PActive (Ident name, p) ->
    let* env, typ = infer_pattern env ~shadow p in
    let pat_typ =
      match TypeEnv.find_exn env name with
      | Scheme (_, typ) -> typ
    in
    let pat_return_typ = find_return_type pat_typ in
    let* subst = unify typ pat_return_typ in
    return (TypeEnv.apply subst env, Substitution.apply subst pat_typ)
;;

let infer_patterns env ~shadow patterns =
  List.fold_right
    patterns
    ~init:(return (env, []))
    ~f:(fun pat acc ->
      let* old_env, typs = acc in
      let* new_env, typ = infer_pattern old_env ~shadow pat in
      return (new_env, typ :: typs))
;;

let infer_match_pattern env ~shadow pattern match_type =
  let* env, pat_typ = infer_pattern env ~shadow pattern in
  let pat_input_typ = find_args_type pat_typ in
  let* subst = unify pat_input_typ match_type in
  let env = TypeEnv.apply subst env in
  let* pat_names = extract_names_from_pattern pattern >>| elements in
  let generalized_schemes =
    List.map pat_names ~f:(fun name ->
      let typ = TypeEnv.find_typ_exn env name in
      let env = TypeEnv.remove env name in
      let generalized_typ = generalize env typ in
      name, generalized_typ)
  in
  let env = TypeEnv.extend_many env generalized_schemes in
  return (env, subst)
;;

let extend_env_with_bind_names env let_binds =
  let bind_names = extract_bind_patterns_from_let_binds let_binds in
  let* env, _ = infer_patterns env ~shadow:true bind_names in
  return env
;;

let check_let_bind_correctness is_rec let_bind =
  match let_bind, is_rec with
  | Let_bind (PVar _, _, _), _ -> return let_bind
  | Let_bind _, Rec -> fail `Not_allowed_left_hand_side_let_rec
  | Let_bind (_, args, _), _ when List.length args <> 0 ->
    fail `Args_after_not_variable_let
  | _ -> return let_bind
;;

let unify_act_pat = function
  | TActPat (n1, t1), TActPat (n2, t2) ->
    Choice (choice_set_many (Map.empty (module String)) [ n1, t1; n2, t2 ])
  | TActPat (name, t), Choice map | Choice map, TActPat (name, t) ->
    Choice (Map.set map ~key:name ~data:t)
  | Choice map1, Choice map2 ->
    Choice (Map.fold ~init:map1 map2 ~f:(fun ~key ~data map1 -> Map.set map1 ~key ~data))
  | fst, _ -> fst
;;

let rec infer_expr env = function
  | Const lt ->
    let* t = infer_lt lt in
    return (Substitution.empty, t)
  | Variable (Ident varname) ->
    (match TypeEnv.find env varname with
     | Some s ->
       let* t = instantiate s in
       return (Substitution.empty, t)
     | None -> fail (`Undef_var varname))
  | Unary_expr (op, e) ->
    let* op_typ =
      match op with
      | Unary_minus -> return int_typ
      | Unary_not -> return bool_typ
    in
    let* e_subst, e_typ = infer_expr env e in
    let* subst = unify op_typ (Substitution.apply e_subst e_typ) in
    let* subst_result = Substitution.compose_all [ e_subst; subst ] in
    return (subst_result, Substitution.apply subst e_typ)
  | Bin_expr (op, e1, e2) ->
    let* subst1, typ1 = infer_expr env e1 in
    let* subst2, typ2 = infer_expr (TypeEnv.apply subst1 env) e2 in
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
        let* fresh_var = make_fresh_var in
        return (fresh_var, fresh_var, bool_typ)
      | Binary_cons ->
        let* fresh_var = make_fresh_var in
        return (fresh_var, Type_list fresh_var, Type_list fresh_var)
    in
    let* subst3 = Substitution.unify (Substitution.apply subst2 typ1) e1typ in
    let* subst4 = Substitution.unify (Substitution.apply subst3 typ2) e2typ in
    let* subst_res = Substitution.compose_all [ subst1; subst2; subst3; subst4 ] in
    return (subst_res, Substitution.apply subst_res etyp)
  | Option None ->
    let* fresh_typ = make_fresh_var in
    return (Substitution.empty, TOption fresh_typ)
  | Option (Some e) ->
    let* subst, typ = infer_expr env e in
    return (subst, TOption typ)
  | Tuple (fst, snd, rest) ->
    let* subst1, typ1 = infer_expr env fst in
    let* subst2, typ2 = infer_expr env snd in
    let* subst_rest, typs_rest =
      List.fold_right
        rest
        ~f:(fun e acc ->
          let* subst_acc, typs = acc in
          let* subst, typ = infer_expr env e in
          let* subst_acc = Substitution.compose subst_acc subst in
          return (subst_acc, typ :: typs))
        ~init:(return (Substitution.empty, []))
    in
    let* subst_result = Substitution.compose_all [ subst1; subst2; subst_rest ] in
    return (subst_result, Type_tuple (typ1, typ2, typs_rest))
  | List [] ->
    let* fresh_var = make_fresh_var in
    return (Substitution.empty, Type_list fresh_var)
  | List (hd :: tl) ->
    let* subst1, typ1 = infer_expr env hd in
    let typ1 = Substitution.apply subst1 typ1 in
    let* subst_unify, typ_unified =
      List.fold
        tl
        ~f:(fun acc e ->
          let* subst_acc, typ_acc = acc in
          let* subst, typ = infer_expr env e in
          let* subst_unify = unify typ_acc typ in
          let typ_acc = Substitution.apply subst_unify typ_acc in
          let* subst_acc = Substitution.compose_all [ subst; subst_acc; subst_unify ] in
          return (subst_acc, typ_acc))
        ~init:(return (subst1, typ1))
    in
    return (subst_unify, Type_list typ_unified)
  | If_then_else (c, th, Some el) ->
    let* subst1, typ1 = infer_expr env c in
    let* subst2, typ2 = infer_expr (TypeEnv.apply subst1 env) th in
    let* subst3, typ3 = infer_expr (TypeEnv.apply subst2 env) el in
    let* subst4 = unify typ1 bool_typ in
    let* subst5 = unify typ2 typ3 in
    let* subst_result =
      Substitution.compose_all [ subst1; subst2; subst3; subst4; subst5 ]
    in
    return (subst_result, Substitution.apply subst5 (unify_act_pat (typ2, typ3)))
  | If_then_else (c, th, None) ->
    let* subst1, typ1 = infer_expr env c in
    let* subst2, typ2 = infer_expr (TypeEnv.apply subst1 env) th in
    let* subst3 = unify typ1 bool_typ in
    let* subst_result = Substitution.compose_all [ subst1; subst2; subst3 ] in
    return (subst_result, Substitution.apply subst2 typ2)
  | Apply (f, arg) ->
    let* subst1, typ1 = infer_expr env f in
    let* subst2, typ2 = infer_expr (TypeEnv.apply subst1 env) arg in
    let typ1 = Substitution.apply subst2 typ1 in
    let* fresh_var = make_fresh_var in
    let* subst3 = unify typ1 (Arrow (typ2, fresh_var)) in
    let* subst_result = Substitution.compose_all [ subst1; subst2; subst3 ] in
    return (subst_result, Substitution.apply subst3 fresh_var)
  | Lambda (arg, args, e) ->
    let* env, arg_types = infer_patterns env ~shadow:true (arg :: args) in
    let* subst, e_type = infer_expr env e in
    return (subst, Substitution.apply subst (arrow_of_types arg_types e_type))
  | LetIn (Rec, let_bind, let_binds, e) ->
    let let_binds = let_bind :: let_binds in
    let* env = extend_env_with_bind_names env let_binds in
    let* env, subst1 = extend_env_with_let_binds env Rec let_binds in
    let* subst2, typ = infer_expr env e in
    let* subst_final = Substitution.compose subst1 subst2 in
    return (subst_final, typ)
  | LetIn (Nonrec, let_bind, let_binds, e) ->
    let* env, subst1 = extend_env_with_let_binds env Nonrec (let_bind :: let_binds) in
    let* subst2, typ = infer_expr env e in
    let* subst_final = Substitution.compose subst1 subst2 in
    return (subst_final, typ)
  | Function ((p1, e1), rest) ->
    let* match_t = make_fresh_var in
    let* return_t = make_fresh_var in
    infer_matching_expr
      env
      ((p1, e1) :: rest)
      Substitution.empty
      match_t
      return_t
      ~with_arg:true
  | Match (e, (p1, e1), rest) ->
    let* subst_init, match_t = infer_expr env e in
    let env = TypeEnv.apply subst_init env in
    let* return_t = make_fresh_var in
    infer_matching_expr env ((p1, e1) :: rest) subst_init match_t return_t ~with_arg:false
  | EConstraint (e, t) ->
    let* subst1, e_type = infer_expr env e in
    let* subst2 = unify e_type (Substitution.apply subst1 t) in
    let* subst_result = Substitution.compose subst1 subst2 in
    return (subst_result, Substitution.apply subst2 e_type)
  | ActPatConstructor (Ident name, body) ->
    let* subst1, body_type = infer_expr env body in
    let (Scheme (_, existing_type)) = TypeEnv.find_exn env name in
    let* subst = unify existing_type body_type in
    let* subst_final = Substitution.compose subst1 subst in
    return (subst_final, TActPat (name, body_type))

and infer_matching_expr env cases subst_init match_t return_t ~with_arg =
  let* subst, return_t =
    List.fold
      cases
      ~init:(return (subst_init, return_t))
      ~f:(fun acc (pat, expr) ->
        let* subst1, return_t = acc in
        let* env, subst2 =
          if with_arg
          then
            let* env, pat = infer_pattern env ~shadow:true pat in
            let* subst2 = unify match_t pat in
            return (env, subst2)
          else infer_match_pattern env ~shadow:true pat match_t
        in
        let* subst12 = Substitution.compose subst1 subst2 in
        let env = TypeEnv.apply subst12 env in
        let* subst3, expr_typ = infer_expr env expr in
        let* subst4 = unify return_t expr_typ in
        let return_type = unify_act_pat (return_t, expr_typ) in
        let* subst = Substitution.compose_all [ subst12; subst3; subst4 ] in
        return (subst, Substitution.apply subst return_type))
  in
  let final_typ =
    if with_arg then Arrow (Substitution.apply subst match_t, return_t) else return_t
  in
  return (subst, final_typ)

and extend_env_with_let_binds env is_rec let_binds =
  List.fold
    let_binds
    ~init:(return (env, Substitution.empty))
    ~f:(fun acc let_bind ->
      let* env, subst_acc = acc in
      let* subst, names_schemes_list = infer_let_bind env is_rec let_bind in
      let env = TypeEnv.extend_many env names_schemes_list in
      let env = TypeEnv.apply subst env in
      let* subst_acc = Substitution.compose subst_acc subst in
      return (env, subst_acc))

and infer_let_bind env is_rec let_bind =
  let* (Let_bind (name, args, e)) = check_let_bind_correctness is_rec let_bind in
  let* env, args_types = infer_patterns env ~shadow:true args in
  let* subst1, rvalue_type = infer_expr env e in
  let bind_type = Substitution.apply subst1 (arrow_of_types args_types rvalue_type) in
  (* If let_bind is recursive, then name was already in environment *)
  let* env, name_type =
    match is_rec with
    | Nonrec -> infer_pattern env ~shadow:true name
    | Rec -> infer_pattern env ~shadow:false name
  in
  let* subst2 = unify (Substitution.apply subst1 name_type) bind_type in
  let* subst = Substitution.compose subst1 subst2 in
  let env = TypeEnv.apply subst env in
  let* names = extract_names_from_pattern name >>| elements in
  let* arg_names = extract_names_from_patterns args >>| elements in
  let names_types = List.map names ~f:(fun n -> n, TypeEnv.find_typ_exn env n) in
  let env = TypeEnv.remove_many env (List.concat [ names; arg_names ]) in
  let names_schemes_list =
    List.map names_types ~f:(fun (name, name_type) -> name, generalize env name_type)
  in
  return (subst, names_schemes_list)
;;

let add_ident env ident ~shadow =
  let* fresh = make_fresh_var in
  let scheme = Scheme (VarSet.empty, fresh) in
  let env, typ =
    if shadow
    then TypeEnv.extend env ident scheme, fresh
    else (
      let typ = TypeEnv.find_typ env ident in
      env, Option.value typ ~default:fresh)
  in
  return (env, typ)
;;

let reconstruct_arrow args return_type =
  List.fold_right args ~init:return_type ~f:(fun arg acc -> Arrow (arg, acc))
;;

let update_pat_types_with_expr_type arg_types names_with_types_list =
  List.map names_with_types_list ~f:(fun (name, return_type) ->
    let new_type = reconstruct_arrow arg_types return_type in
    name, new_type)
;;

let infer_statement env = function
  | Let (rec_flag, let_bind, let_binds) ->
    let let_binds = let_bind :: let_binds in
    let* env =
      match rec_flag with
      | Rec -> extend_env_with_bind_names env let_binds
      | Nonrec -> return env
    in
    let* env, _ = extend_env_with_let_binds env rec_flag let_binds in
    let* bind_names = extract_bind_names_from_let_binds let_binds >>| elements in
    let bind_names_with_types =
      List.fold
        bind_names
        ~init:(Map.empty (module String))
        ~f:(fun map name ->
          match TypeEnv.find_exn env name with
          | Scheme (_, typ) -> Map.set map ~key:name ~data:typ)
    in
    return (env, bind_names_with_types)
  | ActPat (fst, rest, args, body) ->
    (* input all idents and infer args, expr, find type of act pat definition *)
    let* env, _ =
      List.fold_right
        (fst :: rest)
        ~init:(return (env, []))
        ~f:(fun (Ident name) acc ->
          let* old_env, typs = acc in
          let* new_env, typ = add_ident old_env ~shadow:true name in
          return (new_env, typ :: typs))
    in
    let* env, args_types = infer_patterns env ~shadow:true args in
    let* subst1, expr_type = infer_expr env body in
    let env = TypeEnv.apply subst1 env in
    let apat_type = Substitution.apply subst1 (arrow_of_types args_types expr_type) in
    (* form (name, type) pairs for variants and change types so they content args *)
    let variant_names = List.map (fst :: rest) ~f:(function Ident str -> str) in
    let variants_with_types_list =
      List.map variant_names ~f:(fun name ->
        match TypeEnv.find_exn env name with
        | Scheme (_, typ) -> name, typ)
    in
    let arg_types, return_typ = decompose_arrow apat_type in
    (* check that all variants are determined in output *)
    let* _ =
      match return_typ with
      | Choice map
        when Map.length map = List.length variant_names
             && List.exists variant_names ~f:(fun name -> Map.mem map name) -> return ()
      | _ -> fail `Active_pattern_are_not_determined_by_input
    in
    let updated_variants_w_types_list =
      update_pat_types_with_expr_type arg_types variants_with_types_list
    in
    let env =
      List.fold updated_variants_w_types_list ~init:env ~f:(fun env (name, new_typ) ->
        let scheme = generalize env new_typ in
        TypeEnv.extend env name scheme)
    in
    (* delete args from context *)
    let* arg_names = extract_names_from_patterns args >>| elements in
    let env = TypeEnv.remove_many env arg_names in
    (* form result map for printing *)
    let result_name = "|" ^ String.concat ~sep:"|" variant_names ^ "|" in
    let new_res_map = Map.empty (module String) in
    let new_res_map = Map.set new_res_map ~key:result_name ~data:apat_type in
    let names_schemes_list =
      List.map updated_variants_w_types_list ~f:(fun (name, name_type) ->
        name, generalize env name_type)
    in
    let env = TypeEnv.extend_many env names_schemes_list in
    return (env, new_res_map)
;;

let infer_construction env = function
  | Expr exp ->
    let* _, typ = infer_expr env exp in
    return (env, Map.singleton (module String) "-" typ)
  | Statement s ->
    let* env, names_and_types = infer_statement env s in
    return (env, names_and_types)
;;

let infer c env state = run (infer_construction env c) state
