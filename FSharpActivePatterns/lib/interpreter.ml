(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type value =
  | VUnit
  | VInt of int
  | VString of string
  | VBool of bool
  | VTuple of value * value * value list
  | VList of value list
  | VFun of string option * pattern * pattern list * expr * env
  | VFunction of case * case list
  | VOption of value option

and env = (string, value, Base.String.comparator_witness) Base.Map.t

type error =
  [ `Division_by_zero
  | `Match_failure
  | `Type_mismatch
  | `Not_implemented
  | `Unbound_variable of string
  | `Not_allowed_left_hand_side_let_rec
  | `Args_after_not_variable_let
  ]

module R : sig
  type 'a t

  val return : 'a -> 'a t
  val fail : error -> 'a t

  include Base.Monad.Infix with type 'a t := 'a t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( <|> ) : 'a t -> 'a t -> 'a t
end = struct
  open Base
  open Result

  type 'a t = ('a, error) Result.t

  let ( >>| ) x f =
    match x with
    | Ok x -> Ok (f x)
    | Error e -> Error e
  ;;

  let ( >>= ) x f =
    match x with
    | Error x -> Error x
    | Ok a -> f a
  ;;

  let ( <|> ) x y =
    match x with
    | Ok x -> Ok x
    | Error _ -> y
  ;;

  let fail = Base.Result.fail
  let return = Base.Result.return
  let ( let* ) = ( >>= )
  let ( let+ ) = ( >>| )
end

module Env : sig
  val find_err : env -> string -> value R.t
  val extend : env -> string -> value -> env
  val update_exn : env -> string -> f:(value -> value) -> env
  val remove : env -> string -> env
end = struct
  open Base

  let extend mp key data = Map.update mp key ~f:(function _ -> data)

  let update_exn env k ~f =
    let v = Map.find_exn env k in
    extend env k (f v)
  ;;

  let remove = Map.remove

  let find_err env name =
    let open R in
    match Map.find env name with
    | Some v -> return v
    | None ->
      let x = fail (`Unbound_variable name) in
      x
  ;;
end

open R
open Env

let rec match_pattern env =
  let match_pattern_list env pl vl =
    Base.List.fold2_exn
      ~init:(return env)
      ~f:(fun acc pat value ->
        let* acc = acc in
        match_pattern acc (pat, value))
      pl
      vl
  in
  function
  | Wild, _ -> return env
  | PList pl, VList vl when List.length pl = List.length vl ->
    match_pattern_list env pl vl
  | PCons (hd, tl), VList (vhd :: vtl) ->
    match_pattern_list env [ hd; tl ] [ vhd; VList vtl ]
  | PTuple (pfst, psnd, prest), VTuple (vfst, vsnd, vrest) ->
    match_pattern_list env (pfst :: psnd :: prest) (vfst :: vsnd :: vrest)
  | PConst (Int_lt p), VInt v when p = v -> return env
  | PConst (Bool_lt p), VBool v when p = v -> return env
  | PConst (String_lt p), VString v when p = v -> return env
  | PConst Unit_lt, VUnit -> return env
  | PVar (Ident name), v -> return (Env.extend env name v)
  | POption (Some p), VOption (Some v) -> match_pattern env (p, v)
  | POption None, VOption None -> return env
  | PConstraint (p, _), v -> match_pattern env (p, v)
  | _ -> fail `Match_failure
;;

let rec eval_binequal =
  let eval_binequal_list l1 l2 =
    Base.List.fold2_exn
      ~init:(return (VBool true))
      ~f:(fun acc left right ->
        let* acc = acc in
        let* eq_res = eval_binequal (left, right) in
        match acc, eq_res with
        | VBool b1, VBool b2 -> return (VBool (b1 && b2))
        | _ -> fail `Type_mismatch)
      l1
      l2
  in
  function
  | VInt i1, VInt i2 -> return (VBool (i1 = i2))
  | VString s1, VString s2 -> return (VBool (s1 = s2))
  | VBool b1, VBool b2 -> return (VBool (b1 = b2))
  | VUnit, VUnit -> return (VBool true)
  | VList l1, VList l2 when List.length l1 = List.length l2 -> eval_binequal_list l1 l2
  | VTuple (fst1, snd1, rest1), VTuple (fst2, snd2, rest2)
    when List.length rest1 = List.length rest2 ->
    eval_binequal_list (fst1 :: snd1 :: rest1) (fst2 :: snd2 :: rest2)
  | _ -> fail `Type_mismatch
;;

let eval_binexpr op v1 v2 =
  match op, v1, v2 with
  | Binary_equal, v1, v2 -> eval_binequal (v1, v2)
  | Binary_unequal, v1, v2 ->
    let* b = eval_binequal (v1, v2) in
    (match b with
     | VBool b -> return (VBool (not b))
     | _ -> fail `Type_mismatch)
  | Binary_less, VInt v1, VInt v2 -> return (VBool (v1 < v2))
  | Binary_less_or_equal, VInt v1, VInt v2 -> return (VBool (v1 <= v2))
  | Binary_greater, VInt v1, VInt v2 -> return (VBool (v1 > v2))
  | Binary_greater_or_equal, VInt v1, VInt v2 -> return (VBool (v1 >= v2))
  | Binary_add, VInt v1, VInt v2 -> return (VInt (v1 + v2))
  | Binary_subtract, VInt v1, VInt v2 -> return (VInt (v1 - v2))
  | Binary_multiply, VInt v1, VInt v2 -> return (VInt (v1 * v2))
  | Binary_divide, VInt v1, VInt v2 -> return (VInt (v1 / v2))
  | Logical_or, VBool v1, VBool v2 -> return (VBool (v1 || v2))
  | Logical_and, VBool v1, VBool v2 -> return (VBool (v1 && v2))
  | Binary_or_bitwise, VInt v1, VInt v2 -> return (VInt (v1 lor v2))
  | Binary_xor_bitwise, VInt v1, VInt v2 -> return (VInt (v1 lxor v2))
  | Binary_and_bitwise, VInt v1, VInt v2 -> return (VInt (v1 land v2))
  | Binary_cons, v, VList vl -> return (VList (v :: vl))
  | _ -> fail `Type_mismatch
;;

let rec eval_expr env = function
  | Const c ->
    (match c with
     | Unit_lt -> return VUnit
     | Int_lt i -> return (VInt i)
     | String_lt s -> return (VString s)
     | Bool_lt b -> return (VBool b))
  | Tuple (fst, snd, rest) ->
    let* fst_value = eval_expr env fst in
    let* snd_value = eval_expr env snd in
    let* rest_values = eval_expr_fold env rest in
    return (VTuple (fst_value, snd_value, rest_values))
  | List l ->
    let* l_values = eval_expr_fold env l in
    return (VList l_values)
  | Variable (Ident n) -> find_err env n
  | Unary_expr (op, e) ->
    let* v = eval_expr env e in
    (match op, v with
     | Unary_minus, VInt i -> return (VInt (-i))
     | Unary_not, VBool b -> return (VBool (not b))
     | _ -> fail `Type_mismatch)
  | Bin_expr (op, e1, e2) ->
    let* v1 = eval_expr env e1 in
    let* v2 = eval_expr env e2 in
    eval_binexpr op v1 v2
  | If_then_else (c, t, e) ->
    let* c_value = eval_expr env c in
    (match c_value with
     | VBool true -> eval_expr env t
     | VBool false ->
       (match e with
        | None -> return VUnit
        | Some e -> eval_expr env e)
     | _ -> fail `Type_mismatch)
  | Lambda (arg, args, body) -> return (VFun (None, arg, args, body, env))
  | Apply (f, applying_arg) ->
    let* f_value = eval_expr env f in
    let* applying_arg_value = eval_expr env applying_arg in
    (match f_value with
     | VFun (name, arg, args, body, env) ->
       let* env = match_pattern env (arg, applying_arg_value) in
       let env =
         match name with
         | Some name -> Env.extend env name f_value
         | None -> env
       in
       (match args with
        | [] -> eval_expr env body
        | arg1 :: args -> return (VFun (None, arg1, args, body, env)))
     | VFunction (c, cl) -> eval_match env applying_arg_value (c :: cl)
     | _ -> fail `Type_mismatch)
  | Match (e, c, cl) ->
    let* v = eval_expr env e in
    eval_match env v (c :: cl)
  | Function (c, cl) -> return (VFunction (c, cl))
  | LetIn (rec_flag, let_bind, let_binds, e) ->
    let* env = extend_env_with_let_binds env rec_flag (let_bind :: let_binds) in
    let* value = eval_expr env e in
    return value
  | Option None -> return (VOption None)
  | Option (Some e) ->
    let* value = eval_expr env e in
    return (VOption (Some value))
  | EConstraint (e, _) -> eval_expr env e

and eval_expr_fold env l =
  Base.List.fold
    ~init:(return [])
    ~f:(fun acc e ->
      let* acc = acc in
      let* e_value = eval_expr env e in
      return (e_value :: acc))
    l

and eval_match env v = function
  | [] -> fail `Match_failure
  | (pat, expr) :: tl ->
    (let* ext_env = match_pattern env (pat, v) in
     eval_expr ext_env expr)
    <|> eval_match env v tl

and extend_env_with_let_bind env rec_flag = function
  | Let_bind (name, args, body) ->
    (match args with
     | arg1 :: args ->
       (match name with
        | PVar (Ident n) ->
          let value =
            match rec_flag with
            | Rec -> VFun (Some n, arg1, args, body, env)
            | Nonrec -> VFun (None, arg1, args, body, env)
          in
          let* env = match_pattern env (name, value) in
          return (Some n, env)
        | _ -> fail `Args_after_not_variable_let)
     | [] ->
       let n =
         match rec_flag, name with
         | Rec, PVar (Ident n) -> Some n
         | _ -> None
       in
       let* value = eval_expr env body in
       let* env = match_pattern env (name, value) in
       return (n, env))

and extend_env_with_let_binds env rec_flag let_binds =
  let* names, env =
    Base.List.fold
      ~init:(return ([], env))
      ~f:(fun acc let_bind ->
        let* names, env = acc in
        let* name, env = extend_env_with_let_bind env rec_flag let_bind in
        match name with
        | Some name -> return (name :: names, env)
        | None -> return (names, env))
      let_binds
  in
  let env =
    match rec_flag with
    | Rec ->
      Base.List.fold
        ~init:env
        ~f:(fun env name ->
          Env.update_exn env name ~f:(function
            | VFun (n, arg, args, body, _) -> VFun (n, arg, args, body, env)
            | other -> other))
        names
    | Nonrec -> env
  in
  return env
;;

let eval_construction env = function
  | Expr e -> eval_expr env e
  | Statement _ -> fail `Not_implemented
;;
