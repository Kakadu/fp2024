(** Copyright 2024-2025, Ruslan Nafikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

(** main program *)


type error_inter =
  | DivisionByZero (** Interpret Errors*)
  | UnboundValue of string
  | UnboundConstructor of string
  | MatchFailure
  | TypeError
  | Unreachable
  | StringOfLengthZero of string
  | EmptyProgram
  | NotImplemented


let pp_error_inter fmt = function
  | DivisionByZero -> Format.fprintf fmt "Exception: Division_by_zero."
  | UnboundValue s -> Format.fprintf fmt "Error: Unbound value %s" s
  | UnboundConstructor s -> Format.fprintf fmt "Error: Unbound constructor %s" s
  | MatchFailure ->
    Format.fprintf fmt "Exception: this pattern-matching is not exhaustive."
  | EmptyProgram -> Format.fprintf fmt "the program was not provided or was empty"
  | TypeError -> Format.fprintf fmt "Error: type mismatch, a different type was expected"
  | Unreachable ->
    Format.fprintf fmt "Error: Unreachable error... Something went wrong..."
  | StringOfLengthZero name -> Format.fprintf fmt "It must not be of length zero: %s" name
  | NotImplemented -> Format.fprintf fmt "This feature has not yet been implemented"
;;

type value =
  | VString of string
  | VBool of bool
  | VInt of int
  | VList of value list
  | VTuple of value list
  | VFun of pattern * expr * (ident * value) list
  | VLetWAPat of ident * value
  | VNil (** [] *)
[@@deriving show { with_path = false }]

module type MonadFail = sig
  include Base.Monad.S2

  val run : ('a, 'e) t -> ok:('a -> ('b, 'e) t) -> err:('e -> ('b, 'e) t) -> ('b, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

let is_constr = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

type environment = (ident, value, String.comparator_witness) Map.t

module Environment (M : MonadFail) = struct
  open M

  let empty = Map.empty (module Base.String)

  let find_val map key =
    if String.length key = 0
    then fail (StringOfLengthZero key)
    else (
      match Map.find map key with
      | Some value -> return value
      | None when is_constr @@ String.get key 0 -> fail (UnboundConstructor key)
      | _ -> fail (UnboundValue key))
  ;;

  let add_bind map key value = Map.update map key ~f:(fun _ -> value)

  let add_binds map binds =
    List.fold ~f:(fun map (k, v) -> add_bind map k v) ~init:map binds
  ;;
end

module Interpret (M : MonadFail) = struct
  open M
  open Environment (M)

  let rec bind_fun_params ?(env = empty) =
    let bind_pat_list patl argl =
      let binded_list =
        List.fold2
          patl
          argl
          ~f:(fun acc pat arg ->
            let* acc = acc in
            let* binding = bind_fun_params ~env (pat, arg) in
            return (acc @ binding))
          ~init:(return [])
      in
      match binded_list with
      | Ok v -> v
      | _ -> fail MatchFailure
    in
    function
    | Pattern_wild, _ -> return []
    | Pattern_const c, app_arg ->
      (match c, app_arg with
       | Const_bool b1, VBool b2 when Bool.equal b1 b2 -> return []
       | Const_int i1, VInt i2 when i1 = i2 -> return []
       | Const_string s1, VString s2 when String.equal s1 s2 -> return []
       | Const_nil, VList v ->
         (match v with
          | [] -> return []
          | _ -> fail MatchFailure)
       | _ -> fail Unreachable)
    | Pattern_id var, app_arg -> return [ var, app_arg ]
    | (Pattern_list _ as pl), VList vl ->
      (match pl, vl with
       | Pattern_list (h, t), hd :: tl ->
         let* evaledhd = bind_fun_params (h, hd) in
         let* evaledtl = bind_fun_params (t, VList tl) in
         return @@ evaledhd @ evaledtl
       | _ -> fail MatchFailure)
    | Pattern_tuple pl, VTuple vl -> bind_pat_list pl vl
    | _ -> fail MatchFailure

  and eval_binding bind env =
    match bind with
    | Let (is_rec, name, body) ->
      if is_rec
      then
        let* func_body = eval body env in
        return @@ VLetWAPat (name, func_body)
      else eval body env
    | Expression expr -> eval expr env

  and eval expr env =
    match expr with
    | Expr_const v ->
      (match v with
       | Const_bool b -> return @@ VBool b
       | Const_int i -> return @@ VInt i
       | Const_string s -> return @@ VString s
       | Const_nil -> return VNil)
    | Expr_bin_op (op, l, r) ->
      let* rigth_val = eval r env in
      let* left_val = eval l env in
      (match op, left_val, rigth_val with
       | Div, VInt _, VInt 0 -> fail DivisionByZero
       | Mod, VInt _, VInt 0 -> fail DivisionByZero
       | Add, VInt l, VInt r -> return @@ VInt (l + r)
       | Sub, VInt l, VInt r -> return @@ VInt (l - r)
       | Mul, VInt l, VInt r -> return @@ VInt (l * r)
       | Div, VInt l, VInt r -> return @@ VInt (l / r)
       | Mod, VInt l, VInt r -> return @@ VInt (l % r)
       | Lt, VInt l, VInt r -> return @@ VBool (l < r)
       | Leq, VInt l, VInt r -> return @@ VBool (l <= r)
       | Gt, VInt l, VInt r -> return @@ VBool (l > r)
       | Geq, VInt l, VInt r -> return @@ VBool (l >= r)
       | Eq, VInt l, VInt r -> return @@ VBool (l = r)
       | Neq, VInt l, VInt r -> return @@ VBool (l <> r)
       | Con, h, VList tl -> return @@ VList (h :: tl)
       | Con, h, VNil -> return @@ VList (h :: [])
       | _ -> fail TypeError)
    | Expr_var id -> find_val env id
    | Expr_list (h, t) ->
      let* evaled = eval h env in
      let rec helper acc expr =
        match expr with
        | Expr_const Const_nil -> acc
        | Expr_list (hd, tl) ->
          let* acc = acc in
          let* evaled = eval hd env in
          helper (return (evaled :: acc)) tl
        | _ ->
          let* acc = acc in
          let* evaled = eval expr env in
          return (evaled :: acc)
      in
      let* res = helper (return [ evaled ]) t in
      let res = VList (List.rev res) in
      return res
    | Expr_tuple t ->
      let* eval_list = all (List.map t ~f:(fun expr -> eval expr env)) in
      return @@ VTuple eval_list
    | Expr_if (cond, e_then, e_else) ->
      let* cond_branch = eval cond env in
      (match cond_branch with
       | VBool b -> eval (if b then e_then else e_else) env
       | _ -> fail TypeError)
    | Expr_fun (pat, expr) -> return @@ VFun (pat, expr, Map.to_alist env)
    | Expr_app (func, arg) ->
      let* fun_to_apply = eval func env in
      let* evaled_arg = eval arg env in
      (match fun_to_apply with
       | VFun (pat, expr, fun_env) ->
         let* res = bind_fun_params ~env (pat, evaled_arg) in
         eval expr (add_binds (add_binds empty fun_env) res)
       | VLetWAPat (name, VFun (pat, expr, fun_env)) ->
         let* res = bind_fun_params ~env (pat, evaled_arg) in
         eval
           expr
           (add_binds
              (add_bind
                 (add_binds empty fun_env)
                 name
                 (VLetWAPat (name, VFun (pat, expr, fun_env))))
              res)
       | _ -> fail TypeError)
    | Expr_match (expr_match, cases) ->
      let* val_match = eval expr_match env in
      let rec eval_match = function
        | (pat, expr) :: cases ->
          run
            (bind_fun_params ~env (pat, val_match))
            ~ok:(fun binds -> eval expr (add_binds env binds))
            ~err:(fun _ -> eval_match cases)
        | [] -> fail MatchFailure
      in
      eval_match cases
    | Expr_let_in (_, name, expr1, expr2) ->
      let* v_let = eval expr1 env in
      let env = add_bind env name v_let in
      eval expr2 env
  ;;

  let eval_program (program : struct_prog list) : (value, error_inter) t =
    let rec helper env = function
      | h :: [] -> eval_binding h env
      | [] -> fail EmptyProgram
      | h :: tl ->
        let* eval_h = eval_binding h env in
        let eval_env =
          match h with
          | Let (_, f, _) -> add_bind env f eval_h
          | _ -> env
        in
        helper eval_env tl
    in
    helper empty program
  ;;
end

module InterpretResult = Interpret (struct
    include Result

    let run x ~ok ~err =
      match x with
      | Ok v -> ok v
      | Error e -> err e
    ;;

    let ( let* ) monad f = bind monad ~f
  end)






