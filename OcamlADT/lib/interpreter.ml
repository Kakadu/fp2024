(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

type error =
  | DivisionByZero
  | TypeMismatch
  | NotImplemented
  | UnboundVariable of string
  | PatternMismatch
  | IDK

type value =
  | VInt of int
  | VString of string
  | VChar of char
  | VTuple of value List2.t
  | VFun of Pattern.t List1.t * Expression.t
  | VFunction of value (* hz *)

and environment = (string, value, String.comparator_witness) Base.Map.t

let compare_values v1 v2 =
  match v1, v2 with
  | VInt i1, VInt i2 -> i1 = i2
  | VString s1, VString s2 -> Base.String.equal s1 s2
  | VChar c1, VChar c2 -> Base.Char.equal c1 c2
  | _ -> false
;;

let list1_to_list2 lst =
  match lst with
  | el1, el2 :: ell -> Some (el1, el2, ell)
  | _ -> None
;;

module type Error_monad = sig
  (* 'a - successfull value, 'e - error type *)
  type ('a, 'e) t

  (* Wraps in Result type *)
  val return : 'a -> ('a, 'e) t

  (* Monad interface description *)
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

module Env (M : Error_monad) = struct
  (*Environment functor that is used to store the initialized data in some scope.
    It is treated like an abstract mapping (a lil different from one in TypeInf)*)

  open M

  let init = Base.Map.empty (module Base.String)

  let lookup env name =
    match List.Assoc.find ~equal:String.equal env name with
    | Some _ -> return (Some env) (* Возвращаем Option типа с окружением *)
    | None -> fail (UnboundVariable name)
  ;;

  let extend env name value = Base.Map.set env ~key:name ~data:value
end

module Interpreter (M : Error_monad) = struct
  (*Interpretator functor, that uses M monad as a base for evaluation *)
  open M
  module E = Env (M)

  (* mapM applies a monadic function f to each element of a list and combines results in a list *)

  let mapM f env lst =
    let rec aux acc = function
      | [] -> return (List.rev acc)
      | x :: xs ->
        let* res = f env x in
        aux (res :: acc) xs
    in
    aux [] lst
  ;;

  let mapM2 f env lst lst2 =
    let rec aux acc = function
      | [], [] -> return (List.rev acc)
      | x :: xs, y :: ys ->
        let* res = f x y env in
        aux (res :: acc) (xs, ys)
      | _ -> fail PatternMismatch (* In case lists have different lengths *)
    in
    aux [] (lst, lst2)
  ;;

  let eval_const = function
    | Constant.Const_integer i -> return (VInt i)
    | Constant.Const_char c -> return (VChar c)
    | Constant.Const_string s -> return (VString s)
  ;;

  let rec eval_pattern pattern value env =
    match pattern, value with
    | Pattern.Pat_any, _ -> return (Some env)
    | Pattern.Pat_var var, v ->
      let env = E.extend env var v in
      return (Some env)
    | Pattern.Pat_constant c, v ->
      let* const_val = eval_const c in
      if compare_values const_val v then return (Some env) else fail PatternMismatch
    | Pattern.Pat_tuple (p1, p2, ps), VTuple (v1, v2, vs) ->
      let* _ = eval_pattern p1 v1 env in
      let* _ = eval_pattern p2 v2 env in
      let* _ = mapM2 eval_pattern env ps vs in
      return (Some env)
    | Pattern.Pat_constraint (pat, _), v -> eval_pattern pat v env
    | Pattern.Pat_construct (ctor, Some args), VFun (ctor_pat, body) -> fail IDK
    | Pattern.Pat_construct (ctor, None), VString s ->
      if String.equal ctor s then return (Some env) else fail PatternMismatch
    | _ -> fail PatternMismatch
  ;;

  let rec eval_expr (env : environment) = function
    | Expression.Exp_ident _ -> fail IDK
    | Expression.Exp_constant ex -> eval_const ex
    | Expression.Exp_tuple (e1, e2, el) ->
      let* v1 = eval_expr env e1 in
      let* v2 = eval_expr env e2 in
      let* vl = mapM eval_expr env el in
      return (VTuple (v1, v2, vl))
    | Expression.Exp_function (c1, cl) ->
      fail IDK
      (*let* v1 = eval_case c1 in
        let* vl = mapM eval_case cl in
        return (VFunction (v1, vl)) *)
    | Expression.Exp_fun (patterns, body) -> return (VFun (patterns, body))
    | Expression.Exp_apply (func, arg) ->
      let* func_val = eval_expr env func in
      let* arg_val = eval_expr env arg in
      (match func_val with
       | VFun (patterns, body) ->
         (match list1_to_list2 patterns with
          | Some (el1, el2, ell) ->
            let pattern = Pattern.Pat_tuple (el1, el2, ell) in
            let* env' = eval_pattern pattern arg_val env in
            (match env' with
             | None -> fail IDK
             | Some env' -> eval_expr env' body)
          | None -> fail NotImplemented)
       | _ -> fail TypeMismatch)
    | Expression.Exp_match (expr, cases) ->
      let c1, cl = cases in
      let* v = eval_expr env expr in
      let rec eval_cases = function
        | [] -> fail PatternMismatch
        | { Expression.first = pattern; second = body } :: rest ->
          let* env' = eval_pattern pattern v env in
          (match env' with
           | Some env' -> eval_expr env' body
           | _ -> eval_cases rest)
      in
      eval_cases (c1 :: cl)
    | Expression.Exp_if (cond, then_expr, else_expr_opt) ->
      let* cond_val = eval_expr env cond in
      (match cond_val with
       | VInt 0 ->
         (match else_expr_opt with
          | Some else_expr -> eval_expr env else_expr
          | None -> fail PatternMismatch)
       | VInt _ -> eval_expr env then_expr
       | _ -> fail TypeMismatch)
    | Expression.Exp_let (_, bindings, body) ->
      (* Extend the environment based on the bindings *)
      let extend_env env bindings =
        List.fold bindings ~init:(return env) ~f:(fun acc { Expression.pat; expr } ->
          let* env = acc in
          let* value = eval_expr env expr in
          let* env' = eval_pattern pat value env in
          match env' with
          | Some new_env -> return new_env
          | None -> fail PatternMismatch)
      in
      let bindings_list = fst bindings :: snd bindings in
      let* env' = extend_env env bindings_list in
      eval_expr env' body
    | Expression.Exp_construct (ctor, Some arg) ->
      fail IDK
      (*let* v = eval_expr env arg in
        return (VFun ([ Pattern.Pat_constant (Constant.Const_string ctor) ], v)) *)
    | Expression.Exp_construct (ctor, None) -> return (VString ctor)
    | Expression.Exp_constraint (expr, _type_expr) -> eval_expr env expr
  ;;

  let eval_str_item (env : environment) = function
    | Structure.Str_eval str ->
      let* _ = eval_expr env str in
      return env
    | Structure.Str_value (rec_flag, bindings) ->
      let bindings_list = fst bindings :: snd bindings in
      let extend_env env bindings =
        List.fold bindings ~init:(return env) ~f:(fun acc { Expression.pat; expr } ->
          let* env = acc in
          let* value =
            match rec_flag with
            | Nonrecursive -> eval_expr env expr
            | Recursive -> fail IDK
            (* For recursive bindings, create a placeholder for recursive definitions *)
            (*              let placeholder = VFun ((pat, []), expr) in
                            let temp_env = E.extend env pat placeholder in
                            eval_expr temp_env expr *)
          in
          let* env' = eval_pattern pat value env in
          match env' with
          | Some new_env -> return new_env
          | None -> fail PatternMismatch)
      in
      extend_env env bindings_list
  ;;

  (*to add adt*)

  let interpret_expr expr = eval_expr E.init expr

  let interpret_program (prog : program) =
    let rec eval_prog env = function
      | [] -> return ()
      | item :: rest ->
        let* new_env = eval_str_item env item in
        eval_prog new_env rest
    in
    eval_prog E.init prog
  ;;
end

module RESULT_MONAD_ERROR = struct
  (* Basic Result monad extension.
     Result is used for more advanced error handling *)
  include Result

  let ( let* ) m f = m >>= fun x -> f x
end

(* Interpreter functor extension *)
module InterpreterWResult = Interpreter (RESULT_MONAD_ERROR)

let run_interpreter = InterpreterWResult.interpret_program
