(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Stdlib.Format

type env = (ident, value, String.comparator_witness) Map.t

and value =
  | ValueInt of int
  | ValueBool of bool
  | ValueString of string
  | ValueUnit
  | ValueClosure of is_rec * pattern * pattern list * expr * env
  | ValueTuple of value * value * value list
  | ValueList of value list
  | ValueOption of value option
  | ValueBuiltin of (value -> (value, value_error) Result.t)

and value_error =
  | UnboundVariable of ident
  | TypeError
  | DivisionByZeroError
  | PatternMatchingError
  | LHS

let pp_value_error fmt = function
  | UnboundVariable ident -> fprintf fmt "UnboundVariable: %S" ident
  | TypeError -> fprintf fmt "TypeError"
  | DivisionByZeroError -> fprintf fmt "DivisionByZeroError"
  | PatternMatchingError -> fprintf fmt "PatternMatchingError"
  | LHS -> fprintf fmt "LeftHandSide"
;;

module type Monad = sig
  type ('a, 'e) t

  val return : 'a -> ('a, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

module Env (M : Monad) = struct
  open M

  let extend env key value = Map.update env key ~f:(fun _ -> value)

  let find map key =
    match Map.find map key with
    | Some value -> return value
    | None -> fail (UnboundVariable key)
  ;;
end

module Eval (M : Monad) : sig
  val eval_structure : program -> (env, value_error) M.t
end = struct
  open M
  open Env (M)

  let initial_env =
    let open Base.Map in
    empty (module String)
    |> set
         ~key:"print_int"
         ~data:
           (ValueBuiltin
              (function
                | ValueInt i ->
                  Stdlib.print_int i;
                  Stdlib.print_newline ();
                  Result.return ValueUnit
                | _ -> Result.fail TypeError))
    |> set
         ~key:"print_endline"
         ~data:
           (ValueBuiltin
              (function
                | ValueString s ->
                  Stdlib.print_endline s;
                  Result.return ValueUnit
                | _ -> Result.fail TypeError))
    |> set
         ~key:"print_bool"
         ~data:
           (ValueBuiltin
              (function
                | ValueBool b ->
                  Stdlib.print_string (Bool.to_string b);
                  Stdlib.print_newline ();
                  Result.return ValueUnit
                | _ -> Result.fail TypeError))
  ;;

  let rec check_match env = function
    | PatAny, _ -> Some env
    | PatUnit, ValueUnit -> Some env
    | PatConst (ConstInt i1), ValueInt i2 when i1 = i2 -> Some env
    | PatConst (ConstBool b1), ValueBool b2 when Bool.equal b1 b2 -> Some env
    | PatConst (ConstString s1), ValueString s2 when String.equal s1 s2 -> Some env
    | PatVariable x, v -> Some (extend env x v)
    | PatType (pat, _), v -> check_match env (pat, v)
    | PatTuple (p1, p2, pl), ValueTuple (v1, v2, vl) ->
      (match check_match env (p1, v1) with
       | None -> None
       | Some env1 ->
         (match check_match env1 (p2, v2) with
          | None -> None
          | Some env2 ->
            (match
               List.fold2 pl vl ~init:(Some env2) ~f:(fun acc_env p v ->
                 match acc_env with
                 | Some env' -> check_match env' (p, v)
                 | None -> None)
             with
             | Ok result -> result
             | Unequal_lengths -> None)))
    | PatList patterns, ValueList values when List.length patterns = List.length values ->
      let rec match_lists env pat_list val_list =
        match pat_list, val_list with
        | [], [] -> Some env
        | p :: ps, v :: vs ->
          (match check_match env (p, v) with
           | Some new_env -> match_lists new_env ps vs
           | None -> None)
        | _ -> None
      in
      match_lists env patterns values
    | PatOption p, ValueOption v ->
      (match p, v with
       | Some p, Some v -> check_match env (p, v)
       | None, None -> Some env
       | _ -> None)
    | _ -> None
  ;;

  let eval_un_op = function
    | Negative, ValueInt i -> return (ValueInt (-i))
    | Not, ValueBool b -> return (ValueBool (not b))
    | _ -> fail TypeError
  ;;

  let eval_binop (bop, v1, v2) =
    match bop, v1, v2 with
    | Multiply, ValueInt x, ValueInt y -> return (ValueInt (x * y))
    | Division, ValueInt _, ValueInt y when y = 0 -> fail DivisionByZeroError
    | Division, ValueInt x, ValueInt y -> return (ValueInt (x / y))
    | Plus, ValueInt x, ValueInt y -> return (ValueInt (x + y))
    | Minus, ValueInt x, ValueInt y -> return (ValueInt (x - y))
    | Equal, ValueInt x, ValueInt y -> return (ValueBool (x = y))
    | NotEqual, ValueInt x, ValueInt y -> return (ValueBool (x <> y))
    | LowerThan, ValueInt x, ValueInt y -> return (ValueBool (x < y))
    | LowestEqual, ValueInt x, ValueInt y -> return (ValueBool (x <= y))
    | GreaterThan, ValueInt x, ValueInt y -> return (ValueBool (x > y))
    | GretestEqual, ValueInt x, ValueInt y -> return (ValueBool (x >= y))
    | And, ValueBool x, ValueBool y -> return (ValueBool (x && y))
    | Or, ValueBool x, ValueBool y -> return (ValueBool (x || y))
    | _ -> fail TypeError
  ;;

  let rec eval_expr env = function
    | ExpConst c ->
      (match c with
       | ConstInt i -> return (ValueInt i)
       | ConstBool b -> return (ValueBool b)
       | ConstString s -> return (ValueString s))
    | ExpIdent x -> find env x
    | ExpUnarOper (op, e) ->
      let* v = eval_expr env e in
      eval_un_op (op, v)
    | ExpBinOper (op, e1, e2) ->
      let* v1 = eval_expr env e1 in
      let* v2 = eval_expr env e2 in
      eval_binop (op, v1, v2)
    | ExpBranch (cond, then_expr, else_expr_opt) ->
      let* cond_value = eval_expr env cond in
      (match cond_value with
       | ValueBool true -> eval_expr env then_expr
       | ValueBool false ->
         (match else_expr_opt with
          | Some else_expr -> eval_expr env else_expr
          | None -> return ValueUnit)
       | _ -> fail TypeError)
    | ExpLet (false, (PatList patterns, e1), _, e2) ->
      let check_list_pattern = function
        | PatVariable _ | PatAny | PatUnit | PatOption (Some (PatVariable _)) -> true
        | _ -> false
      in
      if not (List.for_all patterns ~f:check_list_pattern)
      then fail LHS
      else
        let* v = eval_expr env e1 in
        (match check_match env (PatList patterns, v) with
         | Some env' -> eval_expr env' e2
         | None -> fail PatternMatchingError)
    | ExpLet (false, (PatTuple (p1, p2, rest), e1), _, e2) ->
      let check_tuple_pattern = function
        | PatVariable _ | PatAny | PatUnit | PatOption (Some (PatVariable _)) -> true
        | _ -> false
      in
      if not (List.for_all ~f:check_tuple_pattern (p1 :: p2 :: rest))
      then fail LHS
      else
        let* v = eval_expr env e1 in
        (match check_match env (PatTuple (p1, p2, rest), v) with
         | Some env' -> eval_expr env' e2
         | None -> fail PatternMatchingError)
    | ExpLet (false, (pat, e1), _, e2) ->
      let check_simple_pattern =
        match pat with
        | PatAny | PatVariable _ | PatUnit | PatOption (Some (PatVariable _)) -> true
        | _ -> false
      in
      if not check_simple_pattern
      then fail LHS
      else
        let* v = eval_expr env e1 in
        (match check_match env (pat, v) with
         | Some env' -> eval_expr env' e2
         | None -> fail PatternMatchingError)
    | ExpLet (true, (pat, e1), [], e2) ->
      (match pat with
       | PatVariable _ ->
         let* v = eval_expr env e1 in
         let* rec_env =
           match check_match env (pat, v) with
           | Some new_env -> return new_env
           | None -> fail PatternMatchingError
         in
         let* recursive_value =
           match v with
           | ValueClosure (_, p, pl, e, _) ->
             return (ValueClosure (true, p, pl, e, rec_env))
           | _ -> fail TypeError
         in
         let* final_env =
           match check_match env (pat, recursive_value) with
           | Some updated_env -> return updated_env
           | None -> fail PatternMatchingError
         in
         eval_expr final_env e2
       | _ -> fail LHS)
    | ExpLet (true, value_binding, value_bindings, e2) ->
      let bindings = List.map ~f:(fun (p, e) -> p, e) (value_binding :: value_bindings) in
      let rec update_env acc_env = function
        | [] -> return acc_env
        | (PatVariable name, expr) :: tl ->
          let* value =
            match expr with
            | ExpLambda (patterns, e) ->
              let head = Option.value_exn (List.hd patterns) in
              let tail = Option.value_exn (List.tl patterns) in
              return (ValueClosure (true, head, tail, e, acc_env))
            | _ -> eval_expr acc_env expr
          in
          let updated_env = extend acc_env name value in
          update_env updated_env tl
        | _ -> fail LHS
      in
      let* final_env = update_env env bindings in
      eval_expr final_env e2
    | ExpTuple (e1, e2, es) ->
      let* v1 = eval_expr env e1 in
      let* v2 = eval_expr env e2 in
      let* vs =
        List.fold_right es ~init:(return []) ~f:(fun e acc ->
          let* acc = acc in
          let* v = eval_expr env e in
          return (v :: acc))
      in
      return (ValueTuple (v1, v2, vs))
    | ExpLambda (patterns, e) ->
      let head = Option.value_exn (List.hd patterns) in
      let tail = Option.value_exn (List.tl patterns) in
      return (ValueClosure (false, head, tail, e, env))
    | ExpTypeAnnotation (e, _) -> eval_expr env e
    | ExpFunction (e1, e2) ->
      let* v1 = eval_expr env e1 in
      let* v2 = eval_expr env e2 in
      (match v1 with
       | ValueBuiltin f ->
         (match f v2 with
          | Ok result -> return result
          | Error err -> fail err)
       | ValueClosure (_, pat, pats, body, func_env) ->
         (match check_match func_env (pat, v2) with
          | Some extended_env ->
            let env' =
              Map.fold extended_env ~init:env ~f:(fun ~key ~data acc_env ->
                Map.update acc_env key ~f:(fun _ -> data))
            in
            (match pats with
             | [] -> eval_expr env' body
             | p :: pl -> return (ValueClosure (false, p, pl, body, env')))
          | None -> fail PatternMatchingError)
       | _ -> fail TypeError)
    | ExpList el ->
      let rec eval_list_elements env = function
        | [] -> return []
        | e :: es ->
          let* v = eval_expr env e in
          let* vs = eval_list_elements env es in
          return (v :: vs)
      in
      let* vl = eval_list_elements env el in
      return (ValueList vl)
    | ExpOption opt ->
      let* value =
        match opt with
        | Some expr ->
          let* v = eval_expr env expr in
          return (Some v)
        | None -> return None
      in
      return (ValueOption value)
  ;;

  let eval_str_item env = function
    | SEval expr ->
      let* _ = eval_expr env expr in
      return env
    | SValue (false, (PatList patterns, e), _) ->
      let check_list_pattern = function
        | PatVariable _ | PatAny | PatUnit | PatOption (Some (PatVariable _)) -> true
        | _ -> false
      in
      if not (List.for_all ~f:check_list_pattern patterns)
      then fail LHS
      else
        let* v = eval_expr env e in
        (match check_match env (PatList patterns, v) with
         | Some env' -> return env'
         | None -> fail PatternMatchingError)
    | SValue (false, (PatTuple (p1, p2, rest), e), _) ->
      let check_tuple_pattern = function
        | PatVariable _ | PatAny | PatUnit | PatOption (Some (PatVariable _)) -> true
        | _ -> false
      in
      if not (List.for_all ~f:check_tuple_pattern (p1 :: p2 :: rest))
      then fail LHS
      else
        let* v = eval_expr env e in
        (match check_match env (PatTuple (p1, p2, rest), v) with
         | Some env' -> return env'
         | None -> fail PatternMatchingError)
    | SValue (false, (pattern, expr), _) ->
      let check_simple_pattern =
        match pattern with
        | PatAny | PatVariable _ | PatUnit | PatOption (Some (PatVariable _)) -> true
        | _ -> false
      in
      if not check_simple_pattern
      then fail LHS
      else
        let* v = eval_expr env expr in
        (match check_match env (pattern, v) with
         | Some env' -> return env'
         | None -> fail PatternMatchingError)
    | SValue (true, ((PatVariable _ as pattern), expr), []) ->
      let* v = eval_expr env expr in
      let* rec_env =
        match check_match env (pattern, v) with
        | Some new_env -> return new_env
        | None -> fail PatternMatchingError
      in
      let* recursive_value =
        match v with
        | ValueClosure (_, p, pl, expr, _) ->
          return (ValueClosure (true, p, pl, expr, rec_env))
        | _ -> fail TypeError
      in
      let* final_env =
        match check_match env (pattern, recursive_value) with
        | Some updated_env -> return updated_env
        | None -> fail PatternMatchingError
      in
      return final_env
    | SValue (true, _, []) -> fail LHS
    | SValue (true, value_binding, value_bindings) ->
      let bindings = value_binding :: value_bindings in
      let rec update_env acc_env = function
        | [] -> return acc_env
        | (PatVariable name, expr) :: tl ->
          let* value =
            match expr with
            | ExpLambda (patterns, expr) ->
              let head = Option.value_exn (List.hd patterns) in
              let tail = Option.value_exn (List.tl patterns) in
              return (ValueClosure (true, head, tail, expr, acc_env))
            | _ -> eval_expr acc_env expr
          in
          let updated_env = extend acc_env name value in
          update_env updated_env tl
        | _ -> fail LHS
      in
      let* final_env = update_env env bindings in
      return final_env
  ;;

  let eval_structure structure =
    List.fold_left structure ~init:(return initial_env) ~f:(fun env str_item ->
      let* env = env in
      let* env = eval_str_item env str_item in
      return env)
  ;;
end

module Inter = Eval (struct
    include Result

    let ( let* ) m f = bind m ~f
  end)
