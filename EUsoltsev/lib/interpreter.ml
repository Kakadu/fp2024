(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format

type builtin =
  | BInt of (int -> unit)
  | BString of (string -> unit)
  | BBool of (bool -> unit)

type value =
  | ValueInt of int
  | ValueBool of bool
  | ValueString of string
  | ValueUnit
  | ValueList of value list
  | ValueTuple of value * value * value list
  | ValueClosure of is_rec * pattern * pattern list * expr * env
  | ValueOption of value option
  | ValueBuiltin of builtin * env
  | ValueFunction of case * case list

and env = (string, value, Base.String.comparator_witness) Base.Map.t

let rec pp_value ppf = function
  | ValueInt x -> fprintf ppf "%d" x
  | ValueBool b -> fprintf ppf "%b" b
  | ValueString s -> fprintf ppf "%S" s
  | ValueUnit -> fprintf ppf "()"
  | ValueTuple (v1, v2, vl) ->
    fprintf
      ppf
      "(%a, %a%a)"
      pp_value
      v1
      pp_value
      v2
      (fun ppf -> function
        | [] -> ()
        | l ->
          fprintf
            ppf
            ", %a"
            (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_value)
            l)
      vl
  | ValueList vl ->
    fprintf
      ppf
      "[%a]"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "; ") pp_value)
      vl
  | ValueClosure _ -> fprintf ppf "<fun>"
  | ValueBuiltin _ -> fprintf ppf "<builtin>"
  | ValueOption opt ->
    (match opt with
     | Some v -> fprintf ppf "Some %a" pp_value v
     | None -> fprintf ppf "None")
  | ValueFunction _ -> fprintf ppf "<fun>"
;;

type value_error =
  | UnboundVariable of ident
  | TypeError
  | DivisionByZeroError
  | PatternMatchingError
  | NotImplemented
  | LeftHandSide

let pp_value_error fmt = function
  | UnboundVariable ident -> fprintf fmt "UnboundVariable: %S" ident
  | TypeError -> fprintf fmt "TypeError"
  | DivisionByZeroError -> fprintf fmt "DivisionByZeroError"
  | PatternMatchingError -> fprintf fmt "PatternMatchingError"
  | NotImplemented -> fprintf fmt "NotImplemented"
  | LeftHandSide -> fprintf fmt "LeftHandSide"
;;

module type MONAD = sig
  type ('a, 'e) t

  val return : 'a -> ('a, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

module Env (M : MONAD) = struct
  open M

  let empty = Base.Map.empty (module Base.String)

  let find map key =
    match Base.Map.find map key with
    | Some value -> return value
    | None -> fail (UnboundVariable key)
  ;;

  let extend env key value = Base.Map.update env key ~f:(fun _ -> value)
end

module Eval (M : MONAD) : sig
  val eval_structure : program -> (env, value_error) M.t
end = struct
  open M
  open Env (M)

  let initial_env =
    let open Base.Map in
    empty (module Base.String)
    |> set
         ~key:"print_int"
         ~data:
           (ValueBuiltin
              ( BInt
                  (fun i ->
                    Stdlib.print_int i;
                    Stdlib.print_newline ())
              , empty (module Base.String) ))
    |> set
         ~key:"print_endline"
         ~data:
           (ValueBuiltin
              (BString (fun s -> Stdlib.print_endline s), empty (module Base.String)))
    |> set
         ~key:"print_bool"
         ~data:
           (ValueBuiltin
              ( BBool
                  (fun b ->
                    Stdlib.print_string (if b then "true" else "false");
                    Stdlib.print_newline ())
              , empty (module Base.String) ))
  ;;

  let rec check_match env = function
    | PatAny, _ -> Some env
    | PatConst ConstUnit, ValueUnit -> Some env
    | PatConst (ConstInt i1), ValueInt i2 when i1 = i2 -> Some env
    | PatConst (ConstBool b1), ValueBool b2 when Bool.equal b1 b2 -> Some env
    | PatConst (ConstString s1), ValueString s2 when String.equal s1 s2 -> Some env
    | PatVariable x, v -> Some (extend env x v)
    | PatList patterns, ValueList values ->
      if List.length patterns <> List.length values
      then None
      else (
        let rec match_lists env pat_list val_list =
          match pat_list, val_list with
          | [], [] -> Some env
          | p :: ps, v :: vs ->
            (match check_match env (p, v) with
             | Some new_env -> match_lists new_env ps vs
             | None -> None)
          | _ -> None
        in
        match_lists env patterns values)
    | PatTuple (p1, p2, pl), ValueTuple (v1, v2, vl) ->
      (match check_match env (p1, v1) with
       | None -> None
       | Some env1 ->
         (match check_match env1 (p2, v2) with
          | None -> None
          | Some env2 ->
            (match
               Base.List.fold2 pl vl ~init:(Some env2) ~f:(fun acc_env p v ->
                 match acc_env with
                 | Some env' -> check_match env' (p, v)
                 | None -> None)
             with
             | Ok result -> result
             | Unequal_lengths -> None)))
    | PatCons (p1, p2), ValueList (v1 :: v2) ->
      (match check_match env (p1, v1) with
       | Some env' -> check_match env' (p2, ValueList v2)
       | None -> None)
    | PatOption p, ValueOption v ->
      (match p, v with
       | Some p, Some v -> check_match env (p, v)
       | None, None -> Some env
       | _ -> None)
    | _ -> None
  ;;

  let eval_un_op = function
    | Negative, ValueInt i -> return (ValueInt (-i))
    | Positive, ValueInt i -> return (ValueInt i)
    | Not, ValueBool b -> return (ValueBool (not b))
    | _ -> fail TypeError
  ;;

  let eval_bin_op = function
    | Multiply, ValueInt x, ValueInt y -> return (ValueInt (x * y))
    | Division, ValueInt _, ValueInt y when y = 0 -> fail DivisionByZeroError
    | Division, ValueInt x, ValueInt y -> return (ValueInt (x / y))
    | Plus, ValueInt x, ValueInt y -> return (ValueInt (x + y))
    | Minus, ValueInt x, ValueInt y -> return (ValueInt (x - y))
    | Equal, ValueInt i1, ValueInt i2 -> return (ValueBool (i1 = i2))
    | NotEqual, ValueInt i1, ValueInt i2 -> return (ValueBool (i1 <> i2))
    | Cons, v, ValueList vl -> return (ValueList (v :: vl))
    | GreaterThan, ValueInt i1, ValueInt i2 -> return (ValueBool (i1 > i2))
    | LowerThan, ValueInt i1, ValueInt i2 -> return (ValueBool (i1 < i2))
    | GretestEqual, ValueInt i1, ValueInt i2 -> return (ValueBool (i1 >= i2))
    | LowestEqual, ValueInt i1, ValueInt i2 -> return (ValueBool (i1 <= i2))
    | And, ValueBool b1, ValueBool b2 -> return (ValueBool (b1 && b2))
    | Or, ValueBool b1, ValueBool b2 -> return (ValueBool (if b1 then true else b2))
    | _ -> fail TypeError
  ;;

  let rec eval_expr env = function
    | ExpConst c ->
      (match c with
       | ConstInt i -> return (ValueInt i)
       | ConstString s -> return (ValueString s)
       | ConstBool b -> return (ValueBool b)
       | ConstUnit -> return ValueUnit)
    | ExpIdent x -> find env x
    | ExpBranch (cond, then_expr, else_expr_opt) ->
      let* cond_value = eval_expr env cond in
      (match cond_value with
       | ValueBool true -> eval_expr env then_expr
       | ValueBool false ->
         (match else_expr_opt with
          | Some else_expr -> eval_expr env else_expr
          | None -> return ValueUnit)
       | _ -> fail TypeError)
    | ExpOption opt ->
      let* value =
        match opt with
        | Some e ->
          let* v = eval_expr env e in
          return (Some v)
        | None -> return None
      in
      return (ValueOption value)
    | ExpTuple (e1, e2, es) ->
      let* v1 = eval_expr env e1 in
      let* v2 = eval_expr env e2 in
      let* vs =
        Base.List.fold_right es ~init:(return []) ~f:(fun e acc ->
          let* acc = acc in
          let* v = eval_expr env e in
          return (v :: acc))
      in
      return (ValueTuple (v1, v2, vs))
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
    | ExpBinOper (op, e1, e2) ->
      let* v1 = eval_expr env e1 in
      let* v2 = eval_expr env e2 in
      eval_bin_op (op, v1, v2)
    | ExpUnarOper (op, e) ->
      let* v = eval_expr env e in
      eval_un_op (op, v)
    | ExpMatch (e, c, cl) ->
      let* v = eval_expr env e in
      let rec match_cases cases =
        match cases with
        | [] -> fail PatternMatchingError
        | ExpCase (pat, expr) :: rest ->
          (match check_match env (pat, v) with
           | Some env' -> eval_expr env' expr
           | None -> match_cases rest)
      in
      match_cases (c :: cl)
    | ExpFunction (patterns, body) -> return (ValueFunction (patterns, body))
    | ExpLambda (tpl, e) ->
      let patterns = List.map (fun (pat, _) -> pat) tpl in
      return (ValueClosure (false, List.hd patterns, List.tl patterns, e, env))
    | ExpApply (e1, e2) ->
      let* v1 = eval_expr env e1 in
      let* v2 = eval_expr env e2 in
      (match v1 with
       | ValueClosure (_, pat, pats, body, func_env) ->
         (match check_match func_env (pat, v2) with
          | Some extended_env ->
            let env' =
              Base.Map.fold extended_env ~init:env ~f:(fun ~key ~data acc_env ->
                Base.Map.update acc_env key ~f:(fun _ -> data))
            in
            (match pats with
             | [] -> eval_expr env' body
             | p :: pl -> return (ValueClosure (false, p, pl, body, env')))
          | None -> fail PatternMatchingError)
       | ValueFunction (case, case_l) ->
         let rec match_cases cases =
           match cases with
           | [] -> fail PatternMatchingError
           | ExpCase (pat, expr) :: rest ->
             (match check_match env (pat, v2) with
              | Some env' -> eval_expr env' expr
              | None -> match_cases rest)
         in
         match_cases (case :: case_l)
       | ValueBuiltin (builtin, _) ->
         (match builtin, v2 with
          | BInt b, ValueInt i ->
            b i;
            return ValueUnit
          | BString b, ValueString s ->
            b s;
            return ValueUnit
          | _ -> fail TypeError)
       | _ -> fail TypeError)
    | ExpLet (false, ExpValueBind ((pat, _), e1), _, e2) ->
      if not
           (match pat with
            | PatAny | PatVariable _
            | PatConst ConstUnit
            | PatOption (Some (PatVariable _)) -> true
            | PatTuple (p1, p2, rest) ->
              List.for_all
                (fun p ->
                  match p with
                  | PatVariable _ | PatAny
                  | PatConst ConstUnit
                  | PatOption (Some (PatVariable _)) -> true
                  | _ -> false)
                (p1 :: p2 :: rest)
            | _ -> false)
      then fail LeftHandSide
      else
        let* v = eval_expr env e1 in
        (match check_match env (pat, v) with
         | Some env' -> eval_expr env' e2
         | None -> fail PatternMatchingError)
    | ExpLet (true, ExpValueBind ((pat, _), e1), [], e2) ->
      if not
           (match pat with
            | PatVariable _ -> true
            | _ -> false)
      then fail LeftHandSide
      else
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
    | ExpLet (true, value_binding, value_bindings, e2) ->
      let bindings =
        List.map (fun (ExpValueBind ((p, _), e)) -> p, e) (value_binding :: value_bindings)
      in
      let rec update_env acc_env = function
        | [] -> return acc_env
        | (PatVariable name, expr) :: tl ->
          let* value =
            match expr with
            | ExpLambda (tpl, e) ->
              let patterns = List.map (fun (pat, _) -> pat) tpl in
              return (ValueClosure (true, List.hd patterns, List.tl patterns, e, acc_env))
            | _ -> eval_expr acc_env expr
          in
          let updated_env = extend acc_env name value in
          update_env updated_env tl
        | _ -> fail LeftHandSide
      in
      let* final_env = update_env env bindings in
      eval_expr final_env e2
    | ExpConstrant (e, _) -> eval_expr env e
  ;;

  let eval_str_item env = function
    | SEval e ->
      let* _ = eval_expr env e in
      return env
    | SValue (false, ExpValueBind ((pat, _), e), _) ->
      if not
           (match pat with
            | PatAny | PatVariable _
            | PatConst ConstUnit
            | PatOption (Some (PatVariable _)) -> true
            | PatTuple (p1, p2, rest) ->
              List.for_all
                (fun p ->
                  match p with
                  | PatVariable _ | PatAny
                  | PatConst ConstUnit
                  | PatOption (Some (PatVariable _)) -> true
                  | _ -> false)
                (p1 :: p2 :: rest)
            | _ -> false)
      then fail LeftHandSide
      else
        let* v = eval_expr env e in
        (match check_match env (pat, v) with
         | Some env' -> return env'
         | None -> fail PatternMatchingError)
    | SValue (true, ExpValueBind ((pat, _), e), []) ->
      if not
           (match pat with
            | PatVariable _ -> true
            | _ -> false)
      then fail LeftHandSide
      else
        let* v = eval_expr env e in
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
        return final_env
    | SValue (true, value_binding, value_bindings) ->
      let bindings =
        List.map (fun (ExpValueBind ((p, _), e)) -> p, e) (value_binding :: value_bindings)
      in
      let rec update_env acc_env = function
        | [] -> return acc_env
        | (PatVariable name, expr) :: tl ->
          let* value =
            match expr with
            | ExpLambda (tpl, e) ->
              let patterns = List.map (fun (pat, _) -> pat) tpl in
              return (ValueClosure (true, List.hd patterns, List.tl patterns, e, acc_env))
            | _ -> eval_expr acc_env expr
          in
          let updated_env = extend acc_env name value in
          update_env updated_env tl
        | _ -> fail LeftHandSide
      in
      let* final_env = update_env env bindings in
      return final_env
  ;;

  let eval_structure structure =
    List.fold_left
      (fun env str_item ->
        let* env = env in
        let* env = eval_str_item env str_item in
        return env)
      (return initial_env)
      structure
  ;;
end

module Interpreter = Eval (struct
    include Base.Result

    let ( let* ) m f = bind m ~f
  end)
