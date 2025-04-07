(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open IntAuxilary.EvalEnv
open IntAuxilary.Res
open Forest.ValuesTree
open Forest.Ast

let rec match_pattern env = function
  | PatAny, _ -> Some env
  | PatConst _, _ -> Some env
  | PatList pats, ValList vals -> match_list_pat env pats vals
  | PatTup (p1, p2, ps), ValTup (v1, v2, vs) ->
    match_list_pat env (p1 :: p2 :: ps) (v1 :: v2 :: vs)
  | PatListCons (p1, p2), ValList (v1 :: v2) ->
    (match match_pattern env (p1, v1) with
     | Some env1 -> match_pattern env1 (p2, ValList v2)
     | None -> None)
  | _ -> None

and match_list_pat env pats vals =
  let helper acc p v =
    match acc with
    | None -> None
    | Some env -> match_pattern env (p, v)
  in
  match Base.List.fold2 pats vals ~f:helper ~init:(Some env) with
  | Ok res -> res
  | _ -> None
;;

let eval_const = function
  | Int v -> return @@ ValInt v
  | Str v -> return @@ ValStr v
  | Bool v -> return @@ ValBool v
  | Unit -> return @@ ValUnit
;;

let eval_unary_op = function
  | Minus, ValInt v -> return @@ ValInt (-v)
  | Plus, ValInt v -> return @@ ValInt v
  | Not, ValBool v -> return @@ ValBool (not v)
  | _ -> fail TypeError
;;

(* thanks to Homka122 for such beautiful bin_op evaluation *)
let arithmetic_int_operators = [ Mul, ( * ); Div, ( / ); Add, ( + ); Sub, ( - ) ]

let comparison_operators =
  [ Eq, ( = ); Le, ( <= ); Lt, ( < ); Gt, ( > ); Ge, ( >= ); Ne, ( <> ) ]
;;

let logical_operators = [ And, ( && ); Or, ( || ) ]

let is_arithmetic_operator name =
  List.exists (fun (list_op, _) -> name = list_op) arithmetic_int_operators
;;

let is_comparison_operator name =
  List.exists (fun (list_op, _) -> name = list_op) comparison_operators
;;

let is_logical_operator name =
  List.exists (fun (list_op, _) -> name = list_op) logical_operators
;;

let eval_arithmetic_int_binop op_name v1 v2 =
  let operator =
    snd (List.find (fun (op_list, _) -> op_list = op_name) arithmetic_int_operators)
  in
  match v1, v2 with
  | ValInt v1, ValInt v2 -> return (ValInt (operator v1 v2))
  | _ -> fail TypeError
;;

let rec eval_comparison_binop eval_expr env op_name v1 v2 =
  let operator () =
    snd (List.find (fun (op_list, _) -> op_list = op_name) comparison_operators)
  in
  let rec eval_list_comparison list1 list2 =
    match list1, list2 with
    | [], [] -> return (ValBool true)
    | l :: ls, r :: rs ->
      let* res = eval_comparison_binop eval_expr env op_name l r in
      (match res with
       | ValBool false -> return (ValBool false)
       | ValBool true -> eval_list_comparison ls rs
       | _ -> fail TypeError)
    | _, _ -> return (ValBool false)
  in
  match v1, v2 with
  | ValInt v1, ValInt v2 -> return (ValBool ((operator ()) v1 v2))
  | ValStr v1, ValStr v2 -> return (ValBool ((operator ()) v1 v2))
  | ValBool v1, ValBool v2 -> return (ValBool ((operator ()) v1 v2))
  | ValUnit, ValUnit -> return (ValBool ((operator ()) () ()))
  | ValList v1, ValList v2 -> eval_list_comparison v1 v2
  | ValTup (l1, l2, ls), ValTup (r1, r2, rs) ->
    eval_list_comparison (l1 :: l2 :: ls) (r1 :: r2 :: rs)
  | ValOption v1, ValOption v2 ->
    (match v1, v2 with
     | Some v1, Some v2 -> eval_comparison_binop eval_expr env op_name v1 v2
     | None, None -> return (ValBool ((operator ()) None None))
     | _ -> return (ValBool false))
  | _ -> fail TypeError
;;

let eval_logical_binop op_name v1 v2 =
  let operator =
    snd (List.find (fun (op_list, _) -> op_list = op_name) logical_operators)
  in
  match v1, v2 with
  | ValBool v1, ValBool v2 -> return (ValBool (operator v1 v2))
  | _ -> fail TypeError
;;

let rec eval_expr (env : env) = function
  | Var name -> find_val env name
  | Const exp -> eval_const exp
  | Unary (op, exp) ->
    let* v = eval_expr env exp in
    eval_unary_op (op, v)
  | BinOp (op, exp1, exp2) when is_comparison_operator op ->
    let* v1 = eval_expr env exp1 in
    let* v2 = eval_expr env exp2 in
    eval_comparison_binop eval_expr env op v1 v2
  | BinOp (op, exp1, exp2) when is_arithmetic_operator op ->
    let* v1 = eval_expr env exp1 in
    let* v2 = eval_expr env exp2 in
    eval_arithmetic_int_binop op v1 v2
  | BinOp (op, exp1, exp2) when is_logical_operator op ->
    let* v1 = eval_expr env exp1 in
    let* v2 = eval_expr env exp2 in
    eval_logical_binop op v1 v2
  | BinOp (_, _, _) -> fail TypeError
  | Option (Some exp) ->
    let* v = eval_expr env exp in
    return (ValOption (Some v))
  | Option None -> return (ValOption None)
  | Let (NonRec, Binding (pat, exp_bind), _, exp_in) ->
    let* v = eval_expr env exp_bind in
    (match match_pattern env (pat, v) with
     | Some env -> eval_expr env exp_in
     | None -> fail PatternMatchingFail)
  | Let (Rec, binding, bindings, exp_in) ->
    let* env = eval_let_bindings env (binding :: bindings) in
    eval_expr env exp_in
  | App (exp1, exp2) ->
    let* v1 = eval_expr env exp1 in
    let* v2 = eval_expr env exp2 in
    (match v1 with
     | ValFun (_, pat, pats, body, func_env) ->
       (* attempt to match the argument against the pattern *)
       (match match_pattern func_env (pat, v2) with
        | Some extended_env ->
          let env' = compose env extended_env in
          (match pats with
           | [] ->
             eval_expr
               env'
               body (* evaluate the function body with the updated environment *)
           | p :: pl -> return (ValFun (NonRec, p, pl, body, env')))
        | None -> fail PatternMatchingFail)
     | ValBuiltIn "print_int" ->
       (match v2 with
        | ValInt v ->
          Format.printf "%d\n" v;
          return ValUnit
        | _ -> fail TypeError)
     | _ -> fail TypeError)
  | Fun (pat, pats, exp) -> return (ValFun (NonRec, pat, pats, exp, env))
  | Branch (cond, _then, Const Unit) ->
    let* v_cond = eval_expr env cond in
    (match v_cond with
     | ValBool true -> eval_expr env _then
     | ValBool false -> return ValUnit
     | _ -> fail TypeError)
  | Branch (cond, _then, _else) ->
    let* v_cond = eval_expr env cond in
    (match v_cond with
     | ValBool true -> eval_expr env _then
     | ValBool false -> eval_expr env _else
     | _ -> fail TypeError)
  | Tup (exp1, exp2, exps) ->
    let* v1 = eval_expr env exp1 in
    let* v2 = eval_expr env exp2 in
    let* vs =
      List.fold_left
        (fun acc exp ->
          let* acc = acc in
          let* v = eval_expr env exp in
          return (v :: acc))
        (return [])
        exps
    in
    return (ValTup (v1, v2, List.rev vs))
  | List exp ->
    let* vl =
      List.fold_left
        (fun acc e ->
          let* acc = acc in
          let* v = eval_expr env e in
          return (v :: acc))
        (return [])
        exp
    in
    return (ValList (List.rev vl))
  | Type (exp, _) -> eval_expr env exp

and eval_let_bindings env binding_list =
  let bindings = List.map (fun (Binding (pat, exp)) -> pat, exp) binding_list in
  let rec update_env acc_env = function
    | [] -> return acc_env
    | (PatVar id, exp) :: list_rest ->
      let* value =
        match exp with
        | Fun (pat, pats, exp) -> return (ValFun (Rec, pat, pats, exp, acc_env))
        | _ -> eval_expr acc_env exp
      in
      (* update env so all names in mutual recursion correspond to their real values *)
      let upd_env = extend acc_env id value in
      update_env upd_env list_rest
    | _ -> fail TypeError
  in
  let* env = update_env env bindings in
  return env
;;
