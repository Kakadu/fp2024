(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Values
open Ast

module type MONAD = sig
  (* a basic monad that has type of state * result *)
  include Base.Monad.S2

  (* error-handling *)
  val fail : error -> ('a, error) t

  (* for readability *)
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

(* utility layer for handling environments, i.e. symbol tables *)
module Env (M : MONAD) = struct
  (* for error propagation *)
  open M

  (* creates an empty environment *)
  let empty = Base.Map.empty (module Base.String)

  (* looks up a variable in the environment *)
  let find env name =
    match Base.Map.find env name with
    | Some x -> return x
    | None -> fail (`Unbound_variable name)
  ;;

  (* adds or updates a binding in the environment *)
  let extend env key value = Base.Map.update env key ~f:(fun _ -> value)

  (* composes two envs: if they have overlapping values, the latter is chosen *)
  let compose env1 env2 =
    Base.Map.fold env2 ~init:env1 ~f:(fun ~key ~data acc_env -> extend acc_env key data)
  ;;
end

module Eval (M : MONAD) : sig end = struct
  open M
  open Env (M)

  let rec match_pattern env = function
    | PAny, _ -> Some env
    | PConst (Int i1), VInt i2 when i1 = i2 -> Some env
    | PConst (Bool b1), VBool b2 when Bool.equal b1 b2 -> Some env
    | PConst (String s1), VString s2 when String.equal s1 s2 -> Some env
    | PConst Unit, VUnit -> Some env
    | PVar (Id name), v -> Some (extend env name v)
    | PList patterns, VList values -> match_list_pattern env patterns values
    | PTuple (p1, p2, p_rest), VTuple (v1, v2, v_rest) ->
      match_list_pattern env (p1 :: p2 :: p_rest) (v1 :: v2 :: v_rest)
    | PCons (p1, p2), VList (v1 :: v2) ->
      (match match_pattern env (p1, v1) with
       | Some env' -> match_pattern env' (p2, VList v2)
       | None -> None)
    | POption p, VOption v ->
      (match p, v with
       | Some p, Some v -> match_pattern env (p, v)
       | None, None -> Some env
       | _ -> None)
    | _ -> None

  and match_list_pattern env patterns values =
    (* to avoid Invalid argument exception *)
    if List.length patterns <> List.length values
    then None
    else (
      let f1 acc p v =
        match acc with
        | None -> None
        | Some env' -> match_pattern env' (p, v)
      in
      List.fold_left2 f1 (Some env) patterns values)
  ;;

  let print_env env =
    let open Stdlib.Format in
    printf "{\n";
    Base.Map.iteri env ~f:(fun ~key ~data -> printf "%s = %a\n" key pp_value data);
    printf "}\n"
  ;;

  let eval_un_op = function
    | Negative, VInt i -> return (VInt (-i))
    | Positive, VInt i -> return (VInt i)
    | Not, VBool b -> return (VBool (not b))
    | _ ->
      Format.printf "ty err8\n";
      fail `Type_error
  ;;

  let rec eval_bin_op = function
    | Add, VInt i1, VInt i2 -> return (VInt (i1 + i2))
    | Mult, VInt i1, VInt i2 -> return (VInt (i1 * i2))
    | Sub, VInt i1, VInt i2 -> return (VInt (i1 - i2))
    | Div, VInt _, VInt i2 when i2 = 0 -> fail `Division_by_zero
    | Div, VInt i1, VInt i2 -> return (VInt (i1 / i2))
    | Cons, v, VList vl -> return (VList (v :: vl))
    | Gt, VInt i1, VInt i2 -> return (VBool (i1 > i2))
    | Lt, VInt i1, VInt i2 -> return (VBool (i1 < i2))
    | Gte, VInt i1, VInt i2 -> return (VBool (i1 >= i2))
    | Lte, VInt i1, VInt i2 -> return (VBool (i1 <= i2))
    | And, VBool b1, VBool b2 -> return (VBool (b1 && b2))
    | Or, VBool b1, VBool b2 -> return (VBool (b1 || b2))
    | Eq, VInt i1, VInt i2 -> return (VBool (i1 = i2))
    | Neq, VInt i1, VInt i2 -> return (VBool (i1 <> i2))
    | Eq, VString s1, VString s2 -> return (VBool (s1 = s2))
    | Neq, VString s1, VString s2 -> return (VBool (s1 <> s2))
    | Eq, VBool b1, VBool b2 -> return (VBool (b1 = b2))
    | Neq, VBool b1, VBool b2 -> return (VBool (b1 <> b2))
    | Eq, VUnit, VUnit -> return (VBool true)
    | Neq, VUnit, VUnit -> return (VBool false)
    | Eq, VList l1, VList l2 -> eval_eq_list Eq l1 l2
    | Neq, VList l1, VList l2 -> eval_eq_list Neq l1 l2
    | Eq, VTuple (v1, v2, v_rest), VTuple (v1', v2', v_rest') ->
      eval_eq_list Eq (v1 :: v2 :: v_rest) (v1' :: v2' :: v_rest')
    | Neq, VTuple (v1, v2, v_rest), VTuple (v1', v2', v_rest') ->
      eval_eq_list Neq (v1 :: v2 :: v_rest) (v1' :: v2' :: v_rest')
    | Eq, VOption o1, VOption o2 ->
      (match o1, o2 with
       | Some o1, Some o2 -> eval_bin_op (Eq, o1, o2)
       | None, None -> return (VBool true)
       | _ -> return (VBool false))
    | Neq, VOption o1, VOption o2 ->
      (match o1, o2 with
       | Some o1, Some o2 -> eval_bin_op (Neq, o1, o2)
       | None, None -> return (VBool true)
       | _ -> return (VBool false))
    | _ ->
      Format.printf "ty err-1\n";
      fail `Type_error

  and eval_eq_list op l1 l2 =
    if List.length l1 <> List.length l2
    then return (VBool false)
    else (
      let f1 acc el1 el2 =
        let* acc = acc in
        match acc with
        | VBool false -> return (VBool false)
        | VBool true ->
          let* res = eval_bin_op (op, el1, el2) in
          (match res with
           | VBool true -> return (VBool true)
           | _ -> return (VBool false))
        | _ ->
          Format.printf "ty err1\n";
          fail `Type_error
      in
      List.fold_left2 f1 (return (VBool true)) l1 l2)
  ;;

    let eval_const = function
    | Int i -> return (VInt i)
    | String s -> return (VString s)
    | Bool b -> return (VBool b)
    | Unit -> return VUnit
  ;;

  (* acceptable patterns for names *)
  let rec validate_pattern_nonrec = function
    | PVar _ | PAny | PConst Unit -> true
    | PTuple (p1, p2, rest) -> List.for_all validate_pattern_nonrec (p1 :: p2 :: rest)
    | _ -> false
  ;;

  (* in rec we can only use a name *)
  let validate_pattern_rec = function
    | PVar _ -> true
    | _ -> false
  ;;

  (* extract pattern from typed pattern *)
  let get_pattern tp =
    match tp with
    | pat, _ -> pat
  ;;

  let get_patterns tpl = List.map get_pattern tpl

    let rec eval_expr env = function
    | Econst c -> eval_const c
    | Evar (Id name) -> find env name
    | Eif_then_else (cond, t, Some e) ->
      let* cond_value = eval_expr env cond in
      (match cond_value with
       | VBool true -> eval_expr env t
       | VBool false -> eval_expr env e
       | _ ->
         Format.printf "ty err2\n";
         fail `Type_error)
    | Eif_then_else (cond, t, None) ->
      let* cond_value = eval_expr env cond in
      (match cond_value with
       | VBool true -> eval_expr env t
       | VBool false -> return VUnit
       | _ ->
         Format.printf "ty err3\n";
         fail `Type_error)
    | Eoption (Some e) ->
      let* value = eval_expr env e in
      return (VOption (Some value))
    | Eoption None -> return (VOption None)
    | Etuple (e1, e2, e_rest) ->
      let* v1 = eval_expr env e1 in
      let* v2 = eval_expr env e2 in
      let* v_rest =
        List.fold_left
          (fun acc e ->
            let* acc = acc in
            let* v = eval_expr env e in
            return (v :: acc))
          (return [])
          e_rest
      in
      return (VTuple (v1, v2, List.rev v_rest))
    | Elist el ->
      let* vl =
        List.fold_left
          (fun acc e ->
            let* acc = acc in
            let* v = eval_expr env e in
            return (v :: acc))
          (return [])
          el
      in
      return (VList (List.rev vl))
    | Ebin_op (op, e1, e2) ->
      let* v1 = eval_expr env e1 in
      let* v2 = eval_expr env e2 in
      eval_bin_op (op, v1, v2)
    | Eun_op (op, e) ->
      let* v = eval_expr env e in
      eval_un_op (op, v)
    | Ematch (Some e, c, cl) ->
      let* v = eval_expr env e in
      (* print_env env;
         Format.printf "e match: %a\n" Pr_printer.pp_expr e;
         Format.printf "v match: %a\n" pp_value v; *)
      (* print_env (compose env extended_env); *)
      let rec match_helper env = function
        | Ecase (pat, expr) :: tl ->
          (* Format.printf "e case: %a\n" Pr_printer.pp_expr expr; *)
          let env' = match_pattern env (pat, v) in
          (match env' with
           (* new environment for evaluating the body of the case *)
           | Some env' ->
             (* print_env env'; *)
             let env'' = compose env env' in
             (* print_env env''; *)
             let* result = eval_expr env'' expr in
             (* Format.printf "res match: %a\n" pp_value result; *)
             return result
           | None -> match_helper env tl)
        | [] -> fail `Pattern_matching_failure
      in
      (* Format.printf "e match: %a\n" Pr_printer.pp_expr e; *)
      match_helper env (c :: cl)
    | Ematch (None, c, cl) ->
      let unique_var = "__arg" in
      return
        (VFun
           ( Non_recursive
           , PVar (Id unique_var)
           , []
           , Ematch (Some (Evar (Id unique_var)), c, cl)
           , env ))
    | Efun (tp, tpl, e) ->
      return (VFun (Non_recursive, get_pattern tp, get_patterns tpl, e, env))
    | Efun_application (e1, e2) ->
      let* v1 = eval_expr env e1 in
      (* print_env env;
         Format.printf "val1 %a \n" pp_value v1; *)
      let* v2 = eval_expr env e2 in
      (* print_env env;
         Format.printf "val2 %a \n" pp_value v2; *)
      (match v1 with
       | VFun (_, pat, pats, body, func_env) ->
         (* Format.printf "func env \n";
            print_env func_env; *)
         (* attempt to match the argument against the pattern *)
         (match match_pattern func_env (pat, v2) with
          | Some extended_env ->
            (* Format.printf "ext env \n";
               print_env (compose env extended_env);
               Format.printf "body expr: %a\n" Pr_printer.pp_expr body; *)
            let env' = compose env extended_env in
            (match pats with
             | [] ->
               eval_expr
                 env'
                 body (* evaluate the function body with the updated environment *)
             | p :: pl -> return (VFun (Non_recursive, p, pl, body, env')))
          | None -> fail `Pattern_matching_failure)
       | VBuiltin (builtin, _) ->
         (match builtin, v2 with
          | BInt b, VInt i ->
            b i;
            return VUnit
          | _ -> fail `Type_error)
       | _ ->
         Format.printf "ty err5\n";
         fail `Type_error)
    | Elet (Non_recursive, Evalue_binding ((pat, _), e1), _, e2) ->
      if not (validate_pattern_nonrec pat)
      then fail (`Ill_left_hand_side "Pattern not acceptable for variable name")
      else
        let* v = eval_expr env e1 in
        (match match_pattern env (pat, v) with
         | Some env' -> eval_expr env' e2
         | None -> fail `Pattern_matching_failure)
    | Elet (Recursive, Evalue_binding ((pat, t), e1), [], e2) ->
      let* final_env = eval_let_rec_expr env (Evalue_binding ((pat, t), e1)) in
      eval_expr final_env e2
    | Elet (Recursive, value_binding, value_bindings, e2) ->
      let* final_env = eval_value_bindings env (value_binding :: value_bindings) in
      eval_expr final_env e2
    | _ -> fail `Type_error

  and eval_let_rec_expr env = function
    | Evalue_binding ((pat, _), e1) ->
      if not (validate_pattern_rec pat)
      then fail (`Ill_left_hand_side "Pattern not acceptable for variable name")
      else
        let* v = eval_expr env e1 in
        let* rec_env =
          match match_pattern env (pat, v) with
          | Some new_env -> return new_env
          | None -> fail `Pattern_matching_failure
        in
        let* recursive_value =
          match v with
          | VFun (_, p, pl, e, _) -> return (VFun (Recursive, p, pl, e, rec_env))
          | _ ->
            Format.printf "ty err6\n";
            fail `Type_error
        in
        let* final_env =
          match match_pattern env (pat, recursive_value) with
          | Some updated_env -> return updated_env
          | None -> fail `Pattern_matching_failure
        in
        return final_env

  and eval_value_bindings env value_bindings =
    let bindings = List.map (fun (Evalue_binding ((p, _), e)) -> p, e) value_bindings in
    (* extend env with all names in mutual recursion *)
    let rec update_env acc_env = function
      | [] -> return acc_env
      | (PVar (Id name), expr) :: tl ->
        let* value =
          match expr with
          | Efun (p, pl, e) ->
            (* Format.printf "1 \n";
               Format.printf "acc_env\n";
               print_env acc_env; *)
            return (VFun (Recursive, get_pattern p, get_patterns pl, e, acc_env))
          | _ ->
            Format.printf "2 \n";
            eval_expr acc_env expr
        in
        (* update env so all names in mutual recursion correspond to their real values *)
        let updated_env = extend acc_env name value in
        (* Format.printf "updated_env\n";
           print_env updated_env; *)
        update_env updated_env tl
      | _ -> fail (`Ill_left_hand_side "Pattern not acceptable for variable name")
    in
    let* final_env = update_env env bindings in
    return final_env
  ;;

  let eval_str_item env str_item =
    let env = extend env "print_int" (VBuiltin (BInt print_int, env)) in
    match str_item with
    | SEval e ->
      let* _ = eval_expr env e in
      return env
    | SValue (Non_recursive, Evalue_binding ((pat, _), e), _) ->
      if not (validate_pattern_nonrec pat)
      then fail (`Ill_left_hand_side "Pattern not acceptable for variable name")
      else
        let* v = eval_expr env e in
        (match match_pattern env (pat, v) with
         | Some env' -> return env'
         | None -> fail `Pattern_matching_failure)
    | SValue (Recursive, Evalue_binding ((pat, t), e), []) ->
      let* final_env = eval_let_rec_expr env (Evalue_binding ((pat, t), e)) in
      return final_env
    | SValue (Recursive, value_binding, value_bindings) ->
      let* final_env = eval_value_bindings env (value_binding :: value_bindings) in
      return final_env
  ;;

  let eval_structure (structure : structure) =
    List.fold_left
      (fun env str_item ->
        let* env = env in
        let* env = eval_str_item env str_item in
        (* Format.printf "env str_item\n"; *)
        (* print_env env; *)
        return env)
      (return empty)
      structure
  ;;
end
