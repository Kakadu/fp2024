(** Copyright 2024-2025, Ekaterina Tenyaeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type error =
  | TypeError
  | DivisionByZero
  | MatchFailure
  | NoVariable of id

let pp_error ppf : error -> _ = function
  | TypeError -> Format.fprintf ppf "Type error"
  | DivisionByZero -> Format.fprintf ppf "Division by zero"
  | MatchFailure -> Format.fprintf ppf "Matching failure"
  | NoVariable id -> Format.fprintf ppf "Undefined variable '%s'" id
;;

type value =
  | ValInt of int
  | ValChar of char
  | ValString of string
  | ValUnit
  | ValBool of bool
  | ValFun of rec_flag * pattern * expr * env
  | ValFunction of match_case list * env
  | ValTuple of value * value * value list
  | ValList of value list
  | ValOption of value option
  | ValBuiltin of id

and env = (id, value, Base.String.comparator_witness) Base.Map.t

let rec pp_value ppf =
  let open Stdlib.Format in
  function
  | ValInt int -> fprintf ppf "%i" int
  | ValChar char -> fprintf ppf "'%c'" char
  | ValString str -> fprintf ppf "%S" str
  | ValBool bool -> fprintf ppf "%b" bool
  | ValUnit -> fprintf ppf "()"
  | ValOption value ->
    (match value with
     | Some value -> fprintf ppf "Some %a" pp_value value
     | None -> fprintf ppf "None")
  | ValList vls ->
      fprintf ppf "[%a]" (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "; ") pp_value) vls
  | ValTuple (fst_val, snd_val, val_list) ->
    fprintf ppf "(%a)"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_value)
      (fst_val :: snd_val :: val_list)
  | ValFun _ -> fprintf ppf "<fun>"
  | ValFunction _ -> fprintf ppf "<function>"
  | ValBuiltin _ -> fprintf ppf "<builtin>"
;;

module Res = struct
  open Base

  type 'a t = ('a, error) Result.t

  let fail = Result.fail
  let return = Result.return

  let ( >>= ) (monad : 'a t) (f : 'a -> 'b t) : 'b t =
    match monad with
    | Ok result -> f result
    | Error x -> fail x
  ;;

  let ( let* ) = ( >>= )
end

module EvalEnv = struct
  open Base

  let empty = Map.empty (module String)
  let extend env key value = Map.update env key ~f:(fun _ -> value)

  let compose env1 env2 =
    Map.fold env2 ~f:(fun ~key ~data env_acc -> extend env_acc key data) ~init:env1
  ;;

  let find_exn env key =
    match Map.find env key with
    | Some value -> Res.return value
    | None -> Res.fail (NoVariable key)
  ;;

  let find_exn1 env key =
    let val' = Map.find_exn env key in
    val'
  ;;
end

module Inter = struct
  open Res
  open EvalEnv

  let eval_arith opr val1 val2 = return (ValInt (opr val1 val2))
  let eval_eq opr val1 val2 = return (ValBool (opr val1 val2))
  let eval_bool opr val1 val2 = return (ValBool (opr val1 val2))

   let eval_un_op = function
    | Neg, ValInt val1 -> return (ValInt (-val1))
    | Not, ValBool val1 -> return (ValBool (not val1))
    | _ -> fail TypeError
  ;;

  let eval_bin_op = function
    | Mult, ValInt val1, ValInt val2 -> eval_arith ( * ) val1 val2
    | Div, ValInt val1, ValInt val2 when val2 <> 0 -> eval_arith ( / ) val1 val2
    | Div, _, ValInt 0 -> fail DivisionByZero
    | Add, ValInt val1, ValInt val2 -> eval_arith ( + ) val1 val2
    | Sub, ValInt val1, ValInt val2 -> eval_arith ( - ) val1 val2
    | GreaterEquals, val1, val2 -> eval_eq ( >= ) val1 val2
    | LessEquals, val1, val2 -> eval_eq ( <= ) val1 val2
    | NotEquals, val1, val2 -> eval_eq ( <> ) val1 val2
    | Equals, val1, val2 -> eval_eq ( = ) val1 val2
    | GreaterThan, val1, val2 -> eval_eq ( > ) val1 val2
    | LessThan, val1, val2 -> eval_eq ( < ) val1 val2
    | And, ValBool val1, ValBool val2 -> eval_bool ( && ) val1 val2
    | Or, ValBool val1, ValBool val2 -> eval_bool ( || ) val1 val2
    | _ -> fail TypeError
  ;;

  let rec match_pattern env = function
    | PatAny, _ -> Some env
    | PatVar name, value -> Some (extend env name value)
    | PatConst (Int pat), ValInt value when pat = value -> Some env
    | PatConst (Char pat), ValChar value when pat = value -> Some env
    | PatConst (Bool pat), ValBool value when pat = value -> Some env
    | PatConst Unit, _ -> Some env
    | PatConst (String pat), ValString value when pat = value -> Some env
    | PatTup (fst_pat, snd_pat, pat_list), ValTuple (fst_val, snd_val, val_list) ->
      let env =
        Base.List.fold2
          ~f:(fun env pat value ->
            match env with
            | Some env -> match_pattern env (pat, value)
            | None -> None)
          ~init:(Some env)
          (fst_pat :: snd_pat :: pat_list)
          (fst_val :: snd_val :: val_list)
      in
      (match env with
       | Ok env -> env
       | _ -> None)
     | PatListConstructor pat_list, ValList val_list ->
      (match pat_list, val_list with
       | first_pat :: rest_pat, first_val :: rest_val ->
         let env = match_pattern env (first_pat, first_val) in
         (match env with
          | Some env ->
            match_pattern env (PatListConstructor rest_pat, ValList rest_val)
          | None -> None)
       | _, _ -> Some env)
    | PatList pat_list, ValList val_list ->
      (match pat_list, val_list with
       | first_pat :: rest_pat, first_val :: rest_val ->
         let env = match_pattern env (first_pat, first_val) in
         (match env with
          | Some env ->
            match_pattern env (PatList rest_pat, ValList rest_val)
          | None -> None)
       | _, _ -> Some env)
    | PatWithTyp (_, pat), value -> match_pattern env (pat, value)
    | PatOption None, ValOption None -> Some env
    | PatOption (Some pat), ValOption (Some value) -> match_pattern env (pat, value)
    | _ -> None
  ;;

  let rec extend_names_from_pat env = function
    | PatAny, _ -> return env
    | PatConst Unit, ValUnit -> return env
    | PatOption None, ValOption None -> return env
    | PatVar id, value -> return (extend env id value)
    | PatTup (fst_pat, snd_pat, pat_list), ValTuple (fst_val, snd_val, val_list) ->
      (match
         Base.List.fold2
           (fst_pat :: snd_pat :: pat_list)
           (fst_val :: snd_val :: val_list)
           ~init:(return env)
           ~f:(fun acc pat value ->
             let* env = acc in
             extend_names_from_pat env (pat, value))
       with
       | Ok acc -> acc
       | _ -> fail TypeError)
    | PatList pat_list, ValList val_list ->
      (match pat_list, val_list with
       | first_pat :: rest_pat, first_val :: rest_val ->
         let* env = extend_names_from_pat env (first_pat, first_val) in
         let* env =
           extend_names_from_pat env (PatList rest_pat, ValList rest_val)
         in
         return env
       | _, _ -> return env)
    | PatListConstructor pat_list, ValList val_list ->
      (match pat_list, val_list with
       | first_pat :: rest_pat, first_val :: rest_val ->
         let* env = extend_names_from_pat env (first_pat, first_val) in
         let* env =
           extend_names_from_pat
             env
             (PatListConstructor rest_pat, ValList rest_val)
         in
         return env
       | _, _ -> return env)
    | PatOption (Some pat), ValOption (Some value) ->
      extend_names_from_pat env (pat, value)
    | _ -> fail TypeError
  ;;

  let rec eval_expression env = function
    | ExpVar id -> find_exn env id
    | ExpConst const ->
      (match const with
       | Int int -> return (ValInt int)
       | Char char -> return (ValChar char)
       | String str -> return (ValString str)
       | Bool bool -> return (ValBool bool)
       | Unit -> return ValUnit)
    | ExpLet (NonRec, value_binding, value_binding_list, exp) ->
      let* env = eval_value_binding_list env (value_binding :: value_binding_list) in
      eval_expression env exp
    | ExpLet (Rec, value_binding, value_binding_list, exp) ->
      let* env = eval_rec_value_binding_list env (value_binding :: value_binding_list) in
      eval_expression env exp
    | ExpFun (pat, exp) ->
      return (ValFun (NonRec, pat, exp, env))
    | ExpFunction (case, case_list) -> return (ValFunction (case :: case_list, env))
    | ExpMatch (exp, case, case_list) ->
      let* match_value = eval_expression env exp in
      find_and_eval_case env match_value (case :: case_list)
    | ExpBinOper (op, exp1, exp2) ->
      let* value1 = eval_expression env exp1 in
      let* value2 = eval_expression env exp2 in
      eval_bin_op (op, value1, value2)
    | ExpUnOper (op, e) ->
      let* v = eval_expression env e in
      eval_un_op (op, v)
    | ExpListConstructor expr_list
    | ExpList expr_list ->
      let* val_list =
        Base.List.fold_right
          ~f:(fun exp acc ->
            let* acc = acc in
            let* value = eval_expression env exp in
            return (value :: acc))
          ~init:(return [])
          expr_list
      in
      return (ValList val_list)
    | ExpApp (exp1, exp2) ->
      let* fun_val = eval_expression env exp1 in
      let* arg_val = eval_expression env exp2 in
        (match fun_val with
          | ValFun (rec_flag, pat, exp, fun_env) ->
            let* new_env =
              match rec_flag, match_pattern fun_env (pat, arg_val) with
              | Rec, Some extended_env -> return (compose env extended_env)
              | NonRec, Some extended_env -> return extended_env
              | _, None -> fail MatchFailure
            in
            eval_expression new_env exp
          | ValFunction (case_list, env) -> find_and_eval_case env arg_val case_list
          | ValBuiltin builtin ->
            (match builtin, arg_val with
             | "print_int", ValInt integer ->
               print_int integer;
               return (ValUnit)
             | "print_endline", ValString str ->
               print_endline str;
               return (ValUnit)
             | _ -> fail TypeError)
          | _ -> fail TypeError)
    | ExpOption None -> return (ValOption None)
    | ExpOption (Some expr) ->
      let* value = eval_expression env expr in
      return (ValOption (Some value))
    | ExpTup (fst_exp, snd_exp, exp_list) ->
      let* fst_val = eval_expression env fst_exp in
      let* snd_val = eval_expression env snd_exp in
      let* val_list =
        Base.List.fold_right
          ~f:(fun exp acc ->
            let* acc = acc in
            let* value = eval_expression env exp in
            return (value :: acc))
          ~init:(return [])
          exp_list
      in
      return (ValTuple (fst_val, snd_val, val_list))
    | ExpIfThenElse (if_exp, then_exp, Some else_exp) ->
      let* value_if_exp = eval_expression env if_exp in
      (match value_if_exp with
       | ValBool true -> eval_expression env then_exp
       | ValBool false -> eval_expression env else_exp
       | _ -> fail TypeError)
    | ExpIfThenElse (fst_val, snd_val, None) ->
      let* value_fst_val = eval_expression env fst_val in
      (match value_fst_val with
       | ValBool true ->
         let* value_snd_val = eval_expression env snd_val in
         (match value_snd_val with
          | ValUnit as v -> return v
          | _ -> fail TypeError)
       | ValBool false -> return ValUnit
       | _ -> fail TypeError)
    | ExpWithTyp (_, exp) -> eval_expression env exp

  and find_and_eval_case env value = function
    | [] -> fail MatchFailure
    | { match_pat; match_expr } :: tail ->
      let env_temp = match_pattern env (match_pat, value) in
      (match env_temp with
       | Some env -> eval_expression env match_expr
       | None -> find_and_eval_case env value tail)

  and eval_value_binding_list env value_binding_list =
    Base.List.fold_left
      ~f:(fun acc { pat; expr } ->
        let* env = acc in
        let* value = eval_expression env expr in
        match pat with
        | PatVar name | PatWithTyp (_, PatVar name) ->
          let env = extend env name value in
          return env
        | _ ->
          let* env = extend_names_from_pat env (pat, value) in
          return env)
      ~init:(return env)
      value_binding_list

  and eval_rec_value_binding_list env value_binding_list =
    Base.List.fold_left
      ~f:(fun acc { pat; expr } ->
        let* env = acc in
        let* value = eval_expression env expr in
        match pat with
        | PatVar name | PatWithTyp (_, PatVar name) ->
          let value =
            match value with
            | ValFun (_, pat, expr, env) ->
              ValFun (Rec, pat, expr, env)
            | other -> other
          in
          let env = extend env name value in
          return env
        | _ -> fail TypeError)
      ~init:(return env)
      value_binding_list
  ;;

  let eval_structure_item env out_list =
    let rec extract_names_from_pat env acc = function
      | PatVar id -> acc @ [ Some id, EvalEnv.find_exn1 env id ]
      | PatTup (fst_pat, snd_pat, pat_list) ->
        Base.List.fold_left
          (fst_pat :: snd_pat :: pat_list)
          ~init:acc
          ~f:(extract_names_from_pat env)
      | PatWithTyp (_, pat) -> extract_names_from_pat env acc pat
      | _ -> acc
    in
    let get_names_from_let_binds env =
      Base.List.fold_left ~init:[] ~f:(fun acc { pat; _ } ->
        extract_names_from_pat env acc pat)
    in
    function
    | EvalExp exp ->
      let* val' = eval_expression env exp in
      return (env, out_list @ [ None, val' ])
    | Binding (NonRec, value_binding, value_binding_list) ->
      let value_binding_list = value_binding :: value_binding_list in
      let* env = eval_value_binding_list env value_binding_list in
      let eval_list = get_names_from_let_binds env value_binding_list in
      return (env, out_list @ eval_list)
    | Binding (Rec, value_binding, value_binding_list) ->
      let value_binding_list = value_binding :: value_binding_list in
      let* env = eval_rec_value_binding_list env value_binding_list in
      let eval_list = get_names_from_let_binds env value_binding_list in
      return (env, out_list @ eval_list)
  ;;

  let eval_structure env ast =
    let* env, out_list =
      Base.List.fold_left
        ~f:(fun acc item ->
          let* env, out_list = acc in
          let* env, out_list = eval_structure_item env out_list item in
          return (env, out_list))
        ~init:(return (env, []))
        ast
    in
    let remove_duplicates =
      let fun_equal el1 el2 =
        match el1, el2 with
        | (Some id1, _), (Some id2, _) -> String.equal id1 id2
        | _ -> false
      in
      function
      | x :: xs when not (Base.List.mem xs x ~equal:fun_equal) -> x :: xs
      | _ :: xs -> xs
      | [] -> []
    in
    return (env, remove_duplicates out_list)
  ;;
end

let empty_env = EvalEnv.empty

let env_with_print_funs =
  let env = EvalEnv.extend empty_env "print_int" (ValBuiltin "print_int") in
  EvalEnv.extend env "print_endline" (ValBuiltin "print_endline")
;;

let run_interpreter = Inter.eval_structure
