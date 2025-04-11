[@@@ocaml.text "/*"]

(** Copyright 2025, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast

type error =
  [ `Type_error
  | `Division_by_zero
  | `Match_failure
  | `Too_many_args_for_anonym_fun
  | `Too_many_args_for_fun of string
  | `No_variable of string
  ]

let print_error (e : error) =
  match e with
  | `Type_error -> Printf.printf "Type error"
  | `Division_by_zero -> Printf.printf "Division by zero"
  | `Match_failure -> Printf.printf "Matching failure"
  | `Too_many_args_for_fun id -> Printf.printf "Too many arguments for function '%s'" id
  | `Too_many_args_for_anonym_fun ->
    Printf.printf "Too many arguments for anonym function"
  | `No_variable id -> Printf.printf "Undefined variable '%s'" id
;;

type value =
  | Val_integer of int
  | Val_char of char
  | Val_string of string
  | Val_unit
  | Val_bool of bool
  | Val_fun of rec_flag * ident list option * pattern list * expression * env
  | Val_function of (pattern * expression) list * env
  | Val_tuple of value list
  | Val_list of value list
  | Val_option of value option
  | Val_builtin of string

and env = (string, value, Base.String.comparator_witness) Base.Map.t

let rec print_value = function
  | Val_integer int -> Printf.printf "%i" int
  | Val_char char -> Printf.printf "'%c'" char
  | Val_string str -> Printf.printf "%S" str
  | Val_unit -> Printf.printf "()"
  | Val_bool bool -> Printf.printf "%b" bool
  | Val_tuple val_list ->
    Printf.printf "(";
    let rec help = function
      | first :: [] -> print_value first
      | first :: rest ->
        print_value first;
        Printf.printf ", ";
        help rest
      | _ -> ()
    in
    help val_list;
    Printf.printf ")"
  | Val_fun _ -> Printf.printf "<fun>"
  | Val_function _ -> Printf.printf "<function>"
  | Val_list val_list ->
    Printf.printf "[";
    (match val_list with
     | first :: second :: rest ->
       print_value first;
       Printf.printf "; ";
       print_value (Val_list (second :: rest))
     | single :: [] -> print_value single
     | [] -> ());
    Printf.printf "]"
  | Val_option value ->
    (match value with
     | Some value ->
       Printf.printf "Some ";
       print_value value
     | None -> Printf.printf "None")
  | Val_builtin _ -> Printf.printf "<builtin>"

and print_env env =
  Printf.printf "Type enviroment: \n";
  Base.Map.iteri env ~f:(fun ~key ~data ->
    Printf.printf "val %s : " key;
    print_value data;
    Printf.printf "\n");
  Printf.printf "\n"
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
    | None -> Res.fail (`No_variable key)
  ;;

  let find_exn1 env key =
    let val' = Map.find_exn env key in
    val'
  ;;
end

module Inter = struct
  open Ast
  open Res
  open EvalEnv

  let eval_arith opr val1 val2 = return (Val_integer (opr val1 val2))
  let eval_eq opr val1 val2 = return (Val_bool (opr val1 val2))
  let eval_bool opr val1 val2 = return (Val_bool (opr val1 val2))

  let eval_bin_op = function
    | Mul, Val_integer val1, Val_integer val2 -> eval_arith ( * ) val1 val2
    | Div, Val_integer val1, Val_integer val2 when val2 <> 0 -> eval_arith ( / ) val1 val2
    | Div, _, Val_integer 0 -> fail `Division_by_zero
    | Plus, Val_integer val1, Val_integer val2 -> eval_arith ( + ) val1 val2
    | Sub, Val_integer val1, Val_integer val2 -> eval_arith ( - ) val1 val2
    | GreaterEqual, val1, val2 -> eval_eq ( >= ) val1 val2
    | LessEqual, val1, val2 -> eval_eq ( <= ) val1 val2
    | NotEqual, val1, val2 -> eval_eq ( <> ) val1 val2
    | Equal, val1, val2 -> eval_eq ( = ) val1 val2
    | Greater, val1, val2 -> eval_eq ( > ) val1 val2
    | Less, val1, val2 -> eval_eq ( < ) val1 val2
    | And, Val_bool val1, Val_bool val2 -> eval_bool ( && ) val1 val2
    | Or, Val_bool val1, Val_bool val2 -> eval_bool ( || ) val1 val2
    | _ -> fail `Type_error
  ;;

  let rec match_pattern env = function
    | Pattern_any, _ -> Some env
    | Pattern_const Const_unit, _ -> Some env
    | Pattern_option None, Val_option None -> Some env
    | Pattern_var name, value -> Some (extend env name value)
    | Pattern_const (Const_int pat), Val_integer value when pat = value -> Some env
    | Pattern_const (Const_char pat), Val_char value when pat = value -> Some env
    | Pattern_const (Const_string pat), Val_string value when pat = value -> Some env
    | Pattern_const (Const_bool pat), Val_bool value when pat = value -> Some env
    | Pattern_tuple pat_list, Val_tuple val_list ->
      let env =
        Base.List.fold2
          ~f:(fun env pat value ->
            match env with
            | Some env -> match_pattern env (pat, value)
            | None -> None)
          ~init:(Some env)
          pat_list
          val_list
      in
      (match env with
       | Ok env -> env
       | _ -> None)
    | Pattern_list_sugar_case pat_list, Val_list val_list ->
      (match pat_list, val_list with
       | [], [] -> Some env
       | first_pat :: rest_pat, first_val :: rest_val ->
         let env = match_pattern env (first_pat, first_val) in
         (match env with
          | Some env ->
            match_pattern env (Pattern_list_sugar_case rest_pat, Val_list rest_val)
          | None -> None)
       | _ -> None)
    | Pattern_list_constructor_case pat_list, Val_list val_list ->
      (match pat_list, val_list with
       | single_pat :: [], val_list -> match_pattern env (single_pat, Val_list val_list)
       | _ :: _ :: _, [] -> None
       | first_pat :: rest_pat, first_val :: rest_val ->
         let env = match_pattern env (first_pat, first_val) in
         (match env with
          | Some env ->
            match_pattern env (Pattern_list_constructor_case rest_pat, Val_list rest_val)
          | None -> None)
       | _ -> None)
    | Pattern_option (Some pat), Val_option (Some value) -> match_pattern env (pat, value)
    | _ -> None
  ;;

  let rec extend_names_from_pat env = function
    | Pattern_any, _ -> return env
    | Pattern_const Const_unit, Val_unit -> return env
    | Pattern_option None, Val_option None -> return env
    | Pattern_var id, value -> return (extend env id value)
    | Pattern_tuple pat_list, Val_tuple val_list ->
      (match
         Base.List.fold2 pat_list val_list ~init:(return env) ~f:(fun acc pat value ->
           let* env = acc in
           extend_names_from_pat env (pat, value))
       with
       | Ok acc -> acc
       | _ -> fail `Type_error)
    | Pattern_list_sugar_case pat_list, Val_list val_list ->
      (match pat_list, val_list with
       | first_pat :: rest_pat, first_val :: rest_val ->
         let* env = extend_names_from_pat env (first_pat, first_val) in
         let* env =
           extend_names_from_pat env (Pattern_list_sugar_case rest_pat, Val_list rest_val)
         in
         return env
       | _, _ -> return env)
    | Pattern_list_constructor_case pat_list, Val_list val_list ->
      (match pat_list, val_list with
       | first_pat :: rest_pat, first_val :: rest_val ->
         let* env = extend_names_from_pat env (first_pat, first_val) in
         let* env =
           extend_names_from_pat
             env
             (Pattern_list_constructor_case rest_pat, Val_list rest_val)
         in
         return env
       | _, _ -> return env)
    | Pattern_option (Some pat), Val_option (Some value) ->
      extend_names_from_pat env (pat, value)
    | _ -> fail `Type_error
  ;;

  let rec eval_expression env = function
    | Expr_var id -> find_exn env id
    | Expr_const const ->
      (match const with
       | Const_int int -> return (Val_integer int)
       | Const_char char -> return (Val_char char)
       | Const_string str -> return (Val_string str)
       | Const_bool bool -> return (Val_bool bool)
       | Const_unit -> return Val_unit)
    | Expr_construct_in (let_binding, expr) ->
      let* env = eval_value_binding env let_binding in
      eval_expression env expr
    | Expr_anonym_fun (pat_list, expr) ->
      return (Val_fun (Non_recursive, None, pat_list, expr, env))
    | Expr_binary_op (op, exp1, exp2) ->
      let* value1 = eval_expression env exp1 in
      let* value2 = eval_expression env exp2 in
      eval_bin_op (op, value1, value2)
    | Expr_application (exp, expr_list) ->
      let* fun_val = eval_expression env exp in
      let val_list = Base.List.map expr_list ~f:(fun expr -> eval_expression env expr) in
      let rec help_fun val_list = function
        | Val_fun (rec_flag, ident, pat_list, expr, fun_env) ->
          let fun_env =
            match ident, rec_flag with
            | Some ident_list, Recursive ->
              Base.List.fold_left ident_list ~init:fun_env ~f:(fun acc_env ident ->
                EvalEnv.extend acc_env ident (EvalEnv.find_exn1 env ident))
            | _ -> fun_env
          in
          let rec helper pat_list val_list fun_env =
            match pat_list, val_list with
            | _ :: _, [] ->
              return (Val_fun (Non_recursive, ident, pat_list, expr, fun_env))
            | first_pat :: rest_pat, first_val :: rest_val ->
              let* arg_val = first_val in
              let new_fun_env = match_pattern fun_env (first_pat, arg_val) in
              (match new_fun_env with
               | Some new_fun_env -> helper rest_pat rest_val new_fun_env
               | None -> fail `Match_failure)
            | [], _ :: _ ->
              let* partial_application_val = eval_expression fun_env expr in
              help_fun val_list partial_application_val
            | [], [] ->
              let _ =
                match ident with
                | Some (first :: []) -> first
                | Some _ -> "several_idents"
                | _ -> "no_ident"
              in
              eval_expression fun_env expr
          in
          helper pat_list val_list fun_env
        | Val_function (case_list, env) ->
          (match expr_list with
           | [] -> return fun_val
           | single :: [] ->
             let* arg_val = eval_expression env single in
             find_and_eval_case env arg_val case_list
           | _ -> fail `Too_many_args_for_anonym_fun)
        | Val_builtin builtin ->
          (match val_list with
           | [] -> return fun_val
           | single :: [] ->
             let* arg_val = single in
             (match builtin, arg_val with
              | "print_int", Val_integer integer ->
                print_int integer;
                Printf.printf "\n";
                (*to make the test results readable*)
                return Val_unit
              | "print_endline", Val_string str ->
                print_endline str;
                return Val_unit
              | _ -> fail `Type_error)
           | _ -> fail (`Too_many_args_for_fun builtin))
        | _ -> fail `Type_error
      in
      help_fun val_list fun_val
    | Expr_function_fun case_list -> return (Val_function (case_list, env))
    | Expr_match_with (expr, case_list) ->
      let* match_value = eval_expression env expr in
      find_and_eval_case env match_value case_list
    | Expr_tuple expr_list ->
      let* val_list =
        Base.List.fold_right
          ~f:(fun exp acc ->
            let* acc = acc in
            let* value = eval_expression env exp in
            return (value :: acc))
          ~init:(return [])
          expr_list
      in
      return (Val_tuple val_list)
    | Expr_list_sugar expr_list ->
      let* val_list =
        Base.List.fold_right
          ~f:(fun exp acc ->
            let* acc = acc in
            let* value = eval_expression env exp in
            return (value :: acc))
          ~init:(return [])
          expr_list
      in
      return (Val_list val_list)
    | Expr_list_construct expr_list ->
      let rec helper acc_list = function
        | [] -> eval_expression env (Expr_list_sugar [])
        | single_expr :: [] ->
          let* value = eval_expression env single_expr in
          (match value with
           | Val_list val_list -> return (Val_list (acc_list @ val_list))
           | _ -> return (Val_list []))
        | first_expr :: rest_exprs ->
          let* value = eval_expression env first_expr in
          helper (acc_list @ [ value ]) rest_exprs
      in
      helper [] expr_list
    | Expr_option (Some expr) ->
      let* value = eval_expression env expr in
      return (Val_option (Some value))
    | Expr_option None -> return (Val_option None)
    | Expr_if_then_else (if_expr, then_expr, else_expr) ->
      let* if_value = eval_expression env if_expr in
      (match if_value with
       | Val_bool true -> eval_expression env then_expr
       | Val_bool false -> eval_expression env else_expr
       | _ -> fail `Type_error)
    | Typed_expression (_, expr) -> eval_expression env expr

  and find_and_eval_case env value = function
    | [] -> fail `Match_failure
    | (pat, expr) :: rest_cases ->
      let env_temp = match_pattern env (pat, value) in
      (match env_temp with
       | Some env -> eval_expression env expr
       | None -> find_and_eval_case env value rest_cases)

  and eval_value_binding env = function
    | Let_binding (rec_flag, Let_fun (id, pat_list), expr) ->
      (match pat_list with
       | _ :: _ ->
         let env = extend env id (Val_fun (rec_flag, Some [ id ], pat_list, expr, env)) in
         return env
       | [] ->
         let* value = eval_expression env expr in
         let env = extend env id value in
         return env)
    | Let_binding (_, Let_pattern pat, expr) ->
      let* value = eval_expression env expr in
      let* env = extend_names_from_pat env (pat, value) in
      return env
    | Let_rec_and_binding binding_list ->
      let list_of_names =
        Base.List.fold_left binding_list ~init:[] ~f:(fun acc_list let_binding ->
          match let_binding with
          | Let_binding (_, Let_fun (id, _), _) -> acc_list @ [ id ]
          | _ -> acc_list)
      in
      Base.List.fold_left binding_list ~init:(return env) ~f:(fun acc_env let_binding ->
        let* env = acc_env in
        let* env = eval_value_binding env let_binding in
        match let_binding with
        | Let_binding (_, Let_fun (id, _), _) ->
          let added_value = EvalEnv.find_exn1 env id in
          (match added_value with
           | Val_fun (Recursive, Some _, pat_list, expr, fun_env) ->
             let env =
               extend
                 env
                 id
                 (Val_fun (Recursive, Some list_of_names, pat_list, expr, fun_env))
             in
             let _ = print_env in
             return env
           | _ -> return env)
        | _ -> return env)
  ;;

  let eval_let_bind env out_list value_binding =
    let rec extract_names_from_pat env acc = function
      | Pattern_var id -> acc @ [ id, EvalEnv.find_exn1 env id ]
      | Pattern_tuple pat_list
      | Pattern_list_sugar_case pat_list
      | Pattern_list_constructor_case pat_list ->
        Base.List.fold_left pat_list ~init:acc ~f:(extract_names_from_pat env)
      | Pattern_option (Some pat) -> extract_names_from_pat env acc pat
      | _ -> acc
    in
    let rec get_names_from_let_binds env = function
      | Let_binding (_, Let_fun (id, _), _) -> [ id, EvalEnv.find_exn1 env id ]
      | Let_binding (_, Let_pattern pat, _) -> extract_names_from_pat env [] pat
      | Let_rec_and_binding binding_list ->
        Base.List.fold_left binding_list ~init:[] ~f:(fun acc_list let_binding ->
          acc_list @ get_names_from_let_binds env let_binding)
    in
    let* env = eval_value_binding env value_binding in
    let eval_list = get_names_from_let_binds env value_binding in
    return (env, out_list @ eval_list)
  ;;

  let eval_structure env ast =
    let* _, out_list =
      Base.List.fold_left
        ~f:(fun acc let_bind ->
          let* env, out_list = acc in
          let* env, out_list = eval_let_bind env out_list let_bind in
          return (env, out_list))
        ~init:(return (env, []))
        ast
    in
    let rec remove_duplicates =
      let fun_equal el1 el2 =
        match el1, el2 with
        | (id1, _), (id2, _) -> String.equal id1 id2
      in
      function
      | x :: xs when not (Base.List.mem xs x ~equal:fun_equal) ->
        x :: remove_duplicates xs
      | _ :: xs -> remove_duplicates xs
      | [] -> []
    in
    return (remove_duplicates out_list)
  ;;
end

let empty_env = EvalEnv.empty

let env_with_print_funs =
  let env = EvalEnv.extend empty_env "print_int" (Val_builtin "print_int") in
  EvalEnv.extend env "print_endline" (Val_builtin "print_endline")
;;

let run_interpreter ast = Inter.eval_structure env_with_print_funs ast

let inter str =
  match Parse.parse str with
  | Ok ast ->
    (match run_interpreter ast with
     | Ok result ->
       Base.List.map result ~f:(fun (name, value) ->
         Printf.printf "val %s : " name;
         print_value value;
         Printf.printf "\n")
     | Error e -> [ print_error e ])
  | Error _ -> [ () ]
;;

let from_file1 filename =
  let in_channel =
    open_in ("/home/anastasia/Documents/repositories/fp2024/manytests/typed/" ^ filename)
  in
  try
    let content = really_input_string in_channel (in_channel_length in_channel) in
    close_in in_channel;
    inter content
  with
  | e ->
    close_in_noerr in_channel;
    raise e
;;

let from_file2 filename =
  let in_channel =
    open_in
      ("/home/anastasia/Documents/repositories/fp2024/manytests/do_not_type/" ^ filename)
  in
  try
    let content = really_input_string in_channel (in_channel_length in_channel) in
    close_in in_channel;
    inter content
  with
  | e ->
    close_in_noerr in_channel;
    raise e
;;

let run_inter1 from_file =
  Base.List.map
    [ "001fac.ml"
    ; "002fac.ml"
    ; "003fib.ml"
    ; "004manyargs.ml"
    ; "005fix.ml"
    ; "006partial2.ml"
    ; "006partial3.ml"
    ; "006partial.ml"
    ; "007order.ml"
    ; "008ascription.ml"
    ; "009let_poly.ml"
    ; "010sukharev.ml"
    ; "015tuples.ml"
    ; "016lists.ml"
    ]
    ~f:(fun name ->
      Printf.printf "\nResult of %s: \n" name;
      from_file name)
;;

let run_inter2 from_file =
  Base.List.map
    [ "001.ml"
    ; "002if.ml"
    ; "003occurs.ml"
    ; "004let_poly.ml"
    ; "005.ml"
    ; "015tuples.ml"
    ; "016tuples_mismatch.ml"
    ; "097fun_vs_list.ml"
    ; "097fun_vs_unit.ml"
    ; "098rec_int.ml"
    ; "099.ml"
    ]
    ~f:(fun name ->
      Printf.printf "\nResult of %s: \n" name;
      from_file name)
;;
