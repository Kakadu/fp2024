(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open EvalMonad
open EvalMonad.Monad
open Ast
open Errors

let rec pp_value = function
  | Value_int i -> Format.asprintf "%d" i
  | Value_bool b -> Format.asprintf "%b" b
  | Value_nil _ -> Format.asprintf "nil"
  | Value_array (i, lst) ->
    Format.asprintf "[%d][%s]" i (PpType.sep_by_comma lst pp_value)
  | Value_chan _ -> Format.asprintf "wtf chan"
  | Value_func _ -> Format.asprintf "wtf func"
  | Value_string s -> Format.asprintf "%s" s
;;

let builtin_print lst = iter (fun x -> return (Format.printf "%s" (pp_value x))) lst
let builtin_println lst = builtin_print lst *> return (Format.printf "\n") *> return ()

let pp printer eval ast =
  match eval ast with
  | Result.Ok res -> print_endline (printer res)
  | Result.Error _ -> print_endline ": some kind of error"
;;

let rec eval_expr = function
  | Expr_const (Const_int n) -> return (Value_int n)
  | Expr_const (Const_string s) -> return (Value_string s)
  | Expr_const (Const_array (size, _, exprs)) ->
    map eval_expr exprs >>= fun values -> return (Value_array (size, values))
  | Expr_const (Const_func afunc) ->
    return (Value_func (Func_initialized (Default, afunc)))
  | Expr_bin_oper (op, a1, a2) -> eval_binop op a1 a2
  | Expr_un_oper (op, a) -> eval_unop op a
  | Expr_ident id -> read_ident id
  | Expr_index (array, index) -> eval_index array index
  | Expr_call (func, args) ->
    eval_func_call (func, args) *> return (Value_int 1) (*ЗАГЛУШКА*)
  | Expr_chan_receive ex -> eval_expr ex (*ДОДЕЛАТЬ*)

and eval_func_call (func, args) =
  eval_expr func
  >>= function
  | Value_func (Func_uninitialized _) -> fail (Runtime_error Uninited_func)
  | Value_func (Func_initialized (is_closure, afc)) ->
    (* тут нужна проверка на замыкание *)
    add_stack_frame
      { local_envs =
          { exec_block = afc.body; var_map = MapIdent.empty; env_type = Default }, []
      ; deferred_funcs = []
      }
    *> iter
         (fun (farg, (ident, _)) ->
           retrieve_arg farg >>= fun value -> save_local_id ident value)
         (List.combine args afc.args)
    *> delete_stack_frame
    *> return None
  | Value_func (Func_builtin ftype) ->
    (match ftype with
     | Print -> (map retrieve_arg args >>= builtin_print) *> return None
     | Println -> (map retrieve_arg args >>= builtin_println) *> return None
     | Make ->
       map retrieve_arg args *> return (Some (Value_chan (Chan_initialized true)))
       (*ДОДЕЛАТЬ*)
     | Recover -> return None (* ДОДЕЛАТЬ, возвращает аргумент паники *)
     | Len ->
       map retrieve_arg args
       >>= (function
        | [ Value_array (len, _) ] -> return (Some (Value_int len))
        | [ Value_string s ] -> return (Some (Value_int (String.length s)))
        | _ -> fail (Runtime_error (DevOnly TypeCheckFailed)))
     | Panic ->
       map retrieve_arg args
       >>= (fun av -> return (String.concat "" (List.map pp_value av)))
       >>= fun msg -> fail (Runtime_error (Panic msg)))
    (* Тут неправильно *)
  | _ -> fail (Runtime_error (DevOnly TypeCheckFailed))

and retrieve_arg = function
  | Arg_expr e -> eval_expr e
  | Arg_type _ -> fail (Runtime_error (DevOnly TypeCheckFailed))

and eval_index array index =
  let* array = eval_expr array in
  let* index = eval_expr index in
  match array, index with
  | Value_array (_, values), Value_int index ->
    (try return (List.nth values index) with
     | Invalid_argument _ -> fail (Runtime_error Negative_array_index)
     | Failure _ -> fail (Runtime_error Array_index_out_of_bound))
  | _ -> fail (Runtime_error (DevOnly TypeCheckFailed))

and eval_unop op expr =
  let* value = eval_expr expr in
  match op, value with
  | Unary_minus, Value_int a -> return (Value_int (-a))
  | Unary_plus, Value_int a -> return (Value_int a)
  | Unary_not, Value_bool a -> return (Value_bool (not a))
  | _ -> fail (Runtime_error (DevOnly TypeCheckFailed))

and eval_binop op a1 a2 =
  let* a1 = eval_expr a1 in
  let* a2 = eval_expr a2 in
  match op, a1, a2 with
  | Bin_sum, Value_int a1, Value_int a2 -> return (Value_int (a1 + a2))
  | Bin_subtract, Value_int a1, Value_int a2 -> return (Value_int (a1 + a2))
  | Bin_multiply, Value_int a1, Value_int a2 -> return (Value_int (a1 * a2))
  | Bin_divide, Value_int _, Value_int 0 -> fail (Runtime_error Division_by_zero)
  | Bin_divide, Value_int a1, Value_int a2 -> return (Value_int (a1 / a2))
  | Bin_modulus, Value_int a1, Value_int a2 -> return (Value_int (a1 mod a2))
  | Bin_and, Value_bool a1, Value_bool a2 -> return (Value_bool (a1 && a2))
  | Bin_or, Value_bool a1, Value_bool a2 -> return (Value_bool (a1 || a2))
  | Bin_equal, a1, a2 -> return (Value_bool (a1 = a2))
  | Bin_less, Value_bool a1, Value_bool a2 -> return (Value_bool (a1 < a2))
  | Bin_less_equal, Value_bool a1, Value_bool a2 -> return (Value_bool (a1 <= a2))
  | Bin_greater, Value_bool a1, Value_bool a2 -> return (Value_bool (a1 > a2))
  | Bin_greater_equal, Value_bool a1, Value_bool a2 -> return (Value_bool (a1 >= a2))
  | _ -> fail (Runtime_error (DevOnly TypeCheckFailed))
;;

let init_state = { global_env = MapIdent.empty; running = None; sleeping = [] }

let eval =
  run (return ()) init_state
  |> function
  | _, res -> res (* mb check final state *)
;;
