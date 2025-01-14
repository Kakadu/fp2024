(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open EvalMonad
open EvalMonad.Monad
open Ast
open Errors

let init_state = { global_env = MapIdent.empty; running = None; sleeping = [] }

let sep_by_comma list print =
  let rec helper acc = function
    | fst :: snd :: tl ->
      let acc = String.concat "" [ acc; print fst; ", " ] in
      helper acc (snd :: tl)
    | fst :: _ -> acc ^ print fst
    | [] -> acc
  in
  helper "" list
;;

let rec pp_value = function
  | Value_int i -> Format.asprintf "%d" i
  | Value_bool b -> Format.asprintf "%b" b
  | Value_nil _ -> Format.asprintf "nil"
  | Value_array (i, lst) -> Format.asprintf "[%d][%s]" i (sep_by_comma lst pp_value)
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

let eval =
  run (return ()) init_state
  |> function
  | _, res -> res (* mb check final state *)
;;

let rec eval_expr = function
  | Expr_const c ->
    (match c with
     | Const_int v -> return (Value_int v)
     | Const_string v -> return (Value_string v)
     | Const_array (i, t, v) -> map eval_expr v >>= fun x -> return (Value_array (i, x))
     | Const_func f -> return (Value_int 1))
  | Expr_bin_oper (op, a1, a2) -> eval_binop op a1 a2
  | Expr_un_oper (op, a) -> eval_unop op a
  | Expr_ident idnt -> read_ident idnt
  | Expr_index (array, index) -> eval_index array index
  | Expr_call (ident, fcall) ->
    eval_expr ident
    >>= (function
     | Value_func (Func_initialized (cl, afc)) ->
       add_stack_frame
         { local_envs =
             { exec_block = afc.body; var_map = MapIdent.empty; env_type = Default }, []
         ; expr_eval = []
         ; deferred_funcs = []
         }
       *> iter
            (fun (farg, (ident, _)) ->
              retrieve_arg farg >>= fun arg -> save_local_id ident arg)
            (List.combine fcall afc.args)
       *> delete_stack_frame
       *> return (Value_int 1)
     | Value_func (Func_uninitialized _) -> fail (Runtime_error Uninited_func)
     | Value_func (Func_builtin ftype) ->
       (match ftype with
        | Print ->
          (map retrieve_arg fcall >>= builtin_print)
          *> fail (Runtime_error (DevOnly TypeCheckFailed))
        | Println ->
          (map retrieve_arg fcall >>= builtin_println)
          *> fail (Runtime_error (DevOnly TypeCheckFailed))
        | Make ->
          map retrieve_arg fcall *> return (Value_chan (Chan_initialized true))
          (*ДОДЕЛАТЬ*)
        | Recover -> return (Value_int 1) (*ДОДЕЛАТЬ*)
        | Len ->
          map retrieve_arg fcall
          >>= (function
           | [ Value_array (len, _) ] -> return (Value_int len)
           | [ Value_string s ] -> return (Value_int (String.length s))
           | _ -> fail (Runtime_error (DevOnly TypeCheckFailed)))
        | Panic ->
          map retrieve_arg fcall
          >>= (fun av -> return (String.concat "" (List.map pp_value av)))
          >>= fun msg -> fail (Runtime_error (Panic msg)))
     | _ -> fail (Runtime_error (DevOnly TypeCheckFailed)))
  | Expr_chan_receive ex -> eval_expr ex

and retrieve_arg = function
  | Arg_expr e -> eval_expr e
  | Arg_type _ -> fail (Runtime_error (DevOnly TypeCheckFailed))

and eval_index array index =
  let* array = eval_expr array in
  let* index = eval_expr index in
  match array, index with
  | Value_array (size, lst), Value_int index ->
    if index >= 0 && index < size - 1
    then return (List.nth lst index)
    else fail (Runtime_error Array_index_out_of_bound)
  | _ -> fail (Runtime_error (DevOnly TypeCheckFailed))

and eval_unop op a =
  let* a = eval_expr a in
  match op, a with
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
