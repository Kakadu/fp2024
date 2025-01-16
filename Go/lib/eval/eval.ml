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

let rpf lst = List.map (fun (y, _) -> y) lst

let pp printer eval ast =
  match eval ast with
  | Result.Ok res -> print_endline (printer res)
  | Result.Error _ -> print_endline ": some kind of error"
;;

let exec_stmt eval_stmt =
  let* stmt = pop_next_statement in
  match stmt with
  | Some st -> eval_stmt st *> return (Some ())
  | None -> return None
;;

let rec exec eval_stmt =
  exec_stmt eval_stmt
  >>= function
  | Some _ -> exec eval_stmt *> return ()
  | None -> return ()
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
    eval_func_call (func, args)
    >>= (function
     | Some value -> return value (* тут может быть тапл *)
     | None -> fail (Runtime_error (DevOnly (TypeCheckFailed "expr"))))
  | Expr_chan_receive ex -> eval_expr ex (*ДОДЕЛАТЬ*)

and eval_func_call (func, args) =
  eval_expr func
  >>= function
  | Value_func (Func_uninitialized _) -> fail (Runtime_error Uninited_func)
  | Value_func (Func_builtin ftype) -> eval_builtin args ftype
  | Value_func (Func_initialized (is_closure, afc)) ->
    (* тут нужна проверка на замыкание *)
    let* local_envs = read_local_envs >>= fun (hd, tl) -> return (List.cons hd tl) in
    create_args_map args (rpf afc.args)
    >>= fun map ->
    add_stack_frame
      { local_envs =
          { exec_block = afc.body; var_map = map; env_type = Default }, local_envs
      ; deferred_funcs = []
      ; closure_envs = Simple
      }
    *> exec eval_stmt
    *> delete_stack_frame
    *> return None
  | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed "func_call")))

and retrieve_arg_value = function
  | Arg_expr e -> eval_expr e
  | Arg_type _ -> fail (Runtime_error (DevOnly (TypeCheckFailed "arg_value")))

and create_args_map args idents =
  let rec save_args map = function
    | [] -> return map
    | (expr, id) :: tl ->
      retrieve_arg_value expr >>= fun vl -> save_args (MapIdent.add id vl map) tl
  in
  save_args MapIdent.empty (List.combine args idents)

and eval_builtin args = function
  | Print ->
    (map retrieve_arg_value args
     >>= iter (fun x -> return (Format.printf "%s" (pp_value x))))
    *> return None
  | Println ->
    (map retrieve_arg_value args
     >>= iter (fun x -> return (Format.printf "%s" (pp_value x))))
    *> return (Format.printf "\n")
    *> return None
  | Make ->
    let* chan_id = create_chanel in
    return (Some (Value_chan (Chan_initialized chan_id)))
  | Close ->
    map retrieve_arg_value args
    >>= (function
     | [ Value_chan chan ] -> close_chanel chan *> return None
     | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed "close"))))
  | Len ->
    map retrieve_arg_value args
    >>= (function
     | [ Value_array (len, _) ] -> return (Some (Value_int len))
     | [ Value_string s ] -> return (Some (Value_int (String.length s)))
     | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed "len"))))
  | Panic ->
    map retrieve_arg_value args
    >>= (fun av -> return (String.concat "" (List.map pp_value av)))
    >>= fun msg -> fail (Runtime_error (Panic msg))
    (* Тут неправильно *)
  | Recover -> return None
(* ДОДЕЛАТЬ, возвращает аргумент паники *)

and retrieve_arg_generic = function
  | Arg_expr _ -> fail (Runtime_error (DevOnly (TypeCheckFailed "arg_generic")))
  | Arg_type t -> return t

and eval_index array index =
  let* array = eval_expr array in
  let* index = eval_expr index in
  match array, index with
  | Value_array (_, values), Value_int index ->
    (try return (List.nth values index) with
     | Invalid_argument _ -> fail (Runtime_error Negative_array_index)
     | Failure _ -> fail (Runtime_error Array_index_out_of_bound))
  | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed "index")))

and eval_unop op expr =
  let* value = eval_expr expr in
  match op, value with
  | Unary_minus, Value_int a -> return (Value_int (-a))
  | Unary_plus, Value_int a -> return (Value_int a)
  | Unary_not, Value_bool a -> return (Value_bool (not a))
  | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed "unop")))

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
  | Bin_less, Value_int a1, Value_int a2 -> return (Value_bool (a1 < a2))
  | Bin_less_equal, Value_int a1, Value_int a2 -> return (Value_bool (a1 <= a2))
  | Bin_greater, Value_int a1, Value_int a2 -> return (Value_bool (a1 > a2))
  | Bin_greater_equal, Value_int a1, Value_int a2 -> return (Value_bool (a1 >= a2))
  | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed "binop")))

and eval_lvalue = function
  | Lvalue_ident id -> return id
  | Lvalue_array_index _ ->
    fail (Runtime_error (Panic "Not supported lvalue array index"))

and eval_stmt = function
  | Stmt_call fcall -> eval_func_call fcall *> return ()
  | Stmt_long_var_decl lvd -> eval_long_var_decl save_local_id lvd
  | Stmt_decr id ->
    read_ident id
    >>= (function
     | Value_int v -> update_ident id (Value_int (v - 1)) *> return ()
     | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed "stmt"))))
  | Stmt_incr id ->
    read_ident id
    >>= (function
     | Value_int v -> update_ident id (Value_int (v + 1)) *> return ()
     | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed "stmt"))))
  | Stmt_assign asgn ->
    (match asgn with
     | Assign_mult_expr (fst, lst) ->
       iter
         (fun (lvalue, expr) ->
           eval_expr expr
           >>= fun vl -> eval_lvalue lvalue >>= fun id -> update_ident id vl)
         (fst :: lst)
     | Assign_one_expr (fst, snd, lst, fcall) ->
       fail (Runtime_error (Panic "Not supported")))
  | Stmt_short_var_decl svd ->
    (match svd with
     | Short_decl_mult_init (sfirst, lst) ->
       iter (fun (ident, expr) -> eval_expr expr >>= save_local_id ident) (sfirst :: lst)
     | Short_decl_one_init _ -> fail (Runtime_error (Panic "Not supported")))
  | Stmt_if if' ->
    eval_expr if'.cond
    >>= (function
     | Value_bool true -> add_env if'.if_body Default *> exec eval_stmt *> delete_env
     | Value_bool false ->
       (match if'.else_body with
        | Some (Else_block body) -> add_env body Default *> exec eval_stmt *> delete_env
        | Some (Else_if if') -> eval_stmt (Stmt_if if')
        | None -> return ())
     | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed "if"))))
  | Stmt_go call -> eval_go call
  | _ -> fail (Runtime_error (Panic "Not supported stmt"))

(*ДОДЕЛАТЬ*)
(*let* cnd = eval_expr if'.cond in
    let* local_envs = read_local_envs >>= fun (hd, tl) -> return (List.cons hd tl) in
    if Value_bool true = cnd
    then
      add_stack_frame
        { local_envs =
            ( { exec_block = if'.if_body; var_map = MapIdent.empty; env_type = Default }
            , local_envs )
        ; deferred_funcs = []
        ; closure_envs = Simple
        }
      *> exec eval_stmt
      *> delete_stack_frame
    else (
      match if'.else_body with
      | Some (Else_block body) ->
        add_stack_frame
          { local_envs =
              { exec_block = body; var_map = MapIdent.empty; env_type = Default }, []
          ; deferred_funcs = []
          ; closure_envs = Simple
          }
        *> exec eval_stmt
        *> delete_stack_frame
      | Some (Else_if if') -> eval_stmt (Stmt_if if')
      | None -> return ())*)
and eval_long_var_decl save_to_env = function
  | Long_decl_mult_init (_, hd, tl) ->
    iter (fun (ident, expr) -> eval_expr expr >>= save_to_env ident) (hd :: tl)
  | _ -> fail (Runtime_error (Panic "Not supported"))
(*ДОДЕЛАТЬ*)

and eval_chan_send (ident, expr) =
  let* value = eval_expr expr in
  read_ident ident
  >>= function
  | Value_chan chan ->
    let* chan_id = find_chanel_fail chan in
    stop_running_goroutine (Sending { chan_id; value })
  | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed "chan send")))

and eval_chan_receive expr =
  eval_expr expr
  >>= function
  | Value_chan chan ->
    let* chan_id = find_chanel_fail chan in
    stop_running_goroutine (Recieving { chan_id })
  | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed "chan receive")))

and eval_go (func, arg_exprs) =
  eval_expr func
  >>= function
  | Value_func (Func_uninitialized Nil) -> fail (Runtime_error Uninited_func)
  | Value_func (Func_builtin _) -> return () (* TODO *)
  | Value_func (Func_initialized (is_closure, { args; body })) ->
    (* TODO closure *)
    let* var_map = create_args_map arg_exprs (rpf args) in
    add_waiting
      Ready
      { stack =
          ( { local_envs = { exec_block = body; var_map; env_type = Default }, []
            ; deferred_funcs = []
            ; closure_envs = Simple
            }
          , [] )
      ; id = 0 (* ignored *)
      }
  | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed "func call")))
;;

let save_builtins =
  save_global_id "true" (Value_bool true)
  *> save_global_id "false" (Value_bool false)
  *> save_global_id "nil" (Value_nil Nil)
  *> save_global_id "print" (Value_func (Func_builtin Print))
  *> save_global_id "println" (Value_func (Func_builtin Println))
  *> save_global_id "make" (Value_func (Func_builtin Make))
  *> save_global_id "len" (Value_func (Func_builtin Len))
  *> save_global_id "recover" (Value_func (Func_builtin Recover))
  *> save_global_id "panic" (Value_func (Func_builtin Panic))
;;

let save_global_vars_and_funcs =
  iter (function
    | Decl_var decl -> eval_long_var_decl save_global_id decl
    | Decl_func (ident, afc) ->
      save_global_id ident (Value_func (Func_initialized (Default, afc))))
;;

let add_main_goroutine =
  iter (function
    | Decl_func ("main", { body }) ->
      add_waiting
        Ready
        { stack =
            ( { local_envs =
                  { exec_block = body; var_map = MapIdent.empty; env_type = Default }, []
              ; deferred_funcs = []
              ; closure_envs = Simple
              }
            , [] )
        ; id = 1
        }
    | _ -> return ())
;;

(* runs all ready goroutines *)
let run_ready_goroutines =
  let* asleep_goroutines = read_waiting in
  match GoSet.find_first_opt (fun { state } -> state = Ready) asleep_goroutines with
  | Some { goroutine } ->
    run_goroutine goroutine
    (* Когда горутина остановится, она либо заснет, либо удалится, и там же должна запуститься другая Ready *)
  | None -> return () (* мб если нет готовых горутин, делать что-то ещё *)
;;

let run_eval file =
  save_builtins
  *> save_global_vars_and_funcs file
  *> add_main_goroutine file
  *> run_ready_goroutines
  *> exec eval_stmt
  *> return ()
;;

let init_state =
  { global_env = MapIdent.empty
  ; running = None
  ; waiting = GoSet.empty
  ; chanels = ChanSet.empty, 1
  }
;;

let eval file =
  run (run_eval file) init_state
  |> function
  | _, res -> res (* mb check final state *)
;;
