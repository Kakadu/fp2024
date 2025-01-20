(** Copyright 2024-2025, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open EvalMonad
open EvalMonad.Monad
open Ast
open Format

let rec pp_value = function
  | Value_int n -> asprintf "%d" n
  | Value_bool b -> asprintf "%b" b
  | Value_nil Nil -> "<nil>"
  | Value_array (_, values) -> asprintf "[%s]" (PpType.sep_by_comma values pp_value)
  | Value_chan chan ->
    (match chan with
     | Chan_uninitialized Nil -> "<nil>"
     | Chan_initialized id -> asprintf "<chan %d>" id)
  | Value_func func ->
    (match func with
     | Func_uninitialized Nil -> "<nil>"
     | _ -> "<func>")
  | Value_string s -> s
  | Value_tuple values -> PpType.sep_by_comma values pp_value
;;

let rpf lst = List.map (fun (y, _) -> y) lst

(** Executes next statement, returns [Some ()] if there was statement to execute,
    and [None] if the end of current execution block was reached *)
let exec_stmt eval_stmt =
  pop_next_statement
  >>= function
  | Some st -> eval_stmt st *> return (Some ())
  | None -> return None
;;

let rec replace_list list index elem =
  match list with
  | [] -> list
  | h :: t ->
    if index = 0
    then elem :: replace_list t (index - 1) elem
    else h :: replace_list t (index - 1) elem
;;

(** Executes current execution block. Stops when its end reached, or when [return]
    statement was executed, when some chanel started to be used
    and when current goroutine is panicking *)
let rec exec eval_stmt =
  exec_stmt eval_stmt
  >>= function
  | None -> return ()
  | Some () ->
    is_using_chanel
    >>= (function
     | Some _ -> return ()
     | None ->
       read_returns
       >>= (function
        | Some _ -> return ()
        | None ->
          read_panics
          >>= (function
           | Some _ -> return ()
           | None -> exec eval_stmt)))
;;

let rec skip_local_env (hd, tl) =
  match hd.env_type with
  | For -> return (hd :: tl)
  | Default ->
    skip_local_env (List.hd tl, List.tl tl)
    >>= fun sk -> return ({ hd with exec_block = [] } :: sk)
;;

(** Runs all ready to run goroutines, after it returns it is guaranteed that all existing goroutines
    are working with chanels. If main goroutine finishes executing here, the whole program finishes running *)
let run_ready_goroutines eval_stmt =
  let rec runner () =
    run_ready_goroutine
    >>= function
    | None -> return ()
    | Some () ->
      exec eval_stmt *> is_using_chanel
      >>= (function
       | Some _ ->
         return ()
         (* chanel is being used, so we need to return to receiver to receive the value *)
       | None ->
         (* goroutine finished executing *)
         (read_panics
          >>= function
          | Some pnc -> fail (Runtime_error (Panic (pp_value (Value_tuple pnc))))
          | None -> return ())
         *> read_running_fail
         >>= (function
          | { go_id = 1; _ } ->
            (* main goroutine finished working and doesn't wait for others to finish *)
            return ()
          | _ ->
            (* some secondary goroutine finished working, don't stop until main finished *)
            delete_running_goroutine *> runner ()))
  in
  runner ()
;;

(** [attempt_chan_interaction id] attempts to use chanel with given id. If both
    sender and receiver are ready, starts using the chanel. Doesn't do anything otherwise *)
let attempt_chan_interaction id inited_by =
  let* ready_to_send = is_send_queue_not_empty id in
  let* ready_to_receive = is_receive_queue_not_empty id in
  match ready_to_send, ready_to_receive with
  | Some (), Some () ->
    let* sending_goroutine, value = pop_from_send_queue id in
    let* receiving_goroutine = pop_from_receive_queue id in
    start_using_chanel { sending_goroutine; receiving_goroutine; value; inited_by }
    (* receiving goroutine will run after return from a function *)
  | _ -> return ()
;;

let rec eval_expr = function
  | Expr_const (Const_int n) -> return (Value_int n)
  | Expr_const (Const_string s) -> return (Value_string s)
  | Expr_const (Const_array (size, _, exprs)) ->
    map eval_expr exprs >>= fun values -> return (Value_array (size, values))
  | Expr_const (Const_func afunc) ->
    return (Value_func (Func_initialized (FuncLit, afunc)))
  | Expr_bin_oper (op, a1, a2) -> eval_binop op a1 a2
  | Expr_un_oper (op, a) -> eval_unop op a
  | Expr_ident id -> read_ident id
  | Expr_index (array, index) -> eval_index array index
  | Expr_call (func, args) ->
    eval_expr func
    >>= (function
     | Value_func (Func_initialized (Closure _, afc)) ->
       eval_closure (func, args)
       >>= fun (ret, vmp) ->
       (match func with
        | Expr_ident id ->
          update_ident id (Value_func (Func_initialized (Closure vmp, afc)))
        | _ -> return ())
       *>
         (match ret with
         | Some lst -> return lst
         | _ -> fail (Runtime_error (TypeCheckFailed "expr")))
     | _ ->
       eval_func_call (func, args)
       >>= (function
        | Some lst -> return lst
        | _ -> fail (Runtime_error (TypeCheckFailed "expr"))))
  | Expr_chan_receive receive -> eval_chan_receive receive

and eval_func_call (func, args) =
  eval_expr func
  >>= function
  | Value_func (Func_uninitialized _) -> fail (Runtime_error Uninited_func)
  | Value_func (Func_builtin ftype) -> eval_builtin args ftype
  | Value_func (Func_initialized (Default, afc)) ->
    create_args_map args (rpf afc.args)
    >>= fun map ->
    add_stack_frame
      { local_envs = { exec_block = afc.body; var_map = map; env_type = Default }, []
      ; deferred_funcs = []
      ; returns = None
      ; panics = None
      }
    *> exec eval_stmt
    *> read_returns
    >>= fun x ->
    (read_deferred >>= iter eval_deferred_func) *> read_panics
    >>= fun panics -> delete_stack_frame *> write_panics panics *> return x
  | Value_func (Func_initialized (FuncLit, afc)) ->
    let* local_envs = read_local_envs >>= fun (fl, lstl) -> return (fl :: lstl) in
    create_args_map args (rpf afc.args)
    >>= fun map ->
    add_stack_frame
      { local_envs =
          { exec_block = afc.body; var_map = map; env_type = Default }, local_envs
      ; deferred_funcs = []
      ; returns = None
      ; panics = None
      }
    *> exec eval_stmt
    *> read_returns
    >>= fun x ->
    read_local_envs
    >>= fun (_, lenv) ->
    (read_deferred >>= iter eval_deferred_func) *> read_panics
    >>= fun panics ->
    delete_stack_frame
    *> write_panics panics
    *> write_local_envs (List.hd lenv, List.tl lenv)
    *> return x
  | _ -> fail (Runtime_error (TypeCheckFailed "defer func_call"))

and eval_closure (func, args) =
  eval_expr func
  >>= function
  | Value_func (Func_initialized (Closure vmap, afc)) ->
    let* local_envs = read_local_envs >>= fun (fl, lstl) -> return (fl :: lstl) in
    create_args_map args (rpf afc.args)
    >>= fun map ->
    add_stack_frame
      { local_envs =
          ( { exec_block = afc.body; var_map = map; env_type = Default }
          , { exec_block = []; var_map = vmap; env_type = Default } :: local_envs )
      ; deferred_funcs = []
      ; returns = None
      ; panics = None
      }
    *> exec eval_stmt
    *> read_panics
    >>= fun panics ->
    write_panics panics *> read_returns
    >>= fun x ->
    read_local_envs
    >>= fun (_, lenv) ->
    (read_deferred >>= iter eval_deferred_func) *> read_panics
    >>= fun panics ->
    delete_stack_frame
    *> write_panics panics
    *> write_local_envs (List.hd (List.tl lenv), List.tl (List.tl lenv))
    *> return (x, (List.hd lenv).var_map)
  | _ -> fail (Runtime_error (TypeCheckFailed "closure_call"))

and eval_deferred_func (vfunc, vargs) =
  match vfunc with
  | Value_func (Func_uninitialized _) -> fail (Runtime_error Uninited_func) *> return ()
  | Value_func (Func_builtin ftype) -> prepare_builtin_eval vargs ftype *> return ()
  | Value_func (Func_initialized (Default, afc)) ->
    let rec save_args map = function
      | [] -> return map
      | (vl, id) :: tl -> save_args (MapIdent.add id vl map) tl
    in
    let* panics = read_panics in
    save_args MapIdent.empty (List.combine vargs (rpf afc.args))
    >>= fun map ->
    add_stack_frame
      { local_envs = { exec_block = afc.body; var_map = map; env_type = Default }, []
      ; deferred_funcs = []
      ; returns = None
      ; panics
      }
    *> exec eval_stmt
    *> read_returns
    *> read_panics
    >>= fun panics -> delete_stack_frame *> write_panics panics *> return ()
  | Value_func (Func_initialized (FuncLit, afc)) ->
    let* local_envs = read_local_envs >>= fun (fl, lstl) -> return (fl :: lstl) in
    let rec save_args map = function
      | [] -> return map
      | (vl, id) :: tl -> save_args (MapIdent.add id vl map) tl
    in
    let* panics = read_panics in
    save_args MapIdent.empty (List.combine vargs (rpf afc.args))
    >>= fun map ->
    add_stack_frame
      { local_envs =
          { exec_block = afc.body; var_map = map; env_type = Default }, local_envs
      ; deferred_funcs = []
      ; returns = None
      ; panics
      }
    *> exec eval_stmt
    *> read_returns
    *> read_local_envs
    >>= fun (_, lenv) ->
    read_panics
    >>= fun panics ->
    delete_stack_frame
    *> write_panics panics
    *> write_local_envs (List.hd lenv, List.tl lenv)
    *> return ()
  | _ -> fail (Runtime_error (TypeCheckFailed "func_call"))

and retrieve_arg_value = function
  | Arg_expr e -> eval_expr e
  | Arg_type _ -> fail (Runtime_error (TypeCheckFailed "arg_value"))

and create_args_map args idents =
  let rec save_args map = function
    | [] -> return map
    | (expr, id) :: tl ->
      retrieve_arg_value expr >>= fun vl -> save_args (MapIdent.add id vl map) tl
  in
  save_args MapIdent.empty (List.combine args idents)

and eval_builtin args func =
  match func with
  | Make -> prepare_builtin_eval [] Make
  | _ -> map retrieve_arg_value args >>= fun vlst -> prepare_builtin_eval vlst func

and prepare_builtin_eval vlist = function
  | Print ->
    let list = List.map pp_value vlist in
    return (print_string (String.concat " " list)) *> return None
  | Println ->
    let list = List.map pp_value vlist in
    return (print_string (String.concat " " list ^ "\n")) *> return None
  | Make ->
    let* chan_id = create_chanel in
    return (Some (Value_chan (Chan_initialized chan_id)))
  | Close ->
    (match vlist with
     | [ Value_chan chan ] -> close_chanel chan *> return None
     | _ -> fail (Runtime_error (TypeCheckFailed "close")))
  | Len ->
    (match vlist with
     | [ Value_array (len, _) ] -> return (Some (Value_int len))
     | [ Value_string s ] -> return (Some (Value_int (String.length s)))
     | _ -> fail (Runtime_error (TypeCheckFailed "len")))
  | Panic -> write_panics (Some vlist) *> return None
  | Recover ->
    read_panics
    >>= fun pnc ->
    write_panics None
    *>
      (match pnc with
      | Some [ single_srg ] -> return (Some single_srg)
      | Some lst -> return (Some (Value_tuple lst))
      | None -> return (Some (Value_nil Nil)))

and eval_index array index =
  let* array = eval_expr array in
  let* index = eval_expr index in
  match array, index with
  | Value_array (_, values), Value_int index ->
    (try return (List.nth values index) with
     | Invalid_argument _ -> fail (Runtime_error Negative_array_index)
     | Failure _ -> fail (Runtime_error Array_index_out_of_bound))
  | _ -> fail (Runtime_error (TypeCheckFailed "index"))

and eval_unop op expr =
  let* value = eval_expr expr in
  match op, value with
  | Unary_minus, Value_int a -> return (Value_int (-a))
  | Unary_plus, Value_int a -> return (Value_int a)
  | Unary_not, Value_bool a -> return (Value_bool (not a))
  | _ -> fail (Runtime_error (TypeCheckFailed "unop"))

and eval_equal = function
  | Value_nil Nil, Value_nil Nil
  | Value_nil Nil, Value_func (Func_uninitialized Nil)
  | Value_nil Nil, Value_chan (Chan_uninitialized Nil)
  | Value_func (Func_uninitialized Nil), Value_nil Nil
  | Value_chan (Chan_uninitialized Nil), Value_nil Nil -> true
  | Value_nil Nil, _ | _, Value_nil Nil -> false
  | v1, v2 -> v1 = v2

and eval_binop op a1 a2 =
  let* a1 = eval_expr a1 in
  let* a2 = eval_expr a2 in
  match op, a1, a2 with
  | Bin_sum, Value_int a1, Value_int a2 -> return (Value_int (a1 + a2))
  | Bin_subtract, Value_int a1, Value_int a2 -> return (Value_int (a1 - a2))
  | Bin_multiply, Value_int a1, Value_int a2 -> return (Value_int (a1 * a2))
  | Bin_divide, Value_int a1, Value_int a2 ->
    (try return (Value_int (a1 / a2)) with
     | Division_by_zero -> fail (Runtime_error Division_by_0))
  | Bin_modulus, Value_int a1, Value_int a2 ->
    (try return (Value_int (a1 mod a2)) with
     | Division_by_zero -> fail (Runtime_error Division_by_0))
  | Bin_and, Value_bool a1, Value_bool a2 -> return (Value_bool (a1 && a2))
  | Bin_or, Value_bool a1, Value_bool a2 -> return (Value_bool (a1 || a2))
  | Bin_equal, a1, a2 -> return (Value_bool (eval_equal (a1, a2)))
  | Bin_not_equal, a1, a2 -> return (Value_bool (not (eval_equal (a1, a2))))
  | Bin_less, Value_int a1, Value_int a2 -> return (Value_bool (a1 < a2))
  | Bin_less_equal, Value_int a1, Value_int a2 -> return (Value_bool (a1 <= a2))
  | Bin_greater, Value_int a1, Value_int a2 -> return (Value_bool (a1 > a2))
  | Bin_greater_equal, Value_int a1, Value_int a2 -> return (Value_bool (a1 >= a2))
  | _ -> fail (Runtime_error (TypeCheckFailed "binop"))

and update_lvalue value = function
  | Lvalue_ident id -> update_ident id value
  | lvalue ->
    let* lvalue_value, indicies = collect_lvalue_indicies [] lvalue in
    let* new_lvalue = change_lvalue_index lvalue_value indicies value in
    update_ident (retrieve_lvalue_ident lvalue) new_lvalue

and collect_lvalue_indicies acc = function
  | Lvalue_ident id ->
    let* lvalue_value = read_ident id in
    return (lvalue_value, acc)
  | Lvalue_array_index (array, index) ->
    eval_expr index
    >>= (function
     | Value_int i -> collect_lvalue_indicies (i :: acc) array
     | _ -> fail (Runtime_error (TypeCheckFailed "prepare lvalue")))

and retrieve_lvalue_ident = function
  | Lvalue_ident id -> id
  | Lvalue_array_index (lv, _) -> retrieve_lvalue_ident lv

and change_lvalue_index target indicies value =
  match target with
  | Value_array (size, values) ->
    (match indicies with
     | [ i ] -> return (Value_array (i, replace_list values i value))
     | _ ->
       (try
          change_lvalue_index
            (List.nth values (List.hd indicies))
            (List.tl indicies)
            value
          >>= fun ls ->
          return (Value_array (size, replace_list values (List.hd indicies) ls))
        with
        | Failure _ -> fail (Runtime_error Array_index_out_of_bound)
        | Invalid_argument _ -> fail (Runtime_error Negative_array_index)))
  | _ -> return target

and eval_init = function
  | Some init ->
    (match init with
     | Init_assign asgn -> eval_stmt (Stmt_assign asgn)
     | Init_call call -> eval_stmt (Stmt_call call)
     | Init_decl decl -> eval_stmt (Stmt_short_var_decl decl)
     | Init_decr decr -> eval_stmt (Stmt_decr decr)
     | Init_incr incr -> eval_stmt (Stmt_incr incr)
     | Init_receive recv -> eval_chan_receive recv *> return ()
     | Init_send send -> eval_chan_send send)
  | None -> return ()

and eval_stmt = function
  | Stmt_call call -> eval_stmt_call call
  | Stmt_long_var_decl lvd -> eval_long_var_decl save_local_id lvd
  | Stmt_decr id ->
    read_ident id
    >>= (function
     | Value_int v -> update_ident id (Value_int (v - 1)) *> return ()
     | _ -> fail (Runtime_error (TypeCheckFailed "stmt")))
  | Stmt_incr id ->
    read_ident id
    >>= (function
     | Value_int v -> update_ident id (Value_int (v + 1)) *> return ()
     | _ -> fail (Runtime_error (TypeCheckFailed "stmt")))
  | Stmt_assign asgn -> eval_assign asgn
  | Stmt_short_var_decl decl -> eval_short_var_decl decl
  | Stmt_if if' -> eval_if if'
  | Stmt_go call -> eval_go call
  | Stmt_block body -> add_env body Default *> exec eval_stmt *> delete_env
  | Stmt_break -> exec eval_stmt *> delete_env
  | Stmt_continue ->
    (read_local_envs
     >>= skip_local_env
     >>= fun lst -> write_local_envs (List.hd lst, List.tl lst))
    *> return ()
  | Stmt_for for' -> eval_for for'
  | Stmt_return exprs -> eval_return exprs
  | Stmt_chan_send send -> eval_chan_send send
  | Stmt_chan_receive recv -> eval_chan_receive recv *> return ()
  | Stmt_defer (func, args) ->
    eval_expr func
    >>= fun func_value ->
    map retrieve_arg_value args >>= fun arg_values -> add_deferred (func_value, arg_values)

and eval_stmt_call (func, args) =
  eval_expr func
  >>= function
  | Value_func (Func_initialized (Closure _, afc)) ->
    eval_closure (func, args)
    >>= fun (_, vmp) ->
    (match func with
     | Expr_ident id -> update_ident id (Value_func (Func_initialized (Closure vmp, afc)))
     | _ -> return ())
  | _ -> eval_func_call (func, args) *> return ()

and eval_short_var_decl = function
  | Short_decl_mult_init (sfirst, lst) ->
    iter (fun (ident, expr) -> eval_expr expr >>= save_local_id ident) (sfirst :: lst)
  | Short_decl_one_init (idnt1, idnt2, idntlst, fcall) ->
    eval_func_call fcall
    >>= (function
     | Some (Value_tuple tup) -> iter2 save_local_id (idnt1 :: idnt2 :: idntlst) tup
     | _ -> fail (Runtime_error (TypeCheckFailed "short decl")))

and eval_if { if_init; if_cond; if_body; else_body } =
  eval_init if_init *> eval_expr if_cond
  >>= function
  | Value_bool true -> add_env if_body Default *> exec eval_stmt *> delete_env
  | Value_bool false ->
    (match else_body with
     | Some (Else_block body) -> add_env body Default *> exec eval_stmt *> delete_env
     | Some (Else_if if') -> eval_stmt (Stmt_if if')
     | None -> return ())
  | _ -> fail (Runtime_error (TypeCheckFailed "if"))

and eval_long_var_decl save_to_env = function
  | Long_decl_mult_init (_, hd, tl) ->
    iter (fun (ident, expr) -> eval_expr expr >>= save_to_env ident) (hd :: tl)
  | Long_decl_one_init (_, idnt1, idnt2, idntlst, fcall) ->
    eval_func_call fcall
    >>= (function
     | Some (Value_tuple tup) -> iter2 save_to_env (idnt1 :: idnt2 :: idntlst) tup
     | _ -> fail (Runtime_error (TypeCheckFailed "short decl")))
  | Long_decl_no_init (typ, id, id_list) ->
    iter (fun idnt -> save_to_env idnt (default_init typ)) (id :: id_list)

and eval_assign = function
  | Assign_mult_expr (fst, lst) ->
    iter
      (fun (lvalue, expr) -> eval_expr expr >>= fun ex -> update_lvalue ex lvalue)
      (fst :: lst)
  | Assign_one_expr (fst, snd, lst, fcall) ->
    eval_func_call fcall
    >>= (function
     | Some (Value_tuple tup) ->
       iter2 (fun v lv -> update_lvalue v lv *> return ()) tup (fst :: snd :: lst)
     | _ -> fail (Runtime_error (TypeCheckFailed "short decl")))

and eval_for { for_init; for_cond; for_post; for_body } =
  let one_iter =
    match for_cond with
    | Some cond ->
      eval_expr cond
      >>= (function
       | Value_bool true ->
         add_env for_body Default
         *> exec eval_stmt
         *> eval_init for_post
         *> delete_env
         *> return true
       | Value_bool false -> delete_env *> return false
       | _ -> fail (Runtime_error (TypeCheckFailed "for cond")))
    | None ->
      add_env for_body Default
      *> exec eval_stmt
      *> eval_init for_post
      *> delete_env
      *> return true
  in
  let rec cycle rfor =
    rfor
    >>= function
    | true ->
      read_local_envs
      >>= fun ({ env_type; _ }, _) ->
      (match env_type with
       | For -> cycle rfor
       | Default -> return ())
    | false -> return ()
  in
  add_env [] For *> eval_init for_init *> cycle one_iter

and eval_return = function
  | [ Expr_const (Const_func afc) ] ->
    read_local_envs
    >>= (fun (hd, _) -> return (Value_func (Func_initialized (Closure hd.var_map, afc))))
    >>= fun vfun -> write_returns (Some vfun)
  | [ expr ] -> eval_expr expr >>= fun ret -> write_returns (Some ret)
  | exprs ->
    map eval_expr exprs >>= fun values -> write_returns (Some (Value_tuple values))

and default_init = function
  | Type_int -> Value_int 0
  | Type_bool -> Value_bool false
  | Type_string -> Value_string ""
  | Type_func _ -> Value_func (Func_uninitialized Nil)
  | Type_chan _ -> Value_chan (Chan_uninitialized Nil)
  | Type_array (size, t) -> Value_array (size, List.init size (fun _ -> default_init t))

and eval_chan_send (ident, expr) =
  let* value = eval_expr expr in
  read_ident ident
  >>= function
  | Value_chan chan ->
    let* chan_id = find_chanel_fail chan in
    let* sending_goroutine = read_running_fail in
    delete_running_goroutine
    *> push_to_send_queue chan_id sending_goroutine value
    *> attempt_chan_interaction chan_id Sender
    *> (is_using_chanel
        >>= function
        | Some _ -> return ()
        | None ->
          check_ready_goroutine
          >>= (function
           | None ->
             fail
               (Runtime_error
                  (Deadlock
                     (asprintf
                        "goroutine %d trying to send to chan %d"
                        sending_goroutine.go_id
                        chan_id)))
           | Some _ -> run_ready_goroutines eval_stmt))
  | _ -> fail (Runtime_error (TypeCheckFailed "chan send"))

and eval_chan_receive expr =
  eval_expr expr
  >>= function
  | Value_chan chan ->
    let* chan_id = find_chanel_fail chan in
    let* receiving_goroutine = read_running_fail in
    delete_running_goroutine
    *> push_to_receive_queue chan_id receiving_goroutine
    *> attempt_chan_interaction chan_id Receiver
    *> (is_using_chanel
        >>= function
        | Some _ -> return ()
        | None ->
          run_ready_goroutines eval_stmt *> is_using_chanel
          >>= (function
           | Some _ -> return ()
           | None ->
             fail
               (Runtime_error
                  (Deadlock
                     (asprintf
                        "goroutine %d trying to receive from chan %d"
                        receiving_goroutine.go_id
                        chan_id)))))
    *>
    let* { receiving_goroutine; sending_goroutine; value; inited_by } = use_chanel in
    (match inited_by with
     | Receiver ->
       add_ready sending_goroutine *> run_goroutine receiving_goroutine *> return value
     | Sender -> run_goroutine receiving_goroutine *> return value)
  | _ -> fail (Runtime_error (TypeCheckFailed "chan receive"))

and eval_go (func, arg_exprs) =
  eval_expr func
  >>= function
  | Value_func (Func_uninitialized Nil) -> fail (Runtime_error Uninited_func)
  | Value_func (Func_builtin _) -> eval_func_call (func, arg_exprs) *> return ()
  | Value_func (Func_initialized (Default, { args; body; _ }))
  | Value_func (Func_initialized (FuncLit, { args; body; _ })) ->
    let* var_map = create_args_map arg_exprs (rpf args) in
    create_goroutine
      { local_envs = { exec_block = body; var_map; env_type = Default }, []
      ; deferred_funcs = []
      ; returns = None
      ; panics = None
      }
  | Value_func (Func_initialized (Closure closure_map, { args; body; _ })) ->
    let* var_map = create_args_map arg_exprs (rpf args) in
    let var_map = MapIdent.union (fun _key v1 _v2 -> Some v1) var_map closure_map in
    create_goroutine
      { local_envs = { exec_block = body; var_map; env_type = Default }, []
      ; deferred_funcs = []
      ; returns = None
      ; panics = None
      }
  | _ -> fail (Runtime_error (TypeCheckFailed "func call"))
;;

let save_builtins =
  save_global_id "true" (Value_bool true)
  *> save_global_id "false" (Value_bool false)
  *> save_global_id "nil" (Value_nil Nil)
  *> save_global_id "print" (Value_func (Func_builtin Print))
  *> save_global_id "println" (Value_func (Func_builtin Println))
  *> save_global_id "make" (Value_func (Func_builtin Make))
  *> save_global_id "close" (Value_func (Func_builtin Close))
  *> save_global_id "len" (Value_func (Func_builtin Len))
  *> save_global_id "recover" (Value_func (Func_builtin Recover))
  *> save_global_id "panic" (Value_func (Func_builtin Panic))
;;

let save_global_vars_and_funcs file =
  save_builtins
  *> create_goroutine (* goroutine for global variables evaluating *)
       { local_envs =
           { exec_block = []; var_map = MapIdent.empty; env_type = Default }, []
       ; deferred_funcs = []
       ; returns = None
       ; panics = None
       }
  *> run_ready_goroutine
  *> iter
       (function
         | Decl_var decl -> eval_long_var_decl save_global_id decl
         | Decl_func (ident, afc) ->
           save_global_id ident (Value_func (Func_initialized (Default, afc))))
       file
  *> delete_running_goroutine
;;

let add_main_goroutine =
  iter (function
    | Decl_func ("main", { body; _ }) ->
      create_goroutine
        { local_envs =
            { exec_block = body; var_map = MapIdent.empty; env_type = Default }, []
        ; deferred_funcs = []
        ; returns = None
        ; panics = None
        }
    | _ -> return ())
;;

let init_state =
  { global_env = MapIdent.empty
  ; running = None
  ; ready = ReadySet.empty
  ; sending = SendingSet.empty
  ; receiving = ReceivingSet.empty
  ; chanels = ChanSet.empty, 1
  ; is_using_chanel = None
  ; next_go_id = 0
  }
;;

let eval file =
  run
    (save_global_vars_and_funcs file
     *> add_main_goroutine file
     *> run_ready_goroutines eval_stmt)
    init_state
  |> function
  | _, res -> res
;;
