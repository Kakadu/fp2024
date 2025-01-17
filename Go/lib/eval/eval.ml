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
  | Value_tuple lst -> Format.asprintf "[%s]" (PpType.sep_by_comma lst pp_value)
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

let rec replace_list list index elem =
  match list with
  | [] -> list
  | h :: t ->
    if index = 0
    then elem :: replace_list t (index - 1) elem
    else h :: replace_list t (index - 1) elem
;;

let rec exec eval_stmt =
  exec_stmt eval_stmt
  >>= function
  | Some _ ->
    read_returns
    >>= (function
     | None ->
       let* is_using_chanel = is_using_chanel in
       if is_using_chanel then return () else exec eval_stmt
     | Some _ -> return ())
  | None -> return ()
;;

let run_ready_goroutines eval_stmt =
  let rec runner waiting_goroutines =
    match GoSet.find_first_opt (fun { state } -> state = Ready) waiting_goroutines with
    | Some { goroutine } ->
      run_goroutine goroutine
      *> exec eval_stmt
      *>
      let* is_using_chanel = is_using_chanel in
      if is_using_chanel
      then return ()
      else
        (* мб тут проверять в каком-то виде останавливаться, если запущенная горутна отправляет что-то в канал *)
        read_running_id
        >>= (function
         | 1 ->
           (* main goroutine finished working and doesn,t wait for others to finish *)
           return ()
         | _ ->
           (* some secondary goroutine finished working, don't stop untill main finished *)
           delete_running_goroutine *> read_waiting >>= runner)
    | None -> return ()
  in
  read_waiting >>= runner
;;

(** [attempt_chan_interaction id] attempts to use chanel with given id. If both sender and receiver are ready,
    starts using the chanel. Doesn't do anything otherwise *)
let attempt_chan_interaction id =
  let* waiting_goroutines = read_waiting in
  let* sending_goroutine =
    match
      GoSet.find_first_opt
        (function
          | { state = Sending { chan_id } } -> chan_id = id
          | _ -> false)
        waiting_goroutines
    with
    | Some { goroutine } -> return (Some goroutine)
    | _ -> return None
  in
  let* receiving_goroutine =
    match
      GoSet.find_first_opt
        (function
          | { state = Receiving { chan_id } } -> chan_id = id
          | _ -> false)
        waiting_goroutines
    with
    | Some { goroutine } -> return (Some goroutine)
    | _ -> return None
  in
  match sending_goroutine, receiving_goroutine with
  | Some sending_goroutine, Some receiving_goroutine ->
    let* value = pop_chan_value id in
    ready_waiting sending_goroutine
    *> ready_waiting receiving_goroutine
    *> start_using_chanel receiving_goroutine value
  | _ -> return ()
;;

let rec eval_expr = function
  | Expr_const (Const_int n) -> return (Value_int n)
  | Expr_const (Const_string s) -> return (Value_string s)
  | Expr_const (Const_array (size, _, exprs)) ->
    map eval_expr exprs >>= fun values -> return (Value_array (size, values))
  | Expr_const (Const_func afunc) ->
    read_local_envs
    >>= fun (hd, _) -> return (Value_func (Func_initialized (FuncLit, afunc)))
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
         | Some lst -> return lst (* тут может быть тапл *)
         | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed "expr"))))
     | _ ->
       eval_func_call (func, args)
       >>= (function
        | Some lst -> return lst (* тут может быть тапл *)
        | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed "expr")))))
  | Expr_chan_receive receive -> eval_chan_receive receive

and eval_func_call (func, args) =
  eval_expr func
  >>= function
  | Value_func (Func_uninitialized _) -> fail (Runtime_error Uninited_func)
  | Value_func (Func_builtin ftype) -> eval_builtin args ftype
  | Value_func (Func_initialized (Default, afc)) ->
    (* тут нужна проверка на замыкание *)
    create_args_map args (rpf afc.args)
    >>= fun map ->
    add_stack_frame
      { local_envs = { exec_block = afc.body; var_map = map; env_type = Default }, []
      ; deferred_funcs = []
      ; returns = None
      }
    *> exec eval_stmt
    *> read_returns
    >>= fun x -> delete_stack_frame *> return x
  | Value_func (Func_initialized (FuncLit, afc)) ->
    let* local_envs = read_local_envs >>= fun (fl, lstl) -> return (fl :: lstl) in
    create_args_map args (rpf afc.args)
    >>= fun map ->
    add_stack_frame
      { local_envs =
          { exec_block = afc.body; var_map = map; env_type = Default }, local_envs
      ; deferred_funcs = []
      ; returns = None
      }
    *> exec eval_stmt
    *> read_returns
    >>= fun x ->
    read_local_envs
    >>= fun (_, lenv) ->
    delete_stack_frame *> write_local_envs (List.hd lenv, List.tl lenv) *> return x
  | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed " func_call")))

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
      }
    *> exec eval_stmt
    *> read_returns
    >>= fun x ->
    read_local_envs
    >>= fun (_, lenv) ->
    delete_stack_frame
    *> write_local_envs (List.hd (List.tl lenv), List.tl (List.tl lenv))
    *> return (x, (List.hd lenv).var_map)
  | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed " closure_call")))

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
  | Bin_subtract, Value_int a1, Value_int a2 -> return (Value_int (a1 - a2))
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

and eval_lvalue value = function
  | Lvalue_ident id -> update_ident id value
  | lvi ->
    prepare_lvalue_index [] lvi
    >>= fun (tr, ind) ->
    change_lvalue_index tr ind value
    >>= fun new_lst -> update_ident (retrieve_lvalue_ident_index lvi) new_lst *> return ()

and prepare_lvalue_index lst = function
  | Lvalue_ident id ->
    let* current = read_ident id in
    return (current, lst)
  | Lvalue_array_index (lv, exp) ->
    eval_expr exp
    >>= (function
     | Value_int i -> prepare_lvalue_index (i :: lst) lv
     | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed " prepare lval"))))

and retrieve_lvalue_ident_index = function
  | Lvalue_ident id -> id
  | Lvalue_array_index (lv, _) -> retrieve_lvalue_ident_index lv

and change_lvalue_index target indexes value =
  match target with
  | Value_array (i, lst) ->
    (match indexes with
     | [ i ] -> return (Value_array (i, replace_list lst i value))
     | _ ->
       if List.hd indexes < i
       then
         change_lvalue_index (List.nth lst (List.hd indexes)) (List.tl indexes) value
         >>= fun ls -> return (Value_array (i, replace_list lst (List.hd indexes) ls))
       else fail (Runtime_error Array_index_out_of_bound))
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
  | Stmt_call (func, args) ->
    eval_expr func
    >>= (function
     | Value_func (Func_initialized (Closure _, afc)) ->
       eval_closure (func, args)
       >>= fun (_, vmp) ->
       (match func with
        | Expr_ident id ->
          update_ident id (Value_func (Func_initialized (Closure vmp, afc)))
        | _ -> return ())
     | _ -> eval_func_call (func, args) *> return ())
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
         (fun (lvalue, expr) -> eval_expr expr >>= fun ex -> eval_lvalue ex lvalue)
         (fst :: lst)
     | Assign_one_expr (fst, snd, lst, fcall) ->
       eval_func_call fcall
       >>= (function
        | Some (Value_tuple tup) ->
          iter2 (fun v lv -> eval_lvalue v lv *> return ()) tup (fst :: snd :: lst)
        | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed "short decl")))))
  | Stmt_short_var_decl svd ->
    (match svd with
     | Short_decl_mult_init (sfirst, lst) ->
       iter (fun (ident, expr) -> eval_expr expr >>= save_local_id ident) (sfirst :: lst)
     | Short_decl_one_init (idnt1, idnt2, idntlst, fcall) ->
       eval_func_call fcall
       >>= (function
        | Some (Value_tuple tup) -> iter2 save_local_id (idnt1 :: idnt2 :: idntlst) tup
        | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed "short decl")))))
  | Stmt_if if' ->
    eval_expr if'.cond
    >>= (function
     | Value_bool true ->
       add_env if'.if_body Default *> eval_init if'.init *> exec eval_stmt *> delete_env
     | Value_bool false ->
       (match if'.else_body with
        | Some (Else_block body) ->
          add_env body Default *> eval_init if'.init *> exec eval_stmt *> delete_env
        | Some (Else_if if') -> eval_stmt (Stmt_if if')
        | None -> return ())
     | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed "if"))))
  | Stmt_go call -> eval_go call
  | Stmt_block body -> add_env body Default *> exec eval_stmt *> delete_env
  | Stmt_break -> exec eval_stmt *> delete_env
  | Stmt_continue -> exec eval_stmt
  | Stmt_for fr ->
    let rec_for =
      return fr.cond
      >>= function
      | Some ex ->
        eval_expr ex
        >>= (function
         | Value_bool true ->
           add_env fr.body Default
           *> exec eval_stmt
           *> eval_init fr.post
           *> delete_env
           *> return true
         | Value_bool false -> delete_env *> return false
         | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed " for"))))
      | None ->
        add_env fr.body Default
        *> exec eval_stmt
        *> eval_init fr.post
        *> delete_env
        *> return true
    in
    let rec rect rfor =
      rfor
      >>= function
      | true ->
        read_local_envs
        >>= fun (ls, _) ->
        (match ls.env_type with
         | For -> rect rfor
         | Default -> return ())
      | false -> return ()
    in
    add_env [] For *> eval_init fr.init *> rect rec_for *> return ()
  | Stmt_return l_exp ->
    (match l_exp with
     | [ x ] ->
       (match x with
        | Expr_const (Const_func afc) ->
          read_local_envs
          >>= (fun (hd, _) ->
                return (Value_func (Func_initialized (Closure hd.var_map, afc))))
          >>= fun vfun -> write_returns (Some vfun)
        | _ -> eval_expr x >>= fun ret -> write_returns (Some ret))
     | _ -> map eval_expr l_exp >>= fun lst -> write_returns (Some (Value_tuple lst)))
  | Stmt_chan_send send -> eval_chan_send send
  | Stmt_chan_receive recv -> eval_chan_receive recv *> return ()
  | Stmt_defer _ -> fail (Runtime_error (Panic "Not supported stmt"))

(*ДОДЕЛАТЬ*)

and eval_long_var_decl save_to_env = function
  | Long_decl_mult_init (_, hd, tl) ->
    iter (fun (ident, expr) -> eval_expr expr >>= save_to_env ident) (hd :: tl)
  | Long_decl_one_init (_, idnt1, idnt2, idntlst, fcall) ->
    eval_func_call fcall
    >>= (function
     | Some (Value_tuple tup) -> iter2 save_to_env (idnt1 :: idnt2 :: idntlst) tup
     | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed "short decl"))))
  | Long_decl_no_init (_, id, id_list) ->
    iter (fun ident -> save_to_env ident (Value_nil Nil)) (id :: id_list)

and eval_chan_send (ident, expr) =
  let* value = eval_expr expr in
  read_ident ident
  >>= function
  | Value_chan chan ->
    let* chan_id = find_chanel_fail chan in
    push_chan_value chan_id value
    *> stop_running_goroutine (Sending { chan_id })
    *> attempt_chan_interaction chan_id
    *>
    let* send_success = is_using_chanel in
    if send_success
    then
      (* the receiver is ready to receive, state will be proccessed and the value will be received *)
      return ()
    else (* the receiver isn't ready to receive yet *)
      run_ready_goroutines eval_stmt
  | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed "chan send")))

and eval_chan_receive expr =
  eval_expr expr
  >>= function
  | Value_chan chan ->
    let* chan_id = find_chanel_fail chan in
    stop_running_goroutine (Receiving { chan_id })
    *> attempt_chan_interaction chan_id
    *>
    let* receive_success = is_using_chanel in
    (if receive_success
     then return ()
     else run_ready_goroutines eval_stmt (* тут мы должны в нужный момент остановиться *))
    *>
    let* receiving_goroutine, value = use_chanel in
    run_goroutine receiving_goroutine *> return value
    (* ЗАГЛУШКА *)
    (* надо как-то получать значение, поэтому после передачи всегда должна запускаться эта горутина *)
  | _ -> fail (Runtime_error (DevOnly (TypeCheckFailed "chan receive")))

and eval_go (func, arg_exprs) =
  eval_expr func
  >>= function
  | Value_func (Func_uninitialized Nil) -> fail (Runtime_error Uninited_func)
  | Value_func (Func_builtin _) -> return () (* TODO *)
  | Value_func (Func_initialized (is_closure, { args; body })) ->
    (* TODO closure *)
    let* var_map = create_args_map arg_exprs (rpf args) in
    create_goroutine
      { local_envs = { exec_block = body; var_map; env_type = Default }, []
      ; deferred_funcs = []
      ; returns = None
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
      create_goroutine
        { local_envs =
            { exec_block = body; var_map = MapIdent.empty; env_type = Default }, []
        ; deferred_funcs = []
        ; returns = None
        }
    | _ -> return ())
;;

let run_eval file =
  save_builtins
  *> save_global_vars_and_funcs file
  *> add_main_goroutine file
  *> run_ready_goroutines eval_stmt
  *> return ()
;;

let init_state =
  { global_env = MapIdent.empty
  ; running = None
  ; waiting = GoSet.empty
  ; chanels = ChanSet.empty, 1
  ; is_using_chanel = None
  }
;;

let eval file =
  run (run_eval file) init_state
  |> function
  | _, res -> res (* mb check final state *)
;;
