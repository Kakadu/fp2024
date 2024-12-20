(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open TypeCheckMonad
open TypeCheckMonad.CheckMonad
open Errors
open Ast

let retrieve_pairs_first args = List.map (fun (fst, _) -> fst) args
let retrieve_pairs_second args = List.map (fun (_, snd) -> snd) args

let find_func name code =
  let func_name = function
    | Decl_func (func_name, _) -> func_name
    | Decl_var _ -> ""
  in
  Stdlib.List.find_opt (fun func -> String.equal (func_name func) name) code
;;

let retrieve_anon_func anon_func =
  let args = retrieve_pairs_second anon_func.args in
  match anon_func.returns with
  | Some (Only_types (hd, tl)) -> Ctype (Type_func (args, hd :: tl))
  | Some (Ident_and_types (hd, tl)) ->
    Ctype (Type_func (args, retrieve_pairs_second (hd :: tl)))
  | None -> Ctype (Type_func (args, []))
;;

let check_anon_func anon_func cstmt =
  let save_args = iter (fun (id, t) -> save_local_ident_l id (Ctype t)) anon_func.args in
  write_env
  *> save_args
  *> iter (fun stmt -> cstmt stmt) anon_func.body
  *> delete_env
  *> return (retrieve_anon_func anon_func)
;;

let retrieve_const caf = function
  | Const_array (size, t, _) -> return (Ctype (Type_array (size, t)))
  | Const_int _ -> return (Ctype Type_int)
  | Const_string _ -> return (Ctype Type_string)
  | Const_func anon_func -> check_anon_func anon_func caf
;;

let check_main file =
  match find_func "main" file with
  | Some (Decl_func (_, { args = []; returns = None; body = _ })) -> return ()
  | Some (Decl_func _) ->
    fail
      (Type_check_error
         (Incorrect_main
            (Printf.sprintf "func main must have no arguments and no return values")))
  | _ -> fail (Type_check_error (Incorrect_main (Printf.sprintf "main func not found")))
;;

(* В следующих трёх функциях надо что-то поменять, они используются не только в биноперах *)
let eq e el1 el2 =
  match e el1 el2 with
  | true -> return el1
  | false -> fail (Type_check_error (Mismatched_types "Types mismatched in binoper"))
;;

let eq_type t1 t2 = eq equal_ctype t1 t2

let check_eq t1 t2 =
  match equal_ctype t1 t2 with
  | true -> return ()
  | false -> fail (Type_check_error (Mismatched_types "Types mismatched in binoper"))
;;

let rec retrieve_expr caf = function
  | Expr_const const -> retrieve_const caf const
  | Expr_un_oper (op, expr) ->
    (match op with
     | Unary_minus | Unary_plus ->
       retrieve_expr caf expr
       >>= fun t -> check_eq t (Ctype Type_int) *> return (Ctype Type_int)
     | Unary_not -> retrieve_expr caf expr)
    >>= fun t -> check_eq t (Ctype Type_bool) *> return (Ctype Type_bool)
  | Expr_ident id -> retrieve_ident id
  | Expr_bin_oper (op, left, right) ->
    let compare_arg_typ type1 type2 =
      retrieve_expr caf type1
      >>= fun type1 -> retrieve_expr caf type2 >>= fun type2 -> eq_type type1 type2
    in
    let compare_operation_typ type1 type2 t =
      compare_arg_typ type1 type2 >>= fun e -> eq_type e t
    in
    (match op with
     | Bin_sum | Bin_divide | Bin_modulus | Bin_multiply | Bin_subtract ->
       compare_operation_typ left right (Ctype Type_int) *> return (Ctype Type_int)
     | Bin_less | Bin_greater | Bin_greater_equal | Bin_less_equal ->
       compare_operation_typ left right (Ctype Type_int) *> return (Ctype Type_bool)
     | Bin_or | Bin_and ->
       compare_operation_typ left right (Ctype Type_bool) *> return (Ctype Type_bool)
     | Bin_equal | Bin_not_equal -> compare_arg_typ left right *> return (Ctype Type_bool))
  | Expr_call (func, args) ->
    map (retrieve_expr caf) args
    *> (retrieve_expr caf) func (* кажется нет проверки того, что аргументы правильные *)
    >>= (function
     | Ctype (Type_func (_, fst :: snd :: tl)) -> return (Ctuple (fst :: snd :: tl))
     | Ctype (Type_func (_, hd :: _)) -> return (Ctype hd)
     | _ ->
       fail (Type_check_error (Mismatched_types "Function without returns in expression")))
  | Expr_chan_receive x -> retrieve_expr caf x (* ошибка *)
  | Expr_index (array, index) when retrieve_expr caf index = return (Ctype Type_int) ->
    retrieve_expr caf array
    >>= (function
     | Ctype (Type_array (_, t)) -> return (Ctype t)
     | _ ->
       fail (Type_check_error (Mismatched_types "Non-array type in array index call")))
  | Expr_index (_, _) ->
    fail (Type_check_error (Mismatched_types "Array index is not int"))
;;

let retrieve_idents_from_long_var_decl caf env decl =
  let env_r =
    match env with
    | Loc -> save_local_ident_r
    | Glob -> save_global_ident_r
  in
  let env_l =
    match env with
    | Loc -> save_local_ident_l
    | Glob -> save_global_ident_l
  in
  match decl with
  | Long_decl_no_init (t, hd, tl) -> iter (env_r (Ctype t)) (hd :: tl)
  | Long_decl_mult_init (Some t, hd, tl) ->
    iter
      ((fun k (id, expr) ->
         (retrieve_expr caf expr >>= fun x -> check_eq x k) *> env_r k id)
         (Ctype t))
      (hd :: tl)
  | Long_decl_mult_init (None, hd, tl) ->
    iter (fun (id, expr) -> retrieve_expr caf expr >>= env_l id) (hd :: tl)
  | Long_decl_one_init (Some t, fst, snd, tl, call)
    when retrieve_expr caf (Expr_call call)
         = return (Ctuple (List.init (List.length (fst :: snd :: tl)) (fun _ -> t))) ->
    iter (env_r (Ctype t)) (fst :: snd :: tl)
  | Long_decl_one_init (None, fst, snd, tl, call) ->
    retrieve_expr caf (Expr_call call)
    >>= (function
     | Ctype _ ->
       fail
         (Type_check_error
            (Mismatched_types "function returns only one element in multiple var decl"))
     | Ctuple types when List.length types = List.length (fst :: snd :: tl) ->
       iter2 (fun x y -> env_l x y) (fst :: snd :: tl) (List.map (fun x -> Ctype x) types)
     | Ctuple _ ->
       fail
         (Type_check_error
            (Mismatched_types
               "function returns wrong number of elements in multiple var decl")))
  | Long_decl_one_init (Some _, _, _, _, _) ->
    fail (Type_check_error (Mismatched_types "multiple return types mismatched"))
;;

(*iter
  (fun (x, z) -> retrieve_type_expr z >>= env_l x)
  (List.combine (x :: y :: z) (List.map (fun _ -> Expr_call l) (x :: y :: z))))*)

let retrieve_idents_from_short_var_decl caf = function
  | Short_decl_mult_init (hd, tl) ->
    iter (fun (id, expr) -> retrieve_expr caf expr >>= save_local_ident_l id) (hd :: tl)
  | Short_decl_one_init (fst, snd, tl, call) ->
    iter
      (fun (id, expr) -> retrieve_expr caf expr >>= save_local_ident_l id)
      (List.combine
         (fst :: snd :: tl)
         (List.map (fun _ -> Expr_call call) (fst :: snd :: tl)))
;;

let check_func_call caf (func, args) =
  retrieve_expr caf func *> map (retrieve_expr caf) args *> return ()
;;

let rec retrieve_lvalue caf = function
  | Lvalue_ident id -> retrieve_ident id
  | Lvalue_array_index (Lvalue_ident array, index)
    when retrieve_expr caf index = return (Ctype Type_int) ->
    retrieve_ident array
    >>= (function
     | Ctype (Type_array (_, t)) -> return (Ctype t)
     | _ ->
       fail (Type_check_error (Mismatched_types "Non-array type in array index call")))
  | Lvalue_array_index (lvalue_array_index, index)
    when retrieve_expr caf index = return (Ctype Type_int) ->
    retrieve_lvalue caf lvalue_array_index
  | Lvalue_array_index (_, _) ->
    fail (Type_check_error (Mismatched_types "Array index is not int"))
;;

let check_assign caf = function
  | Assign_mult_expr (hd, tl) ->
    iter
      (fun (lvalue, expr) ->
        retrieve_lvalue caf lvalue
        >>= fun type1 -> retrieve_expr caf expr >>= fun type2 -> check_eq type1 type2)
      (hd :: tl)
  | Assign_one_expr (x, y, _, w) ->
    retrieve_lvalue caf x
    *> retrieve_lvalue caf y
    *> check_func_call caf w (* кажется неправильно *)
;;

let check_init caf = function
  | Some (Init_assign assign) -> check_assign caf assign
  | Some (Init_call call) -> check_func_call caf call
  | Some (Init_decl _) -> return () (*доделать*)
  | Some (Init_decr id) -> retrieve_ident id >>= fun t -> check_eq t (Ctype Type_int)
  | Some (Init_incr id) -> retrieve_ident id >>= fun t -> check_eq t (Ctype Type_int)
  | Some (Init_receive chan) ->
    retrieve_expr caf chan *> return () (* кажется неправильно *)
  | Some (Init_send (id, expr)) -> seek_ident id *> retrieve_expr caf expr *> return ()
  | None -> return ()
;;

let rec check_stmt = function
  | Stmt_long_var_decl long_decl ->
    retrieve_idents_from_long_var_decl check_stmt Loc long_decl
  | Stmt_short_var_decl short_decl ->
    retrieve_idents_from_short_var_decl check_stmt short_decl
  | Stmt_incr id -> retrieve_ident id >>= fun t -> check_eq t (Ctype Type_int)
  | Stmt_decr id -> retrieve_ident id >>= fun t -> check_eq t (Ctype Type_int)
  | Stmt_assign assign -> check_assign check_stmt assign
  | Stmt_call call -> check_func_call check_stmt call
  | Stmt_defer call -> check_func_call check_stmt call
  | Stmt_go call -> check_func_call check_stmt call
  | Stmt_chan_send (id, expr) ->
    seek_ident id *> retrieve_expr check_stmt expr *> return ()
  | Stmt_block block ->
    iter check_stmt block
    (* Тут (и в ифе с фором) по идее надо создавать новую мапу, внутри блока свои локальные переменные *)
  | Stmt_break -> return ()
  | Stmt_chan_receive chan -> retrieve_expr check_stmt chan *> return ()
  | Stmt_continue -> return ()
  | Stmt_return exprs ->
    (get_func_name
     >>= retrieve_ident
     >>= (function
            | Ctype (Type_func (_, return_types)) ->
              (match List.length exprs = List.length return_types with
               | true ->
                 return (List.combine exprs (List.map (fun x -> Ctype x) return_types))
               | false ->
                 fail (Type_check_error (Mismatched_types "func return types mismatch")))
            | _ -> fail (Type_check_error Check_failed))
     >>= iter (fun (x, y) -> retrieve_expr check_stmt x >>= fun type1 -> check_eq y type1)
    )
    *> return ()
  | Stmt_if if' ->
    (* надо создавать мапу *)
    check_init check_stmt if'.init
    *> retrieve_expr check_stmt if'.cond
    *> iter check_stmt if'.if_body
    *>
      (match if'.else_body with
      | Some (Else_block block) -> iter check_stmt block
      | Some (Else_if if') -> check_stmt (Stmt_if if')
      | None -> return ())
  | Stmt_for { init; cond; post; body } ->
    check_init check_stmt init (* надо создавать мапу *)
    *> (match cond with
      | Some x -> retrieve_expr check_stmt x *> return ()
      | None -> return ())
    *> check_init check_stmt post
    *> iter check_stmt body
;;

(* непон зачем тут две функции *)
let check_top_decl_funcs = function
  | Decl_func (id, args_returns_and_body) ->
    save_global_ident_r (retrieve_anon_func args_returns_and_body) id
  | Decl_var _ -> return ()
;;

let check_top_decl = function
  | Decl_func (x, y) -> write_func_name x *> check_anon_func y check_stmt *> return ()
  | Decl_var x -> retrieve_idents_from_long_var_decl check_stmt Glob x
;;

let type_check file =
  run
    (check_main file *> iter check_top_decl_funcs file *> iter check_top_decl file)
    (MapIdent.empty, [ MapIdent.empty ], None)
;;

let pp ast =
  match type_check ast with
  | _, Result.Ok _ -> print_endline "CORRECT"
  | _, Result.Error x ->
    prerr_string "ERROR WHILE TYPECHECK WITH ";
    (match x with
     | Type_check_error Check_failed -> prerr_endline "Check failed"
     | Type_check_error (Multiple_declaration x) ->
       prerr_string ("Multiple declaration error: " ^ x)
     | Type_check_error (Incorrect_main x) -> prerr_endline ("Incorrect main error: " ^ x)
     | Type_check_error (Undefined_ident x) ->
       prerr_endline ("Undefined ident error: " ^ x)
     | Type_check_error (Mismatched_types x) -> prerr_endline ("Mismatched types: " ^ x)
     | Type_check_error (Cannot_assign x) -> prerr_endline ("Mismatched types: " ^ x))
;;
