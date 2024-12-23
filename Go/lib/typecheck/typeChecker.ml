(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open TypeCheckMonad
open TypeCheckMonad.CheckMonad
open Errors
open Ast

let get_afunc_type afunc =
  Ctype (Type_func (List.map (fun (_, snd) -> snd) afunc.args, afunc.returns))
;;

let check_anon_func afunc cstmt =
  let save_args = iter (fun (id, t) -> save_ident id (Ctype t)) afunc.args in
  add_env
  *> save_func (Ctuple afunc.returns)
  *> save_args
  *> iter cstmt afunc.body
  *> delete_func
  *> delete_env
  *> return (get_afunc_type afunc)
;;

let eq_type t1 t2 =
  if equal_ctype t1 t2
  then return t1
  else
    fail
      (Type_check_error
         (Mismatched_types (Printf.sprintf "%s and %s" (print_type t1) (print_type t2))))
;;

let check_eq t1 t2 =
  if equal_ctype t1 t2
  then return ()
  else
    fail
      (Type_check_error
         (Mismatched_types (Printf.sprintf "%s and %s" (print_type t1) (print_type t2))))
;;

let retrieve_const cstmt rexpr = function
  | Const_array (size, type', inits) when List.length inits <= size ->
    iter
      (fun init ->
        rexpr init
        >>= function
        | Ctuple _ -> fail (Type_check_error (Mismatched_types "Expected single type"))
        | t -> check_eq t (Ctype type'))
      inits
    *> return (Ctype (Type_array (size, type')))
  | Const_array _ -> fail (Type_check_error Check_failed)
  | Const_int _ -> return (Ctype Type_int)
  | Const_string _ -> return (Ctype Type_string)
  | Const_func anon_func -> check_anon_func anon_func cstmt
;;

let check_func_call rexpr (func, args) =
  let* expected_arg_types =
    rexpr func
    >>= function
    | Ctype (Type_func (lst, _)) -> map (fun t -> return (Ctype t)) lst
    | _ -> fail (Type_check_error (Mismatched_types "Expected func type"))
  in
  let* actual_arg_types =
    map
      (fun arg ->
        rexpr arg
        >>= function
        | Ctuple _ -> fail (Type_check_error (Mismatched_types "Expected single type"))
        | t -> return t)
      args
  in
  try iter2 check_eq expected_arg_types actual_arg_types with
  | Invalid_argument _ ->
    fail (Type_check_error (Mismatched_types "Number of given args mismatched"))
;;

let rec nested_array t depth =
  match t, depth with
  | t, 0 -> return t
  | Ctype (Type_array (_, t)), depth -> nested_array (Ctype t) (depth - 1)
  | _, _ ->
    fail
      (Type_check_error
         (Mismatched_types "Number of indicies in array element assigment is incorrect"))
;;

let rec retrieve_expr cstmt = function
  | Expr_const const -> retrieve_const cstmt (retrieve_expr cstmt) const
  | Expr_un_oper (Unary_minus, expr) | Expr_un_oper (Unary_plus, expr) ->
    retrieve_expr cstmt expr >>= eq_type (Ctype Type_int)
  | Expr_un_oper (Unary_not, expr) ->
    retrieve_expr cstmt expr >>= fun t -> eq_type t (Ctype Type_bool)
  | Expr_ident id -> retrieve_ident id
  | Expr_bin_oper (op, left, right) ->
    let compare_arg_typ left right =
      let* ltype = retrieve_expr cstmt left in
      let* rtype = retrieve_expr cstmt right in
      eq_type ltype rtype
    in
    let compare_operation_typ left right t = compare_arg_typ left right >>= eq_type t in
    (match op with
     | Bin_sum | Bin_divide | Bin_modulus | Bin_multiply | Bin_subtract ->
       compare_operation_typ left right (Ctype Type_int)
     | Bin_less | Bin_greater | Bin_greater_equal | Bin_less_equal ->
       compare_operation_typ left right (Ctype Type_int)
     | Bin_or | Bin_and -> compare_operation_typ left right (Ctype Type_bool)
     | Bin_equal | Bin_not_equal -> compare_arg_typ left right *> return (Ctype Type_bool))
  | Expr_call (func, args) ->
    check_func_call (retrieve_expr cstmt) (func, args)
    *> map (retrieve_expr cstmt) args
    *> (retrieve_expr cstmt) func
    >>= (function
     | Ctype (Type_func (_, fst :: snd :: tl)) -> return (Ctuple (fst :: snd :: tl))
     | Ctype (Type_func (_, hd :: _)) -> return (Ctype hd)
     | _ ->
       fail (Type_check_error (Mismatched_types "Function without returns in expression")))
  | Expr_chan_receive chan ->
    retrieve_expr cstmt chan
    >>= (function
     | Ctype (Type_chan (_, y)) -> return (Ctype y)
     | _ -> fail (Type_check_error (Mismatched_types "Chan type mismatch")))
  | Expr_index (array, index) ->
    (retrieve_expr cstmt index >>= check_eq (Ctype Type_int))
    *> (retrieve_expr cstmt array
        >>= function
        | Ctype (Type_array (_, t)) -> return (Ctype t)
        | _ ->
          fail (Type_check_error (Mismatched_types "Non-array type in array index call"))
       )
;;

let check_long_var_decl cstmt save_ident = function
  | Long_decl_no_init (t, hd, tl) -> iter (fun id -> save_ident id (Ctype t)) (hd :: tl)
  | Long_decl_mult_init (Some type', hd, tl) ->
    iter
      (fun (id, expr) ->
        (retrieve_expr cstmt expr >>= check_eq (Ctype type'))
        *> save_ident id (Ctype type'))
      (hd :: tl)
  | Long_decl_mult_init (None, hd, tl) ->
    iter (fun (id, expr) -> retrieve_expr cstmt expr >>= save_ident id) (hd :: tl)
  | Long_decl_one_init (Some type', fst, snd, tl, call) ->
    (retrieve_expr cstmt (Expr_call call)
     >>= check_eq (Ctuple (List.init (List.length (fst :: snd :: tl)) (fun _ -> type'))))
    *> iter (fun id -> save_ident id (Ctype type')) (fst :: snd :: tl)
  | Long_decl_one_init (None, fst, snd, tl, call) ->
    retrieve_expr cstmt (Expr_call call)
    >>= (function
     | Ctype _ ->
       fail
         (Type_check_error
            (Mismatched_types "function returns only one element in multiple var decl"))
     | Ctuple types ->
       (try iter2 save_ident (fst :: snd :: tl) (List.map (fun t -> Ctype t) types) with
        | Invalid_argument _ ->
          fail
            (Type_check_error
               (Mismatched_types
                  "function returns wrong number of elements in multiple var assign"))))
;;

let check_short_var_decl cstmt = function
  | Short_decl_mult_init (hd, tl) ->
    iter (fun (id, expr) -> retrieve_expr cstmt expr >>= save_ident id) (hd :: tl)
  | Short_decl_one_init (fst, snd, tl, call) ->
    retrieve_expr cstmt (Expr_call call)
    >>= (function
     | Ctype _ ->
       fail
         (Type_check_error
            (Mismatched_types
               "function returns wrong number of elements in multiple var decl"))
     | Ctuple types ->
       (try
          iter
            (fun (id, tp) -> save_ident id (Ctype tp))
            (List.combine (fst :: snd :: tl) types)
        with
        | Invalid_argument _ ->
          fail
            (Type_check_error
               (Mismatched_types
                  "function returns wrong number of elements in multiple var decl"))))
;;

let rec retrieve_lvalue ind cstmt = function
  | Lvalue_ident id -> retrieve_ident id
  | Lvalue_array_index (Lvalue_ident array, index) ->
    (retrieve_expr cstmt index >>= check_eq (Ctype Type_int)) *> retrieve_ident array
    >>= (function
     | Ctype (Type_array (_, t)) -> nested_array (Ctype t) ind
     | _ ->
       fail (Type_check_error (Mismatched_types "Non-array type in array index call")))
  | Lvalue_array_index (lvalue_array_index, index) ->
    (retrieve_expr cstmt index >>= check_eq (Ctype Type_int))
    *> retrieve_lvalue (ind + 1) cstmt lvalue_array_index
;;

let check_assign cstmt = function
  | Assign_mult_expr (hd, tl) ->
    iter
      (fun (lvalue, expr) ->
        let* expected_type = retrieve_lvalue 0 cstmt lvalue in
        let* actual_type = retrieve_expr cstmt expr in
        check_eq expected_type actual_type)
      (hd :: tl)
  | Assign_one_expr (fst, snd, tl, call) ->
    retrieve_expr cstmt (Expr_call call)
    >>= (function
     | Ctype _ -> fail (Type_check_error (Cannot_assign "Multiple return assign failed"))
     | Ctuple types ->
       (try
          iter2
            (fun lvalue t -> retrieve_lvalue 0 cstmt lvalue >>= check_eq (Ctype t))
            (fst :: snd :: tl)
            types
        with
        | Invalid_argument _ ->
          fail (Type_check_error (Cannot_assign "Multiple return assign failed"))))
;;

let check_chan_send cstmt (id, expr) =
  let* expr_type = retrieve_expr cstmt expr in
  retrieve_ident id
  >>= function
  | Ctype (Type_chan (_, chan_type)) -> check_eq expr_type (Ctype chan_type)
  | _ -> fail (Type_check_error (Mismatched_types "expected chan type")) *> return ()
;;

let check_init cstmt = function
  | Some (Init_assign assign) -> check_assign cstmt assign
  | Some (Init_call call) -> check_func_call (retrieve_expr cstmt) call
  | Some (Init_decl decl) -> check_short_var_decl cstmt decl *> return ()
  | Some (Init_decr id) -> retrieve_ident id >>= check_eq (Ctype Type_int)
  | Some (Init_incr id) -> retrieve_ident id >>= check_eq (Ctype Type_int)
  | Some (Init_receive chan) -> retrieve_expr cstmt chan *> return ()
  | Some (Init_send send) -> check_chan_send cstmt send
  | None -> return ()
;;

let rec check_stmt = function
  | Stmt_long_var_decl long_decl -> check_long_var_decl check_stmt save_ident long_decl
  | Stmt_short_var_decl short_decl -> check_short_var_decl check_stmt short_decl
  | Stmt_incr id -> retrieve_ident id >>= check_eq (Ctype Type_int)
  | Stmt_decr id -> retrieve_ident id >>= check_eq (Ctype Type_int)
  | Stmt_assign assign -> check_assign check_stmt assign
  | Stmt_call call -> check_func_call (retrieve_expr check_stmt) call
  | Stmt_defer call -> check_func_call (retrieve_expr check_stmt) call
  | Stmt_go call -> check_func_call (retrieve_expr check_stmt) call
  | Stmt_chan_send send -> check_chan_send check_stmt send
  | Stmt_block block -> add_env *> iter check_stmt block *> delete_env
  | Stmt_break -> return ()
  | Stmt_chan_receive chan -> retrieve_expr check_stmt chan *> return ()
  | Stmt_continue -> return ()
  | Stmt_return exprs ->
    (get_func_return_type
     >>= (function
            | Ctuple rtv ->
              (try return (List.combine exprs (List.map (fun t -> Ctype t) rtv)) with
               | Invalid_argument _ ->
                 fail (Type_check_error (Mismatched_types "func return types mismatch")))
            | _ -> fail (Type_check_error Check_failed))
     >>= iter (fun (expr, return_type) ->
       retrieve_expr check_stmt expr >>= check_eq return_type))
    *> return ()
  | Stmt_if if' ->
    add_env
    *> check_init check_stmt if'.init
    *> (retrieve_expr check_stmt if'.cond >>= check_eq (Ctype Type_bool))
    *> iter check_stmt if'.if_body
    *> delete_env
    *>
      (match if'.else_body with
      | Some (Else_block block) -> iter check_stmt block
      | Some (Else_if if') -> check_stmt (Stmt_if if')
      | None -> return ())
  | Stmt_for { init; cond; post; body } ->
    add_env
    *> check_init check_stmt init
    *> (match cond with
      | Some expr -> retrieve_expr check_stmt expr >>= check_eq (Ctype Type_bool)
      | None -> return ())
    *> check_init check_stmt post
    *> iter check_stmt body
    *> delete_env
;;

let save_top_decl_funcs = function
  | Decl_func (id, args_returns_and_body) ->
    save_ident id (get_afunc_type args_returns_and_body)
  | Decl_var _ -> return ()
;;

let check_and_save_top_decl_vars = function
  | Decl_func _ -> return ()
  | Decl_var decl -> check_long_var_decl check_stmt save_ident decl
;;

let check_top_decl_funcs = function
  | Decl_func (_, afunc) -> check_anon_func afunc check_stmt *> return ()
  | Decl_var _ -> return ()
;;

let check_main =
  retrieve_ident "main"
  >>= function
  | Ctype (Type_func ([], [])) -> return ()
  | Ctype (Type_func _) ->
    fail
      (Type_check_error
         (Incorrect_main "func main must have no arguments and no return values"))
  | _ -> fail (Type_check_error (Incorrect_main "main func not found"))
;;

let type_check file =
  run
    (save_ident "true" (Ctype Type_bool)
     *> save_ident "false" (Ctype Type_bool)
     *> add_env
     *> iter save_top_decl_funcs file
     *> iter check_and_save_top_decl_vars file
     *> iter check_top_decl_funcs file
     *> check_main)
    ([ MapIdent.empty ], [])
  |> function
  | _, res -> res
;;
