(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open TypeCheckMonad
open TypeCheckMonad.CheckMonad
open Errors
open Ast

let lps args = List.map (fun (_, snd) -> snd) args
let get_afunc_type afunc = Ctype (Type_func (lps afunc.args, afunc.returns))

let check_anon_func afunc cstmt =
  let save_args = iter (fun (id, t) -> save_local_ident id (Ctype t)) afunc.args in
  write_env
  *> write_func (Ctuple afunc.returns)
  *> save_args
  *> iter cstmt afunc.body
  *> delete_func
  *> delete_env
  *> return (get_afunc_type afunc)
;;

let retrieve_const cstmt = function
  | Const_array (size, t, _) -> return (Ctype (Type_array (size, t)))
  | Const_int _ -> return (Ctype Type_int)
  | Const_string _ -> return (Ctype Type_string)
  | Const_func anon_func -> check_anon_func anon_func cstmt
;;

let check_main =
  read_global_ident "main"
  >>= function
  | Some (Ctype (Type_func ([], []))) -> return ()
  | Some (Ctype (Type_func _)) ->
    fail
      (Type_check_error
         (Incorrect_main "func main must have no arguments and no return values"))
  | _ -> fail (Type_check_error (Incorrect_main "main func not found"))
;;

let eq_type t1 t2 =
  if equal_ctype t1 t2
  then return t1
  else fail (Type_check_error (Mismatched_types "Types mismatched in equation"))
;;

let check_eq t1 t2 =
  match equal_ctype t1 t2 with
  | true -> return ()
  | false -> fail (Type_check_error (Mismatched_types "Types mismatched in equation"))
;;

let check_func_call rexpr (func, args) =
  let ftype =
    rexpr func
    >>= function
    | Ctype (Type_func (lst, _)) -> map (fun t -> return (Ctype t)) lst
    | _ -> fail (Type_check_error (Mismatched_types "Expected func type"))
  in
  let argtypes =
    map
      (fun arg ->
        rexpr arg
        >>= function
        | Ctuple _ -> fail (Type_check_error (Mismatched_types "Expected single type"))
        | t -> return t)
      args
  in
  (argtypes
   >>= (fun at -> ftype >>= fun ft -> return (List.length ft = List.length at))
   >>= function
   | true ->
     ftype
     >>= fun type_list -> iter2 (fun arg typ -> rexpr arg >>= check_eq typ) args type_list
   | false -> fail (Type_check_error (Mismatched_types "Number of given args mismached"))
  )
  *> return ()
;;

let rec retrieve_expr cstmt = function
  | Expr_const const -> retrieve_const cstmt const
  | Expr_un_oper (op, expr) ->
    (match op with
     | Unary_minus | Unary_plus ->
       retrieve_expr cstmt expr
       >>= fun t -> check_eq t (Ctype Type_int) *> return (Ctype Type_int)
     | Unary_not -> retrieve_expr cstmt expr)
    >>= fun t -> check_eq t (Ctype Type_bool) *> return (Ctype Type_bool)
  | Expr_ident id -> retrieve_ident id
  | Expr_bin_oper (op, left, right) ->
    let compare_arg_typ type1 type2 =
      retrieve_expr cstmt type1
      >>= fun type1 -> retrieve_expr cstmt type2 >>= eq_type type1
    in
    let compare_operation_typ type1 type2 t = compare_arg_typ type1 type2 >>= eq_type t in
    (match op with
     | Bin_sum | Bin_divide | Bin_modulus | Bin_multiply | Bin_subtract ->
       compare_operation_typ left right (Ctype Type_int) *> return (Ctype Type_int)
     | Bin_less | Bin_greater | Bin_greater_equal | Bin_less_equal ->
       compare_operation_typ left right (Ctype Type_int) *> return (Ctype Type_bool)
     | Bin_or | Bin_and ->
       compare_operation_typ left right (Ctype Type_bool) *> return (Ctype Type_bool)
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
        (retrieve_expr cstmt expr >>= fun t -> check_eq t (Ctype type'))
        *> save_ident id (Ctype type'))
      (hd :: tl)
  | Long_decl_mult_init (None, hd, tl) ->
    iter (fun (id, expr) -> retrieve_expr cstmt expr >>= save_ident id) (hd :: tl)
  | Long_decl_one_init (Some type', fst, snd, tl, call) ->
    retrieve_expr cstmt (Expr_call call)
    >>= fun t ->
    check_eq t (Ctuple (List.init (List.length (fst :: snd :: tl)) (fun _ -> type')))
    *> iter (fun id -> save_ident id (Ctype type')) (fst :: snd :: tl)
  | Long_decl_one_init (None, fst, snd, tl, call) ->
    retrieve_expr cstmt (Expr_call call)
    >>= (function
     | Ctype _ ->
       fail
         (Type_check_error
            (Mismatched_types "function returns only one element in multiple var decl"))
     | Ctuple types when List.length types = List.length (fst :: snd :: tl) ->
       iter2 save_ident (fst :: snd :: tl) (List.map (fun t -> Ctype t) types)
     | Ctuple _ ->
       fail
         (Type_check_error
            (Mismatched_types
               "function returns wrong number of elements in multiple var assign")))
;;

let check_short_var_decl cstmt = function
  | Short_decl_mult_init (hd, tl) ->
    iter (fun (id, expr) -> retrieve_expr cstmt expr >>= save_local_ident id) (hd :: tl)
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
            (fun (id, tp) -> save_local_ident id (Ctype tp))
            (List.combine (fst :: snd :: tl) types)
        with
        | Invalid_argument _ ->
          fail
            (Type_check_error
               (Mismatched_types
                  "function returns wrong number of elements in multiple var decl"))))
;;

let rec retrieve_lvalue cstmt = function
  | Lvalue_ident id -> retrieve_ident id
  | Lvalue_array_index (Lvalue_ident array, index) ->
    (retrieve_expr cstmt index >>= check_eq (Ctype Type_int)) *> retrieve_ident array
    >>= (function
     | Ctype (Type_array (_, t)) -> return (Ctype t)
     | _ ->
       fail (Type_check_error (Mismatched_types "Non-array type in array index call")))
  | Lvalue_array_index (lvalue_array_index, index) ->
    (retrieve_expr cstmt index >>= check_eq (Ctype Type_int))
    *> retrieve_lvalue cstmt lvalue_array_index
;;

let check_assign cstmt = function
  | Assign_mult_expr (hd, tl) ->
    iter
      (fun (lvalue, expr) ->
        retrieve_lvalue cstmt lvalue
        >>= fun type1 -> retrieve_expr cstmt expr >>= check_eq type1)
      (hd :: tl)
  | Assign_one_expr (l1, l2, ls, w) ->
    retrieve_expr cstmt (Expr_call w)
    >>= (function
     | Ctype _ -> fail (Type_check_error (Cannot_assign "Multiple return assign failed"))
     | Ctuple types ->
       (try
          iter2
            (fun lvalue t -> retrieve_lvalue cstmt lvalue >>= check_eq (Ctype t))
            (l1 :: l2 :: ls)
            types
        with
        | Invalid_argument _ ->
          fail (Type_check_error (Cannot_assign "Multiple return assign failed"))))
;;

let check_chan_send cstmt (id, expr) =
  retrieve_expr cstmt expr
  >>= fun expr_type ->
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
  | Stmt_long_var_decl long_decl ->
    check_long_var_decl check_stmt save_local_ident long_decl
  | Stmt_short_var_decl short_decl -> check_short_var_decl check_stmt short_decl
  | Stmt_incr id -> retrieve_ident id >>= check_eq (Ctype Type_int)
  | Stmt_decr id -> retrieve_ident id >>= check_eq (Ctype Type_int)
  | Stmt_assign assign -> check_assign check_stmt assign
  | Stmt_call call -> check_func_call (retrieve_expr check_stmt) call
  | Stmt_defer call -> check_func_call (retrieve_expr check_stmt) call
  | Stmt_go call -> check_func_call (retrieve_expr check_stmt) call
  | Stmt_chan_send send -> check_chan_send check_stmt send
  | Stmt_block block -> write_env *> iter check_stmt block *> delete_env
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
    write_env
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
    write_env
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
    save_global_ident id (get_afunc_type args_returns_and_body)
  | Decl_var _ -> return ()
;;

let check_and_save_top_decl_vars = function
  | Decl_func _ -> return ()
  | Decl_var decl -> check_long_var_decl check_stmt save_global_ident decl
;;

let check_top_decl_funcs = function
  | Decl_func (_, afunc) -> check_anon_func afunc check_stmt *> return ()
  | Decl_var _ -> return ()
;;

let type_check file =
  run
    (iter save_top_decl_funcs file
     *> iter check_and_save_top_decl_vars file
     *> iter check_top_decl_funcs file
     *> check_main)
    (MapIdent.empty, [], [])
  |> function
  | _, res -> res
;;

let pp ast =
  match type_check ast with
  | Result.Ok _ -> print_endline "CORRECT"
  | Result.Error err ->
    prerr_string "ERROR WHILE TYPECHECK WITH ";
    (match err with
     | Type_check_error Check_failed -> prerr_endline "Check failed"
     | Type_check_error (Multiple_declaration msg) ->
       prerr_string ("Multiple declaration error: " ^ msg)
     | Type_check_error (Incorrect_main msg) ->
       prerr_endline ("Incorrect main error: " ^ msg)
     | Type_check_error (Undefined_ident msg) ->
       prerr_endline ("Undefined ident error: " ^ msg)
     | Type_check_error (Mismatched_types msg) ->
       prerr_endline ("Mismatched types: " ^ msg)
     | Type_check_error (Cannot_assign msg) -> prerr_endline ("Mismatched types: " ^ msg))
;;
