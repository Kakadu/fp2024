(** Copyright 2024-2025, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open TypeCheckMonad
open TypeCheckMonad.Monad
open Errors
open Format
open Ast

let get_afunc_type afunc =
  Ctype (Type_func (List.map (fun (_, snd) -> snd) afunc.args, afunc.returns))
;;

let print_type = function
  | Ctype t -> PpType.print_type t
  | Ctuple x -> asprintf "(%s)" (String.concat ", " (List.map PpType.print_type x))
  | _ -> "<not printable type>"
;;

let fail_if_break_continue =
  iter (function
    | Stmt_break -> fail (Type_check_error (Unexpected_operation "break"))
    | Stmt_continue -> fail (Type_check_error (Unexpected_operation "continue"))
    | _ -> return ())
;;

let check_return body =
  let rec find_return_rec body =
    List.mem
      true
      (List.map
         (function
           | Stmt_return _ -> true
           | Stmt_if if' ->
             find_return_rec if'.if_body
             && (function
                  | Some (Else_block body) -> find_return_rec body
                  | Some (Else_if if') -> find_return_rec [ Stmt_if if' ]
                  | None -> false)
                  if'.else_body
           | Stmt_for for' -> find_return_rec for'.for_body
           | Stmt_block block -> find_return_rec block
           | _ -> false)
         body)
  in
  if find_return_rec body
  then return ()
  else fail (Type_check_error (Missing_return "Missing return"))
;;

let check_anon_func afunc cstmt =
  let save_args = iter (fun (id, t) -> save_ident id (Ctype t)) afunc.args in
  add_env
  *> (match afunc.returns with
    | _ :: _ -> check_return afunc.body
    | [] -> return ())
  *> save_func (Ctuple afunc.returns)
  *> save_args
  *> fail_if_break_continue afunc.body
  *> iter cstmt afunc.body
  *> delete_func
  *> delete_env
  *> return (get_afunc_type afunc)
;;

let eq_type t1 t2 =
  match t1, t2 with
  | Cpolymorphic Recover, t -> return t
  | t, Cpolymorphic Recover -> return t
  | t1, t2 ->
    if t1 = t2
    then return t1
    else
      fail
        (Type_check_error
           (Mismatched_types (Printf.sprintf "%s and %s" (print_type t1) (print_type t2))))
;;

let check_eq t1 t2 =
  if t1 = t2
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
  | Const_array _ ->
    fail (Type_check_error (Mismatched_types "Array's size less thai it's inits count"))
  | Const_int _ -> return (Ctype Type_int)
  | Const_string _ -> return (Ctype Type_string)
  | Const_func anon_func -> check_anon_func anon_func cstmt
;;

let check_generic_type = function
  | CgenT _ -> fail (Type_check_error (Mismatched_types "Generic type in wrong place"))
  | _ -> return ()
;;

let check_func_call rexpr rarg (func, args) =
  let* actual_arg_types =
    map
      (fun arg ->
        rarg arg
        >>= function
        | Ctuple _ -> fail (Type_check_error (Mismatched_types "Expected single type"))
        | t -> return t)
      args
  in
  let* ftype =
    rexpr func
    >>= function
    | Ctype (Type_func (lst, _)) -> map (fun t -> return (Ctype t)) lst
    | Cpolymorphic Print | Cpolymorphic Println | Cpolymorphic Panic ->
      iter check_generic_type actual_arg_types *> return actual_arg_types
    | Cpolymorphic Make ->
      (match actual_arg_types with
       | [] ->
         fail
           (Type_check_error (Invalid_operation "Make should take at least 1 argument"))
       | lst ->
         (match List.hd lst with
          | CgenT _ -> iter check_generic_type (List.tl lst) *> return actual_arg_types
          | _ ->
            fail
              (Type_check_error
                 (Invalid_operation "Make should take some type as 1st argument"))))
    | Cpolymorphic Close ->
      (match actual_arg_types with
       | [] ->
         fail
           (Type_check_error (Invalid_operation "Make should take at least 1 argument"))
       | [ x ] ->
         (match x with
          | Ctype (Type_chan t) -> return [ Ctype (Type_chan t) ]
          | _ ->
            fail
              (Type_check_error
                 (Invalid_operation "Close should take chan type as 1st argument")))
       | _ ->
         fail (Type_check_error (Mismatched_types "Close should take only 1 argument")))
    | Cpolymorphic Len ->
      (match actual_arg_types with
       | [ Ctype Type_string ] -> return [ Ctype Type_string ]
       | [ Ctype (Type_array (x, y)) ] -> return [ Ctype (Type_array (x, y)) ]
       | _ ->
         fail (Type_check_error (Mismatched_types "len takes only string and array args")))
    | Cpolymorphic Recover ->
      (match actual_arg_types with
       | [] -> return []
       | _ -> fail (Type_check_error (Mismatched_types "recover takes no args")))
    | _ -> fail (Type_check_error (Mismatched_types "Expected func type"))
  in
  try iter2 check_eq ftype actual_arg_types with
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

let rec check_expr cstmt rarg = function
  | Expr_const const -> retrieve_const cstmt (check_expr cstmt rarg) const
  | Expr_un_oper (Unary_minus, expr) | Expr_un_oper (Unary_plus, expr) ->
    check_expr cstmt rarg expr >>= eq_type (Ctype Type_int)
  | Expr_un_oper (Unary_not, expr) ->
    check_expr cstmt rarg expr >>= fun t -> eq_type t (Ctype Type_bool)
  | Expr_ident id -> retrieve_ident id
  | Expr_bin_oper (op, left, right) ->
    let compare_arg_typ left right =
      let* ltype = check_expr cstmt rarg left in
      let* rtype = check_expr cstmt rarg right in
      eq_type ltype rtype
    in
    let compare_operation_typ left right t = compare_arg_typ left right >>= eq_type t in
    (match op with
     | Bin_sum | Bin_divide | Bin_modulus | Bin_multiply | Bin_subtract ->
       compare_operation_typ left right (Ctype Type_int)
     | Bin_less | Bin_greater | Bin_greater_equal | Bin_less_equal ->
       compare_operation_typ left right (Ctype Type_int) *> return (Ctype Type_bool)
     | Bin_or | Bin_and ->
       compare_operation_typ left right (Ctype Type_bool) *> return (Ctype Type_bool)
     | Bin_equal | Bin_not_equal -> compare_arg_typ left right *> return (Ctype Type_bool))
  | Expr_call (func, args) ->
    check_func_call (check_expr cstmt rarg) rarg (func, args)
    *> map rarg args
    *> (check_expr cstmt rarg) func
    >>= (function
     | Ctype (Type_func (_, fst :: snd :: tl)) -> return (Ctuple (fst :: snd :: tl))
     | Ctype (Type_func (_, hd :: _)) -> return (Ctype hd)
     | Cpolymorphic Print | Cpolymorphic Close | Cpolymorphic Println ->
       fail
         (Type_check_error (Invalid_operation "print/println/close func makes no return"))
     | Cpolymorphic Len -> return (Ctype Type_int)
     | Cpolymorphic Make ->
       (match List.hd args with
        | Arg_type t -> return (Ctype t)
        | _ ->
          fail
            (Type_check_error
               (Mismatched_types "make should be used like make(T, arg) when T is a type")))
     | Cpolymorphic Recover -> return (Cpolymorphic Recover)
     | _ ->
       fail (Type_check_error (Mismatched_types "Function without returns in expression")))
  | Expr_chan_receive chan ->
    check_expr cstmt rarg chan
    >>= (function
     | Ctype (Type_chan t) -> return (Ctype t)
     | _ -> fail (Type_check_error (Mismatched_types "Chan type mismatch")))
  | Expr_index (array, index) ->
    (check_expr cstmt rarg index >>= check_eq (Ctype Type_int))
    *> (check_expr cstmt rarg array
        >>= function
        | Ctype (Type_array (_, t)) -> return (Ctype t)
        | _ ->
          fail (Type_check_error (Mismatched_types "Non-array type in array index call"))
       )
;;

let rec retrieve_arg cstmt = function
  | Arg_expr exp -> check_expr cstmt (retrieve_arg cstmt) exp
  | Arg_type t -> return (CgenT t)
;;

let check_nil arg possible_nil =
  match possible_nil with
  | Cpolymorphic Nil ->
    (match arg with
     | Ctype (Type_chan t) -> return (Ctype (Type_chan t))
     | Ctype (Type_func (args, returns)) -> return (Ctype (Type_func (args, returns)))
     | t ->
       fail
         (Type_check_error
            (Mismatched_types
               (Printf.sprintf "nil type cannot be assigned to %s" (print_type t)))))
  | _ -> return possible_nil
;;

let fail_if_nil = function
  | Cpolymorphic Nil ->
    fail
      (Type_check_error (Mismatched_types (Printf.sprintf "nil type cannot be used here")))
  | x -> return x
;;

let check_long_var_decl cstmt save_ident = function
  | Long_decl_no_init (t, hd, tl) -> iter (fun id -> save_ident id (Ctype t)) (hd :: tl)
  | Long_decl_mult_init (Some type', hd, tl) ->
    iter
      (fun (id, expr) ->
        (check_expr cstmt (retrieve_arg cstmt) expr
         >>= (fun t -> check_nil (Ctype type') t)
         >>= check_eq (Ctype type'))
        *> save_ident id (Ctype type'))
      (hd :: tl)
  | Long_decl_mult_init (None, hd, tl) ->
    iter
      (fun (id, expr) ->
        check_expr cstmt (retrieve_arg cstmt) expr >>= fail_if_nil >>= save_ident id)
      (hd :: tl)
  | Long_decl_one_init (Some type', fst, snd, tl, call) ->
    (check_expr cstmt (retrieve_arg cstmt) (Expr_call call)
     >>= check_eq (Ctuple (List.init (List.length (fst :: snd :: tl)) (fun _ -> type'))))
    *> iter (fun id -> save_ident id (Ctype type')) (fst :: snd :: tl)
  | Long_decl_one_init (None, fst, snd, tl, call) ->
    check_expr cstmt (retrieve_arg cstmt) (Expr_call call)
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
                  "function returns wrong number of elements in multiple var assign")))
     | _ ->
       fail
         (Type_check_error
            (Mismatched_types "simple type or built-in func cannot be used as return")))
;;

let check_short_var_decl cstmt = function
  | Short_decl_mult_init (hd, tl) ->
    iter
      (fun (id, expr) ->
        check_expr cstmt (retrieve_arg cstmt) expr
        >>= (function
               | Ctype t -> return (Ctype t)
               | Cpolymorphic Nil ->
                 fail
                   (Type_check_error
                      (Invalid_operation "Cannot assign nil in short var declaration"))
               | Cpolymorphic Recover -> return (Cpolymorphic Recover)
               | _ ->
                 fail
                   (Type_check_error
                      (Mismatched_types "Incorrect assignment in short var decl")))
        >>= save_ident id)
      (hd :: tl)
  | Short_decl_one_init (fst, snd, tl, call) ->
    check_expr cstmt (retrieve_arg cstmt) (Expr_call call)
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
                  "function returns wrong number of elements in multiple var decl")))
     | Cpolymorphic Nil ->
       fail
         (Type_check_error
            (Invalid_operation "Cannot assign nil in short var declaration"))
     | CgenT _ | Cpolymorphic _ ->
       fail
         (Type_check_error
            (Mismatched_types "simple type or built-in func cannot be used as return")))
;;

let rec retrieve_lvalue ind cstmt = function
  | Lvalue_ident id -> retrieve_ident id
  | Lvalue_array_index (Lvalue_ident array, index) ->
    (check_expr cstmt (retrieve_arg cstmt) index >>= check_eq (Ctype Type_int))
    *> retrieve_ident array
    >>= (function
     | Ctype (Type_array (_, t)) -> nested_array (Ctype t) ind
     | _ ->
       fail (Type_check_error (Mismatched_types "Non-array type in array index call")))
  | Lvalue_array_index (lvalue_array_index, index) ->
    (check_expr cstmt (retrieve_arg cstmt) index >>= check_eq (Ctype Type_int))
    *> retrieve_lvalue (ind + 1) cstmt lvalue_array_index
;;

let check_assign cstmt = function
  | Assign_mult_expr (hd, tl) ->
    iter
      (fun (lvalue, expr) ->
        let* expected_type = retrieve_lvalue 0 cstmt lvalue in
        let* actual_type = check_expr cstmt (retrieve_arg cstmt) expr in
        let* actual_type = check_nil expected_type actual_type in
        check_eq expected_type actual_type)
      (hd :: tl)
  | Assign_one_expr (fst, snd, tl, call) ->
    check_expr cstmt (retrieve_arg cstmt) (Expr_call call)
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
          fail (Type_check_error (Cannot_assign "Multiple return assign failed")))
     | _ ->
       fail
         (Type_check_error
            (Mismatched_types "simple type or built-in func cannot be used as return")))
;;

let check_chan_send cstmt (id, expr) =
  let* expr_type = check_expr cstmt (retrieve_arg cstmt) expr in
  retrieve_ident id
  >>= function
  | Ctype (Type_chan t) -> check_eq expr_type (Ctype t)
  | _ -> fail (Type_check_error (Mismatched_types "expected chan type")) *> return ()
;;

let check_init cstmt = function
  | Some (Init_assign assign) -> check_assign cstmt assign
  | Some (Init_call call) ->
    check_func_call (check_expr cstmt (retrieve_arg cstmt)) (retrieve_arg cstmt) call
  | Some (Init_decl decl) -> check_short_var_decl cstmt decl *> return ()
  | Some (Init_decr id) -> retrieve_ident id >>= check_eq (Ctype Type_int)
  | Some (Init_incr id) -> retrieve_ident id >>= check_eq (Ctype Type_int)
  | Some (Init_receive chan) -> check_expr cstmt (retrieve_arg cstmt) chan *> return ()
  | Some (Init_send send) -> check_chan_send cstmt send
  | None -> return ()
;;

let rec check_stmt = function
  | Stmt_long_var_decl long_decl -> check_long_var_decl check_stmt save_ident long_decl
  | Stmt_short_var_decl short_decl -> check_short_var_decl check_stmt short_decl
  | Stmt_incr id -> retrieve_ident id >>= check_eq (Ctype Type_int)
  | Stmt_decr id -> retrieve_ident id >>= check_eq (Ctype Type_int)
  | Stmt_assign assign -> check_assign check_stmt assign
  | Stmt_call call ->
    check_func_call
      (check_expr check_stmt (retrieve_arg check_stmt))
      (retrieve_arg check_stmt)
      call
  | Stmt_defer call ->
    check_func_call
      (check_expr check_stmt (retrieve_arg check_stmt))
      (retrieve_arg check_stmt)
      call
  | Stmt_go (func, args) ->
    ((check_expr check_stmt (retrieve_arg check_stmt)) func
     >>= function
     | Cpolymorphic Make -> fail (Type_check_error Go_make)
     | _ -> return ())
    *> check_func_call
         (check_expr check_stmt (retrieve_arg check_stmt))
         (retrieve_arg check_stmt)
         (func, args)
  | Stmt_chan_send send -> check_chan_send check_stmt send
  | Stmt_block block -> add_env *> iter check_stmt block *> delete_env
  | Stmt_break -> return ()
  | Stmt_chan_receive chan ->
    check_expr check_stmt (retrieve_arg check_stmt) chan *> return ()
  | Stmt_continue -> return ()
  | Stmt_return exprs ->
    (get_func_return_type
     >>= (function
            | Ctuple rtv ->
              (try return (List.combine exprs (List.map (fun t -> Ctype t) rtv)) with
               | Invalid_argument _ ->
                 fail (Type_check_error (Mismatched_types "func return types mismatch")))
            | _ ->
              fail
                (Type_check_error
                   (Mismatched_types
                      "simple type or built-in func cannot be used as return")))
     >>= iter (fun (expr, return_type) ->
       check_expr check_stmt (retrieve_arg check_stmt) expr >>= check_eq return_type))
    *> return ()
  | Stmt_if { if_init; if_cond; if_body; else_body } ->
    add_env
    *> check_init check_stmt if_init
    *> (check_expr check_stmt (retrieve_arg check_stmt) if_cond
        >>= check_eq (Ctype Type_bool))
    *> iter check_stmt if_body
    *> delete_env
    *>
      (match else_body with
      | Some (Else_block block) -> iter check_stmt block
      | Some (Else_if if') -> check_stmt (Stmt_if if')
      | None -> return ())
  | Stmt_for { for_init; for_cond; for_post; for_body } ->
    add_env
    *> check_init check_stmt for_init
    *> (match for_cond with
      | Some expr ->
        check_expr check_stmt (retrieve_arg check_stmt) expr
        >>= check_eq (Ctype Type_bool)
      | None -> return ())
    *> check_init check_stmt for_post
    *> iter check_stmt for_body
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
     *> save_ident "make" (Cpolymorphic Make)
     *> save_ident "print" (Cpolymorphic Print)
     *> save_ident "panic" (Cpolymorphic Panic)
     *> save_ident "len" (Cpolymorphic Len)
     *> save_ident "recover" (Cpolymorphic Recover)
     *> save_ident "close" (Cpolymorphic Close)
     *> save_ident "println" (Cpolymorphic Println)
     *> save_ident "nil" (Cpolymorphic Nil)
     *> add_env
     *> iter save_top_decl_funcs file
     *> iter check_and_save_top_decl_vars file
     *> iter check_top_decl_funcs file
     *> check_main)
    ([ MapIdent.empty ], [])
  |> function
  | _, res -> res
;;
