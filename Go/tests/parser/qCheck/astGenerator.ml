(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open QCheck.Gen
open Ast

let int5 = int_range 0 5
let int1_5 = int_range 1 5
let gen_ident_first_char = oneof [ char_range 'a' 'z'; char_range 'A' 'Z'; return '_' ]

let gen_ident_char =
  oneof [ char_range 'a' 'z'; char_range 'A' 'Z'; return '_'; char_range '0' '9' ]
;;

let gen_ident =
  let* first_char = gen_ident_first_char in
  let* rest_chars = string_size ~gen:gen_ident_char int5 in
  return (Char.to_string first_char ^ rest_chars)
;;

(********** types **********)

let gen_array_type gtype =
  let* size = int5 in
  let* type' = gtype in
  return (Type_array (size, type'))
;;

let gen_func_type gtype =
  let* arg_types = (list_size int5) gtype in
  let* return_types = (list_size int5) gtype in
  return (Type_func (arg_types, return_types))
;;

let gen_chan_type gtype =
  let* type' = gtype in
  let* chan_type =
    oneofl [ Chan_bidirectional type'; Chan_receive type'; Chan_send type' ]
  in
  return (Type_chan chan_type)
;;

let gen_type =
  sized_size (int_range 0 3)
  @@ fix (fun self n ->
    match n with
    | 0 -> oneofl [ Type_int; Type_string; Type_bool ]
    | n ->
      oneof
        [ return Type_int
        ; return Type_string
        ; return Type_bool
        ; gen_array_type (self (n - 1))
        ; gen_func_type (self (n - 1))
        ; gen_chan_type (self (n - 1))
        ])
;;

(********** const **********)

let gen_const_int =
  let* num = int_range 0 1000 in
  return (Const_int num)
;;

let gen_const_string =
  let* str = string_size int5 in
  return (Const_string str)
;;

let gen_const_array gexpr =
  let* size = int5 in
  let* type' = gen_type in
  let* inits = list_size int5 gexpr in
  return (Const_array (size, type', inits))
;;

let gen_return_values =
  let gen_only_types =
    let* types = (list_size int1_5) gen_type in
    return (Only_types types)
  in
  let gen_ident_and_types =
    let* idents_and_types = (list_size int1_5) (pair gen_ident gen_type) in
    return (Ident_and_types idents_and_types)
  in
  oneof [ gen_only_types; gen_ident_and_types ]
;;

let gen_anon_func gblock =
  let* args = (list_size int1_5) (pair gen_ident gen_type) in
  let* returns = option gen_return_values in
  let* body = gblock in
  return { args; returns; body }
;;

let gen_const_func gblock =
  let* anon_func = gen_anon_func gblock in
  return (Const_func anon_func)
;;

let gen_const gexpr gblock =
  oneof [ gen_const_int; gen_const_string; gen_const_array gexpr; gen_const_func gblock ]
;;

(********** expr **********)

let gen_bin_op =
  oneofl
    [ Bin_sum
    ; Bin_multiply
    ; Bin_subtract
    ; Bin_divide
    ; Bin_modulus
    ; Bin_equal
    ; Bin_not_equal
    ; Bin_greater
    ; Bin_greater_equal
    ; Bin_less
    ; Bin_less_equal
    ; Bin_and
    ; Bin_or
    ]
;;

let gen_un_op = oneofl [ Unary_not; Unary_plus; Unary_minus; Unary_recieve ]

let gen_expr_const gexpr gblock =
  let* const = gen_const gexpr gblock in
  return (Expr_const const)
;;

let gen_expr_ident =
  let* ident = gen_ident in
  return (Expr_ident ident)
;;

let gen_expr_index gexpr =
  let* array = gexpr in
  let* index = gexpr in
  return (Expr_index (array, index))
;;

let gen_expr_bin_oper gexpr =
  let* bin_op = gen_bin_op in
  let* left_operand = gexpr in
  let* right_operand = gexpr in
  return (Expr_bin_oper (bin_op, left_operand, right_operand))
;;

let gen_expr_un_oper gexpr =
  let* operator = gen_un_op in
  let* operand = gexpr in
  return (Expr_un_oper (operator, operand))
;;

let gen_func_call gexpr =
  let* func = gexpr in
  let* args = (list_size int5) gexpr in
  return (func, args)
;;

let gen_expr_func_call gexpr =
  let* call = gen_func_call gexpr in
  return (Expr_call call)
;;

let gen_expr gblock =
  sized_size (int_range 0 10)
  @@ fix (fun self n ->
    match n with
    | 0 -> gen_expr_ident
    | n ->
      oneof
        [ gen_expr_ident
        ; gen_expr_const (self (n - 1)) gblock
        ; gen_expr_index (self (n - 1))
        ; gen_expr_bin_oper (self (n - 1))
        ; gen_expr_un_oper (self (n - 1))
        ; gen_expr_func_call (self (n - 1))
        ])
;;

(********** stmt **********)

let gen_long_decl gblock =
  let* type' = gen_type in
  let* idents = list_size int1_5 gen_ident in
  oneof
    [ return (Long_decl_no_init (type', idents))
    ; (let* assigns = list_size int1_5 (pair gen_ident (gen_expr gblock)) in
       return (Long_decl_mult_init (Option.some type', assigns)))
    ; (let* call = gen_expr_func_call (gen_expr gblock) in
       return (Long_decl_one_init (Option.some type', idents, call)))
    ]
;;

let gen_stmt_long_decl gblock =
  let* decl = gen_long_decl gblock in
  return (Stmt_long_var_decl decl)
;;

let gen_stmt_short_decl gblock =
  let* decl =
    oneof
      [ (let* assigns = list_size int1_5 (pair gen_ident (gen_expr gblock)) in
         return (Short_decl_mult_init assigns))
      ; (let* idents = list_size int1_5 gen_ident in
         let* call = gen_expr_func_call (gen_expr gblock) in
         return (Short_decl_one_init (idents, call)))
      ]
  in
  return (Stmt_short_var_decl decl)
;;

let gen_stmt_incr_decr =
  let* ident = gen_ident in
  oneofl [ Stmt_incr ident; Stmt_decr ident ]
;;

let gen_stmt_break_continue = oneofl [ Stmt_break; Stmt_continue ]

let gen_assign_lvalue gblock =
  let gen_lvalue_ident =
    let* ident = gen_ident in
    return (Lvalue_ident ident)
  in
  sized_size (int_range 0 3)
  @@ fix (fun self n ->
    match n with
    | 0 -> gen_lvalue_ident
    | n ->
      oneof
        [ gen_lvalue_ident
        ; (let* array = self (n - 1) in
           let* index = gen_expr gblock in
           return (Lvalue_array_index (array, index)))
        ])
;;

let gen_stmt_assign gblock =
  let* assign =
    oneof
      [ (let* assigns =
           list_size int1_5 (pair (gen_assign_lvalue gblock) (gen_expr gblock))
         in
         return (Assign_mult_expr assigns))
      ; (let* lvalues = list_size int1_5 (gen_assign_lvalue gblock) in
         let* call = gen_expr_func_call (gen_expr gblock) in
         return (Assign_one_expr (lvalues, call)))
      ]
  in
  return (Stmt_assign assign)
;;

let gen_stmt_return gblock =
  let* exprs = list_size int5 (gen_expr gblock) in
  return (Stmt_return exprs)
;;

let gen_stmt_chan_send gblock =
  let* chan = gen_ident in
  let* expr = gen_expr gblock in
  return (Stmt_chan_send (chan, expr))
;;

let gen_stmt_call gblock =
  let* call = gen_func_call (gen_expr gblock) in
  return (Stmt_call call)
;;

let gen_stmt_defer_go gblock =
  let* call = gen_func_call (gen_expr gblock) in
  oneofl [ Stmt_defer call; Stmt_go call ]
;;

let gen_block gstmt = list_size int5 gstmt

let gen_stmt_block gstmt =
  let* block = gen_block gstmt in
  return (Stmt_block block)
;;

(* for if and for init and post *)
let gen_init_and_post_stmt gstmt =
  oneof
    [ gen_stmt_short_decl (gen_block gstmt)
    ; gen_stmt_assign (gen_block gstmt)
    ; gen_stmt_incr_decr
    ; gen_stmt_call (gen_block gstmt)
    ]
;;

let gen_stmt_if gstmt =
  sized_size (int_range 0 2)
  @@ fix (fun self n ->
    let* init = option (gen_init_and_post_stmt gstmt) in
    let* cond = gen_expr (gen_block gstmt) in
    let* if_body = gen_block gstmt in
    let* else_body =
      match n with
      | 0 -> return None
      | n -> oneof [ return None; option (oneof [ self (n - 1); gen_stmt_block gstmt ]) ]
    in
    return (Stmt_if { init; cond; if_body; else_body }))
;;

let gen_stmt_for gstmt =
  let* init = option (gen_init_and_post_stmt gstmt) in
  let* cond = option (gen_expr (gen_block gstmt)) in
  let* post = option (gen_init_and_post_stmt gstmt) in
  let* body = gen_block gstmt in
  return (Stmt_for { init; cond; post; body })
;;

let gen_stmt_range gstmt =
  let* index = gen_ident in
  let* element = option gen_ident in
  let* array = gen_expr (gen_block gstmt) in
  let* body = gen_block gstmt in
  oneofl
    [ Stmt_range (Range_decl { index; element; array; body })
    ; Stmt_range (Range_assign { index; element; array; body })
    ]
;;

let gen_stmt =
  sized_size (int_range 0 5)
  @@ fix (fun self n ->
    match n with
    | 0 -> oneof [ gen_stmt_incr_decr; gen_stmt_break_continue ]
    | n ->
      let gblock = gen_block (self (n - 1)) in
      oneof
        [ gen_stmt_incr_decr
        ; gen_stmt_break_continue
        ; gen_stmt_long_decl gblock
        ; gen_stmt_short_decl gblock
        ; gen_stmt_assign gblock
        ; gen_stmt_return gblock
        ; gen_stmt_block (self (n - 1))
        ; gen_stmt_chan_send gblock
        ; gen_stmt_call gblock
        ; gen_stmt_defer_go gblock
        ; gen_stmt_if (self (n - 1))
        ; gen_stmt_for (self (n - 1))
        ; gen_stmt_range (self (n - 1))
        ])
;;

(********** top decl **********)

let gen_var_top_decl =
  let* decl = gen_long_decl (gen_block gen_stmt) in
  return (Decl_var decl)
;;

let gen_func_top_decl =
  let* ident = gen_ident in
  let* func = gen_anon_func (gen_block gen_stmt) in
  return (Decl_func (ident, func))
;;

let gen_file = list_size (int_range 0 10) (oneof [ gen_var_top_decl; gen_func_top_decl ])
