(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Format

let sep_by sep list print =
  let rec helper acc list =
    match list with
    | hd :: tl ->
      let sep =
        match tl with
        | _ :: _ -> sep
        | [] -> ""
      in
      let acc = acc ^ print hd ^ sep in
      helper acc tl
    | [] -> acc
  in
  helper "" list
;;

let sep_by_comma list print = sep_by ", " list print
let print_ident ident = ident

let rec print_type = function
  | Type_int -> "int"
  | Type_string -> "string"
  | Type_bool -> "bool"
  | Type_array (size, type') -> asprintf "[%i]%s" size (print_type type')
  | Type_func (arg_types, return_types) ->
    asprintf
      "func(%s) (%s)"
      (sep_by_comma arg_types print_type)
      (sep_by_comma return_types print_type)
  | Type_chan chan_type ->
    (match chan_type with
     | Chan_bidirectional t -> asprintf "chan %s" (print_type t)
     | Chan_receive t -> asprintf "<-chan %s" (print_type t)
     | Chan_send t -> asprintf "chan<- %s" (print_type t))
;;

let print_idents_with_types list =
  let rec helper acc list =
    match list with
    | (id, t) :: tl ->
      let sep =
        match tl with
        | _ :: _ -> ", "
        | [] -> ""
      in
      let acc = acc ^ id ^ " " ^ print_type t ^ sep in
      helper acc tl
    | [] -> acc
  in
  helper "" list
;;

let print_func_args_returns_and_body pblock anon_func =
  let { args; returns; body } = anon_func in
  let print_returns =
    match returns with
    | Some (Only_types types) -> sep_by_comma types print_type
    | Some (Ident_and_types pairs) -> print_idents_with_types pairs
    | None -> ""
  in
  asprintf "(%s) (%s) %s" (print_idents_with_types args) print_returns (pblock body)
;;

let print_const pexpr pblock = function
  | Const_int num -> asprintf "%i" num
  | Const_string str -> "\"" ^ str ^ "\""
  | Const_array (size, type', inits) ->
    asprintf "[%i]%s{%s}" size (print_type type') (sep_by_comma inits pexpr)
  | Const_func anon_func -> "func" ^ print_func_args_returns_and_body pblock anon_func
;;

let print_bin_op = function
  | Bin_sum -> "+"
  | Bin_multiply -> "*"
  | Bin_subtract -> "-"
  | Bin_divide -> "/"
  | Bin_modulus -> "%"
  | Bin_equal -> "=="
  | Bin_not_equal -> "!="
  | Bin_greater -> ">"
  | Bin_greater_equal -> ">="
  | Bin_less -> "<"
  | Bin_less_equal -> "<="
  | Bin_and -> "&&"
  | Bin_or -> "||"
;;

let print_un_op = function
  | Unary_not -> "!"
  | Unary_plus -> "+"
  | Unary_minus -> "-"
  | Unary_recieve -> "<-"
;;

let print_func_call pexpr call =
  let func, args = call in
  asprintf "%s(%s)" (pexpr func) (sep_by_comma args pexpr)
;;

let rec print_expr pblock = function
  | Expr_const const -> print_const (print_expr pblock) pblock const
  | Expr_ident id -> id
  | Expr_index (array, index) ->
    asprintf "%s[%s]" ((print_expr pblock) array) ((print_expr pblock) index)
  | Expr_bin_oper (operator, left_operand, right_operand) ->
    asprintf
      "(%s) %s (%s)"
      ((print_expr pblock) left_operand)
      (print_bin_op operator)
      ((print_expr pblock) right_operand)
  | Expr_un_oper (operator, operand) -> print_un_op operator ^ (print_expr pblock) operand
  | Expr_call call -> print_func_call (print_expr pblock) call
;;

let print_long_decl pblock = function
  | Long_decl_no_init (type', idents) ->
    asprintf "var %s %s" (print_type type') (sep_by_comma idents Fun.id)
  | Long_decl_mult_init (type', assigns) ->
    let print_type =
      match type' with
      | Some t -> print_type t
      | None -> ""
    in
    let idents, inits = List.split assigns in
    asprintf
      "var %s %s = %s"
      (sep_by_comma idents Fun.id)
      print_type
      (sep_by_comma inits (print_expr pblock))
  | Long_decl_one_init (type', idents, init) ->
    let print_type =
      match type' with
      | Some t -> print_type t
      | None -> ""
    in
    asprintf
      "var %s %s = %s"
      (sep_by_comma idents Fun.id)
      print_type
      (print_expr pblock init)
;;

let print_short_decl pblock = function
  | Short_decl_mult_init assigns ->
    let idents, inits = List.split assigns in
    asprintf
      "%s := %s"
      (sep_by_comma idents Fun.id)
      (sep_by_comma inits (print_expr pblock))
  | Short_decl_one_init (idents, init) ->
    asprintf "%s := %s" (sep_by_comma idents Fun.id) (print_expr pblock init)
;;

let rec print_lvalue pblock = function
  | Lvalue_ident id -> id
  | Lvalue_array_index (lvalue, index) ->
    asprintf "%s[%s]" (print_lvalue pblock lvalue) (print_expr pblock index)
;;

let print_assign pblock = function
  | Assign_mult_expr assigns ->
    let lvalues, inits = List.split assigns in
    asprintf
      "%s = %s"
      (sep_by_comma lvalues (print_lvalue pblock))
      (sep_by_comma inits (print_expr pblock))
  | Assign_one_expr (lvalues, init) ->
    asprintf
      "%s = %s"
      (sep_by_comma lvalues (print_lvalue pblock))
      (print_expr pblock init)
;;

let print_if pstmt pblock = function
  | Stmt_if { init; cond; if_body; else_body } ->
    let print_init =
      match init with
      | Some init -> pstmt init ^ "; "
      | None -> ""
    in
    let print_else_body =
      match else_body with
      | Some else_body -> "else " ^ pstmt else_body
      | None -> ""
    in
    asprintf
      "if %s%s %s %s"
      print_init
      (print_expr pblock cond)
      (pblock if_body)
      print_else_body
  | _ -> ""
;;

let print_for pstmt pblock = function
  | Stmt_for { init; cond; post; body } ->
    let print_init =
      match init with
      | Some init -> pstmt init
      | None -> ""
    in
    let print_cond =
      match cond with
      | Some cond -> print_expr pblock cond
      | None -> ""
    in
    let print_post =
      match post with
      | Some post -> pstmt post
      | None -> ""
    in
    (match init, cond, post with
     | None, None, None -> asprintf "for %s" (pblock body)
     | None, Some _, None -> asprintf "for %s %s" print_cond (pblock body)
     | _ -> asprintf "for %s; %s; %s %s" print_init print_cond print_post (pblock body))
  | _ -> ""
;;

let print_range pblock range =
  let index, element, array, body, operator =
    match range with
    | Range_decl { index; element; array; body } -> index, element, array, body, ":="
    | Range_assign { index; element; array; body } -> index, element, array, body, "="
  in
  let print_element =
    match element with
    | Some elem -> ", " ^ elem
    | None -> ""
  in
  asprintf
    "for %s%s %s range %s %s"
    index
    print_element
    operator
    (print_expr pblock array)
    (pblock body)
;;

let rec print_stmt pblock = function
  | Stmt_long_var_decl decl -> print_long_decl pblock decl
  | Stmt_short_var_decl decl -> print_short_decl pblock decl
  | Stmt_assign assign -> print_assign pblock assign
  | Stmt_incr id -> asprintf "%s++" id
  | Stmt_decr id -> asprintf "%s--" id
  | Stmt_break -> "break"
  | Stmt_continue -> "continue"
  | Stmt_return exprs -> "return " ^ sep_by_comma exprs (print_expr pblock)
  | Stmt_block block -> pblock block
  | Stmt_call call -> print_func_call (print_expr pblock) call
  | Stmt_defer call -> "defer " ^ print_func_call (print_expr pblock) call
  | Stmt_go call -> "go " ^ print_func_call (print_expr pblock) call
  | Stmt_chan_send (chan, expr) -> asprintf "%s <- %s" chan (print_expr pblock expr)
  | Stmt_if _ as if_stmt -> print_if (print_stmt pblock) pblock if_stmt
  | Stmt_for _ as for_stmt -> print_for (print_stmt pblock) pblock for_stmt
  | Stmt_range range -> print_range pblock range
;;

let rec print_block block =
  match block with
  | [] -> "{}"
  | _ :: _ ->
    let replace old_sub new_sub str =
      let old_sub = Str.regexp_string old_sub in
      Str.global_replace old_sub new_sub str
    in
    replace "\n" "\n    " (asprintf "{\n%s" (sep_by "\n" block (print_stmt print_block)))
    ^ "\n}"
;;

let print_top_decl = function
  | Decl_var decl -> print_long_decl print_block decl
  | Decl_func decl ->
    let ident, args_returns_and_body = decl in
    asprintf
      "func %s%s"
      ident
      (print_func_args_returns_and_body print_block args_returns_and_body)
;;

let print_expr = print_expr print_block
let print_const const = print_const print_expr print_block const
let print_stmt = print_stmt print_block
let print_file file = sep_by "\n\n" file print_top_decl
