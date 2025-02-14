(** Copyright 2024-2025, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Format

let concat = String.concat ""

let sep_by sep list print =
  let rec helper acc = function
    | fst :: snd :: tl ->
      let acc = concat [ acc; print fst; sep ] in
      helper acc (snd :: tl)
    | fst :: _ -> acc ^ print fst
    | [] -> acc
  in
  helper "" list
;;

let sep_by_comma list print = sep_by ", " list print
let print_ident ident = ident
let print_type = PpType.print_type

let print_idents_with_types list =
  let rec helper acc = function
    | (id, t) :: snd :: tl ->
      let acc = concat [ acc; id; " "; print_type t; ", " ] in
      helper acc (snd :: tl)
    | (id, t) :: _ -> concat [ acc; id; " "; print_type t ]
    | [] -> acc
  in
  helper "" list
;;

let print_func_args_returns_and_body pblock anon_func =
  let { args; returns; body } = anon_func in
  let print_returns =
    match returns with
    | [] -> ""
    | [ t ] -> " " ^ print_type t
    | types -> asprintf " (%s)" (sep_by_comma types print_type)
  in
  asprintf "(%s)%s %s" (print_idents_with_types args) print_returns (pblock body)
;;

let print_const pexpr pblock = function
  | Const_int num -> asprintf "%i" num
  | Const_string str ->
    let rec string_builder acc = function
      | char :: tl ->
        let new_piece =
          match char with
          | '\"' -> "\\\""
          | _ -> Char.escaped char
        in
        string_builder (acc ^ new_piece) tl
      | [] -> acc
    in
    let chars = List.of_seq (String.to_seq str) in
    concat [ "\""; string_builder "" chars; "\"" ]
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
;;

let precedence = function
  | Expr_const _ | Expr_ident _ -> 8
  | Expr_call _ | Expr_index _ -> 7
  | Expr_un_oper _ | Expr_chan_receive _ -> 6
  | Expr_bin_oper (op, _, _) ->
    (match op with
     | Bin_multiply | Bin_divide | Bin_modulus -> 5
     | Bin_sum | Bin_subtract -> 4
     | Bin_equal
     | Bin_not_equal
     | Bin_greater
     | Bin_greater_equal
     | Bin_less
     | Bin_less_equal -> 3
     | Bin_and -> 2
     | Bin_or -> 1)
;;

type assoc =
  | Left
  | Right

let assoc = function
  | Bin_and | Bin_or -> Right
  | _ -> Left
;;

let print_func_call pexpr call =
  let func, args = call in
  let print_func =
    if 7 > precedence func then asprintf "(%s)" (pexpr func) else pexpr func
  in
  let print_arg = function
    | Arg_expr e -> pexpr e
    | Arg_type t -> print_type t
  in
  asprintf "%s(%s)" print_func (sep_by_comma args print_arg)
;;

let rec print_expr pblock = function
  | Expr_const const -> print_const (print_expr pblock) pblock const
  | Expr_ident id -> id
  | Expr_index (array, index) as expr ->
    let print_array =
      if precedence expr > precedence array
      then asprintf "(%s)" ((print_expr pblock) array)
      else (print_expr pblock) array
    in
    asprintf "%s[%s]" print_array ((print_expr pblock) index)
  | Expr_bin_oper (operator, left_operand, right_operand) as expr ->
    let print_left =
      if precedence expr > precedence left_operand
      then asprintf "(%s)" ((print_expr pblock) left_operand)
      else if precedence expr = precedence left_operand && assoc operator = Right
      then asprintf "(%s)" ((print_expr pblock) left_operand)
      else (print_expr pblock) left_operand
    in
    let print_oper = print_bin_op operator in
    let print_right =
      if precedence expr > precedence right_operand
      then asprintf "(%s)" ((print_expr pblock) right_operand)
      else if precedence expr = precedence right_operand && assoc operator = Left
      then asprintf "(%s)" ((print_expr pblock) right_operand)
      else (print_expr pblock) right_operand
    in
    asprintf "%s %s %s" print_left print_oper print_right
  | Expr_un_oper (operator, operand) as expr ->
    let print_operand =
      if precedence expr > precedence operand
      then asprintf "(%s)" ((print_expr pblock) operand)
      else (print_expr pblock) operand
    in
    print_un_op operator ^ print_operand
  | Expr_chan_receive operand as expr ->
    let print_operand =
      if precedence expr > precedence operand
      then asprintf "(%s)" ((print_expr pblock) operand)
      else (print_expr pblock) operand
    in
    asprintf "<-%s" print_operand
  | Expr_call call -> print_func_call (print_expr pblock) call
;;

let print_long_decl pblock = function
  | Long_decl_no_init (type', hd, tl) ->
    asprintf "var %s %s" (sep_by_comma (hd :: tl) print_ident) (print_type type')
  | Long_decl_mult_init (type', hd, tl) ->
    let print_type =
      match type' with
      | Some t -> " " ^ print_type t
      | None -> ""
    in
    let idents, inits = List.split (hd :: tl) in
    asprintf
      "var %s%s = %s"
      (sep_by_comma idents print_ident)
      print_type
      (sep_by_comma inits (print_expr pblock))
  | Long_decl_one_init (type', fst, snd, tl, init) ->
    let print_type =
      match type' with
      | Some t -> print_type t
      | None -> ""
    in
    asprintf
      "var %s %s = %s"
      (sep_by_comma (fst :: snd :: tl) print_ident)
      print_type
      (print_func_call (print_expr pblock) init)
;;

let print_short_decl pblock = function
  | Short_decl_mult_init (hd, tl) ->
    let idents, inits = List.split (hd :: tl) in
    asprintf
      "%s := %s"
      (sep_by_comma idents print_ident)
      (sep_by_comma inits (print_expr pblock))
  | Short_decl_one_init (fst, snd, tl, init) ->
    asprintf
      "%s := %s"
      (sep_by_comma (fst :: snd :: tl) print_ident)
      (print_func_call (print_expr pblock) init)
;;

let rec print_lvalue pblock = function
  | Lvalue_ident id -> id
  | Lvalue_array_index (lvalue, index) ->
    asprintf "%s[%s]" (print_lvalue pblock lvalue) (print_expr pblock index)
;;

let print_assign pblock = function
  | Assign_mult_expr (hd, tl) ->
    let lvalues, inits = List.split (hd :: tl) in
    asprintf
      "%s = %s"
      (sep_by_comma lvalues (print_lvalue pblock))
      (sep_by_comma inits (print_expr pblock))
  | Assign_one_expr (fst, snd, tl, init) ->
    asprintf
      "%s = %s"
      (sep_by_comma (fst :: snd :: tl) (print_lvalue pblock))
      (print_func_call (print_expr pblock) init)
;;

let print_if_for_init pblock = function
  | Init_assign assign -> print_assign pblock assign
  | Init_decl decl -> print_short_decl pblock decl
  | Init_incr id -> asprintf "%s++" id
  | Init_decr id -> asprintf "%s--" id
  | Init_call call -> print_func_call (print_expr pblock) call
  | Init_send (chan, expr) -> asprintf "%s <- %s" chan (print_expr pblock expr)
  | Init_receive chan -> asprintf "<-%s" (print_expr pblock chan)
;;

let rec print_if pblock { if_init; if_cond; if_body; else_body } =
  let print_init =
    match if_init with
    | Some init -> print_if_for_init pblock init ^ "; "
    | None -> ""
  in
  let print_else_body =
    match else_body with
    | Some (Else_block block) -> "else " ^ pblock block
    | Some (Else_if if') -> "else " ^ print_if pblock if'
    | None -> ""
  in
  asprintf
    "if %s%s %s %s"
    print_init
    (print_expr pblock if_cond)
    (pblock if_body)
    print_else_body
;;

let print_for pblock { for_init; for_cond; for_post; for_body } =
  let print_init =
    match for_init with
    | Some init -> print_if_for_init pblock init
    | None -> ""
  in
  let print_cond =
    match for_cond with
    | Some cond -> " " ^ print_expr pblock cond
    | None -> ""
  in
  let print_post =
    match for_post with
    | Some post -> " " ^ print_if_for_init pblock post
    | None -> ""
  in
  match for_init, for_cond, for_post with
  | None, None, None -> asprintf "for %s" (pblock for_body)
  | None, Some _, None -> asprintf "for%s %s" print_cond (pblock for_body)
  | _ -> asprintf "for %s;%s;%s %s" print_init print_cond print_post (pblock for_body)
;;

let print_stmt pblock = function
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
  | Stmt_chan_receive chan -> asprintf "<-%s" (print_expr pblock chan)
  | Stmt_if if' -> print_if pblock if'
  | Stmt_for for' -> print_for pblock for'
;;

let rec print_block block =
  match block with
  | [] -> "{}"
  | _ :: _ ->
    Str.global_replace
      (Str.regexp "\n")
      "\n    "
      (asprintf "{\n%s" (sep_by "\n" block (print_stmt print_block)))
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
let print_stmt = print_stmt print_block
let print_file file = sep_by "\n\n" file print_top_decl ^ "\n"
