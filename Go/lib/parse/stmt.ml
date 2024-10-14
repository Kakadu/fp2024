(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Angstrom
open Common
open Expr

let rec combine_lists l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | x :: xs, y :: ys -> (x, y) :: combine_lists xs ys
  | _, _ -> assert false
;;

let parse_inits =
  ws_line
  *> char '='
  *> ws
  *> many_sep ~sep:(ws_line *> char ',' *> ws) ~parser:parse_expr
;;

(* Decl_with_init (None, []) should cause error, will be proccessed at interpretation state *)
let parse_var_decl_top_level =
  let build_var_decl_parser idents vars_type inits =
    match vars_type, inits with
    | Some t, _ :: _ ->
      if List.length idents != List.length inits
      then Decl_with_init (None, [])
      else Decl_with_init (Some t, combine_lists idents inits)
    | Some t, [] -> Decl_no_init (t, idents)
    | None, _ :: _ ->
      if List.length idents != List.length inits
      then Decl_with_init (None, [])
      else Decl_with_init (Some Type_int, combine_lists idents inits)
    | None, [] -> Decl_with_init (None, [])
  in
  let parse_vars_type =
    ws_line *> parse_type <* ws_line >>| (fun t -> Some t) <|> ws_line *> return None
  in
  lift3
    build_var_decl_parser
    (string "var"
     *> ws
     *> many_sep ~sep:(ws_line *> char ',' *> ws_line) ~parser:parse_ident)
    parse_vars_type
    (char '=' *> ws *> parse_inits)
;;

(* let parse_var_decl_in_func = return () *)
let parse_var_decl_any =
  parse_var_decl_top_level
  (* <|> parse_var_decl_in_func *) >>| fun decl -> Stmt_var_decl decl
;;

(* let parse_assign = return () *)

let parse_incr = parse_ident <* ws_line <* string "++" >>| fun id -> Stmt_incr id
let parse_decr = parse_ident <* ws_line <* string "--" >>| fun id -> Stmt_decr id

(* в ините нужно парсить только parse_var_decl_in_func *)
let parse_if =
  let parse_init =
    parse_stmt <* parse_stmt_sep >>| (fun init -> Some init) <|> return None
  in
  let parse_else =
    string "else" *> ws *> parse_block >>| (fun block -> Some block) <|> return None
  in
  string "if"
  *> ws
  *> lift4
       (fun init cond if_body else_body -> Stmt_if (init, cond, if_body, else_body))
       parse_init
       (parse_expr <* ws_line)
       (parse_block <* ws_line)
       parse_else
;;

(* можно парсить [for range 1000] как [for i := 0; i < 1000; i++]
   let parse_for = return ()
   let parse_range = return () *)
let parse_break = string "break" *> return Stmt_break
let parse_continue = string "continue" *> return Stmt_continue

(* TODO: return multiple expressions *)
let parse_return =
  string "return" *> ws_line *> parse_expr >>| fun expr -> Stmt_return (Some expr)
;;

(* let parse_chan_send = return ()
   let parse_chan_receive = return ()
   let parse_stmt_call = return ()
   let parse_defer = string "defer" *> ws_line *> return ()
   let parse_go = string "go" *> ws_line *> return () *)

let rec parse_stmt =
  choice
    [ parse_var_decl_any
    ; parse_incr
    ; parse_decr
    ; parse_if
    ; parse_break
    ; parse_continue
    ; parse_return
      (*  ; parse_assign
          ; parse_for
          ; parse_range
          ; parse_chan_send
          ; parse_chan_receive
          ; parse_stmt_call
          ; parse_defer
          ; parse_go *)
    ]
    ~failure_msg:"Incorrect statement"
;;

let parse_block : block t =
  let parse_stmts = many_sep ~sep:parse_stmt_sep ~parser:parse_stmt in
  char '{' *> ws *> parse_stmts <* ws <* char '}'
;;
