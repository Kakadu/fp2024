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
  | _, _ -> assert false (* bad, mb [] instead *)
;;

let parse_lvalues = sep_by_comma1 parse_ident
let parse_rvalues pblock = sep_by_comma1 (parse_expr pblock)

let parse_long_var_decl pblock =
  let* _ = string "var" *> ws in
  let* lvalues = parse_lvalues <* ws_line in
  let* vars_type = parse_type >>| (fun t -> Some t) <|> return None in
  let* with_init = ws_line *> char '=' *> ws *> return true <|> return false in
  if not with_init
  then (
    match vars_type with
    | Some t -> return (Long_decl_no_init (t, lvalues))
    | None -> fail "Long variable declaration without initializers should have type")
  else
    let* rvalues = parse_rvalues pblock in
    if List.length lvalues = 0
    then fail "No identifiers in long variable declaration"
    else (
      match rvalues, List.length lvalues = List.length rvalues with
      | _ :: _, true ->
        return (Long_decl_mult_init (vars_type, combine_lists lvalues rvalues))
      | _ :: _, false ->
        if List.length rvalues = 1
        then (
          match List.nth rvalues 0 with
          | Some (Expr_call _ as expr) ->
            return (Long_decl_one_init (vars_type, lvalues, expr))
          | Some _ | None ->
            fail
              "Initializer has to ba a function call in variable declarations with \
               multiple identifiers and one initializer")
        else
          fail
            "Number of lvalues and rvalues in variable declarations should be the same \
             or rvalue should be a function that returns multiple values"
      | [], _ -> assert false)
;;

let parse_short_var_decl pblock =
  let* lvalues = parse_lvalues in
  let* _ = ws_line *> string ":=" *> ws in
  let* rvalues = parse_rvalues pblock in
  if List.length lvalues = 0 || List.length rvalues = 0
  then fail "No identifiers or initializers in short vaiable declarations"
  else if List.length lvalues != List.length rvalues
  then
    if List.length rvalues = 1
    then (
      match List.nth rvalues 0 with
      | Some (Expr_call _ as expr) ->
        return (Stmt_short_var_decl (Short_decl_one_init (lvalues, expr)))
      | Some _ | None ->
        fail
          "Initializer has to ba a function call in variavle declarations with multiple \
           identifiers and one initializer")
    else
      fail
        "Number of lvalues and rvalues should be the same or rvalue should be a function \
         that returns multiple values"
  else return (Stmt_short_var_decl (Short_decl_mult_init (combine_lists lvalues rvalues)))
;;

let parse_assign_lvalues pblock =
  let parse_lvalue =
    let rec helper acc =
      parse_index (parse_expr pblock) acc
      >>= (fun (array, index) -> helper (Lvalue_array_index (array, index)))
      <|> return acc
    in
    parse_ident >>= fun ident -> helper (Lvalue_ident ident)
  in
  sep_by_comma1 parse_lvalue
;;

let parse_assign pblock =
  let* lvalues = parse_assign_lvalues pblock in
  let* _ = ws_line *> char '=' *> ws in
  let* rvalues = parse_rvalues pblock in
  if List.length lvalues = 0 || List.length rvalues = 0
  then fail "No identifiers or initializers in assignment"
  else if List.length lvalues != List.length rvalues
  then
    if List.length rvalues == 1
    then (
      match List.nth rvalues 0 with
      | Some (Expr_call _ as expr) ->
        return (Stmt_assign (Assign_one_expr (lvalues, expr)))
      | Some _ | None ->
        fail
          "Initializer has to ba a function call in assignments with multiple \
           identifiers and one initializer")
    else
      fail
        "Number of identifiers and initializers is not equal in assignment or \
         initializer is not a single function with multiple returns"
  else return (Stmt_assign (Assign_mult_expr (combine_lists lvalues rvalues)))
;;

let parse_incr = parse_ident <* ws_line <* string "++" >>| fun id -> Stmt_incr id
let parse_decr = parse_ident <* ws_line <* string "--" >>| fun id -> Stmt_decr id

let parse_func_call pblock =
  parse_expr pblock
  >>= fun expr ->
  match expr with
  | Expr_call call -> return call
  | _ -> fail "Not a function call"
;;

let parse_stmt_call pblock = parse_func_call pblock >>| fun call -> Stmt_call call

let parse_defer pblock =
  string "defer" *> ws *> parse_func_call pblock >>| fun call -> Stmt_defer call
;;

let parse_go pblock =
  string "go" *> ws *> parse_func_call pblock >>| fun call -> Stmt_go call
;;

let parse_chan_send pblock =
  let* chan = parse_ident in
  let* expr = token "<-" *> parse_expr pblock in
  return (Stmt_chan_send (chan, expr))
;;

let parse_break = string "break" *> return Stmt_break
let parse_continue = string "continue" *> return Stmt_continue

let parse_return pblock =
  string "return" *> ws_line *> sep_by_comma (parse_expr pblock)
  >>| fun expr_list -> Stmt_return expr_list
;;

let is_valid_init_and_post = function
  | Some (Stmt_short_var_decl _)
  | Some (Stmt_assign _)
  | Some (Stmt_incr _)
  | Some (Stmt_decr _)
  | Some (Stmt_call _)
  | None -> true
  | _ -> false
;;

let parse_if pstmt pblock =
  let* _ = string "if" *> ws in
  let* init = pstmt >>| (fun init -> Some init) <|> return None in
  if not (is_valid_init_and_post init)
  then fail "Incorrect statement in if initialization"
  else
    let* _ = parse_stmt_sep <|> return () in
    let* cond = ws *> parse_expr pblock in
    let* if_body = ws_line *> pblock <* ws_line in
    let* else_body =
      let* else_body_exists = string "else" *> ws *> return true <|> return false in
      if else_body_exists
      then
        let* else_body = pstmt in
        match else_body with
        | Stmt_if _ | Stmt_block _ -> return (Some else_body)
        | _ -> fail "Only block or if statement can be used after else"
      else return None
    in
    return (Stmt_if { init; cond; if_body; else_body })
;;

let parse_default_for pstmt pblock =
  let* init = pstmt >>| Option.some <|> return None in
  let ok_init = if is_valid_init_and_post init then true else false in
  let* _ = parse_stmt_sep in
  let* cond = parse_expr pblock >>| Option.some <|> return None in
  let* _ = parse_stmt_sep in
  let* post =
    let* next_char = peek_char_fail in
    match next_char with
    | '{' -> return None
    | _ -> pstmt >>| Option.some
  in
  let ok_post = if is_valid_init_and_post init then true else false in
  if not (ok_init && ok_post)
  then fail "Incorrect statement in for initialization or post statement"
  else
    let* body = ws_line *> pblock in
    return (Stmt_for { init; cond; post; body })
;;

let parse_for_only_cond pblock =
  let* next_char = peek_char_fail in
  let* cond =
    match next_char with
    | '{' -> return None
    | _ -> parse_expr pblock >>| Option.some
  in
  let* body = ws_line *> pblock in
  return (Stmt_for { init = None; cond; post = None; body })
;;

let parse_for_range_n pblock =
  let* _ = string "range" *> ws in
  let* range = parse_expr pblock in
  let* body = ws_line *> pblock in
  return
    (Stmt_for
       { init =
           Some
             (Stmt_short_var_decl (Short_decl_mult_init [ "i", Expr_const (Const_int 0) ]))
       ; cond = Some (Expr_bin_oper (Bin_less, Expr_ident "i", range))
       ; post = Some (Stmt_incr "i")
       ; body
       })
;;

let parse_for pstmt pblock =
  string "for"
  *> ws
  *> choice
       [ parse_default_for pstmt pblock
       ; parse_for_only_cond pblock
       ; parse_for_range_n pblock
       ]
;;

let parse_range pblock =
  let* _ = string "for" *> ws in
  let* idents = sep_by_comma1 parse_ident in
  if List.length idents > 2
  then fail "for with range stmt can have no more than two identifiers"
  else (
    let index, element =
      match idents with
      | first :: second :: _ -> first, Some second
      | first :: _ -> first, None
      | [] -> assert false
    in
    let* is_with_decl =
      ws_line
      *> (string ":=" *> return true
          <|> string "=" *> return false
          <|> fail "Assignment or declaration missed in for with range stmt")
    in
    let* array = ws *> string "range" *> ws *> parse_expr pblock in
    let* body = ws_line *> pblock in
    if is_with_decl
    then return (Stmt_range (Range_decl { index; element; array; body }))
    else return (Stmt_range (Range_assign { index; element; array; body })))
;;

let parse_stmt pblock =
  fix (fun pstmt ->
    choice
      [ (parse_long_var_decl pblock >>| fun decl -> Stmt_long_var_decl decl)
      ; parse_short_var_decl pblock
      ; parse_incr
      ; parse_decr
      ; parse_if pstmt pblock
      ; parse_chan_send pblock
      ; parse_break
      ; parse_continue
      ; parse_return pblock
      ; parse_stmt_call pblock
      ; parse_assign pblock
      ; parse_defer pblock
      ; parse_go pblock
      ; (pblock >>| fun block -> Stmt_block block)
      ; parse_for pstmt pblock
      ; parse_range pblock
      ]
      ~failure_msg:"Incorrect statement")
;;

let parse_block : block t =
  fix (fun pblock ->
    char '{'
    *> skip_many (ws *> parse_stmt_sep *> ws)
    *> ws
    *> sep_by (many1 parse_stmt_sep) (parse_stmt pblock)
    <* skip_many (ws *> parse_stmt_sep *> ws)
    <* ws
    <* char '}')
;;
