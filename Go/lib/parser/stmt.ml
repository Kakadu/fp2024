(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Angstrom
open Common
open Expr

let rec combine_lists l1 l2 =
  match l1, l2 with
  | [], [] -> []
  | x :: xs, y :: ys -> (x, y) :: combine_lists xs ys
  | _, _ -> []
;;

let parse_lvalues = sep_by_comma1 parse_ident
let parse_rvalues pblock = sep_by_comma1 (parse_expr pblock)

let parse_long_var_decl pblock =
  let* () = string "var" *> ws in
  let* lvalues = parse_lvalues <* ws_line in
  let* vars_type = parse_type >>| (fun t -> Some t) <|> return None in
  let* with_init = ws_line *> char '=' *> ws *> return true <|> return false in
  if not with_init
  then (
    match vars_type, lvalues with
    | Some t, hd :: tl -> return (Long_decl_no_init (t, hd, tl))
    | _ -> fail)
  else
    let* rvalues = parse_rvalues pblock in
    let* () = fail_if (Base.List.is_empty lvalues) in
    match lvalues, rvalues, List.length lvalues = List.length rvalues with
    | lhd :: ltl, rhd :: rtl, true ->
      return (Long_decl_mult_init (vars_type, (lhd, rhd), combine_lists ltl rtl))
    | lfst :: lsnd :: ltl, rhd :: _, false ->
      let* () = fail_if (List.length rvalues <> 1) in
      (match rhd with
       | Expr_call call -> return (Long_decl_one_init (vars_type, lfst, lsnd, ltl, call))
       | _ -> fail)
    | _ -> fail
;;

let parse_short_var_decl pblock =
  let* lvalues = parse_lvalues in
  let* () = ws_line *> string ":=" *> ws in
  let* rvalues = parse_rvalues pblock in
  match lvalues, rvalues, List.length lvalues = List.length rvalues with
  | lhd :: ltl, rhd :: rtl, true ->
    return (Short_decl_mult_init ((lhd, rhd), combine_lists ltl rtl))
  | lfst :: lsnd :: ltl, rhd :: _, false ->
    let* () = fail_if (List.length rvalues <> 1) in
    (match rhd with
     | Expr_call call -> return (Short_decl_one_init (lfst, lsnd, ltl, call))
     | _ -> fail)
  | _ -> fail
;;

(** [parse_assign_lvalues pblock] parses {i one} or more assign lvalues
    separated by comma such as: [a], [a, b], [a[][]], [a[], b] *)
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
  let* () = ws_line *> char '=' *> ws in
  let* rvalues = parse_rvalues pblock in
  match lvalues, rvalues, List.length lvalues = List.length rvalues with
  | lhd :: ltl, rhd :: rtl, true ->
    return (Assign_mult_expr ((lhd, rhd), combine_lists ltl rtl))
  | lfst :: lsnd :: ltl, rhd :: _, false ->
    let* () = fail_if (List.length rvalues <> 1) in
    (match rhd with
     | Expr_call call -> return (Assign_one_expr (lfst, lsnd, ltl, call))
     | _ -> fail)
  | _ -> fail
;;

let parse_incr = parse_ident <* ws_line <* string "++"
let parse_decr = parse_ident <* ws_line <* string "--"

let parse_func_call pblock =
  parse_expr pblock
  >>= function
  | Expr_call call -> return call
  | _ -> fail
;;

let parse_defer pblock =
  string "defer" *> ws *> parse_func_call pblock >>| fun call -> Stmt_defer call
;;

let parse_go pblock =
  string "go" *> ws *> parse_func_call pblock >>| fun call -> Stmt_go call
;;

let parse_chan_send pblock =
  let* chan = parse_ident in
  let* expr = token "<-" *> parse_expr pblock in
  return (chan, expr)
;;

let parse_chan_receive pblock =
  let* chan = string "<-" *> ws *> parse_expr pblock in
  return chan
;;

let parse_return pblock =
  string "return" *> ws_line *> sep_by_comma (parse_expr pblock)
  >>| fun expr_list -> Stmt_return expr_list
;;

let parse_if_for_init pblock =
  choice
    [ (parse_short_var_decl pblock >>| fun s -> Init_decl s)
    ; (parse_assign pblock >>| fun s -> Init_assign s)
    ; (parse_incr >>| fun s -> Init_incr s)
    ; (parse_decr >>| fun s -> Init_decr s)
    ; (parse_func_call pblock >>| fun s -> Init_call s)
    ; (parse_chan_send pblock >>| fun s -> Init_send s)
    ; (parse_chan_receive pblock >>| fun s -> Init_receive s)
    ]
;;

let parse_if pblock =
  fix (fun parse_if ->
    let* () = string "if" *> ws in
    let* init =
      parse_if_for_init pblock >>| Option.some <* parse_stmt_sep <|> return None
    in
    let* () =
      match init with
      | None -> parse_stmt_sep <|> return ()
      | Some _ -> return ()
    in
    let* cond = ws *> parse_expr pblock in
    let* if_body = ws_line *> pblock <* ws_line in
    let* else_body =
      let* else_body_exists = string "else" *> ws *> return true <|> return false in
      if else_body_exists
      then
        pblock
        >>| (fun block -> Some (Else_block block))
        <|> (parse_if >>| fun block -> Some (Else_if block))
      else return None
    in
    return { init; cond; if_body; else_body })
;;

let parse_default_for pblock =
  let* init = parse_if_for_init pblock >>| Option.some <|> return None in
  let* () = parse_stmt_sep in
  let* cond = parse_expr pblock >>| Option.some <|> return None in
  let* () = parse_stmt_sep in
  let* post =
    let* next_char = peek_char_fail in
    match next_char with
    | '{' -> return None
    | _ -> parse_if_for_init pblock >>| Option.some
  in
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

let parse_for pblock =
  string "for" *> ws *> (parse_default_for pblock <|> parse_for_only_cond pblock)
;;

let parse_stmt pblock =
  choice
    [ (parse_long_var_decl pblock >>| fun s -> Stmt_long_var_decl s)
    ; (parse_short_var_decl pblock >>| fun s -> Stmt_short_var_decl s)
    ; (parse_incr >>| fun s -> Stmt_incr s)
    ; (parse_decr >>| fun s -> Stmt_decr s)
    ; (parse_if pblock >>| fun s -> Stmt_if s)
    ; (parse_chan_send pblock >>| fun s -> Stmt_chan_send s)
    ; (parse_chan_receive pblock >>| fun s -> Stmt_chan_receive s)
    ; string "break" *> return Stmt_break
    ; string "continue" *> return Stmt_continue
    ; parse_return pblock
    ; (parse_func_call pblock >>| fun s -> Stmt_call s)
    ; (parse_assign pblock >>| fun s -> Stmt_assign s)
    ; parse_defer pblock
    ; parse_go pblock
    ; (pblock >>| fun block -> Stmt_block block)
    ; parse_for pblock
    ]
;;

let parse_block : block t =
  fix (fun pblock ->
    char '{'
    *> ws
    *> skip_many (parse_stmt_sep *> ws)
    *> sep_by (many1 parse_stmt_sep) (parse_stmt pblock)
    <* ws
    <* skip_many (parse_stmt_sep *> ws)
    <* char '}')
;;
