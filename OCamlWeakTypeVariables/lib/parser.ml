[@@@ocaml.text "/*"]

(** Copyright 2024-2025, Damir Yunusov and Ilhom Kombaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Angstrom

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_keyword = function
  | "let"
  | "rec"
  | "if"
  | "then"
  | "else"
  | "true"
  | "false"
  | "match"
  | "with"
  | "in"
  | "fun"
  | "type"
  | "int"
  | "string"
  | "bool" -> true
  | _ -> false
;;

let ws = take_while is_whitespace
let wss t = ws *> t <* ws
let token s = ws *> string s <* ws
let parens t = token "(" *> t <* token ")"

let p_const_int =
  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
  in
  let* sign = choice [ token "-"; token "+"; token "" ] in
  let* first_digit = satisfy is_digit in
  let+ digits =
    take_while (function
      | '0' .. '9' | '_' -> true
      | _ -> false)
  in
  Pconst_int (int_of_string (sign ^ Char.escaped first_digit ^ digits))
;;

let p_const_string =
  let+ s =
    token "\""
    *> take_while (function
      | '"' -> false
      | _ -> true)
    <* token "\""
  in
  Pconst_string s
;;

let p_const_bool =
  let+ bool_str = choice [ token "true"; token "false" ] in
  Pconst_boolean (bool_of_string bool_str)
;;

let p_const =
  choice
    ~failure_msg:"Error while parsing literal"
    [ p_const_int; p_const_string; p_const_bool ]
;;

let pexpr_const = p_const >>| fun x -> Pexp_constant x

let lowercase_ident =
  let* first =
    ws
    *> satisfy (function
      | 'a' .. 'z' | '_' -> true
      | _ -> false)
  in
  let* rest =
    take_while (function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
      | _ -> false)
    <* ws
  in
  let word = Char.escaped first ^ rest in
  if is_keyword word then fail "Keyword identificators are not allowed." else return word
;;

(* TODO: readable error message *)
let p_id : id t =
  let+ var = lowercase_ident in
  Id var
;;

let pexp_ident = p_id >>| fun i -> Pexp_ident i

let chain1l expr op =
  let rec go acc = lift2 (fun f x -> f acc x) op expr >>= go <|> return acc in
  expr >>= go
;;

let p_binop p expr =
  chain1l
    expr
    (p >>= fun c -> return (fun x y -> Pexp_apply (Pexp_ident (Id c), [ x; y ])))
;;

let p_tuple expr =
  token "(" *> sep_by (token ",") expr <* token ")" >>| fun x -> Pexp_tuple x
;;

let p_pattern =
  let pat_any = token "_" >>| fun _ -> Ppat_any in
  let pat_const = p_const >>| fun c -> Ppat_constant c in
  let pat_var = lowercase_ident >>| fun var -> Ppat_var var in
  let pat_interval =
    p_const >>= fun f -> token ".." *> p_const >>| fun s -> Ppat_interval (f, s)
  in
  fix (fun pattern : pattern t ->
    let pat_const =
      choice [ parens pattern; pat_interval; pat_const; pat_any; pat_var ]
    in
    let pat_tuple =
      lift2 (fun l ls -> Ppat_tuple (l :: ls)) pat_const (many1 (token "," *> pat_const))
      <|> pat_const
    in
    pat_tuple)
;;

let p_fun expr =
  let* _ = token "fun" in
  let* ps = many1 p_pattern in
  let* _ = token "->" in
  let+ e = expr in
  List.fold_right (fun f p -> Pexp_fun (f, p)) ps e
;;

let p_branch (expr : expression t) =
  let* first = token "if" *> expr in
  let* second = token "then" *> expr in
  let+ third = option None (token "else" *> expr >>| fun e -> Some e) in
  Pexp_ifthenelse (first, second, third)
;;

let p_apply expr =
  let* first = wss expr in
  let+ second = many1 (wss expr) in
  Pexp_apply (first, second)
;;

let p_rec_flag = token "rec" >>| (fun _ -> Recursive) <|> return NonRecursive

let p_value_binding expr =
  let* pattern = p_pattern in
  let* xs = many p_pattern in
  let+ expr = token "=" *> expr in
  { pvb_pat = pattern
  ; pvb_expr =
      (match xs with
       | [] -> expr
       | _ -> List.fold_right (fun f p -> Pexp_fun (f, p)) xs expr)
  }
;;

let p_let_in expr =
  let* rec_flag = token "let" *> p_rec_flag in
  let* value_bindings = many1 (p_value_binding expr) in
  let+ expr = token "in" *> expr in
  Pexp_let (rec_flag, value_bindings, expr)
;;

let token_or xs : string t =
  let token_functions = List.map token xs in
  match token_functions with
  | h :: t -> List.fold_right ( <|> ) t h
  | _ -> fail "token_or require two or more tokens"
;;

let p_expr =
  fix (fun expr ->
    let expr_const =
      choice [ parens expr; pexpr_const; pexp_ident; p_tuple expr; p_branch expr ]
    in
    let expr_mul_div = p_binop (token "*" <|> token "/") expr_const <|> expr_const in
    let expr_add_sub = p_binop (token "+" <|> token "-") expr_mul_div <|> expr_mul_div in
    let expr_comparison =
      p_binop (token_or [ "<"; "<="; ">"; ">="; "="; "<>" ]) expr_add_sub <|> expr_add_sub
    in
    let expr_fun = p_fun expr <|> expr_comparison in
    let expr_apply = p_apply expr_fun <|> expr_fun in
    let expr_let_in = p_let_in expr <|> expr_apply in
    expr_let_in)
;;

let p_str_value expr =
  let* rec_flag = token "let" *> p_rec_flag in
  let+ value_bindings = many1 (p_value_binding expr) in
  Pstr_value (rec_flag, value_bindings)
;;

let p_structure =
  let str_value = p_str_value p_expr in
  let str_eval = p_expr >>| (fun ex -> Pstr_eval ex) <|> str_value in
  str_eval
;;

let parse_structure str = parse_string ~consume:All p_structure str
let parse = parse_structure
