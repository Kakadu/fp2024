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
  | "and"
  | "if"
  | "then"
  | "else"
  | "true"
  | "false"
  | "match"
  | "with"
  | "in"
  | "fun"
  | "type" -> true
  | _ -> false
;;

let ws = take_while is_whitespace
let wss t = ws *> t <* ws
let token s = ws *> string s <* ws
let word s = ws *> string s <* take_while1 is_whitespace
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

let capitalized_ident =
  let* first =
    ws
    *> satisfy (function
      | 'A' .. 'Z' -> true
      | _ -> false)
  in
  let* rest =
    take_while (function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
      | _ -> false)
    <* ws
  in
  let word = Char.escaped first ^ rest in
  return word
;;

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
  var
;;

let pexp_ident = p_id >>| fun i -> Pexp_ident i

let chain1l expr op =
  let rec go acc = lift2 (fun f x -> f acc x) op expr >>= go <|> return acc in
  expr >>= go
;;

let p_binop p expr =
  chain1l expr (p >>= fun c -> return (fun x y -> Pexp_apply (Pexp_ident c, [ x; y ])))
;;

let p_tuple expr =
  let* first = expr <* token "," in
  let+ es = sep_by (token ",") expr in
  Pexp_tuple (first :: es)
;;

let p_pattern =
  let pat_const = p_const >>| fun c -> Ppat_constant c in
  let pat_var =
    lowercase_ident >>| fun var -> if var = "_" then Ppat_any else Ppat_var var
  in
  let pat_interval =
    p_const >>= fun f -> token ".." *> p_const >>| fun s -> Ppat_interval (f, s)
  in
  fix (fun pattern : pattern t ->
    let pat_const = choice [ parens pattern; pat_interval; pat_const; pat_var ] in
    let pat_construct =
      (let* name = capitalized_ident in
       let+ body = option None (pattern >>| fun p -> Some p) in
       Ppat_construct (name, body))
      <|> pat_const
    in
    let pat_tuple =
      lift2
        (fun l ls -> Ppat_tuple (l :: ls))
        pat_construct
        (many1 (token "," *> pat_construct))
      <|> pat_construct
    in
    let pat_unit = word "()" >>| (fun _ -> Ppat_unit) <|> pat_tuple in
    pat_unit)
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
  let* else_token = option None (token "else" >>| fun _ -> Some 1) in
  match else_token with
  | None -> return (Pexp_ifthenelse (first, second, None))
  | Some _ ->
    let+ third = expr >>| fun e -> Some e in
    Pexp_ifthenelse (first, second, third)
;;

let p_apply expr =
  let* first = wss expr in
  let* single = wss (peek_string 1) in
  match single with
  | "+" | "-" -> fail ""
  | _ ->
    let+ second = many1 (wss expr) in
    Pexp_apply (first, second)
;;

let p_rec_flag = word "rec" >>| (fun _ -> Recursive) <|> return NonRecursive

let p_value_binding expr =
  let* pattern = p_pattern in
  let rec helper = function
    | Ppat_any | Ppat_unit | Ppat_var _ | Ppat_construct _ -> true
    | Ppat_tuple pts -> List.fold_left (fun acc pat -> acc && helper pat) true pts
    | Ppat_constant _ | Ppat_interval _ -> false
  in
  (* Zanuda thinks it's better *)
  if not (helper pattern)
  then fail "Pattern name must be wildcard, variable or tuple of them"
  else
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
  let* vb = p_value_binding expr in
  let* value_bindings = many (token "and" *> p_value_binding expr) in
  let+ expr = token "in" *> expr in
  Pexp_let (rec_flag, vb :: value_bindings, expr)
;;

let token_or xs : string t =
  let token_functions = List.map token xs in
  match token_functions with
  | h :: t -> List.fold_right ( <|> ) t h
  | _ -> fail "token_or require two or more tokens"
;;

let p_unary =
  let* first = take 1 in
  if first = "-" || first = "+"
  then Pexp_ident first |> return
  else "Failed when parse unary" |> fail
;;

let pexpr_constraint expr =
  let* expr = token "(" *> expr in
  let+ ty = token ":" *> lowercase_ident <* token ")" in
  Pexp_constraint (expr, Ptyp_constr ty)
;;

let p_construct expr =
  let* name = capitalized_ident in
  let+ body = option None (expr >>| fun x -> Some x) in
  Pexp_construct (name, body)
;;

let p_pattern_matching expr =
  let case =
    let* p = p_pattern in
    let* e = token "->" *> expr in
    return { pc_lhs = p; pc_rhs = e }
  in
  let* first = (token "|" <|> ws) *> case in
  (* Format.printf "%a\n" pp_case first; *)
  let+ cases = many (token "|" *> case) in
  first :: cases
;;

let p_match expr =
  let* e = word "match" *> expr in
  let+ cases = word "with" *> p_pattern_matching expr in
  Pexp_match (e, cases)
;;

let p_expr =
  fix (fun expr ->
    let expr_const =
      choice
        [ parens expr; pexpr_const; pexpr_constraint expr; pexp_ident; p_branch expr ]
    in
    let expr_construct = p_construct expr <|> expr_const in
    let expr_fun = p_fun expr <|> expr_construct in
    let expr_apply = p_apply expr_fun <|> expr_fun in
    let expr_mul_div = p_binop (token "*" <|> token "/") expr_apply <|> expr_apply in
    let expr_add_sub = p_binop (token "+" <|> token "-") expr_mul_div <|> expr_mul_div in
    let expr_comparison =
      p_binop (token_or [ "<"; "<="; ">"; ">="; "="; "<>" ]) expr_add_sub <|> expr_add_sub
    in
    let expr_let_in = p_let_in expr <|> expr_comparison in
    let expr_match = p_match expr <|> expr_let_in in
    let expr_tuple = p_tuple expr_match <|> expr_match in
    expr_tuple)
;;

let p_str_value expr =
  let* rec_flag = token "let" *> p_rec_flag in
  let* vb = p_value_binding expr in
  let+ value_bindings = many (token "and" *> p_value_binding expr) in
  Pstr_value (rec_flag, vb :: value_bindings)
;;

let p_structure =
  let str_value = p_str_value p_expr in
  let str_eval = p_expr >>| (fun ex -> Pstr_eval ex) <|> str_value in
  str_eval
;;

let parse_expr str = parse_string ~consume:All p_expr str
let parse_structure str = parse_string ~consume:All p_structure str
let parse = parse_structure
let parse_program str = parse_string ~consume:All (many p_structure <* end_of_input) str
