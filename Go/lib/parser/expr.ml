(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Angstrom
open Common

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let parse_unary_not =
  char '!' *> ws *> return (fun expr -> Expr_un_oper (Unary_not, expr))
;;

let parse_unary_minus =
  char '-' *> ws *> return (fun expr -> Expr_un_oper (Unary_minus, expr))
;;

let parse_unary_plus =
  char '+' *> ws *> return (fun expr -> Expr_un_oper (Unary_plus, expr))
;;

let parse_unary_receive =
  string "<-" *> ws *> return (fun expr -> Expr_un_oper (Unary_recieve, expr))
;;

let parse_mult_unary_op pexpr =
  let rec helper acc =
    choice [ parse_unary_not; parse_unary_minus; parse_unary_plus; parse_unary_receive ]
    >>= (fun new_oper -> helper (fun expr -> acc @@ new_oper expr))
    <|> return acc
  in
  let* unary_operators = helper Fun.id in
  let* expr = pexpr in
  return (unary_operators expr)
;;

let parse_sum = token "+" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_sum, exp1, exp2))

let parse_mult =
  token "*" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_multiply, exp1, exp2))
;;

let parse_subtraction =
  token "-" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_subtract, exp1, exp2))
;;

let parse_division =
  token "/" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_divide, exp1, exp2))
;;

let parse_modulus =
  token "%" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_modulus, exp1, exp2))
;;

let parse_equal =
  token "==" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_equal, exp1, exp2))
;;

let parse_not_equal =
  token "!= " *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_not_equal, exp1, exp2))
;;

let parse_greater =
  token ">" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_greater, exp1, exp2))
;;

let parse_greater_equal =
  token ">=" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_greater_equal, exp1, exp2))
;;

let parse_less =
  token "<" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_less, exp1, exp2))
;;

let parse_less_equal =
  token "<=" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_less_equal, exp1, exp2))
;;

let parse_and =
  token "&&" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_and, exp1, exp2))
;;

let parse_or = token "||" *> return (fun exp1 exp2 -> Expr_bin_oper (Bin_or, exp1, exp2))
let parse_const_int = parse_int >>| fun num -> Const_int num

let parse_const_string =
  char '"' *> take_till (Char.equal '"') <* char '"' >>| fun string -> Const_string string
;;

let parse_idents_with_types =
  let* args_lists =
    sep_by_comma1
      (let* idents = sep_by_comma1 parse_ident in
       let* t = ws_line *> parse_type in
       return (Base.List.map ~f:(fun id -> id, t) idents))
  in
  return (List.concat args_lists)
;;

let parse_func_args = parens parse_idents_with_types <|> (parens ws >>| fun _ -> [])

let parse_func_return_values =
  choice
    [ (parens parse_idents_with_types >>| fun returns -> Some (Ident_and_types returns))
    ; (parens (sep_by_comma1 parse_type) >>| fun types -> Some (Only_types types))
    ; (parse_type >>| fun t -> Some (Only_types [ t ]))
    ; (let* _ = parens ws <|> return () in
       let* char = ws_line *> peek_char_fail in
       match char with
       | '{' -> return None
       | _ -> fail "Incorrect func return values")
    ]
;;

let parse_func_args_returns_and_body pblock =
  let* args = parse_func_args <* ws_line in
  let* returns = parse_func_return_values <* ws_line in
  let* body = pblock in
  return { args; returns; body }
;;

let parse_const_func pblock =
  string "func" *> ws *> parse_func_args_returns_and_body pblock
  >>| fun anon_func -> Const_func anon_func
;;

let parse_const_array pexpr =
  let* size =
    square_brackets (parse_int >>| Option.some <|> string "..." *> return None)
  in
  let* type' = ws *> parse_type in
  let* inits = curly_braces (sep_by_comma pexpr) in
  let size =
    match size with
    | Some size -> size
    | None -> List.length inits
  in
  return (Const_array (size, type', inits))
;;

let parse_const pexpr pblock =
  choice
    [ parse_const_int
    ; parse_const_string
    ; parse_const_array pexpr
    ; parse_const_func pblock
    ]
  >>| fun const -> Expr_const const
;;

let parse_expr_ident = parse_ident >>| fun ident -> Expr_ident ident

let parse_expr_func_call pexpr func =
  let* args = parens (sep_by_comma pexpr) in
  return (Expr_call (func, args))
;;

let parse_index pexpr array =
  let* index = square_brackets pexpr in
  return (array, index)
;;

let parse_expr_index pexpr array =
  let* array, index = parse_index pexpr array in
  return (Expr_index (array, index))
;;

let parse_nested_calls_and_indices pexpr parse_func_or_array =
  let rec helper acc =
    parse_expr_func_call pexpr acc
    <|> parse_expr_index pexpr acc
    >>= helper
    <|> return acc
  in
  parse_func_or_array >>= helper
;;

let parse_expr pblock =
  fix (fun pexpr ->
    let arg = parens pexpr <|> parse_const pexpr pblock <|> parse_expr_ident in
    let arg = parse_nested_calls_and_indices pexpr arg in
    let arg = parse_mult_unary_op arg in
    let arg = chainl1 arg (parse_mult <|> parse_modulus <|> parse_division) in
    let arg = chainl1 arg (parse_sum <|> parse_subtraction) in
    let arg =
      chainl1
        arg
        (choice
           [ parse_greater_equal
           ; parse_less_equal
           ; parse_greater
           ; parse_less
           ; parse_equal
           ; parse_not_equal
           ])
    in
    let arg = chainr1 arg parse_and in
    let arg = chainr1 arg parse_or in
    arg)
;;
