(** Copyright 2024-2025, Ruslan Nafikov *)

(** SPDX_License-Identifier: LGPL-3.0 -or-later *)

open Angstrom
open Ast
open Base

let start_parsing parser string = parse_string ~consume:All parser string

let is_char = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_keyword = function
  | "let"
  | "rec"
  | "fun"
  | "if"
  | "then"
  | "else"
  | "true"
  | "false"
  | "match"
  | "with"
  | "in" -> true
  | _ -> false
;;

let is_whitespace = function
  | ' ' | '\n' | '\t' | '\r' -> true
  | _ -> false
;;

let is_underscore = function
  | c -> Char.equal c '_'
;;

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

(* Simple parsers *)

let parse_white_space = take_while is_whitespace
let parse_white_space1 = take_while1 is_whitespace
let parse_token s = parse_white_space *> s
let parse_token1 s = parse_white_space1 *> s
let pstrtoken s = parse_white_space *> string s
let pstrtoken1 s = parse_white_space1 *> string s
let parens p = pstrtoken "(" *> p <* pstrtoken ")"

(* Parse const *)

let parse_bool =
  (fun _ -> Const_bool true)
  <$> string "true"
  <|> ((fun _ -> Const_bool false) <$> string "false")
;;

let parse_int =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
  >>= fun digits -> return (Const_int (int_of_string digits))
;;

let parse_string =
  (fun strk -> Const_string strk)
  <$> (char '"' *> take_while1 (fun a -> a != '"') <* char '"')
;;

let parse_const = parse_bool <|> parse_int <|> parse_string

let check_var cond =
  parse_white_space *> take_while1 cond
  >>= fun v ->
  if String.length v == 0
  then fail "Not identifier"
  else if is_keyword v
  then fail ("You can not use" ^ v ^ "keywords as vars")
  else if Char.is_digit @@ String.get v 0
  then fail "Identifier first sumbol is letter, not digit"
  else return v
;;

let parse_var =
  let is_entry = function
    | c -> is_char c || is_underscore c || is_digit c
  in
  check_var is_entry
;;

(* Parse pattern *)

let parse_pattern_var = (fun v -> Pattern_id v) <$> parse_var
let parse_wild = (fun _ -> Pattern_wild) <$> pstrtoken "_"
let parse_pattern_const = (fun c -> Pattern_const c) <$> parse_const

let parse_tuple parser =
  lift2
    (fun a b -> Pattern_tuple (a :: b))
    (parse_token parser)
    (many1 (pstrtoken "," *> parser))
;;

let rec constr_con = function
  | [] -> Pattern_const Const_nil
  | hd :: [] when equal_pattern hd (Pattern_const Const_nil) -> Pattern_const Const_nil
  | [ f; s ] -> Pattern_list (f, s)
  | hd :: tl -> Pattern_list (hd, constr_con tl)
;;

let parser_con c =
  lift2
    (fun a b -> constr_con @@ (a :: b))
    (c <* pstrtoken "::" <|> (parens c <* pstrtoken "::"))
    (sep_by (pstrtoken "::") (c <|> parens c))
;;

let parse_con_2 parser constructor =
  constructor <$> (pstrtoken "[" *> sep_by1 (pstrtoken ";") parser <* pstrtoken "]")
;;

let parse_pattern =
  fix
  @@ fun pack ->
  let value = parse_wild <|> parse_pattern_const <|> parse_pattern_var in
  let tuple = parens @@ parse_tuple (value <|> pack) in
  let con =
    parser_con (tuple <|> parse_con_2 pack constr_con <|> value)
    <|> parse_con_2 (tuple <|> pack) constr_con
  in
  choice [ con; tuple; value ]
;;

(* Parse expression *)

(* Parse expr *)

let p_op char_op op = pstrtoken char_op *> return (fun e1 e2 -> Expr_bin_op (op, e1, e2))
let pmulti = p_op "*" Mul <|> p_op "/" Div <|> p_op "%" Mod
let pcons = p_op "::" Con
let padd = p_op "+" Add <|> p_op "-" Sub
let pcomp = p_op ">=" Geq <|> p_op ">" Gt <|> p_op "<=" Leq <|> p_op "<" Lt
let peq = p_op "=" Eq <|> p_op "<>" Neq
let pconj = p_op "&&" And
let pdisj = p_op "||" Or
let constr_case pat expr = pat, expr
let constr_efun pl e = List.fold_right ~init:e ~f:(fun p e -> Expr_fun (p, e)) pl
let parse_econst = (fun v -> Expr_const v) <$> parse_const
let parse_evar = (fun v -> Expr_var v) <$> parse_var

let parse_cons_semicolon_expr parser constructor =
  constructor <$> (pstrtoken "[" *> sep_by1 (pstrtoken ";") parser <* pstrtoken "]")
;;

let parse_tuple_expr parser =
  lift2
    (fun a b -> Expr_tuple (a :: b))
    (parser <* pstrtoken ",")
    (sep_by1 (pstrtoken ",") parser)
;;

let plet_body pargs pexpr =
  parse_token1 pargs
  >>= fun args -> pstrtoken "=" *> pexpr >>| fun expr -> constr_efun args expr
;;

let parse_fun_args = fix @@ fun p -> many1 parse_pattern <|> parens p

type edispatch =
  { list_e : edispatch -> expr t
  ; tuple_e : edispatch -> expr t
  ; fun_e : edispatch -> expr t
  ; app_e : edispatch -> expr t
  ; if_e : edispatch -> expr t
  ; let_in_e : edispatch -> expr t
  ; matching_e : edispatch -> expr t
  ; bin_e : edispatch -> expr t
  ; expr_parsers : edispatch -> expr t
  }

let pack =
  let expr_parsers pack = pack.bin_e pack <|> pack.matching_e pack in
  let value_e = fix @@ fun _ -> parse_evar <|> parse_econst in
  let op_parsers pack =
    parse_white_space
    *> choice
         [ pack.if_e pack
         ; pack.let_in_e pack
         ; pack.fun_e pack
         ; pack.app_e pack <|> parens @@ pack.app_e pack
         ; parens @@ choice [ pack.tuple_e pack; pack.bin_e pack ]
         ; value_e
         ; pack.list_e pack
         ]
  in
  let app_args_parsers pack =
    choice
      [ pack.list_e pack
      ; parens
        @@ choice
             [ pack.expr_parsers pack
             ; pack.let_in_e pack
             ; pack.app_e pack
             ; pack.fun_e pack
             ; pack.tuple_e pack
             ]
      ; value_e
      ]
  in
  let bin_e pack =
    fix
    @@ fun _ ->
    let multi = chainl1 (op_parsers pack) pmulti in
    let add = chainl1 multi padd in
    let cons = chainl1 add pcons in
    let comp = chainl1 cons pcomp in
    let eq = chainl1 comp peq in
    let conj = chainl1 eq pconj in
    chainl1 conj pdisj <* parse_white_space
  in
  let matching_e pack =
    fix
    @@ fun _ ->
    lift2
      (fun e cases -> Expr_match (e, cases))
      (pstrtoken "match" *> op_parsers pack <* pstrtoken1 "with")
      (let case2 =
         lift2
           constr_case
           (pstrtoken "|" *> parse_pattern <* pstrtoken "->")
           (op_parsers pack)
       in
       let case1 =
         lift2 constr_case (parse_pattern <* pstrtoken "->") (op_parsers pack)
       in
       let cases = lift2 (fun h tl -> h :: tl) (case1 <|> case2) (many case2) in
       cases)
  in
  let if_e pack =
    fix
    @@ fun _ ->
    lift3
      (fun e1 e2 e3 -> Expr_if (e1, e2, e3))
      (pstrtoken "if" *> expr_parsers pack)
      (pstrtoken "then" *> expr_parsers pack)
      (pstrtoken "else" *> expr_parsers pack)
  in
  let list_e pack =
    fix
    @@ fun _ ->
    let rec create_cons_sc = function
      | [] -> Expr_const Const_nil
      | hd :: [] when equal_expr hd (Expr_const Const_nil) -> Expr_const Const_nil
      | hd :: tl -> Expr_list (hd, create_cons_sc tl)
    in
    parse_cons_semicolon_expr (expr_parsers pack) create_cons_sc
  in
  let tuple_e pack = fix @@ fun _ -> parse_tuple_expr (expr_parsers pack) in
  let let_in_e pack =
    let lift5 f p1 p2 p3 p4 p5 = f <$> p1 <*> p2 <*> p3 <*> p4 <*> p5 in
    fix
    @@ fun _ ->
    lift5
      (fun is_rec name args expr1 expr2 ->
         let expr = constr_efun args expr1 in
         Expr_let_in (is_rec, name, expr, expr2))
      (pstrtoken "let"
       *> option false (parse_token (string "rec") <* parse_white_space1 >>| fun _ -> true)
      )
      parse_var
      (many parse_pattern)
      (pstrtoken1 "=" *> expr_parsers pack)
      (pstrtoken "in" *> expr_parsers pack)
  in
  let fun_e pack =
    fix
    @@ fun _ ->
    lift2
      constr_efun
      (pstrtoken "fun" *> parse_fun_args)
      (pstrtoken "->" *> expr_parsers pack)
  in
  let app_e pack =
    fix
    @@ fun _ ->
    lift2
      (fun f args -> List.fold_left ~init:f ~f:(fun f arg -> Expr_app (f, arg)) args)
      (value_e <|> parens @@ choice [ fun_e pack; pack.app_e pack ])
      (many1 (parse_token1 @@ app_args_parsers pack))
  in
  { list_e; tuple_e; fun_e; let_in_e; app_e; if_e; expr_parsers; matching_e; bin_e }
;;

let parse_expression = pack.expr_parsers pack

let let_e parse =
  fix
  @@ fun _ ->
  lift4
    (fun flag name args body ->
       let body = constr_efun args body in
       Let (flag, name, body))
    (pstrtoken "let"
     *> option false (parse_token (string "rec") <* parse_white_space1 >>| fun _ -> true)
    )
    parse_var
    (parse_white_space *> many (parse_pattern <|> parens parse_pattern))
    (pstrtoken "=" *> parse)
;;

let expr_main = (fun expr -> Expression expr) <$> parse_expression
let parse_bind = choice [ let_e parse_expression; expr_main ]
let program = many1 (parse_token parse_bind <* parse_token (many1 (pstrtoken ";;")))
let main_parse str = start_parsing program (String.strip str)

(* TESTS  PARSER *)

let start_test parser show input =
  let res = start_parsing parser input in
  match res with
  | Ok res -> Format.printf "%s" (show res)
  | Error err -> Format.printf "%s" err
;;

(* Test const parser *)

let%expect_test _ =
  let test = "true" in
  start_test parse_bool show_const test;
  [%expect {| (Const_bool true) |}]
;;

let%expect_test _ =
  let test = "false" in
  start_test parse_bool show_const test;
  [%expect {| (Const_bool false) |}]
;;

let%expect_test _ =
  let test = "56894" in
  start_test parse_int show_const test;
  [%expect {| (Const_int 56894) |}]
;;

let%expect_test _ =
  let test = "\"I like Kakadu <3\"" in
  start_test parse_string show_const test;
  [%expect {| (Const_string "I like Kakadu <3") |}]
;;

let%expect_test _ =
  let test = "somethingname" in
  start_test parse_pattern_var show_pattern test;
  [%expect {| (Pattern_id "somethingname") |}]
;;

let%expect_test _ =
  let test = "somethinggg" in
  start_test parse_pattern show_pattern test;
  [%expect {| (Pattern_id "somethinggg") |}]
;;

let%expect_test _ =
  let test = "1" in
  start_test parse_pattern show_pattern test;
  [%expect {| (Pattern_const (Const_int 1)) |}]
;;

let%expect_test _ =
  let test = "\"SomeString\"" in
  start_test parse_pattern show_pattern test;
  [%expect {| (Pattern_const (Const_string "SomeString")) |}]
;;

let%expect_test _ =
  let test = "_" in
  start_test parse_pattern show_pattern test;
  [%expect {| Pattern_wild |}]
;;

let%expect_test _ =
  let test = "(1,2,3,4)" in
  start_test parse_pattern show_pattern test;
  [%expect
    {|
    (Pattern_tuple
       [(Pattern_const (Const_int 1)); (Pattern_const (Const_int 2));
         (Pattern_const (Const_int 3)); (Pattern_const (Const_int 4))]) |}]
;;

let%expect_test _ =
  let test = "[1;2;3;4]" in
  start_test parse_pattern show_pattern test;
  [%expect
    {|
    (Pattern_list ((Pattern_const (Const_int 1)),
       (Pattern_list ((Pattern_const (Const_int 2)),
          (Pattern_list ((Pattern_const (Const_int 3)),
             (Pattern_const (Const_int 4))))
          ))
       )) |}]
;;

let%expect_test _ =
  let test = "(1,2,3,[4;5])" in
  start_test parse_bind show_struct_prog test;
  [%expect
    {|
    (Expression
       (Expr_tuple
          [(Expr_const (Const_int 1)); (Expr_const (Const_int 2));
            (Expr_const (Const_int 3));
            (Expr_list ((Expr_const (Const_int 4)),
               (Expr_list ((Expr_const (Const_int 5)), (Expr_const Const_nil)))))
            ])) |}]
;;

let%expect_test _ =
  let test = "1 + 2" in
  start_test parse_bind show_struct_prog test;
  [%expect
    {|
    (Expression
       (Expr_bin_op (Add, (Expr_const (Const_int 1)), (Expr_const (Const_int 2))
          ))) |}]
;;

let%expect_test _ =
  let test = "let x = 42" in
  start_test parse_bind show_struct_prog test;
  [%expect {| (Let (false, "x", (Expr_const (Const_int 42)))) |}]
;;

let%expect_test _ =
  let test = "let rec fact n = if n = 1 then 1 else n * (fact (n - 1))" in
  start_test parse_bind show_struct_prog test;
  [%expect
    {|
    (Let (true, "fact",
       (Expr_fun ((Pattern_id "n"),
          (Expr_if (
             (Expr_bin_op (Eq, (Expr_var "n"), (Expr_const (Const_int 1)))),
             (Expr_const (Const_int 1)),
             (Expr_bin_op (Mul, (Expr_var "n"),
                (Expr_app ((Expr_var "fact"),
                   (Expr_bin_op (Sub, (Expr_var "n"), (Expr_const (Const_int 1))
                      ))
                   ))
                ))
             ))
          ))
       )) |}]
;;
