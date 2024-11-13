(** Copyright 2021-2023, Daniil Kadochnikov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Result

type error =
  [ `Parsing_Error of string
  | `Some_Error
  ]

let pp_error ppf : error -> _ = function
  | `Parsing_Error msg -> Format.fprintf ppf "Parser Error: %s" msg
  | `Some_Error -> Format.fprintf ppf "Parser Error:"
;;

(* Tools for parsers*)
let spaces = skip_many (char ' ' <|> char '\t' <|> char '\n')
let parens parse_expr = char '(' *> spaces *> parse_expr <* spaces <* char ')'
let with_spaces parse_expr = spaces *> parse_expr <* spaces

(* functions for chains of operations *)
let chainl1 p op =
  let rec go acc = lift2 (fun f x -> f acc x) op p >>= go <|> return acc in
  p >>= go
;;

let chainl1_pat p op =
  let rec go acc = lift (fun f -> f acc) op >>= go <|> return acc in
  p >>= go
;;

let chainr1 p op =
  let rec go acc =
    op
    >>= (fun f -> p >>= fun x -> go x >>= fun result -> return (f acc result))
    <|> return acc
  in
  p >>= go
;;

(* type_expr parsing *)
let parse_type_int = string "int" >>| fun _ -> TInt
let parse_type_bool = string "bool" >>| fun _ -> TBool
let parse_type_string = string "string" >>| fun _ -> TString

let parse_keyword_list_opt =
  choice
    [ (string "list" >>| fun _ t -> TList t); (string "option" >>| fun _ t -> TOption t) ]
;;

let parse_type_expr =
  fix (fun parse_type_expr ->
    let term =
      choice
        [ parse_type_int; parse_type_bool; parse_type_string; parens parse_type_expr ]
    in
    let parse_type_list_opt = chainl1_pat term (spaces *> parse_keyword_list_opt) in
    let parse_type_tuple =
      lift2
        (fun head tail -> TTuple (head :: tail))
        parse_type_list_opt
        (many1 (with_spaces (string "*") *> parse_type_list_opt))
      <|> parse_type_list_opt
    in
    let parse_type_fun =
      chainr1 parse_type_tuple (with_spaces (string "->") >>| fun _ a b -> TFun (a, b))
    in
    parse_type_fun)
;;

(* name of variable parsing *)
let is_first_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

let is_letter_or_digit_or_underscore = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let keywords =
  [ "in"
  ; "let"
  ; "rec"
  ; "fun"
  ; "true"
  ; "false"
  ; "and"
  ; "Some"
  ; "None"
  ; "if"
  ; "then"
  ; "else"
  ]
;;

let is_keyword s = List.mem s keywords

let variable =
  satisfy is_first_letter
  >>= fun first_char ->
  take_while is_letter_or_digit_or_underscore
  >>= fun rest ->
  let ident = String.make 1 first_char ^ rest in
  if is_keyword ident
  then fail "Error: Syntax error: variable should not use a keyword name"
  else return ident
;;

(* Variable or typed variable parsing *)
let parse_typed_var =
  parens
    (variable
     >>= fun n -> with_spaces (char ':') *> parse_type_expr >>| fun t -> n, Some t)
  <|> (variable >>| fun n -> n, None)
;;

(* pattern parsing *)
let parse_pattern = parse_typed_var >>| fun (n, t) -> PVar (n, t)

(* rec_flag parsing *)
let parse_rec_flag = option NonRecursive (string "rec" >>| fun _ -> Recursive)

(* IntLiteral parsing *)
let integer_literal_parser =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
  >>| fun s -> EInt (int_of_string s)
;;

(* BoolLiteral parsing*)
let boolean_literal_parser =
  string "true" >>| (fun _ -> EBool true) <|> (string "false" >>| fun _ -> EBool false)
;;

(* Var parsing*)
let var_literal_parser = parse_typed_var >>| fun (n, t) -> EVar (n, t)

(* StringLiteral parsing*)
let string_literal_parser =
  char '"' *> take_while (fun c -> c <> '"') <* char '"' >>| fun s -> EString s
;;

(* BinaryOp parsing *)
let mul_div_literal =
  choice
    [ (char '*' >>| fun _ lhs rhs -> EBinOp (Mul, lhs, rhs))
    ; (char '/' >>| fun _ lhs rhs -> EBinOp (Div, lhs, rhs))
    ]
;;

let plus_sub_literal =
  choice
    [ (char '+' >>| fun _ lhs rhs -> EBinOp (Add, lhs, rhs))
    ; (char '-' >>| fun _ lhs rhs -> EBinOp (Sub, lhs, rhs))
    ]
;;

let compare_literal =
  choice
    [ (string "<=" >>| fun _ lhs rhs -> EBinOp (Le, lhs, rhs))
    ; (string ">=" >>| fun _ lhs rhs -> EBinOp (Ge, lhs, rhs))
    ; (string "<>" >>| fun _ lhs rhs -> EBinOp (Neq, lhs, rhs))
    ; (char '=' >>| fun _ lhs rhs -> EBinOp (Eq, lhs, rhs))
    ; (char '<' >>| fun _ lhs rhs -> EBinOp (Lt, lhs, rhs))
    ; (char '>' >>| fun _ lhs rhs -> EBinOp (Gt, lhs, rhs))
    ]
;;

let and_literal = string "&&" >>| fun _ lhs rhs -> EBinOp (And, lhs, rhs)
let or_literal = string "||" >>| fun _ lhs rhs -> EBinOp (Or, lhs, rhs)

(* Fun parsing *)
let parse_fun parse_expr =
  string "fun"
  *>
  let inner_func =
    fix (fun inner_func ->
      spaces *> parse_pattern
      >>= fun pat ->
      with_spaces (string "->") *> parse_expr
      <|> inner_func
      >>| fun expr -> EFun (pat, expr))
  in
  inner_func
;;

(* Part of let parsing for expressions and declarations *)
let parse_let_part parse_expr =
  let parse_body =
    return (fun name expr -> name, expr)
    <*> spaces *> parse_pattern
    <*> with_spaces (string "=") *> parse_expr
    <|> (return (fun name pats expr ->
           name, List.fold_right (fun p acc -> EFun (p, acc)) pats expr)
         <*> (spaces *> variable >>| fun n -> PVar (n, None))
         <*> many1 (spaces *> parse_pattern)
         <*> with_spaces (string "=") *> parse_expr)
  in
  return (fun isrec body_list -> isrec, body_list)
  <*> (string "let" *> spaces *> parse_rec_flag <* spaces)
  <*> lift2
        (fun f tl -> f :: tl)
        parse_body
        (many (with_spaces (string "and") *> parse_body))
;;

(* Parse Let *)
let parse_let parse_expr =
  parse_let_part parse_expr
  >>= fun (rf, b) ->
  option None (with_spaces (string "in") *> parse_expr >>| fun ex -> Some ex)
  >>| fun ex_opt -> ELet (rf, b, ex_opt)
;;

(* List parsing *)
let parse_list parse_expr =
  char '[' *> spaces *> sep_by (with_spaces (char ';')) parse_expr
  <* spaces
  <* char ']'
  >>| fun exprs -> EList exprs
;;

(* Some, None parsing *)
let parse_some parse_expr =
  string "Some" *> spaces *> parse_expr
  >>| (fun ex -> ESome ex)
  <|> (string "None" >>| fun _ -> ENone)
;;

(* App parsing *)
let parse_app parse_expr = chainl1 parse_expr (spaces >>| fun _ lhs rhs -> EApp (lhs, rhs))

(* Tuple parsing *)
let parse_tuple parse_expr =
  parse_expr
  >>= fun first ->
  many1 (with_spaces (char ',') *> parse_expr)
  >>= fun rest -> return (ETuple (first :: rest))
;;

(* IfThenElse parsing *)
let parse_if parse_expr =
  with_spaces (string "if") *> parse_expr
  >>= fun e1 ->
  with_spaces (string "then") *> parse_expr
  >>= fun e2 -> with_spaces (string "else") *> parse_expr >>| fun e3 -> EIf (e1, e2, e3)
;;

(* expr parsing *)
let parse_expr =
  fix (fun parse_expr ->
    let term =
      choice
        [ integer_literal_parser
        ; boolean_literal_parser
        ; string_literal_parser
        ; var_literal_parser
        ; parens parse_expr
        ]
    in
    let parse_app_or_some = parse_some term <|> parse_app term in
    let mul_div = chainl1 parse_app_or_some (with_spaces mul_div_literal) in
    let add_sub = chainl1 mul_div (with_spaces plus_sub_literal) in
    let comparison = chainl1 add_sub (with_spaces compare_literal) in
    let logical_and = chainl1 comparison (with_spaces and_literal) in
    let logical_or = chainl1 logical_and (with_spaces or_literal) in
    let parse_let_fun =
      parse_let parse_expr
      <|> parse_fun parse_expr
      <|> parse_list parse_expr
      <|> parse_if parse_expr
      <|> logical_or
    in
    parse_tuple parse_let_fun <|> parse_let_fun)
;;

(* Функция для парсинга строки *)
let parse s =
  match parse_string ~consume:All (spaces *> parse_expr <* spaces <* end_of_input) s with
  | Ok ast -> Ok ast
  | Error msg -> Error (`Parsing_Error msg)
;;

(* type_expr tests *)
let parse_test s =
  match parse s with
  | Ok ast -> print_endline (show_expr ast)
  | Error msg -> pp_error Format.std_formatter msg
;;

let%expect_test "parsing type: int" =
  parse_test "(x: int)";
  [%expect {| (EVar ("x", (Some TInt))) |}]
;;

let%expect_test "parsing type: bool" =
  parse_test "(x: bool)";
  [%expect {| (EVar ("x", (Some TBool))) |}]
;;

let%expect_test "parsing type: string" =
  parse_test "(x: string)";
  [%expect {| (EVar ("x", (Some TString))) |}]
;;

let%expect_test "parsing type: tuple" =
  parse_test "(x: int * bool)";
  [%expect {| (EVar ("x", (Some (TTuple [TInt; TBool])))) |}]
;;

let%expect_test "parsing type: nested tuple" =
  parse_test "(x: int * (bool * string))";
  [%expect {| (EVar ("x", (Some (TTuple [TInt; (TTuple [TBool; TString])])))) |}]
;;

let%expect_test "parsing type: list" =
  parse_test "(x: int list)";
  [%expect {| (EVar ("x", (Some (TList TInt)))) |}]
;;

let%expect_test "parsing type: list of tuples" =
  parse_test "(x: (int * bool) list)";
  [%expect {| (EVar ("x", (Some (TList (TTuple [TInt; TBool]))))) |}]
;;

let%expect_test "parsing type: option" =
  parse_test "(x: int option)";
  [%expect {| (EVar ("x", (Some (TOption TInt)))) |}]
;;

let%expect_test "parsing type: option of lists" =
  parse_test "(x: int list option)";
  [%expect {| (EVar ("x", (Some (TOption (TList TInt))))) |}]
;;

let%expect_test "parsing type: function" =
  parse_test "(x: int -> bool)";
  [%expect {| (EVar ("x", (Some (TFun (TInt, TBool))))) |}]
;;

let%expect_test "parsing type: function with multiple arguments" =
  parse_test "(x: int -> bool -> string)";
  [%expect {| (EVar ("x", (Some (TFun (TInt, (TFun (TBool, TString))))))) |}]
;;

let%expect_test "parsing type: function with tuple argument" =
  parse_test "(x: (int * bool) -> string)";
  [%expect {| (EVar ("x", (Some (TFun ((TTuple [TInt; TBool]), TString))))) |}]
;;

let%expect_test "parsing type: function returning a function" =
  parse_test "(x: int -> (bool -> string))";
  [%expect {| (EVar ("x", (Some (TFun (TInt, (TFun (TBool, TString))))))) |}]
;;

let%expect_test "parsing type: complex type" =
  parse_test "(x: (int -> bool) list option -> (string * int) -> string option list)";
  [%expect
    {| 
      (EVar ("x",
         (Some (TFun ((TOption (TList (TFun (TInt, TBool)))),
                  (TFun ((TTuple [TString; TInt]), (TList (TOption TString)))))))
         )) |}]
;;

(* expr tests *)
(* int test *)
let%expect_test "parsing expr: integer" =
  parse_test "123";
  [%expect {| (EInt 123) |}]
;;

(* bool test *)
let%expect_test "parsing expr: boolean true" =
  parse_test "true";
  [%expect {| (EBool true) |}]
;;

let%expect_test "parsing expr: boolean false" =
  parse_test "false";
  [%expect {| (EBool false) |}]
;;

(* var test *)
let%expect_test "parsing expr: identifier 1" =
  parse_test "my_variable";
  [%expect {| (EVar ("my_variable", None)) |}]
;;

let%expect_test "parsing expr: identifier 2" =
  parse_test "_123";
  [%expect {| (EVar ("_123", None)) |}]
;;

let%expect_test "parsing expr: identifier 3" =
  parse_test "Papa__123";
  [%expect {| (EVar ("Papa__123", None)) |}]
;;

let%expect_test "parsing expr: identifier 4" =
  parse_test "(smth : int)";
  [%expect {| (EVar ("smth", (Some TInt))) |}]
;;

(* string test *)
let%expect_test "parsing expr: string" =
  parse_test "\"hello, world!\"";
  [%expect {| (EString "hello, world!") |}]
;;

let%expect_test "parsing expr: empty string" =
  parse_test "\"\"";
  [%expect {| (EString "") |}]
;;

(* binOp test *)
let%expect_test "parsing expr: simple addition" =
  parse_test "1 + 2";
  [%expect {| (EBinOp (Add, (EInt 1), (EInt 2))) |}]
;;

let%expect_test "parsing expr: simple subtraction" =
  parse_test "5 - 3";
  [%expect {| (EBinOp (Sub, (EInt 5), (EInt 3))) |}]
;;

let%expect_test "parsing expr: simple multiplication" =
  parse_test "4 * 2";
  [%expect {| (EBinOp (Mul, (EInt 4), (EInt 2))) |}]
;;

let%expect_test "parsing expr: simple division" =
  parse_test "8 / 4";
  [%expect {| (EBinOp (Div, (EInt 8), (EInt 4))) |}]
;;

let%expect_test "parsing expr: addition and multiplication" =
  parse_test "1 + 2 * 3";
  [%expect {|
      (EBinOp (Add, (EInt 1), (EBinOp (Mul, (EInt 2), (EInt 3))))) |}]
;;

let%expect_test "parsing expr: subtraction and division" =
  parse_test "10 - 8 / 2";
  [%expect {|
      (EBinOp (Sub, (EInt 10), (EBinOp (Div, (EInt 8), (EInt 2))))) |}]
;;

let%expect_test "parsing expr: parentheses with multiplication" =
  parse_test "(1 + 2) * 3";
  [%expect {|
      (EBinOp (Mul, (EBinOp (Add, (EInt 1), (EInt 2))), (EInt 3))) |}]
;;

let%expect_test "parsing expr: nested parentheses" =
  parse_test "((1 + 2) * (3 - 4)) / 5";
  [%expect
    {|
    (EBinOp (Div,
       (EBinOp (Mul, (EBinOp (Add, (EInt 1), (EInt 2))),
          (EBinOp (Sub, (EInt 3), (EInt 4))))),
       (EInt 5))) |}]
;;

let%expect_test "parsing expr: equality" =
  parse_test "1 + 2 = 3";
  [%expect {|
      (EBinOp (Eq, (EBinOp (Add, (EInt 1), (EInt 2))), (EInt 3))) |}]
;;

let%expect_test "parsing expr: inequality" =
  parse_test "4 <> 5";
  [%expect {| (EBinOp (Neq, (EInt 4), (EInt 5))) |}]
;;

let%expect_test "parsing expr: less than" =
  parse_test "2 < 3";
  [%expect {| (EBinOp (Lt, (EInt 2), (EInt 3))) |}]
;;

let%expect_test "parsing expr: greater than or equal" =
  parse_test "5 >= 4";
  [%expect {| (EBinOp (Ge, (EInt 5), (EInt 4))) |}]
;;

let%expect_test "parsing expr: mixed arithmetic and comparison" =
  parse_test "1 + 2 * 3 > 5";
  [%expect
    {|
      (EBinOp (Gt, (EBinOp (Add, (EInt 1), (EBinOp (Mul, (EInt 2), (EInt 3))))),
         (EInt 5))) |}]
;;

let%expect_test "parsing expr: arithmetic and logical and" =
  parse_test "1 < 2 && 3 > 2";
  [%expect
    {|
      (EBinOp (And, (EBinOp (Lt, (EInt 1), (EInt 2))),
         (EBinOp (Gt, (EInt 3), (EInt 2))))) |}]
;;

let%expect_test "parsing expr: logical or and and" =
  parse_test "true || false && true";
  [%expect
    {|
      (EBinOp (Or, (EBool true), (EBinOp (And, (EBool false), (EBool true))))) |}]
;;

let%expect_test "parsing expr: logical and with comparison" =
  parse_test "1 + 2 * 3 <= 7 && 4 > 2";
  [%expect
    {|
      (EBinOp (And,
         (EBinOp (Le, (EBinOp (Add, (EInt 1), (EBinOp (Mul, (EInt 2), (EInt 3))))),
            (EInt 7))),
         (EBinOp (Gt, (EInt 4), (EInt 2))))) |}]
;;

let%expect_test "parsing expr: logical or with comparison" =
  parse_test "true || 1 + 2 * 3 < 7";
  [%expect
    {|
      (EBinOp (Or, (EBool true),
         (EBinOp (Lt, (EBinOp (Add, (EInt 1), (EBinOp (Mul, (EInt 2), (EInt 3))))),
            (EInt 7)))
         )) |}]
;;

(* Fun test *)
let%expect_test "parsing expr: function" =
  parse_test "fun x -> x + 1";
  [%expect
    {|
       (EFun ((PVar ("x", None)), (EBinOp (Add, (EVar ("x", None)), (EInt 1))))) |}]
;;

let%expect_test "parsing expr: function with multiple arguments" =
  parse_test "fun x y z -> x + y * z";
  [%expect
    {|
    (EFun ((PVar ("x", None)),
       (EFun ((PVar ("y", None)),
          (EFun ((PVar ("z", None)),
             (EBinOp (Add, (EVar ("x", None)),
                (EBinOp (Mul, (EVar ("y", None)), (EVar ("z", None))))))
             ))
          ))
       )) |}]
;;

let%expect_test "parsing expr: function in parentheses" =
  parse_test "(fun x -> x + 1)";
  [%expect
    {|
      (EFun ((PVar ("x", None)), (EBinOp (Add, (EVar ("x", None)), (EInt 1))))) |}]
;;

let%expect_test "parsing expr: function returning another function" =
  parse_test "fun x -> fun y -> x + y";
  [%expect
    {|
    (EFun ((PVar ("x", None)),
       (EFun ((PVar ("y", None)),
          (EBinOp (Add, (EVar ("x", None)), (EVar ("y", None))))))
       )) |}]
;;

(* App test *)
let%expect_test "parsing expr: application" =
  parse_test "x y";
  [%expect {| (EApp ((EVar ("x", None)), (EVar ("y", None)))) |}]
;;

let%expect_test "parsing expr: application with parentheses" =
  parse_test "(x) (y)";
  [%expect {| (EApp ((EVar ("x", None)), (EVar ("y", None)))) |}]
;;

let%expect_test "parsing expr: nested application left associative" =
  parse_test "x y z";
  [%expect
    {| (EApp ((EApp ((EVar ("x", None)), (EVar ("y", None)))), (EVar ("z", None)))) |}]
;;

let%expect_test "parsing expr: nested application with parentheses" =
  parse_test "x (y z)";
  [%expect
    {| (EApp ((EVar ("x", None)), (EApp ((EVar ("y", None)), (EVar ("z", None)))))) |}]
;;

(* Let test *)
let%expect_test "parsing expr: let variable" =
  parse_test "let x = 5 in x";
  [%expect
    {|
      (ELet (NonRecursive, [((PVar ("x", None)), (EInt 5))],
         (Some (EVar ("x", None))))) |}]
;;

let%expect_test "parsing expr: let variable" =
  parse_test "let x = 5";
  [%expect {| (ELet (NonRecursive, [((PVar ("x", None)), (EInt 5))], None)) |}]
;;

let%expect_test "parsing expr: let typed variable" =
  parse_test "let (x : int) = x + 1 in x";
  [%expect
    {|
       (ELet (NonRecursive,
          [((PVar ("x", (Some TInt))), (EBinOp (Add, (EVar ("x", None)), (EInt 1))))
            ],
          (Some (EVar ("x", None))))) |}]
;;

let%expect_test "parsing expr: let function with multiple patterns" =
  parse_test "let f x y = 5 in f x y";
  [%expect
    {|
       (ELet (NonRecursive,
          [((PVar ("f", None)),
            (EFun ((PVar ("x", None)), (EFun ((PVar ("y", None)), (EInt 5))))))],
          (Some (EApp ((EApp ((EVar ("f", None)), (EVar ("x", None)))),
                   (EVar ("y", None)))))
          )) |}]
;;

let%expect_test "parsing expr: let function with multiple typed patterns" =
  parse_test "let f x (y : int) = x + y in f";
  [%expect
    {|
       (ELet (NonRecursive,
          [((PVar ("f", None)),
            (EFun ((PVar ("x", None)),
               (EFun ((PVar ("y", (Some TInt))),
                  (EBinOp (Add, (EVar ("x", None)), (EVar ("y", None))))))
               )))
            ],
          (Some (EVar ("f", None))))) |}]
;;

let%expect_test "parsing expr: let rec with multiple patterns" =
  parse_test "let rec f (x: string) y = x + y in f";
  [%expect
    {|
       (ELet (Recursive,
          [((PVar ("f", None)),
            (EFun ((PVar ("x", (Some TString))),
               (EFun ((PVar ("y", None)),
                  (EBinOp (Add, (EVar ("x", None)), (EVar ("y", None))))))
               )))
            ],
          (Some (EVar ("f", None))))) |}]
;;

let%expect_test "parsing expr: mutual recursion with two functions" =
  parse_test "let rec even x = x + 1 and odd x = x + 2 in even 4";
  [%expect
    {|
    (ELet (Recursive,
       [((PVar ("even", None)),
         (EFun ((PVar ("x", None)), (EBinOp (Add, (EVar ("x", None)), (EInt 1)))
            )));
         ((PVar ("odd", None)),
          (EFun ((PVar ("x", None)), (EBinOp (Add, (EVar ("x", None)), (EInt 2)))
             )))
         ],
       (Some (EApp ((EVar ("even", None)), (EInt 4)))))) |}]
;;

let%expect_test "parsing expr: mutual recursion with three functions" =
  parse_test "let rec f x = g x and g x = h x and h x = x in f 5";
  [%expect
    {|
    (ELet (Recursive,
       [((PVar ("f", None)),
         (EFun ((PVar ("x", None)),
            (EApp ((EVar ("g", None)), (EVar ("x", None)))))));
         ((PVar ("g", None)),
          (EFun ((PVar ("x", None)),
             (EApp ((EVar ("h", None)), (EVar ("x", None)))))));
         ((PVar ("h", None)), (EFun ((PVar ("x", None)), (EVar ("x", None)))))],
       (Some (EApp ((EVar ("f", None)), (EInt 5)))))) |}]
;;

(* Tuple test *)
let%expect_test "parsing expr: tuple" =
  parse_test "(1, 2, 3)";
  [%expect {| (ETuple [(EInt 1); (EInt 2); (EInt 3)]) |}]
;;

let%expect_test "parsing expr: nested tuple" =
  parse_test "((1, 2), (3, 4))";
  [%expect
    {|
    (ETuple [(ETuple [(EInt 1); (EInt 2)]); (ETuple [(EInt 3); (EInt 4)])]) |}]
;;

let%expect_test "parsing expr: tuple with different types" =
  parse_test "(1, true, \"hello\")";
  [%expect {| (ETuple [(EInt 1); (EBool true); (EString "hello")]) |}]
;;

(* List test *)
let%expect_test "parsing expr: empty list" =
  parse_test "[]";
  [%expect {| (EList []) |}]
;;

let%expect_test "parsing expr: list" =
  parse_test "[1; 2; 3]";
  [%expect {| (EList [(EInt 1); (EInt 2); (EInt 3)]) |}]
;;

let%expect_test "parsing expr: nested list" =
  parse_test "[[1; 2]; [3; 4]]";
  [%expect {|
    (EList [(EList [(EInt 1); (EInt 2)]); (EList [(EInt 3); (EInt 4)])]) |}]
;;

let%expect_test "parsing expr: list with different types" =
  parse_test "[1; true; \"hello\"]";
  [%expect {| (EList [(EInt 1); (EBool true); (EString "hello")]) |}]
;;

(* Some test *)
let%expect_test "parsing expr: Some with integer literal" =
  parse_test "Some 10";
  [%expect {| (ESome (EInt 10)) |}]
;;

let%expect_test "parsing expr: Some with variable" =
  parse_test "Some x";
  [%expect {| (ESome (EVar ("x", None))) |}]
;;

(* None test *)
let%expect_test "parsing expr: None" =
  parse_test "None";
  [%expect {| ENone |}]
;;

(* Eif test *)
let%expect_test "parsing expr: if-then-else true case" =
  parse_test "if true then 1 else 0";
  [%expect {| (EIf ((EBool true), (EInt 1), (EInt 0))) |}]
;;

let%expect_test "parsing expr: if-then-else false case" =
  parse_test "if false then 42 else 99";
  [%expect {| (EIf ((EBool false), (EInt 42), (EInt 99))) |}]
;;

let%expect_test "parsing expr: nested if-then-else" =
  parse_test "if true then (if false then 1 else 2) else 3";
  [%expect
    {| (EIf ((EBool true), (EIf ((EBool false), (EInt 1), (EInt 2))), (EInt 3))) |}]
;;

let%expect_test "parsing expr: if-then-else with expressions" =
  parse_test "if x > 0 then x + 1 else x - 1";
  [%expect
    {|
      (EIf ((EBinOp (Gt, (EVar ("x", None)), (EInt 0))),
         (EBinOp (Add, (EVar ("x", None)), (EInt 1))),
         (EBinOp (Sub, (EVar ("x", None)), (EInt 1))))) |}]
;;
