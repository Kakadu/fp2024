[@@@ocaml.text "/*"]

(** Copyright 2024-2025, Damir Yunusov and Ilhom Kombaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Angstrom

let parse_with p str = parse_string ~consume:All p str
let test_ok p str result = parse_with p str = Result.Ok result
let test_fail p str error = parse_with p str = Result.error error

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
  let* sign = choice [ token "-"; token "+"; token "" ] in
  let* first_digit =
    take 1
    >>= fun i ->
    match int_of_string_opt i with
    | Some x -> return (string_of_int x)
    | None -> fail "Error while parsing int"
  in
  let+ digits =
    take_while (function
      | '0' .. '9' | '_' -> true
      | _ -> false)
  in
  Pconst_int (int_of_string (sign ^ first_digit ^ digits))
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

module Const_tests = struct
  let ( === ) = test_ok pexpr_const
  let ( <=> ) = test_fail pexpr_const
  let test_fail_const = test_fail pexpr_const
  let%test _ = "1" === Pexp_constant (Pconst_int 1)
  let%test _ = "1_000" === Pexp_constant (Pconst_int 1_000)
  let%test _ = "1___1" === Pexp_constant (Pconst_int 1___1)
  let%test _ = "1_000_000" === Pexp_constant (Pconst_int 1_000_000)
  let%test _ = "_" <=> ": Error while parsing literal"
  let%test _ = {| "Homka" |} === Pexp_constant (Pconst_string "Homka")
  let%test _ = "true" === Pexp_constant (Pconst_boolean true)
end

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
let%test _ = parse_with pexp_ident "homka  " = Result.Ok (Pexp_ident (Id "homka"))

let%test _ =
  parse_with (pexp_ident <* pexp_ident) "damir homka"
  = Result.Ok (Pexp_ident (Id "damir"))
;;

let%test _ =
  parse_with (pexp_ident *> pexp_ident) "damir homka" = Ok (Pexp_ident (Id "homka"))
;;

let%test _ = parse_with pexp_ident "1" = Result.Error ": satisfy: '1'"

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
  token "[" *> sep_by (token ";") expr <* token "]" >>| fun x -> Pexp_tuple x
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

let p_pattern_test s r = parse_string ~consume:All p_pattern s = Result.Ok r

module Pattern_tests = struct
  let ( === ) = p_pattern_test
  let%test _ = "_" === Ppat_any
  let%test _ = "homka" === Ppat_var "homka"
  let%test _ = "122" === Ppat_constant (Pconst_int 122)
  let%test _ = "_, _" === Ppat_tuple [ Ppat_any; Ppat_any ]
  let%test _ = "_, _, (_)" === Ppat_tuple [ Ppat_any; Ppat_any; Ppat_any ]
  let%test _ = "_, (_, _)" === Ppat_tuple [ Ppat_any; Ppat_tuple [ Ppat_any; Ppat_any ] ]

  (* FIX THIS, ADD char *)
  let%test _ = "\"a\" .. \"b\"" === Ppat_interval (Pconst_string "a", Pconst_string "b")
end

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
      (if List.length xs = 0
       then expr
       else List.fold_right (fun f p -> Pexp_fun (f, p)) xs expr)
  }
;;

let p_let_in expr =
  let* rec_flag = token "let" *> p_rec_flag in
  let* value_bindings = many1 (p_value_binding expr) in
  let+ expr = token "in" *> expr in
  Pexp_let (rec_flag, value_bindings, expr)
;;

let token_or xs : string t =
  let token_functions = List.map (fun s -> token s) xs in
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

let p_expr_test s r = parse_string ~consume:All p_expr s = Result.Ok r
let ( === ) = p_expr_test
let%test "const" = "1" === Pexp_constant (Pconst_int 1)
let%test "const" = "(1)" === Pexp_constant (Pconst_int 1)
let%test "const" = "((((homka))))" === Pexp_ident (Id "homka")
let%test "fun" = "fun x -> x" === Pexp_fun (Ppat_var "x", Pexp_ident (Id "x"))

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

let pp e =
  match e with
  | Ok e -> print_string (show_structure_item e)
  | Error str -> print_string str
;;

let parse str = parse_string ~consume:All p_structure str
let parse_prefix str = parse_string ~consume:Prefix p_structure str

(* mult tests *)
let%expect_test "mul_div_1" =
  pp @@ parse "2 * 2";
  [%expect
    {|
    (Pstr_eval
       (Pexp_apply ((Pexp_ident (Id "*")),
          [(Pexp_constant (Pconst_int 2)); (Pexp_constant (Pconst_int 2))]))) |}]
;;

let%expect_test "mul_div_2" =
  pp @@ parse "2 * ((2 * (124 * homka))) * (((2 * 1)))";
  [%expect
    {|
    (Pstr_eval
       (Pexp_apply ((Pexp_ident (Id "*")),
          [(Pexp_apply ((Pexp_ident (Id "*")),
              [(Pexp_constant (Pconst_int 2));
                (Pexp_apply ((Pexp_ident (Id "*")),
                   [(Pexp_constant (Pconst_int 2));
                     (Pexp_apply ((Pexp_ident (Id "*")),
                        [(Pexp_constant (Pconst_int 124));
                          (Pexp_ident (Id "homka"))]
                        ))
                     ]
                   ))
                ]
              ));
            (Pexp_apply ((Pexp_ident (Id "*")),
               [(Pexp_constant (Pconst_int 2)); (Pexp_constant (Pconst_int 1))]))
            ]
          ))) |}]
;;

let%expect_test "mul_div_3" =
  pp @@ parse "2 / 2";
  [%expect
    {|
    (Pstr_eval
       (Pexp_apply ((Pexp_ident (Id "/")),
          [(Pexp_constant (Pconst_int 2)); (Pexp_constant (Pconst_int 2))]))) |}]
;;

let%expect_test "mul_div_4" =
  pp @@ parse "2 * ((2 / (124 / homka))) * (1) * (2 / 2) * (((2 * 1)))";
  [%expect
    {|
    (Pstr_eval
       (Pexp_apply ((Pexp_ident (Id "*")),
          [(Pexp_apply ((Pexp_ident (Id "*")),
              [(Pexp_apply ((Pexp_ident (Id "*")),
                  [(Pexp_apply ((Pexp_ident (Id "*")),
                      [(Pexp_constant (Pconst_int 2));
                        (Pexp_apply ((Pexp_ident (Id "/")),
                           [(Pexp_constant (Pconst_int 2));
                             (Pexp_apply ((Pexp_ident (Id "/")),
                                [(Pexp_constant (Pconst_int 124));
                                  (Pexp_ident (Id "homka"))]
                                ))
                             ]
                           ))
                        ]
                      ));
                    (Pexp_constant (Pconst_int 1))]
                  ));
                (Pexp_apply ((Pexp_ident (Id "/")),
                   [(Pexp_constant (Pconst_int 2));
                     (Pexp_constant (Pconst_int 2))]
                   ))
                ]
              ));
            (Pexp_apply ((Pexp_ident (Id "*")),
               [(Pexp_constant (Pconst_int 2)); (Pexp_constant (Pconst_int 1))]))
            ]
          ))) |}]
;;

let%expect_test "fun 1" =
  pp @@ parse "fun x -> 5";
  [%expect
    {|
    (Pstr_eval (Pexp_fun ((Ppat_var "x"), (Pexp_constant (Pconst_int 5))))) |}]
;;

let%expect_test "fun 2" =
  pp @@ parse "fun x -> fun y -> fun z -> 5";
  [%expect
    {|
    (Pstr_eval
       (Pexp_fun ((Ppat_var "x"),
          (Pexp_fun ((Ppat_var "y"),
             (Pexp_fun ((Ppat_var "z"), (Pexp_constant (Pconst_int 5))))))
          ))) |}]
;;

let%expect_test "fun 3" =
  pp @@ parse "fun x y z -> 5";
  [%expect
    {|
    (Pstr_eval
       (Pexp_fun ((Ppat_var "x"),
          (Pexp_fun ((Ppat_var "y"),
             (Pexp_fun ((Ppat_var "z"), (Pexp_constant (Pconst_int 5))))))
          ))) |}]
;;

let%expect_test "If then else" =
  pp @@ parse "if x then y else z";
  [%expect
    {|
    (Pstr_eval
       (Pexp_ifthenelse ((Pexp_ident (Id "x")), (Pexp_ident (Id "y")),
          (Some (Pexp_ident (Id "z")))))) |}]
;;

let%expect_test "If then else without else" =
  pp @@ parse "if x then y";
  [%expect
    {|
    (Pstr_eval
       (Pexp_ifthenelse ((Pexp_ident (Id "x")), (Pexp_ident (Id "y")), None))) |}]
;;

let%expect_test "If then else with inner ifelse" =
  pp @@ parse "if x then if y then z";
  [%expect
    {|
    (Pstr_eval
       (Pexp_ifthenelse ((Pexp_ident (Id "x")),
          (Pexp_ifthenelse ((Pexp_ident (Id "y")), (Pexp_ident (Id "z")), None)),
          None))) |}]
;;

let%expect_test "If then else mult" =
  pp @@ parse "2 * if true then 2 else 1";
  [%expect
    {|
    (Pstr_eval
       (Pexp_apply ((Pexp_ident (Id "*")),
          [(Pexp_constant (Pconst_int 2));
            (Pexp_ifthenelse ((Pexp_constant (Pconst_boolean true)),
               (Pexp_constant (Pconst_int 2)),
               (Some (Pexp_constant (Pconst_int 1)))))
            ]
          ))) |}]
;;

let%expect_test "fun with if else" =
  pp @@ parse "fun x y -> if x then y";
  [%expect
    {|
    (Pstr_eval
       (Pexp_fun ((Ppat_var "x"),
          (Pexp_fun ((Ppat_var "y"),
             (Pexp_ifthenelse ((Pexp_ident (Id "x")), (Pexp_ident (Id "y")), None
                ))
             ))
          ))) |}]
;;

let%expect_test "fun with if else 2" =
  pp @@ parse "fun x -> fun y -> if x then y else x";
  [%expect
    {|
    (Pstr_eval
       (Pexp_fun ((Ppat_var "x"),
          (Pexp_fun ((Ppat_var "y"),
             (Pexp_ifthenelse ((Pexp_ident (Id "x")), (Pexp_ident (Id "y")),
                (Some (Pexp_ident (Id "x")))))
             ))
          ))) |}]
;;

let%expect_test "apply" =
  pp @@ parse "f y z";
  [%expect
    {|
    (Pstr_eval
       (Pexp_apply ((Pexp_ident (Id "f")),
          [(Pexp_ident (Id "y")); (Pexp_ident (Id "z"))]))) |}]
;;

let%expect_test "let in" =
  pp @@ parse "let homka = 5 in homka";
  [%expect
    {|
    (Pstr_eval
       (Pexp_let (NonRecursive,
          [{ pvb_pat = (Ppat_var "homka");
             pvb_expr = (Pexp_constant (Pconst_int 5)) }
            ],
          (Pexp_ident (Id "homka"))))) |}]
;;

let%expect_test "let in with fun" =
  pp @@ parse "let homka = fun x -> x + 2 in homka";
  [%expect
    {|
    (Pstr_eval
       (Pexp_let (NonRecursive,
          [{ pvb_pat = (Ppat_var "homka");
             pvb_expr =
             (Pexp_fun ((Ppat_var "x"),
                (Pexp_apply ((Pexp_ident (Id "+")),
                   [(Pexp_ident (Id "x")); (Pexp_constant (Pconst_int 2))]))
                ))
             }
            ],
          (Pexp_ident (Id "homka"))))) |}]
;;

let%expect_test "factorial" =
  pp @@ parse "let rec factorial n = if n = 0 then 1 else n * factorial (n - 1)";
  [%expect
    {|
    (Pstr_value (Recursive,
       [{ pvb_pat = (Ppat_var "factorial");
          pvb_expr =
          (Pexp_fun ((Ppat_var "n"),
             (Pexp_ifthenelse (
                (Pexp_apply ((Pexp_ident (Id "=")),
                   [(Pexp_ident (Id "n")); (Pexp_constant (Pconst_int 0))])),
                (Pexp_constant (Pconst_int 1)),
                (Some (Pexp_apply (
                         (Pexp_apply ((Pexp_ident (Id "*")),
                            [(Pexp_ident (Id "n")); (Pexp_ident (Id "factorial"))
                              ]
                            )),
                         [(Pexp_apply ((Pexp_ident (Id "-")),
                             [(Pexp_ident (Id "n"));
                               (Pexp_constant (Pconst_int 1))]
                             ))
                           ]
                         )))
                ))
             ))
          }
         ]
       )) |}]
;;
