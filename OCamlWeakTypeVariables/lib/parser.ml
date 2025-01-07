[@@@ocaml.text "/*"]

(** Copyright 2024-2025, Damir Yunusov and Ilhom Kombaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Angstrom

let parse_with p str = parse_string ~consume:All p str
let test_ok p str result = parse_with p str = Result.Ok result
let test_fail p str error = parse_with p str = Result.error error

let chain1l expr op =
  let rec go acc = lift2 (fun f x -> f acc x) op expr >>= go <|> return acc in
  expr >>= go
;;

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
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
let ( === ) = test_ok pexpr_const

(** let (!==) = test_fail pexpr_const doen't work. Bug in ocaml *)
let test_fail_const = test_fail pexpr_const

let%test _ = "1" === Pexp_constant (Pconst_int 1)
let%test _ = "1_000" === Pexp_constant (Pconst_int 1_000)
let%test _ = "1___1" === Pexp_constant (Pconst_int 1___1)
let%test _ = "1_000_000" === Pexp_constant (Pconst_int 1_000_000)
let%test _ = test_fail_const "_" ": Error while parsing literal"
let%test _ = "\"HomkaChmo\"" === Pexp_constant (Pconst_string "HomkaChmo")
let%test _ = "\"HomkaChmo\"" === Pexp_constant (Pconst_string "Homka")
let%test _ = "\"HomkaChmo\"" === Pexp_constant (Pconst_string "HomkaChmo")
let%test _ = "true" === Pexp_constant (Pconst_boolean true)

let p_var =
  let* first =
    ws
    *> satisfy (function
      | 'a' .. 'z' | '_' -> true
      | _ -> false)
  in
  let+ rest =
    take_while (function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
      | _ -> false)
    <* ws
  in
  Char.escaped first ^ rest
;;

(* TODO: readable error message *)
let p_id : id t =
  let+ var = p_var in
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
  let pat_var = p_var >>| fun var -> Ppat_var var in
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
let ( === ) = p_pattern_test
let%test "pattern" = "_" === Ppat_any
let%test "pattern" = "homka" === Ppat_var "homka"
let%test "pattern" = "122" === Ppat_constant (Pconst_int 122)
let%test "pattern" = "_, _" === Ppat_tuple [ Ppat_any; Ppat_any ]
let%test "pattern" = "_, _, (_)" === Ppat_tuple [ Ppat_any; Ppat_any; Ppat_any ]

let%test "pattern" =
  "_, _, (_, _)" === Ppat_tuple [ Ppat_any; Ppat_any; Ppat_tuple [ Ppat_any; Ppat_any ] ]
;;

(* FIX THIS, ADD char *)
let%test "pattern" =
  "\"a\" .. \"b\"" === Ppat_interval (Pconst_string "a", Pconst_string "b")
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

let p_expr =
  fix (fun expr ->
    let expr_const = choice [ parens expr; pexpr_const; pexp_ident; p_tuple expr ] in
    let expr_mul_div = p_binop (token "*" <|> token "/") expr_const <|> expr_const in
    let expr_add_sub = p_binop (token "+" <|> token "-") expr_mul_div <|> expr_mul_div in
    let expr_fun = p_fun expr <|> expr_add_sub in
    let expr_branch = p_branch expr <|> expr_fun in
    expr_branch)
;;

let p_expr_test s r = parse_string ~consume:All p_expr s = Result.Ok r
let ( === ) = p_expr_test

(* const tests *)
let%test "const" = p_expr_test "1" @@ Pexp_constant (Pconst_int 1)
let%test "const" = p_expr_test "(1)" @@ Pexp_constant (Pconst_int 1)
let%test "const" = p_expr_test "((((homka))))" @@ Pexp_ident (Id "homka")
let%test "fun" = "fun x -> x" === Pexp_fun (Ppat_var "x", Pexp_ident (Id "x"))

(* let%test "fun" = "fun x y z -> x" === Pexp_fun (Ppat_var "x", Pexp_ident (Id "x")) *)
let%test "fun" = "fun x -> x" === Pexp_fun (Ppat_var "x", Pexp_ident (Id "x"))

let pp e =
  match e with
  | Ok e -> print_string (show_expression e)
  | Error str -> print_string str
;;

let parse str = parse_string ~consume:All p_expr str

(* mult tests *)
let%expect_test "mul_div_1" =
  pp @@ parse "2 * 2";
  [%expect
    {|
    (Pexp_apply ((Pexp_ident (Id "*")),
       [(Pexp_constant (Pconst_int 2)); (Pexp_constant (Pconst_int 2))])) |}]
;;

let%expect_test "mul_div_2" =
  pp @@ parse "2 * ((2 * (124 * homka))) * (((2 * 1)))";
  [%expect
    {|
    (Pexp_apply ((Pexp_ident (Id "*")),
       [(Pexp_apply ((Pexp_ident (Id "*")),
           [(Pexp_constant (Pconst_int 2));
             (Pexp_apply ((Pexp_ident (Id "*")),
                [(Pexp_constant (Pconst_int 2));
                  (Pexp_apply ((Pexp_ident (Id "*")),
                     [(Pexp_constant (Pconst_int 124)); (Pexp_ident (Id "homka"))
                       ]
                     ))
                  ]
                ))
             ]
           ));
         (Pexp_apply ((Pexp_ident (Id "*")),
            [(Pexp_constant (Pconst_int 2)); (Pexp_constant (Pconst_int 1))]))
         ]
       )) |}]
;;

let%expect_test "mul_div_3" =
  pp @@ parse "2 / 2";
  [%expect
    {|
    (Pexp_apply ((Pexp_ident (Id "/")),
       [(Pexp_constant (Pconst_int 2)); (Pexp_constant (Pconst_int 2))])) |}]
;;

let%expect_test "mul_div_4" =
  pp @@ parse "2 * ((2 / (124 / homka))) * (1) * (2 / 2) * (((2 * 1)))";
  [%expect
    {|
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
                [(Pexp_constant (Pconst_int 2)); (Pexp_constant (Pconst_int 2))]
                ))
             ]
           ));
         (Pexp_apply ((Pexp_ident (Id "*")),
            [(Pexp_constant (Pconst_int 2)); (Pexp_constant (Pconst_int 1))]))
         ]
       )) |}]
;;

let%expect_test "fun 1" =
  pp @@ parse "fun x -> 5";
  [%expect {|
    (Pexp_fun ((Ppat_var "x"), (Pexp_constant (Pconst_int 5)))) |}]
;;

let%expect_test "fun 2" =
  pp @@ parse "fun x -> fun y -> fun z -> 5";
  [%expect
    {|
    (Pexp_fun ((Ppat_var "x"),
       (Pexp_fun ((Ppat_var "y"),
          (Pexp_fun ((Ppat_var "z"), (Pexp_constant (Pconst_int 5))))))
       )) |}]
;;

let%expect_test "fun 3" =
  pp @@ parse "fun x y z -> 5";
  [%expect
    {|
    (Pexp_fun ((Ppat_var "x"),
       (Pexp_fun ((Ppat_var "y"),
          (Pexp_fun ((Ppat_var "z"), (Pexp_constant (Pconst_int 5))))))
       )) |}]
;;

let%expect_test "If then else" =
  pp @@ parse "if x then y else z";
  [%expect
    {|
    (Pexp_ifthenelse ((Pexp_ident (Id "x")), (Pexp_ident (Id "y")),
       (Some (Pexp_ident (Id "z"))))) |}]
;;

let%expect_test "If then else without else" =
  pp @@ parse "if x then y";
  [%expect
    {|
    (Pexp_ifthenelse ((Pexp_ident (Id "x")), (Pexp_ident (Id "y")), None)) |}]
;;

let%expect_test "If then else with inner ifelse" =
  pp @@ parse "if x then if y then z";
  [%expect
    {|
    (Pexp_ifthenelse ((Pexp_ident (Id "x")),
       (Pexp_ifthenelse ((Pexp_ident (Id "y")), (Pexp_ident (Id "z")), None)),
       None)) |}]
;;

let%expect_test "fun with if else" =
  pp @@ parse "fun x y -> if x then y else x";
  [%expect
    {|
    (Pexp_fun ((Ppat_var "x"),
       (Pexp_fun ((Ppat_var "y"),
          (Pexp_ifthenelse ((Pexp_ident (Id "x")), (Pexp_ident (Id "y")),
             (Some (Pexp_ident (Id "x")))))
          ))
       )) |}]
;;

let%expect_test "fun with if else 2" =
  pp @@ parse "fun x -> fun y -> if x then y else x";
  [%expect
    {|
    (Pexp_fun ((Ppat_var "x"),
       (Pexp_fun ((Ppat_var "y"),
          (Pexp_ifthenelse ((Pexp_ident (Id "x")), (Pexp_ident (Id "y")),
             (Some (Pexp_ident (Id "x")))))
          ))
       )) |}]
;;
