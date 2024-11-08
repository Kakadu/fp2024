(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Ast
open Integer

let ws1 =
  many1
  @@ satisfy
  @@ function
  | ' ' | '\t' -> true
  | _ -> false
;;

let ws = option [] ws1
let ( >>>= ) p f = ws *> p >>= f
let ( let** ) = ( >>>= )
let ( <**> ) f p = f <*> ws *> p
let ( **> ) p f = ws *> p *> (ws *> f)
let etp = (None : tp option) (* remove later (tp parser is required) *)
let parens p = char '(' *> ws *> p <* (ws <* char ')')

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let nonnegative_integer =
  let* y = take_while1 is_digit in
  match Nonnegative_integer.of_string_opt y with
  | None -> fail ""
  | Some x -> return x
;;

let const =
  choice
    [ (let+ x = nonnegative_integer in
       Integer x)
    ; string "()" *> return Unit
    ; string "True" *> return (Bool true)
    ; string "False" *> return (Bool false)
    ]
;;

let%test "const_valid_num" =
  parse_string ~consume:Prefix const "123"
  = Result.Ok (Integer (Nonnegative_integer.of_int 123))
;;

let%test "const_invalid_num" =
  parse_string ~consume:Prefix const "123ab"
  = Result.Ok (Integer (Nonnegative_integer.of_int 123))
;;

let%test "const_invalid_num_negative" =
  parse_string ~consume:Prefix const "-123" = Result.Error ": no more choices"
;;

let%test "const_valid_unit" = parse_string ~consume:Prefix const "()" = Result.Ok Unit

let%test "const_valid_true" =
  parse_string ~consume:Prefix const "True" = Result.Ok (Bool true)
;;

let%test "const_valid_false" =
  parse_string ~consume:Prefix const "False" = Result.Ok (Bool false)
;;

let%test "const_invalid" =
  parse_string ~consume:Prefix const "beb" = Result.Error ": no more choices"
;;

let is_char_suitable_for_ident c =
  is_digit c || is_alpha c || Char.equal '_' c || Char.equal '\'' c
;;

let is_char_suitable_for_oper = function
  | '&' | '|' | '+' | '-' | ':' | '*' | '=' | '^' | '/' | '\\' | '<' | '>' -> true
  | _ -> false
;;

let oper expected =
  let* parsed = take_while is_char_suitable_for_oper in
  if String.equal expected parsed then return expected else fail ""
;;

let ident =
  let keywords = [ "case"; "of"; "if"; "then"; "else"; "let"; "in"; "where" ] in
  (let* x =
     satisfy (function
       | 'a' .. 'z' -> true
       | _ -> false)
   in
   let* y = take_while is_char_suitable_for_ident in
   return (Printf.sprintf "%c%s" x y))
  <|> (let* x = satisfy (Char.equal '_') in
       let* y = take_while1 is_char_suitable_for_ident in
       return (Printf.sprintf "%c%s" x y))
  >>= fun identifier ->
  match List.find_opt (String.equal identifier) keywords with
  | None -> return (Ident identifier)
  | Some k -> fail (Printf.sprintf "keyword '%s' cannot be an identifier" k)
;;

let%test "ident_valid_starts_with_underline" =
  parse_string ~consume:Prefix ident "_123abc" = Result.Ok (Ident "_123abc")
;;

let%test "ident_invalid" =
  parse_string ~consume:Prefix ident "_" = Result.Error ": count_while1"
;;

let%test "ident_valid_'" =
  parse_string ~consume:Prefix ident "x'" = Result.Ok (Ident "x'")
;;

let%test "ident_invalid_keyword" =
  parse_string ~consume:Prefix ident "then"
  = Result.Error ": keyword 'then' cannot be an identifier"
;;

let prs_ln call str = parse_string ~consume:Prefix call str

let prs_and_prnt_ln call sh str =
  match prs_ln call str with
  | Ok v -> print_endline (sh v)
  | Error msg -> Printf.fprintf stderr "error: %s" msg
;;

let word req_word =
  let open String in
  if equal req_word empty
  then return empty
  else
    let* fst_smb = satisfy is_alpha in
    let* w = take_while is_char_suitable_for_ident in
    if equal (Printf.sprintf "%c%s" fst_smb w) req_word
    then return req_word
    else Printf.sprintf "couldn't parse word '%s'" req_word |> fail
;;

let%test "word_valid" =
  parse_string ~consume:Prefix (word "then") "then" = Result.Ok "then"
;;

let%test "word_invalid" =
  parse_string ~consume:Prefix (word "then") "thena"
  = Result.Error ": couldn't parse word 'then'"
;;

let tuple_or_parensed_item item tuple_cons item_cons =
  parens (sep_by1 (ws *> char ',' *> ws) item)
  >>= fun l ->
  match l with
  | hd :: [] -> item_cons hd
  | fs :: sn :: tl -> tuple_cons fs sn tl
  | [] -> fail "sep_by1 result can't be empty"
;;

let nothing f = string "Nothing" *> f

let tree item nul_cons node_cons =
  char '$' *> nul_cons
  <|> (parens (sep_by (ws *> char ';') item)
       >>= function
       | [ it1; it2; it3 ] -> node_cons it1 it2 it3
       | _ -> fail "cannot parse tree")
;;

let pnegation =
  oper "-" *> ws *> nonnegative_integer >>| fun a -> [], PConst (NegativePInteger a), etp
;;

let pat ptrn =
  choice
    [ (let* pt = const in
       return (PConst (OrdinaryPConst pt)))
    ; (let* pt = ident in
       return (PIdentificator pt))
    ; char '_' *> return PWildcard
    ; nothing (return (PMaybe Nothing))
    ; tree
        (ws *> (ptrn <|> pnegation))
        (return (PTree PNul))
        (fun d t1 t2 -> return (PTree (PNode (d, t1, t2))))
    ]
;;

let ptrn ptrn =
  choice
    [ (let* ident = ident in
       char '@' *> (ptrn >>= fun (idents, pat, tp) -> return (ident :: idents, pat, tp)))
    ; (let* pat = pat ptrn in
       (* let* tp = tp in *)
       return ([], pat, etp))
    ; tuple_or_parensed_item
        (ptrn <|> pnegation)
        (fun p1 p2 pp -> return ([], PTuple (p1, p2, pp), etp))
        (fun p -> return p)
    ]
;;

type unparanced_neg_handling =
  | Ban
  | Allow

let pattern unp_neg_h =
  ptrn (fix ptrn)
  <|>
  match unp_neg_h with
  | Ban -> fail ""
  | Allow -> pnegation
;;

let%test "pattern_valid_as" =
  parse_string ~consume:Prefix (pattern Allow) "adada@(   x   )"
  = Result.Ok ([ Ident "adada" ], PIdentificator (Ident "x"), None)
;;

let%test "pattern_valid_parens_oth" =
  parse_string ~consume:Prefix (pattern Allow) "(   x   )"
  = Result.Ok ([], PIdentificator (Ident "x"), None)
;;

let%test "pattern_valid_neg" =
  parse_string ~consume:Prefix (pattern Allow) "-1"
  = Result.Ok ([], PConst (NegativePInteger (Nonnegative_integer.of_int 1)), None)
;;

let%test "pattern_invalid_banned_neg" =
  parse_string ~consume:Prefix (pattern Ban) "-1" = Result.Error ": "
;;

let%test "pattern_valid_double_as" =
  parse_string ~consume:Prefix (pattern Allow) "a@b@2"
  = Result.Ok
      ( [ Ident "a"; Ident "b" ]
      , PConst (OrdinaryPConst (Integer (Nonnegative_integer.of_int 2)))
      , None )
;;

let%test "pattern_valid_with_parens" =
  parse_string ~consume:Prefix (pattern Allow) "(a@(b@(2)))"
  = Result.Ok
      ( [ Ident "a"; Ident "b" ]
      , PConst (OrdinaryPConst (Integer (Nonnegative_integer.of_int 2)))
      , None )
;;

let%expect_test "pattern_valid_tuple" =
  prs_and_prnt_ln (pattern Allow) show_pattern "(x, y,(x,y))";
  [%expect
    {|
      ([],
       (PTuple (([], (PIdentificator (Ident "x")), None),
          ([], (PIdentificator (Ident "y")), None),
          [([],
            (PTuple (([], (PIdentificator (Ident "x")), None),
               ([], (PIdentificator (Ident "y")), None), [])),
            None)]
          )),
       None) |}]
;;

let%expect_test "pattern_valid_tuple_labeled" =
  prs_and_prnt_ln (pattern Allow) show_pattern "a@(x, e@y,b@(x,y))";
  [%expect
    {|
      ([(Ident "a")],
       (PTuple (([], (PIdentificator (Ident "x")), None),
          ([(Ident "e")], (PIdentificator (Ident "y")), None),
          [([(Ident "b")],
            (PTuple (([], (PIdentificator (Ident "x")), None),
               ([], (PIdentificator (Ident "y")), None), [])),
            None)]
          )),
       None) |}]
;;

let%expect_test "pattern_invalid_tuple_labeled" =
  prs_and_prnt_ln (pattern Allow) show_pattern "(x, e@y,(x,y)@(x,y))";
  [%expect {|
      error: : |}]
;;

let%expect_test "pattern_valid_tree" =
  prs_and_prnt_ln (pattern Allow) show_pattern "(2; $; $)";
  [%expect
    {|
      ([],
       (PTree
          (PNode (([], (PConst (OrdinaryPConst (Integer 2))), None),
             ([], (PTree PNul), None), ([], (PTree PNul), None)))),
       None) |}]
;;

let%expect_test "pattern_invalid_tree" =
  prs_and_prnt_ln (pattern Allow) show_pattern "(2; $)";
  [%expect {|
      error: : |}]
;;

let bindingbody e sep =
  (sep
   **> let* ex = e in
       return (OrdBody ex))
  <|>
  let* ee_pairs =
    many1
      (oper "|"
       **> let* ex1 = e in
           sep
           **> let* ex2 = e in
               return (ex1, ex2))
  in
  match ee_pairs with
  | [] -> fail " many1 result can't be empty"
  | hd :: tl -> Guards (hd, tl) |> return
;;

let bnd e bnd =
  (let** ident = ident in
   (* let* ft = option None (functype <** char ';') in *)
   let** pt = pattern Ban in
   let* pts = many (ws *> pattern Ban) in
   return (fun bb where_binds -> FunBind ((ident, None), pt, pts, bb, where_binds)))
  <|> (let** pt = pattern Ban in
       return (fun bb where_binds -> VarsBind (pt, bb, where_binds)))
  <**> bindingbody e (oper "=")
  <**> option [] @@ (word "where" **> sep_by (ws *> char ';' *> ws) bnd)
;;

let binding e = fix (bnd e) |> bnd e

type assoc =
  | Left
  | Right
  | Non

let prios_list =
  [ None, [ (Right, oper "||", fun a b -> Binop (a, Or, b), etp) ]
  ; None, [ (Right, oper "&&", fun a b -> Binop (a, And, b), etp) ]
  ; ( None
    , [ (Non, oper "==", fun a b -> Binop (a, Equality, b), etp)
      ; (Non, oper "/=", fun a b -> Binop (a, Inequality, b), etp)
      ; (Non, oper ">=", fun a b -> Binop (a, EqualityOrGreater, b), etp)
      ; (Non, oper "<=", fun a b -> Binop (a, EqualityOrLess, b), etp)
      ; (Non, oper ">", fun a b -> Binop (a, Greater, b), etp)
      ; (Non, oper "<", fun a b -> Binop (a, Less, b), etp)
      ] )
  ; ( Some (oper "-", fun a -> Neg a, etp)
    , [ (Right, oper ":", fun a b -> Binop (a, Cons, b), etp)
      ; (Left, oper "+", fun a b -> Binop (a, Plus, b), etp)
      ; (Left, oper "-", fun a b -> Binop (a, Minus, b), etp)
      ] )
  ; ( None
    , [ (Left, oper "`div`", fun a b -> Binop (a, Divide, b), etp)
      ; (Left, oper "*", fun a b -> Binop (a, Multiply, b), etp)
      ; (Left, oper "`mod`", fun a b -> Binop (a, Mod, b), etp)
      ] )
  ; None, [ (Right, string "^", fun a b -> Binop (a, Pow, b), etp) ]
  ]
;;

let non_assoc_ops_seq_check l =
  List.fold_left
    (fun (prev_assoc, error_flag) (ass, _, _) ->
      ass, if error_flag || (prev_assoc == Non && ass == Non) then true else false)
    (Left, false)
    l
  |> snd
  |> fun error_flag ->
  if error_flag
  then fail "cannot mix two non-associative operators in the same infix expression"
  else return l
;;

let bo expr prios_list =
  let rec loop acc = function
    | [] -> acc
    | (Right, op, r) :: tl -> op acc (loop r tl)
    | ((Left | Non), op, r) :: tl -> loop (op acc r) tl
  in
  let rec helper = function
    | [] -> ws *> expr
    | hd :: tl ->
      return loop
      <**> helper tl
      <*> (choice
             (List.map
                (fun (ass, op, f) -> op **> helper tl >>= fun r -> return (ass, f, r))
                (snd hd))
           |> many
           >>= non_assoc_ops_seq_check)
      <|>
        (match fst hd with
        | Some (op, f) -> ws *> (op **> helper tl) >>= fun a -> return (f a)
        | _ -> fail "")
  in
  helper prios_list
;;

let const_e =
  let+ c = const in
  Const c, etp
;;

let ident_e =
  let+ i = ident in
  Identificator i, etp
;;

let if_then_else e =
  let+ cond = word "if" **> e
  and+ th_br = word "then" **> e
  and+ el_br = word "else" **> e in
  IfThenEsle (cond, th_br, el_br), etp
;;

let inner_bindings e =
  word "let"
  **> let+ bnd = binding e
      and+ bnds = many @@ (char ';' **> binding e)
      and+ ex = word "in" **> e in
      InnerBindings (bnd, bnds, ex), etp
;;

let just =
  word "Just"
  *> return
       ( Lambda
           ( ([], PIdentificator (Ident "X"), etp)
           , []
           , (OptionBld (Just (Identificator (Ident "X"), None)), etp) )
       , etp )
;;

let lambda e =
  oper "\\"
  *> let** pt = pattern Ban in
     let* pts = many (ws *> pattern Ban) in
     let* ex = string "->" **> e in
     return (Lambda (pt, pts, ex), etp)
;;

let tree_e e =
  tree
    e
    ((BinTreeBld Nul, etp) |> return)
    (fun ex1 ex2 ex3 -> return (BinTreeBld (Node (ex1, ex2, ex3)), etp))
;;

let case e =
  word "case"
  *> let** ex = e in
     word "of"
     **>
     let* br1, brs =
       sep_by1 (ws *> char ';' *> ws) (both (pattern Allow) (bindingbody e (string "->")))
       >>= function
       | [] -> fail "sep_by1 cant return empty list"
       | hd :: tl -> return (hd, tl)
     in
     return (Case (ex, br1, brs), etp)
;;

let tuple_or_parensed_item_e e =
  tuple_or_parensed_item
    e
    (fun ex1 ex2 exs -> return (TupleBld (ex1, ex2, exs), etp))
    (fun ex -> return ex)
;;

let other_expr e fa =
  choice
    [ const_e
    ; ident_e
    ; nothing (return (OptionBld Nothing, etp))
    ; just
    ; if_then_else e
    ; case e
    ; inner_bindings e
    ; lambda e
    ; tree_e e
    ; tuple_or_parensed_item_e e
    ]
  >>= fun ex -> fa ex e <|> return ex
;;

let binop e fa = bo (other_expr e fa) prios_list

let function_application ex e =
  let* r =
    many1
      (ws
       *> choice
            [ const_e
            ; ident_e
            ; just
            ; nothing (return (OptionBld Nothing, etp))
            ; tree_e e
            ; tuple_or_parensed_item_e e
            ])
  in
  match r with
  | [] -> fail "many1 result can't be empty"
  | hd :: tl -> (FunctionApply (ex, hd, tl), etp) |> return
;;

let e e =
  binop e function_application
  <|> other_expr e function_application
  >>= fun ex -> function_application ex e <|> return ex
;;

let expr = e (fix e)

let%expect_test "expr_const" =
  prs_and_prnt_ln expr show_expr "123456789012345678901234567890";
  [%expect {|
      ((Const (Integer 123456789012345678901234567890)), None) |}]
;;

let%expect_test "expr_prio" =
  prs_and_prnt_ln expr show_expr "(1 + 1)*2 > 1";
  [%expect
    {|
      ((Binop (
          ((Binop (
              ((Binop (((Const (Integer 1)), None), Plus,
                  ((Const (Integer 1)), None))),
               None),
              Multiply, ((Const (Integer 2)), None))),
           None),
          Greater, ((Const (Integer 1)), None))),
       None) |}]
;;

let%expect_test "expr_right_assoc" =
  prs_and_prnt_ln expr show_expr "2^3^4";
  [%expect
    {|
      ((Binop (((Const (Integer 2)), None), Pow,
          ((Binop (((Const (Integer 3)), None), Pow, ((Const (Integer 4)), None))),
           None)
          )),
       None) |}]
;;

let%expect_test "expr_with_Just" =
  prs_and_prnt_ln expr show_expr "Just 2 + 1";
  [%expect
    {|
      ((Binop (
          ((FunctionApply (
              ((Lambda (([], (PIdentificator (Ident "X")), None), [],
                  ((OptionBld (Just ((Identificator (Ident "X")), None))), None))),
               None),
              ((Const (Integer 2)), None), [])),
           None),
          Plus, ((Const (Integer 1)), None))),
       None) |}]
;;

let%expect_test "expr_with_func_apply" =
  prs_and_prnt_ln expr show_expr "f(x) g(2) + 1";
  [%expect
    {|
      ((Binop (
          ((FunctionApply (((Identificator (Ident "f")), None),
              ((Identificator (Ident "x")), None),
              [((Identificator (Ident "g")), None); ((Const (Integer 2)), None)])),
           None),
          Plus, ((Const (Integer 1)), None))),
       None) |}]
;;

let%expect_test "expr_with_func_apply_strange_but_valid1" =
  prs_and_prnt_ln expr show_expr "f 9a";
  [%expect
    {|
      ((FunctionApply (((Identificator (Ident "f")), None),
          ((Const (Integer 9)), None), [((Identificator (Ident "a")), None)])),
       None) |}]
;;

let%expect_test "expr_with_func_apply_strange_but_valid2" =
  prs_and_prnt_ln expr show_expr "f Just(1)";
  [%expect
    {|
      ((FunctionApply (((Identificator (Ident "f")), None),
          ((Lambda (([], (PIdentificator (Ident "X")), None), [],
              ((OptionBld (Just ((Identificator (Ident "X")), None))), None))),
           None),
          [((Const (Integer 1)), None)])),
       None) |}]
;;

let%expect_test "expr_with_non-assoc_op_simple" =
  prs_and_prnt_ln expr show_expr "x == y";
  [%expect
    {|
      ((Binop (((Identificator (Ident "x")), None), Equality,
          ((Identificator (Ident "y")), None))),
       None) |}]
;;

let%expect_test "expr_with_non-assoc_ops_invalid" =
  prs_and_prnt_ln expr show_expr "x == y + 1 >= z";
  [%expect {|
      ((Identificator (Ident "x")), None) |}]
;;

let%expect_test "expr_with_non-assoc_ops_valid" =
  prs_and_prnt_ln expr show_expr "x == y && z == z'";
  [%expect
    {|
      ((Binop (
          ((Binop (((Identificator (Ident "x")), None), Equality,
              ((Identificator (Ident "y")), None))),
           None),
          And,
          ((Binop (((Identificator (Ident "z")), None), Equality,
              ((Identificator (Ident "z'")), None))),
           None)
          )),
       None) |}]
;;

let%expect_test "expr_case_statement" =
  prs_and_prnt_ln expr show_expr "case x of 1 -> 1; _ -> 2 ";
  [%expect
    {|
      ((Case (((Identificator (Ident "x")), None),
          (([], (PConst (OrdinaryPConst (Integer 1))), None),
           (OrdBody ((Const (Integer 1)), None))),
          [(([], PWildcard, None), (OrdBody ((Const (Integer 2)), None)))])),
       None) |}]
;;

let%expect_test "expr_case_statement_with_guards" =
  prs_and_prnt_ln expr show_expr "case x of y | y > 10 -> 1 | otherwise -> 2;  _ -> 3 ";
  [%expect
    {|
      ((Case (((Identificator (Ident "x")), None),
          (([], (PIdentificator (Ident "y")), None),
           (Guards (
              (((Binop (((Identificator (Ident "y")), None), Greater,
                   ((Const (Integer 10)), None))),
                None),
               ((Const (Integer 1)), None)),
              [(((Identificator (Ident "otherwise")), None),
                ((Const (Integer 2)), None))]
              ))),
          [(([], PWildcard, None), (OrdBody ((Const (Integer 3)), None)))])),
       None) |}]
;;

let%expect_test "expr_tuple" =
  prs_and_prnt_ln expr show_expr " (x,1 , 2,(x, y))";
  [%expect
    {|
      ((TupleBld (((Identificator (Ident "x")), None), ((Const (Integer 1)), None),
          [((Const (Integer 2)), None);
            ((TupleBld (((Identificator (Ident "x")), None),
                ((Identificator (Ident "y")), None), [])),
             None)
            ]
          )),
       None) |}]
;;

let%expect_test "expr_lambda" =
  prs_and_prnt_ln expr show_expr " \\x -> x+1";
  [%expect
    {|
      ((Lambda (([], (PIdentificator (Ident "x")), None), [],
          ((Binop (((Identificator (Ident "x")), None), Plus,
              ((Const (Integer 1)), None))),
           None)
          )),
       None) |}]
;;

let%expect_test "expr_tree" =
  prs_and_prnt_ln expr show_expr "1 + (2; $; $)";
  [%expect
    {|
      ((Binop (((Const (Integer 1)), None), Plus,
          ((BinTreeBld
              (Node (((Const (Integer 2)), None), ((BinTreeBld Nul), None),
                 ((BinTreeBld Nul), None)))),
           None)
          )),
       None) |}]
;;

let%expect_test "expr_plus_neg" =
  prs_and_prnt_ln expr show_expr "1 + -1";
  [%expect {|
      ((Const (Integer 1)), None) |}]
;;

let%expect_test "expr_and_neg" =
  prs_and_prnt_ln expr show_expr "1 && -1";
  [%expect
    {|
      ((Binop (((Const (Integer 1)), None), And,
          ((Neg ((Const (Integer 1)), None)), None))),
       None) |}]
;;

let%expect_test "expr_tuple_neg" =
  prs_and_prnt_ln expr show_expr "(-1, 1)";
  [%expect
    {|
      ((TupleBld (((Neg ((Const (Integer 1)), None)), None),
          ((Const (Integer 1)), None), [])),
       None) |}]
;;

let%expect_test "expr_lambda neg" =
  prs_and_prnt_ln expr show_expr " \\ -1 -> 1";
  [%expect {|
      error: : no more choices |}]
;;

let%expect_test "expr_case_neg" =
  prs_and_prnt_ln expr show_expr "case-1of-1->1";
  [%expect
    {|
      ((Case (((Neg ((Const (Integer 1)), None)), None),
          (([], (PConst (NegativePInteger 1)), None),
           (OrdBody ((Const (Integer 1)), None))),
          [])),
       None) |}]
;;

let binding = binding expr

let%expect_test "var_binding_simple" =
  prs_and_prnt_ln binding show_binding "x = 1";
  [%expect
    {|
      (VarsBind (([], (PIdentificator (Ident "x")), None),
         (OrdBody ((Const (Integer 1)), None)), [])) |}]
;;

let%expect_test "var_binding_with_where" =
  prs_and_prnt_ln binding show_binding "x = y where y = 1; k = 2 ";
  [%expect
    {|
      (VarsBind (([], (PIdentificator (Ident "x")), None),
         (OrdBody ((Identificator (Ident "y")), None)),
         [(VarsBind (([], (PIdentificator (Ident "y")), None),
             (OrdBody ((Const (Integer 1)), None)), []));
           (VarsBind (([], (PIdentificator (Ident "k")), None),
              (OrdBody ((Const (Integer 2)), None)), []))
           ]
         )) |}]
;;

let%expect_test "fun_binding_simple" =
  prs_and_prnt_ln binding show_binding "f x = x + 1";
  [%expect
    {|
      (FunBind (((Ident "f"), None), ([], (PIdentificator (Ident "x")), None),
         [],
         (OrdBody
            ((Binop (((Identificator (Ident "x")), None), Plus,
                ((Const (Integer 1)), None))),
             None)),
         [])) |}]
;;

let%expect_test "fun_binding_simple_strange_but_valid1" =
  prs_and_prnt_ln binding show_binding "f(x)y = x + y";
  [%expect
    {|
      (FunBind (((Ident "f"), None), ([], (PIdentificator (Ident "x")), None),
         [([], (PIdentificator (Ident "y")), None)],
         (OrdBody
            ((Binop (((Identificator (Ident "x")), None), Plus,
                ((Identificator (Ident "y")), None))),
             None)),
         [])) |}]
;;

let%expect_test "fun_binding_simple_strange_but_valid2" =
  prs_and_prnt_ln binding show_binding "f 9y = y";
  [%expect
    {|
      (FunBind (((Ident "f"), None),
         ([], (PConst (OrdinaryPConst (Integer 9))), None),
         [([], (PIdentificator (Ident "y")), None)],
         (OrdBody ((Identificator (Ident "y")), None)), [])) |}]
;;

let%expect_test "fun_binding_guards" =
  prs_and_prnt_ln binding show_binding "f x |x > 1 = 0 | otherwise = 1";
  [%expect
    {|
      (FunBind (((Ident "f"), None), ([], (PIdentificator (Ident "x")), None),
         [],
         (Guards (
            (((Binop (((Identificator (Ident "x")), None), Greater,
                 ((Const (Integer 1)), None))),
              None),
             ((Const (Integer 0)), None)),
            [(((Identificator (Ident "otherwise")), None),
              ((Const (Integer 1)), None))]
            )),
         [])) |}]
;;

let parse_and_print_line = prs_and_prnt_ln binding show_binding
let parse_line str = prs_ln binding str
