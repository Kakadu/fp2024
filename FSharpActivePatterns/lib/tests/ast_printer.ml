(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let%expect_test "print Ast factorial" =
  let open FSharpActivePatterns.Ast in
  let open FSharpActivePatterns.AstPrinter in
  let open Format in
  let factorial =
    Lambda
      ( PConst (Int_lt 4)
      , []
      , If_then_else
          ( Bin_expr
              ( Logical_or
              , Bin_expr (Binary_equal, Variable (Ident "n"), Const (Int_lt 0))
              , Bin_expr (Binary_equal, Variable (Ident "n"), Const (Int_lt 1)) )
          , Const (Int_lt 1)
          , Some
              (Bin_expr
                 ( Binary_multiply
                 , Variable (Ident "n")
                 , Apply
                     ( Variable (Ident "factorial")
                     , Bin_expr (Binary_subtract, Variable (Ident "n"), Const (Int_lt 1))
                     ) )) ) )
  in
  let program =
    [ Statement (Let (Nonrec, Let_bind (PVar (Ident "a"), [], Const (Int_lt 10)), []))
    ; Expr factorial
    ; Expr (Apply (factorial, Variable (Ident "a")))
    ]
  in
  List.iter (print_construction std_formatter) program;
  [%expect
    {|
     | Let:
       Let_binds
    --| Let_bind:
          NAME:
    ------| PVar(a)
          ARGS:
          BODY:
    ----| Const(Int: 10)
    | Lambda:
      ARGS
    ----| PConst:
    ------Int: 4
      BODY
    ----| If Then Else(
          CONDITION
    ------| Binary expr(
    ------| Logical Or
    --------| Binary expr(
    --------| Binary Equal
    ----------| Variable(n)
    ----------| Const(Int: 0)
    --------| Binary expr(
    --------| Binary Equal
    ----------| Variable(n)
    ----------| Const(Int: 1)
          THEN BRANCH
    ------| Const(Int: 1)
          ELSE BRANCH
    ------| Binary expr(
    ------| Binary Multiply
    --------| Variable(n)
    --------| Apply:
              FUNCTION
    ----------| Variable(factorial)
              ARGS
    ----------| Binary expr(
    ----------| Binary Subtract
    ------------| Variable(n)
    ------------| Const(Int: 1)
    | Apply:
      FUNCTION
    --| Lambda:
        ARGS
    ------| PConst:
    --------Int: 4
        BODY
    ------| If Then Else(
            CONDITION
    --------| Binary expr(
    --------| Logical Or
    ----------| Binary expr(
    ----------| Binary Equal
    ------------| Variable(n)
    ------------| Const(Int: 0)
    ----------| Binary expr(
    ----------| Binary Equal
    ------------| Variable(n)
    ------------| Const(Int: 1)
            THEN BRANCH
    --------| Const(Int: 1)
            ELSE BRANCH
    --------| Binary expr(
    --------| Binary Multiply
    ----------| Variable(n)
    ----------| Apply:
                FUNCTION
    ------------| Variable(factorial)
                ARGS
    ------------| Binary expr(
    ------------| Binary Subtract
    --------------| Variable(n)
    --------------| Const(Int: 1)
      ARGS
    --| Variable(a) |}]
;;

let%expect_test "print Ast double func" =
  let open FSharpActivePatterns.Ast in
  let open FSharpActivePatterns.AstPrinter in
  let open Format in
  let ident = Ident "n" in
  let pat = PConst (Int_lt 4) in
  let args = [] in
  let binary_expr = Bin_expr (Binary_multiply, Const (Int_lt 2), Variable ident) in
  let double = Lambda (pat, args, binary_expr) in
  print_construction std_formatter @@ Expr double;
  [%expect
    {|
    | Lambda:
      ARGS
    ----| PConst:
    ------Int: 4
      BODY
    ----| Binary expr(
    ----| Binary Multiply
    ------| Const(Int: 2)
    ------| Variable(n)|}]
;;

let%expect_test "print Ast tuple of binary operators" =
  let open FSharpActivePatterns.Ast in
  let open FSharpActivePatterns.AstPrinter in
  let open Format in
  let first = Const (Int_lt 3) in
  let second = Const (Int_lt 10) in
  let operators =
    [ Binary_unequal
    ; Binary_less
    ; Binary_less_or_equal
    ; Binary_greater
    ; Binary_greater_or_equal
    ; Binary_add
    ; Logical_and
    ; Binary_divide
    ; Binary_or_bitwise
    ; Binary_xor_bitwise
    ; Binary_and_bitwise
    ]
  in
  let print_binary_constr operator =
    print_construction std_formatter @@ Expr (Bin_expr (operator, first, second))
  in
  List.iter print_binary_constr operators;
  [%expect
    {|
    | Binary expr(
    | Binary Unequal
    --| Const(Int: 3)
    --| Const(Int: 10)
    | Binary expr(
    | Binary Less
    --| Const(Int: 3)
    --| Const(Int: 10)
    | Binary expr(
    | Binary Less Or Equal
    --| Const(Int: 3)
    --| Const(Int: 10)
    | Binary expr(
    | Binary Greater
    --| Const(Int: 3)
    --| Const(Int: 10)
    | Binary expr(
    | Binary Greater Or Equal
    --| Const(Int: 3)
    --| Const(Int: 10)
    | Binary expr(
    | Binary Add
    --| Const(Int: 3)
    --| Const(Int: 10)
    | Binary expr(
    | Logical And
    --| Const(Int: 3)
    --| Const(Int: 10)
    | Binary expr(
    | Binary Divide
    --| Const(Int: 3)
    --| Const(Int: 10)
    | Binary expr(
    | Binary Or Bitwise
    --| Const(Int: 3)
    --| Const(Int: 10)
    | Binary expr(
    | Binary Xor Bitwise
    --| Const(Int: 3)
    --| Const(Int: 10)
    | Binary expr(
    | Binary And Bitwise
    --| Const(Int: 3)
    --| Const(Int: 10) |}]
;;

let%expect_test "print Ast of LetIn" =
  let open FSharpActivePatterns.Ast in
  let open FSharpActivePatterns.AstPrinter in
  let open Format in
  let sum =
    Expr
      (LetIn
         ( Nonrec
         , Let_bind (PVar (Ident "x"), [], Const (Int_lt 5))
         , []
         , Bin_expr (Binary_add, Variable (Ident "x"), Const (Int_lt 5)) ))
  in
  print_construction std_formatter sum;
  [%expect
    {|
    | LetIn=
      Let_binds
    --| Let_bind:
          NAME:
    ------| PVar(x)
          ARGS:
          BODY:
    ----| Const(Int: 5)
      INNER_EXPRESSION
    --| Binary expr(
    --| Binary Add
    ----| Variable(x)
    ----| Const(Int: 5) |}]
;;

let%expect_test "print Ast of match_expr" =
  let open FSharpActivePatterns.Ast in
  let open FSharpActivePatterns.AstPrinter in
  let open Format in
  let patterns =
    [ PConst (Int_lt 5)
    ; PConst (String_lt " bar foo")
    ; PList [ Wild; PVar (Ident "xs") ]
    ]
  in
  let pattern_values = List.map (fun p -> p, Const (Int_lt 4)) patterns in
  let match_expr =
    Match (Variable (Ident "x"), (PConst (Int_lt 4), Const (Int_lt 4)), pattern_values)
  in
  print_construction std_formatter (Expr match_expr);
  [%expect
    {|
    | Match:
    --| Value:
    ----| Variable(x)
    --| Pattern:
    ----| PConst:
    ------Int: 4
    --| Case expr:
    ----| Const(Int: 4)
    --| Pattern:
    ----| PConst:
    ------Int: 5
    --| Case expr:
    ----| Const(Int: 4)
    --| Pattern:
    ----| PConst:
    ------String: " bar foo"
    --| Case expr:
    ----| Const(Int: 4)
    --| Pattern:
    ----| PList:
    ------| Wild
    ------| PVar(xs)
    --| Case expr:
    ----| Const(Int: 4) |}]
;;
