(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

let%test "pp Int" = Format.asprintf "%a" Haskell_lib.Pprintast.pp_const (Int 18) = "18"

let%test "pp const Bool" =
  Format.asprintf "%a" Haskell_lib.Pprintast.pp_const (Bool true) = "True"
;;

let%test "pp const Unit" = Format.asprintf "%a" Haskell_lib.Pprintast.pp_const Unit = "()"

let%test "pp functype" =
  Format.asprintf
    "%a"
    Haskell_lib.Pprintast.pp_functype
    (FuncT (TInt, TBool, [ TBool; TUnit ]))
  = "Int -> Bool -> Bool -> ()"
;;

let%test "pp tp TUnit" = Format.asprintf "%a" Haskell_lib.Pprintast.pp_tp TUnit = "()"
let%test "pp tp TInt" = Format.asprintf "%a" Haskell_lib.Pprintast.pp_tp TInt = "Int"
let%test "pp tp TBool" = Format.asprintf "%a" Haskell_lib.Pprintast.pp_tp TBool = "Bool"

let%test "pp tp TreeParam" =
  Format.asprintf "%a" Haskell_lib.Pprintast.pp_tp (TreeParam TInt) = "{Int}"
;;

let%test "pp tp ListParam" =
  Format.asprintf "%a" Haskell_lib.Pprintast.pp_tp (ListParam TBool) = "[Bool]"
;;

let%test "pp tp TupleParams" =
  Format.asprintf
    "%a"
    Haskell_lib.Pprintast.pp_tp
    (TupleParams (TInt, TBool, [ TBool; TInt ]))
  = "(Int, Bool, Bool, Int)"
;;

let%test "pp tp FunctionType" =
  Format.asprintf
    "%a"
    Haskell_lib.Pprintast.pp_tp
    (FunctionType (FuncT (TBool, TUnit, [])))
  = "Bool -> ()"
;;

let%test "pp listpat PCons" =
  Format.asprintf
    "%a"
    Haskell_lib.Pprintast.pp_pat
    (PList
       (PCons (([], PIdentificator (Ident "x"), []), ([], PIdentificator (Ident "xs"), []))))
  = "x : xs"
;;

let%test "pp listpat PEnum" =
  Format.asprintf
    "%a"
    Haskell_lib.Pprintast.pp_pat
    (PList
       (PEnum
          [ [], PIdentificator (Ident "x"), []
          ; [], PIdentificator (Ident "y"), []
          ; [], PIdentificator (Ident "z"), []
          ]))
  = "[x, y, z]"
;;

let%test "pp treepat PNul" =
  Format.asprintf "%a" Haskell_lib.Pprintast.pp_pat (PTree PNul) = "$"
;;

let%test "pp treepat PNode" =
  Format.asprintf
    "%a"
    Haskell_lib.Pprintast.pp_pat
    (PTree
       (PNode
          ( ([], PIdentificator (Ident "x"), [])
          , ([], PIdentificator (Ident "y"), [])
          , ([], PIdentificator (Ident "z"), []) )))
  = "(x; y; z)"
;;

let%test "pp pconst OrdinaryPConst" =
  Format.asprintf "%a" Haskell_lib.Pprintast.pp_pat (PConst (OrdinaryPConst (Bool true)))
  = "True"
;;

let%test "pp pconst NegativePInt" =
  Format.asprintf "%a" Haskell_lib.Pprintast.pp_pat (PConst (NegativePInt 18)) = "-18"
;;

let%test "pp pat PWildcard" =
  Format.asprintf "%a" Haskell_lib.Pprintast.pp_pat PWildcard = "_"
;;

let%test "pp pat PConst" =
  Format.asprintf "%a" Haskell_lib.Pprintast.pp_pat (PConst (OrdinaryPConst (Bool true)))
  = "True"
;;

let%test "pp pat PIdentificator" =
  Format.asprintf "%a" Haskell_lib.Pprintast.pp_pat (PIdentificator (Ident "x")) = "x"
;;

let%test "pp pat PList" =
  Format.asprintf
    "%a"
    Haskell_lib.Pprintast.pp_pat
    (PList
       (PCons (([], PIdentificator (Ident "x"), []), ([], PIdentificator (Ident "xs"), []))))
  = "x : xs"
;;

let%test "pp pat PTuple" =
  Format.asprintf
    "%a"
    Haskell_lib.Pprintast.pp_pat
    (PTuple
       ( ([], PIdentificator (Ident "a"), [])
       , ([], PIdentificator (Ident "b"), [])
       , [ [], PIdentificator (Ident "c"), []; [], PIdentificator (Ident "d"), [] ] ))
  = "(a, b, c, d)"
;;

let%test "pp pat PMaybe Nothing" =
  Format.asprintf "%a" Haskell_lib.Pprintast.pp_pat (PMaybe Nothing) = "Nothing"
;;

let%test "pp pat PMaybe Just" =
  Format.asprintf
    "%a"
    Haskell_lib.Pprintast.pp_pat
    (PMaybe (Just ([], PIdentificator (Ident "x"), [])))
  = "Just x"
;;

let%test "pp pat PTree" =
  Format.asprintf
    "%a"
    Haskell_lib.Pprintast.pp_pat
    (PTree
       (PNode
          ( ([], PIdentificator (Ident "x"), [])
          , ([], PIdentificator (Ident "y"), [])
          , ([], PIdentificator (Ident "z"), []) )))
  = "(x; y; z)"
;;

let%test "pp pattern without capture, without type" =
  Format.asprintf
    "%a"
    Haskell_lib.Pprintast.pp_pattern
    ([], PIdentificator (Ident "x"), [])
  = "x"
;;

let%test "pp pattern with capture, without type" =
  Format.asprintf
    "%a"
    Haskell_lib.Pprintast.pp_pattern
    ([ Ident "my"; Ident "first"; Ident "variable" ], PIdentificator (Ident "x"), [])
  = "my@first@variable@x"
;;

let%test "pp pattern with capture, with type" =
  Format.asprintf
    "%a"
    Haskell_lib.Pprintast.pp_pattern
    ([ Ident "my"; Ident "first"; Ident "variable" ], PIdentificator (Ident "x"), [ TInt ])
  = "my@first@variable@x :: Int"
;;

let%expect_test "expr_with_prio" =
  Format.printf
    "%a"
    Haskell_lib.Pprintast.pp_expr
    ( Binop
        ( ( Binop
              ( ( Binop
                    ( Haskell_lib.Pprintast.i_const 1
                    , Plus
                    , Haskell_lib.Pprintast.i_const 0 )
                , [] )
              , Multiply
              , Haskell_lib.Pprintast.i_const 2 )
          , [] )
        , Greater
        , Haskell_lib.Pprintast.i_const 1 )
    , [] );
  [%expect {|
      (1 + 0) * 2 > 1 |}]
;;

let%expect_test "expr_with_prio_tp" =
  Format.printf
    "%a"
    Haskell_lib.Pprintast.pp_expr
    ( Binop
        ( ( Binop
              ( ( Binop
                    ( Haskell_lib.Pprintast.i_const 1
                    , Plus
                    , Haskell_lib.Pprintast.i_const 0 )
                , [ TInt ] )
              , Multiply
              , Haskell_lib.Pprintast.i_const 2 )
          , [] )
        , Greater
        , Haskell_lib.Pprintast.i_const 1 )
    , [ TBool ] );
  [%expect {|
      (1 + 0 :: Int) * 2 > 1 :: Bool |}]
;;

let%expect_test "expr_with_fun_app_tp" =
  Format.printf
    "%a"
    Haskell_lib.Pprintast.pp_expr
    ( Binop
        ( ( FunctionApply
              ( (Identificator (Ident "f"), [ FunctionType (FuncT (TInt, TInt, [])) ])
              , (Identificator (Ident "x"), [ TInt ])
              , [ Identificator (Ident "g"), []; Haskell_lib.Pprintast.i_const 2 ] )
          , [] )
        , Plus
        , Haskell_lib.Pprintast.i_const 1 )
    , [] );
  [%expect {|
      (f :: Int -> Int) (x :: Int) g 2 + 1 |}]
;;

let%expect_test "expr_case_neg" =
  Format.printf
    "%a"
    Haskell_lib.Pprintast.pp_expr
    ( Case
        ( (Neg (Haskell_lib.Pprintast.i_const 1), [])
        , (([], PConst (NegativePInt 1), []), OrdBody (Haskell_lib.Pprintast.i_const 1))
        , [] )
    , [] );
  [%expect {|
      (case - 1 of -1 -> 1) |}]
;;

let%expect_test "expr_case_tp" =
  Format.printf
    "%a"
    Haskell_lib.Pprintast.pp_expr
    ( Case
        ( (Neg (Haskell_lib.Pprintast.i_const 1), [ TInt ])
        , (([], PConst (NegativePInt 1), [ TInt ]), OrdBody (Const (Int 1), [ TInt ]))
        , [] )
    , [ TInt ] );
  [%expect {|
      ((case - 1 :: Int of (-1 :: Int) -> 1 :: Int)) :: Int |}]
;;

let%expect_test "expr_doble_cons_and_lam" =
  Format.printf
    "%a"
    Haskell_lib.Pprintast.pp_expr
    ( Lambda
        ( ( []
          , PList
              (PCons
                 ( ([], PIdentificator (Ident "x"), [])
                 , ([], PIdentificator (Ident "xs"), []) ))
          , [] )
        , []
        , ( Binop
              ( ( Binop
                    ( (Identificator (Ident "x1"), [])
                    , Cons
                    , (Identificator (Ident "x2"), []) )
                , [] )
              , Cons
              , (Identificator (Ident "xs"), []) )
          , [] ) )
    , [] );
  [%expect {|
      (\ (x : xs) -> (x1 : x2) : xs) |}]
;;

let%expect_test "expr_cons_lin" =
  Format.printf
    "%a"
    Haskell_lib.Pprintast.pp_expr
    ( Binop
        ( (Identificator (Ident "xs"), [])
        , Cons
        , ( Binop
              ((Identificator (Ident "x1"), []), Cons, (Identificator (Ident "x2"), []))
          , [] ) )
    , [] );
  [%expect {|
      xs : x1 : x2 |}]
;;

let%expect_test "expr_cons_lin_tp" =
  Format.printf
    "%a"
    Haskell_lib.Pprintast.pp_expr
    ( Binop
        ( (Identificator (Ident "xs"), [ TUnit ])
        , Cons
        , ( Binop
              ((Identificator (Ident "x1"), []), Cons, (Identificator (Ident "x2"), []))
          , [] ) )
    , [ ListParam TUnit ] );
  [%expect {|
      (xs :: ()) : x1 : x2 :: [()] |}]
;;

let%expect_test "fac" =
  Format.printf
    "%a"
    Haskell_lib.Pprintast.pp_binding
    (Def
       (FunDef
          ( Ident "fac"
          , ([], PIdentificator (Ident "n"), [])
          , []
          , OrdBody
              ( IfThenEsle
                  ( ( Binop
                        ( (Identificator (Ident "n"), [])
                        , Less
                        , Haskell_lib.Pprintast.i_const 0 )
                    , [] )
                  , (ENothing, [])
                  , ( FunctionApply
                        ( (EJust, [])
                        , ( FunctionApply
                              ( (Identificator (Ident "save_fac"), [])
                              , (Identificator (Ident "n"), [])
                              , [] )
                          , [] )
                        , [] )
                    , [] ) )
              , [] )
          , [ Def
                (FunDef
                   ( Ident "save_fac"
                   , ([], PIdentificator (Ident "y"), [])
                   , []
                   , Guards
                       ( ( ( Binop
                               ( (Identificator (Ident "y"), [])
                               , Equality
                               , Haskell_lib.Pprintast.i_const 0 )
                           , [] )
                         , Haskell_lib.Pprintast.i_const 1 )
                       , [ ( (Identificator (Ident "otherwise"), [])
                           , ( Binop
                                 ( (Identificator (Ident "y"), [])
                                 , Multiply
                                 , ( FunctionApply
                                       ( (Identificator (Ident "save_fac"), [])
                                       , ( Binop
                                             ( (Identificator (Ident "y"), [])
                                             , Minus
                                             , Haskell_lib.Pprintast.i_const 1 )
                                         , [] )
                                       , [] )
                                   , [] ) )
                             , [] ) )
                         ] )
                   , [] ))
            ] )));
  [%expect
    {|
      fac n = (if n < 0 then Nothing else Just (save_fac n)) where save_fac y | y == 0 = 1 | otherwise = y * save_fac (y - 1) |}]
;;
