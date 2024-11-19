(** Copyright 2024, Kostya Oreshin and Nikita Shchutskii *)

(** SPDX-License-Identifier: MIT *)

<<<<<<< HEAD
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
=======
open Format

let pp_const = Haskell_lib.Pprintast.pp_const
let pp_functype = Haskell_lib.Pprintast.pp_functype
let pp_binop = Haskell_lib.Pprintast.pp_binop
let pp_tp = Haskell_lib.Pprintast.pp_tp
let pp_ident = Haskell_lib.Pprintast.pp_ident
let pp_pat = Haskell_lib.Pprintast.pp_pat
let pp_pattern = Haskell_lib.Pprintast.pp_pattern
let pp_listpat = Haskell_lib.Pprintast.pp_listpat
let pp_treepat = Haskell_lib.Pprintast.pp_treepat
let pp_pconst = Haskell_lib.Pprintast.pp_pconst
let pp_expr = Haskell_lib.Pprintast.pp_expr
let i_const = Haskell_lib.Pprintast.i_const
let pp_binding = Haskell_lib.Pprintast.pp_binding
let%test "pp Int" = asprintf "%a" pp_const (Int 18) = "18"
let%test "pp const Bool" = asprintf "%a" pp_const (Bool true) = "True"
let%test "pp const Unit" = asprintf "%a" pp_const Unit = "()"

let%test "pp functype" =
  asprintf "%a" pp_functype (FuncT (TInt, TBool, [ TBool; TUnit ]))
  = "Int -> Bool -> Bool -> ()"
;;

let%test "pp tp TUnit" = asprintf "%a" pp_tp TUnit = "()"
let%test "pp tp TInt" = asprintf "%a" pp_tp TInt = "Int"
let%test "pp tp TBool" = asprintf "%a" pp_tp TBool = "Bool"
let%test "pp tp TreeParam" = asprintf "%a" pp_tp (TreeParam TInt) = "{Int}"
let%test "pp tp ListParam" = asprintf "%a" pp_tp (ListParam TBool) = "[Bool]"

let%test "pp tp TupleParams" =
  asprintf "%a" pp_tp (TupleParams (TInt, TBool, [ TBool; TInt ]))
>>>>>>> b1515bc (test: put tests into separate file)
  = "(Int, Bool, Bool, Int)"
;;

let%test "pp tp FunctionType" =
<<<<<<< HEAD
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
=======
  asprintf "%a" pp_tp (FunctionType (FuncT (TBool, TUnit, []))) = "Bool -> ()"
;;

let%test "pp listpat PCons" =
  asprintf
    "%a"
    pp_listpat
    (PCons (([], PIdentificator (Ident "x"), []), ([], PIdentificator (Ident "xs"), [])))
>>>>>>> b1515bc (test: put tests into separate file)
  = "x : xs"
;;

let%test "pp listpat PEnum" =
<<<<<<< HEAD
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
=======
  asprintf
    "%a"
    pp_listpat
    (PEnum
       [ [], PIdentificator (Ident "x"), []
       ; [], PIdentificator (Ident "y"), []
       ; [], PIdentificator (Ident "z"), []
       ])
  = "[x, y, z]"
;;

let%test "pp treepat PNul" = asprintf "%a" pp_treepat PNul = "$"

let%test "pp treepat PNode" =
  asprintf
    "%a"
    pp_treepat
    (PNode
       ( ([], PIdentificator (Ident "x"), [])
       , ([], PIdentificator (Ident "y"), [])
       , ([], PIdentificator (Ident "z"), []) ))
>>>>>>> b1515bc (test: put tests into separate file)
  = "(x; y; z)"
;;

let%test "pp pconst OrdinaryPConst" =
<<<<<<< HEAD
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
=======
  asprintf "%a" pp_pconst (OrdinaryPConst (Bool true)) = "True"
;;

let%test "pp pconst NegativePInt" = asprintf "%a" pp_pconst (NegativePInt 18) = "-18"
let%test "pp pat PWildcard" = asprintf "%a" pp_pat PWildcard = "_"

let%test "pp pat PConst" =
  asprintf "%a" pp_pat (PConst (OrdinaryPConst (Bool true))) = "True"
;;

let%test "pp pat PIdentificator" = asprintf "%a" pp_pat (PIdentificator (Ident "x")) = "x"

let%test "pp pat PList" =
  asprintf
    "%a"
    pp_pat
>>>>>>> b1515bc (test: put tests into separate file)
    (PList
       (PCons (([], PIdentificator (Ident "x"), []), ([], PIdentificator (Ident "xs"), []))))
  = "x : xs"
;;

let%test "pp pat PTuple" =
<<<<<<< HEAD
  Format.asprintf
    "%a"
    Haskell_lib.Pprintast.pp_pat
=======
  asprintf
    "%a"
    pp_pat
>>>>>>> b1515bc (test: put tests into separate file)
    (PTuple
       ( ([], PIdentificator (Ident "a"), [])
       , ([], PIdentificator (Ident "b"), [])
       , [ [], PIdentificator (Ident "c"), []; [], PIdentificator (Ident "d"), [] ] ))
  = "(a, b, c, d)"
;;

<<<<<<< HEAD
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
=======
let%test "pp pat PMaybe Nothing" = asprintf "%a" pp_pat (PMaybe Nothing) = "Nothing"

let%test "pp pat PMaybe Just" =
  asprintf "%a" pp_pat (PMaybe (Just ([], PIdentificator (Ident "x"), []))) = "Just x"
;;

let%test "pp pat PTree" =
  asprintf
    "%a"
    pp_pat
>>>>>>> b1515bc (test: put tests into separate file)
    (PTree
       (PNode
          ( ([], PIdentificator (Ident "x"), [])
          , ([], PIdentificator (Ident "y"), [])
          , ([], PIdentificator (Ident "z"), []) )))
  = "(x; y; z)"
;;

let%test "pp pattern without capture, without type" =
<<<<<<< HEAD
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
=======
  asprintf "%a" pp_pattern ([], PIdentificator (Ident "x"), []) = "x"
;;

let%test "pp pattern with capture, without type" =
  asprintf
    "%a"
    pp_pattern
>>>>>>> b1515bc (test: put tests into separate file)
    ([ Ident "my"; Ident "first"; Ident "variable" ], PIdentificator (Ident "x"), [])
  = "my@first@variable@x"
;;

let%test "pp pattern with capture, with type" =
<<<<<<< HEAD
  Format.asprintf
    "%a"
    Haskell_lib.Pprintast.pp_pattern
=======
  asprintf
    "%a"
    pp_pattern
>>>>>>> b1515bc (test: put tests into separate file)
    ([ Ident "my"; Ident "first"; Ident "variable" ], PIdentificator (Ident "x"), [ TInt ])
  = "my@first@variable@x :: Int"
;;

let%expect_test "expr_with_prio" =
<<<<<<< HEAD
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
=======
  printf
    "%a"
    pp_expr
    ( Binop
        ( (Binop ((Binop (i_const 1, Plus, i_const 0), []), Multiply, i_const 2), [])
        , Greater
        , i_const 1 )
>>>>>>> b1515bc (test: put tests into separate file)
    , [] );
  [%expect {|
      (1 + 0) * 2 > 1 |}]
;;

let%expect_test "expr_with_prio_tp" =
<<<<<<< HEAD
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
=======
  printf
    "%a"
    pp_expr
    ( Binop
        ( (Binop ((Binop (i_const 1, Plus, i_const 0), [ TInt ]), Multiply, i_const 2), [])
        , Greater
        , i_const 1 )
>>>>>>> b1515bc (test: put tests into separate file)
    , [ TBool ] );
  [%expect {|
      (1 + 0 :: Int) * 2 > 1 :: Bool |}]
;;

let%expect_test "expr_with_fun_app_tp" =
<<<<<<< HEAD
  Format.printf
    "%a"
    Haskell_lib.Pprintast.pp_expr
=======
  printf
    "%a"
    pp_expr
>>>>>>> b1515bc (test: put tests into separate file)
    ( Binop
        ( ( FunctionApply
              ( (Identificator (Ident "f"), [ FunctionType (FuncT (TInt, TInt, [])) ])
              , (Identificator (Ident "x"), [ TInt ])
<<<<<<< HEAD
              , [ Identificator (Ident "g"), []; Haskell_lib.Pprintast.i_const 2 ] )
          , [] )
        , Plus
        , Haskell_lib.Pprintast.i_const 1 )
=======
              , [ Identificator (Ident "g"), []; i_const 2 ] )
          , [] )
        , Plus
        , i_const 1 )
>>>>>>> b1515bc (test: put tests into separate file)
    , [] );
  [%expect {|
      (f :: Int -> Int) (x :: Int) g 2 + 1 |}]
;;

let%expect_test "expr_case_neg" =
<<<<<<< HEAD
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
=======
  printf
    "%a"
    pp_expr
    ( Case
        ( (Neg (i_const 1), [])
        , (([], PConst (NegativePInt 1), []), OrdBody (i_const 1))
        , [] )
    , [] );
  [%expect {|
      case - 1 of -1 -> 1 |}]
;;

let%expect_test "expr_case_tp" =
  printf
    "%a"
    pp_expr
    ( Case
        ( (Neg (i_const 1), [ TInt ])
>>>>>>> b1515bc (test: put tests into separate file)
        , (([], PConst (NegativePInt 1), [ TInt ]), OrdBody (Const (Int 1), [ TInt ]))
        , [] )
    , [ TInt ] );
  [%expect {|
<<<<<<< HEAD
      ((case - 1 :: Int of (-1 :: Int) -> 1 :: Int)) :: Int |}]
;;

let%expect_test "expr_doble_cons_and_lam" =
  Format.printf
    "%a"
    Haskell_lib.Pprintast.pp_expr
=======
      (case - 1 :: Int of (-1 :: Int) -> 1 :: Int) :: Int |}]
;;

let%expect_test "expr_doble_cons_and_lam" =
  printf
    "%a"
    pp_expr
>>>>>>> b1515bc (test: put tests into separate file)
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
<<<<<<< HEAD
      (\ (x : xs) -> (x1 : x2) : xs) |}]
;;

let%expect_test "expr_cons_lin" =
  Format.printf
    "%a"
    Haskell_lib.Pprintast.pp_expr
=======
      \ (x : xs) -> (x1 : x2) : xs |}]
;;

let%expect_test "expr_cons_lin" =
  printf
    "%a"
    pp_expr
>>>>>>> b1515bc (test: put tests into separate file)
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
<<<<<<< HEAD
  Format.printf
    "%a"
    Haskell_lib.Pprintast.pp_expr
=======
  printf
    "%a"
    pp_expr
>>>>>>> b1515bc (test: put tests into separate file)
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
<<<<<<< HEAD
  Format.printf
    "%a"
    Haskell_lib.Pprintast.pp_binding
=======
  printf
    "%a"
    pp_binding
>>>>>>> b1515bc (test: put tests into separate file)
    (FunDef
       ( Ident "fac"
       , ([], PIdentificator (Ident "n"), [])
       , []
       , OrdBody
           ( IfThenEsle
<<<<<<< HEAD
               ( ( Binop
                     ( (Identificator (Ident "n"), [])
                     , Less
                     , Haskell_lib.Pprintast.i_const 0 )
                 , [] )
=======
               ( (Binop ((Identificator (Ident "n"), []), Less, i_const 0), [])
>>>>>>> b1515bc (test: put tests into separate file)
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
       , [ FunDef
             ( Ident "save_fac"
             , ([], PIdentificator (Ident "y"), [])
             , []
             , Guards
<<<<<<< HEAD
                 ( ( ( Binop
                         ( (Identificator (Ident "y"), [])
                         , Equality
                         , Haskell_lib.Pprintast.i_const 0 )
                     , [] )
                   , Haskell_lib.Pprintast.i_const 1 )
=======
                 ( ( (Binop ((Identificator (Ident "y"), []), Equality, i_const 0), [])
                   , i_const 1 )
>>>>>>> b1515bc (test: put tests into separate file)
                 , [ ( (Identificator (Ident "otherwise"), [])
                     , ( Binop
                           ( (Identificator (Ident "y"), [])
                           , Multiply
                           , ( FunctionApply
                                 ( (Identificator (Ident "save_fac"), [])
                                 , ( Binop
<<<<<<< HEAD
                                       ( (Identificator (Ident "y"), [])
                                       , Minus
                                       , Haskell_lib.Pprintast.i_const 1 )
=======
                                       ((Identificator (Ident "y"), []), Minus, i_const 1)
>>>>>>> b1515bc (test: put tests into separate file)
                                   , [] )
                                 , [] )
                             , [] ) )
                       , [] ) )
                   ] )
             , [] )
         ] ));
  [%expect
    {|
<<<<<<< HEAD
      fac n = (if n < 0 then Nothing else Just (save_fac n)) where save_fac y | y == 0 = 1 | otherwise = y * save_fac (y - 1) |}]
=======
      fac n = if n < 0 then Nothing else Just (save_fac n) where save_fac y | y == 0 = 1 | otherwise = y * save_fac (y - 1) |}]
>>>>>>> b1515bc (test: put tests into separate file)
;;
