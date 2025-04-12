(** Copyright 2025, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open C_sharp_strange_lib.Ast
open C_sharp_strange_lib.Parser

let%test "Parse one integer" = apply_parser parse_int {|1|} = Ok (ValInt 1)
let%test "Parse one char" = apply_parser parse_char {|'c'|} = Ok (ValChar 'c')
let%test "Parse true" = apply_parser parse_bool {|true|} = Ok (ValBool true)
let%test "Parse false" = apply_parser parse_bool {|false|} = Ok (ValBool false)

let%test "Parse string" =
  apply_parser parse_val_string {|"sample"|} = Ok (ValString "sample")
;;

let%test "Parse parens" = apply_parser (parens parse_int) {|(1)|} = Ok (ValInt 1)
let%test "Parse braces" = apply_parser (braces parse_int) {|{1}|} = Ok (ValInt 1)
let%test "Parse brackets" = apply_parser (brackets parse_int) {|[1]|} = Ok (ValInt 1)
let%test "Parse one modifier 1" = apply_parser parse_modifiers {|static|} = Ok [ MStatic ]
let%test "Parse one modifier 2" = apply_parser parse_modifiers {|public|} = Ok [ MPublic ]

let%test "Parse two modifiers" =
  apply_parser parse_modifiers {|const async|} = Ok [ MConst; MAsync ]
;;

let%test "Parse add 1" =
  apply_parser parse_ops {|  1  +  2|}
  = Ok (EBinOp (OpAdd, EValue (ValInt 1), EValue (ValInt 2)))
;;

let%test "Parse add 2" =
  apply_parser parse_ops {|   a +   b|} = Ok (EBinOp (OpAdd, EId (Id "a"), EId (Id "b")))
;;

let%test "Parse many adds" =
  apply_parser parse_ops {|  1 +   2   +  3|}
  = Ok
      (EBinOp
         (OpAdd, EBinOp (OpAdd, EValue (ValInt 1), EValue (ValInt 2)), EValue (ValInt 3)))
;;

let%test "Parse adds with mul 1" =
  apply_parser parse_ops {|1  + 2 * 3|}
  = Ok
      (EBinOp
         (OpAdd, EValue (ValInt 1), EBinOp (OpMul, EValue (ValInt 2), EValue (ValInt 3))))
;;

let%test "Parse adds with mul 2" =
  apply_parser parse_ops {|  (1  +  2  )   *  3|}
  = Ok
      (EBinOp
         (OpMul, EBinOp (OpAdd, EValue (ValInt 1), EValue (ValInt 2)), EValue (ValInt 3)))
;;

let%test "Parse div with mod" =
  apply_parser parse_ops {|  1  /  2  %  3|}
  = Ok
      (EBinOp
         (OpMod, EBinOp (OpDiv, EValue (ValInt 1), EValue (ValInt 2)), EValue (ValInt 3)))
;;

let%test "Parse div with mod" =
  apply_parser parse_ops {| 1  -  2  /  3     +   4|}
  = Ok
      (EBinOp
         ( OpAdd
         , EBinOp
             ( OpSub
             , EValue (ValInt 1)
             , EBinOp (OpDiv, EValue (ValInt 2), EValue (ValInt 3)) )
         , EValue (ValInt 4) ))
;;

let%test "Parse simple boolean expression" =
  apply_parser parse_ops {| (   1 + 2 == 3 + 4 )|}
  = Ok
      (EBinOp
         ( OpEqual
         , EBinOp (OpAdd, EValue (ValInt 1), EValue (ValInt 2))
         , EBinOp (OpAdd, EValue (ValInt 3), EValue (ValInt 4)) ))
;;

let%test "Parse complex boolean expression" =
  apply_parser parse_ops {|( 1  +  2 < 3 +  4) && (5  == 8)|}
  = Ok
      (EBinOp
         ( OpAnd
         , EBinOp
             ( OpLess
             , EBinOp (OpAdd, EValue (ValInt 1), EValue (ValInt 2))
             , EBinOp (OpAdd, EValue (ValInt 3), EValue (ValInt 4)) )
         , EBinOp (OpEqual, EValue (ValInt 5), EValue (ValInt 8)) ))
;;

let%test "Parse ident expr" = apply_parser parse_ops {|  x|} = Ok (EId (Id "x"))
let%test "Parse id in expressions 1" = apply_parser parse_ops {| x|} = Ok (EId (Id "x"))

let%test "Parse id in expressions 2" =
  apply_parser parse_ops {|x + 1|} = Ok (EBinOp (OpAdd, EId (Id "x"), EValue (ValInt 1)))
;;

let%test "Parse var declaration 1" =
  apply_parser parse_decl {|int x|}
  = Ok (SDecl (Var (TypeVar (TypeBase TypeInt), Id "x"), None))
;;

let%test "Parse var declaration 2" =
  apply_parser parse_decl {|int x = 1|}
  = Ok (SDecl (Var (TypeVar (TypeBase TypeInt), Id "x"), Some (EValue (ValInt 1))))
;;

let%test "Parse multiple var declarations" =
  apply_parser parse_decl {|int x = y = z = 1|}
  = Ok
      (SDecl
         ( Var (TypeVar (TypeBase TypeInt), Id "x")
         , Some
             (EBinOp
                ( OpAssign
                , EId (Id "y")
                , EBinOp (OpAssign, EId (Id "z"), EValue (ValInt 1)) )) ))
;;

let%test "Parse array declaration" =
  apply_parser parse_decl {|int[] x|}
  = Ok (SDecl (Var (TypeVar (TypeArray TypeInt), Id "x"), None))
;;

let%test "Parse return 1" =
  apply_parser parse_return {|return 5|} = Ok (SReturn (Some (EValue (ValInt 5))))
;;

let%test "Parse return 2" = apply_parser parse_return {|return|} = Ok (SReturn None)
let%test "Parse break" = apply_parser parse_break {|break|} = Ok SBreak
let%test "Parse continue" = apply_parser parse_continue {|continue|} = Ok SContinue
let%test "Parse empty block 1" = apply_parser parse_block {|{}|} = Ok (SBlock [])
let%test "Parse empty block 2" = apply_parser parse_block {|{;;;;}|} = Ok (SBlock [])

let%test "Parse block 1" =
  apply_parser parse_block {|{return 5;}|}
  = Ok (SBlock [ SReturn (Some (EValue (ValInt 5))) ])
;;

let%test "Parse block 2" =
  apply_parser parse_block {|{int x = 6; x = 6 + 1; return x;}|}
  = Ok
      (SBlock
         [ SDecl (Var (TypeVar (TypeBase TypeInt), Id "x"), Some (EValue (ValInt 6)))
         ; SExpr
             (EBinOp
                ( OpAssign
                , EId (Id "x")
                , EBinOp (OpAdd, EValue (ValInt 6), EValue (ValInt 1)) ))
         ; SReturn (Some (EId (Id "x")))
         ])
;;

let%test "Parse while" =
  apply_parser
    parse_block
    {| 
       {  
  int x = 1;
       while ( x <   1 )
       {
    x = 2;
        break;
        continue;
       } 
       }|}
  = Ok
      (SBlock
         [ SDecl (Var (TypeVar (TypeBase TypeInt), Id "x"), Some (EValue (ValInt 1)))
         ; SWhile
             ( EBinOp (OpLess, EId (Id "x"), EValue (ValInt 1))
             , SBlock
                 [ SExpr (EBinOp (OpAssign, EId (Id "x"), EValue (ValInt 2)))
                 ; SBreak
                 ; SContinue
                 ] )
         ])
;;

let%test "Parse for" =
  apply_parser
    parse_block
    {|{
  for (int i = 1;i < 5; i = i+1)
  {
      i = i + 1;
  }
      }|}
  = Ok
      (SBlock
         [ SFor
             ( Some
                 (SDecl
                    (Var (TypeVar (TypeBase TypeInt), Id "i"), Some (EValue (ValInt 1))))
             , Some (EBinOp (OpLess, EId (Id "i"), EValue (ValInt 5)))
             , Some
                 (EBinOp
                    ( OpAssign
                    , EId (Id "i")
                    , EBinOp (OpAdd, EId (Id "i"), EValue (ValInt 1)) ))
             , SBlock
                 [ SExpr
                     (EBinOp
                        ( OpAssign
                        , EId (Id "i")
                        , EBinOp (OpAdd, EId (Id "i"), EValue (ValInt 1)) ))
                 ] )
         ])
;;

let%test "Parse if" =
  apply_parser
    parse_block
    {|{if (x == 5) 
    x=1;
      else if (x == 2)
  { 
    x=2;
  }
    }|}
  = Ok
      (SBlock
         [ SIf
             ( EBinOp (OpEqual, EId (Id "x"), EValue (ValInt 5))
             , SExpr (EBinOp (OpAssign, EId (Id "x"), EValue (ValInt 1)))
             , Some
                 (SIf
                    ( EBinOp (OpEqual, EId (Id "x"), EValue (ValInt 2))
                    , SBlock
                        [ SExpr (EBinOp (OpAssign, EId (Id "x"), EValue (ValInt 2))) ]
                    , None )) )
         ])
;;

let%test "Parse field 1" =
  apply_parser parse_field_member {|public int X;|}
  = Ok (VarField ([ MPublic ], TypeVar (TypeBase TypeInt), Id "X", None))
;;

let%test "Parse field 2" =
  apply_parser parse_field_member {|public int X = 1;|}
  = Ok
      (VarField
         ( [ MPublic ]
         , TypeVar (TypeBase TypeInt)
         , Id "X"
         , Some (EBinOp (OpAssign, EId (Id "X"), EValue (ValInt 1))) ))
;;

let%test "Parse method 1" =
  apply_parser parse_method_member {|public int Func() {}|}
  = Ok (Method ([ MPublic ], TypeBase TypeInt, Id "Func", Params [], SBlock []))
;;

let%test "Parse method 2" =
  apply_parser
    parse_method_member
    {|public int Func() 
  {
    return 2;
  }|}
  = Ok
      (Method
         ( [ MPublic ]
         , TypeBase TypeInt
         , Id "Func"
         , Params []
         , SBlock [ SReturn (Some (EValue (ValInt 2))) ] ))
;;

let%test "Parse method 3" =
  apply_parser
    parse_method_member
    {|public int Factorial(int n) 
    {
        if (n == 0)
        {
            return 1;
        }
        else
        {
            return n * Factorial(n - 1);
        }
    }|}
  = Ok
      (Method
         ( [ MPublic ]
         , TypeBase TypeInt
         , Id "Factorial"
         , Params [ Var (TypeVar (TypeBase TypeInt), Id "n") ]
         , SBlock
             [ SIf
                 ( EBinOp (OpEqual, EId (Id "n"), EValue (ValInt 0))
                 , SBlock [ SReturn (Some (EValue (ValInt 1))) ]
                 , Some
                     (SBlock
                        [ SReturn
                            (Some
                               (EBinOp
                                  ( OpMul
                                  , EId (Id "n")
                                  , EFuncCall
                                      ( EId (Id "Factorial")
                                      , [ EBinOp (OpSub, EId (Id "n"), EValue (ValInt 1))
                                        ] ) )))
                        ]) )
             ] ))
;;

let%test "Parse class 1" =
  apply_parser
    parse_class
    {|
  public class Sample {}|}
  = Ok (Class ([ MPublic ], Id "Sample", []))
;;

let%test "Parse class 2" =
  apply_parser
    parse_class
    {|
  public class Sample {
      public int X;
      public int Y = 1;
  }|}
  = Ok
      (Class
         ( [ MPublic ]
         , Id "Sample"
         , [ VarField ([ MPublic ], TypeVar (TypeBase TypeInt), Id "X", None)
           ; VarField
               ( [ MPublic ]
               , TypeVar (TypeBase TypeInt)
               , Id "Y"
               , Some (EBinOp (OpAssign, EId (Id "Y"), EValue (ValInt 1))) )
           ] ))
;;

let%test "Parse class 3" =
  apply_parser
    parse_class
    {|
  public class Sample {
  
      public int X;

      public int add(int x) {
          X = X + x;
      }
  }|}
  = Ok
      (Class
         ( [ MPublic ]
         , Id "Sample"
         , [ VarField ([ MPublic ], TypeVar (TypeBase TypeInt), Id "X", None)
           ; Method
               ( [ MPublic ]
               , TypeBase TypeInt
               , Id "add"
               , Params [ Var (TypeVar (TypeBase TypeInt), Id "x") ]
               , SBlock
                   [ SExpr
                       (EBinOp
                          ( OpAssign
                          , EId (Id "X")
                          , EBinOp (OpAdd, EId (Id "X"), EId (Id "x")) ))
                   ] )
           ] ))
;;

let%test "Parse factorial" =
  apply_parser
    parse_prog
    {|
  public class Program
  {
    public static void Main() {}

    public int Factorial(int n) 
    {
        if (n == 0)
        {
            return 1;
        }
        else
        {
            return n * Factorial(n - 1);
        }
    }
  }
    
  |}
  = Ok
      (Program
         (Class
            ( [ MPublic ]
            , Id "Program"
            , [ Method ([ MPublic; MStatic ], TypeVoid, Id "Main", Params [], SBlock [])
              ; Method
                  ( [ MPublic ]
                  , TypeBase TypeInt
                  , Id "Factorial"
                  , Params [ Var (TypeVar (TypeBase TypeInt), Id "n") ]
                  , SBlock
                      [ SIf
                          ( EBinOp (OpEqual, EId (Id "n"), EValue (ValInt 0))
                          , SBlock [ SReturn (Some (EValue (ValInt 1))) ]
                          , Some
                              (SBlock
                                 [ SReturn
                                     (Some
                                        (EBinOp
                                           ( OpMul
                                           , EId (Id "n")
                                           , EFuncCall
                                               ( EId (Id "Factorial")
                                               , [ EBinOp
                                                     ( OpSub
                                                     , EId (Id "n")
                                                     , EValue (ValInt 1) )
                                                 ] ) )))
                                 ]) )
                      ] )
              ] )))
;;
