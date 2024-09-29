open Ast

let test =
        ( Recursive
        , Binding(Pattern_ident "factorial"
            , Expr_fun
                  ( Pattern_ident "n"
                  , Expr_ifthenelse
                      ( Expr_apply
                          ( Expr_apply (Expr_ident "<=", Expr_ident "n")
                          , Expr_const (Const_int 1) )
                      , Expr_const (Const_int 1)
                      , Some
                          (Expr_apply
                             ( Expr_apply (Expr_ident "*", Expr_ident "n")
                             , Expr_apply
                                 ( Expr_ident "factorial"
                                 , Expr_apply
                                     ( Expr_apply (Expr_ident "-", Expr_ident "n")
                                     , Expr_const (Const_int 1) ) ) )) ) )
         ) )
;;
