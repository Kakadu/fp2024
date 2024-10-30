open Ast

(*
   let rec factorial n =
   if n <= 1 then 1
   else n * factorial (n-1)
*)

let () =
  let factorial_ast =
    [ Str_item_def
        ( Recursive
        , [ Binding
              ( Pattern_ident "factorial"
              , Expr_ifthenelse
                  ( Expr_apply
                      ( Expr_apply (Expr_ident_or_op "<=", Expr_const (Const_int 1))
                      , Expr_ident_or_op "n" )
                  , Expr_const (Const_int 1)
                  , Some
                      (Expr_apply
                         ( Expr_apply
                             ( Expr_ident_or_op "*"
                             , Expr_apply
                                 (Expr_ident_or_op "factorial", Expr_ident_or_op "n-1") )
                         , Expr_ident_or_op "n" )) ) )
          ] )
    ]
  in
  print_endline (show_program factorial_ast)
;;
