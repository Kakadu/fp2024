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
                      ( Expr_apply (Expr_ident "<=", Expr_const (Const_int 1))
                      , Expr_ident "n" )
                  , Expr_const (Const_int 1)
                  , Some
                      (Expr_apply
                         ( Expr_apply
                             ( Expr_ident "*"
                             , Expr_apply (Expr_ident "factorial", Expr_ident "n-1") )
                         , Expr_ident "n" )) ) )
          ] )
    ]
  in
  print_endline (show_program factorial_ast)
;;
