(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Pp
open TopLevel

let%expect_test "file with one var decl with ws" =
  pp pp_file parse_file {|

    /* hello */  var a int
// hey

|};
  [%expect
    {|
    [(Decl_var
        (Long_decl_mult_init ((Some Type_int),
           [("a", (Expr_const (Const_int 0)))])))
      ] |}]
;;

let%expect_test "file with multiple var decls separated by semicolon" =
  pp pp_file parse_file {|var a, b int;var c = "hello"|};
  [%expect
    {|
    [(Decl_var
        (Long_decl_mult_init ((Some Type_int),
           [("a", (Expr_const (Const_int 0))); ("b", (Expr_const (Const_int 0)))]
           )));
      (Decl_var
         (Long_decl_mult_init (None, [("c", (Expr_const (Const_string "hello")))]
            )))
      ] |}]
;;

let%expect_test "file with one simple func decl" =
  pp pp_file parse_file {|func _() {}|};
  [%expect {|
    [(Decl_func ("_", { args = []; returns = None; body = [] }))] |}]
;;

let%expect_test "file with one default func decl" =
  pp pp_file parse_file {|func sum3(a, b, c int) int {
        return a + b + c
  }|};
  [%expect
    {|
    [(Decl_func
        ("sum3",
         { args = [("a", Type_int); ("b", Type_int); ("c", Type_int)];
           returns = (Some (Only_types [Type_int]));
           body =
           [(Stmt_return
               [(Expr_bin_oper (Bin_sum,
                   (Expr_bin_oper (Bin_sum, (Expr_ident "a"), (Expr_ident "b"))),
                   (Expr_ident "c")))
                 ])
             ]
           }))
      ] |}]
;;

let%expect_test "file with one complex func decl" =
  pp
    pp_file
    parse_file
    {|func test(a, b int, c string) (sum int, c string) {
        sum = a + b
        s = c
        return
  }|};
  [%expect
    {|
    [(Decl_func
        ("test",
         { args = [("a", Type_int); ("b", Type_int); ("c", Type_string)];
           returns =
           (Some (Ident_and_types [("sum", Type_int); ("c", Type_string)]));
           body =
           [(Stmt_assign
               (Assign_mult_expr
                  [((Lvalue_ident "sum"),
                    (Expr_bin_oper (Bin_sum, (Expr_ident "a"), (Expr_ident "b"))))
                    ]));
             (Stmt_assign
                (Assign_mult_expr [((Lvalue_ident "s"), (Expr_ident "c"))]));
             (Stmt_return [])]
           }))
      ] |}]
;;

let%expect_test "file with mixed func and var decls" =
  pp
    pp_file
    parse_file
    {|

var a = 5

func test (
// hey
) (

/* hello */	) { 
	return 
}

func id(a int) (int) {
	return a
}

var f int

func main() {
	defer test()

go println(id(10))
}

|};
  [%expect
    {|
    [(Decl_var (Long_decl_mult_init (None, [("a", (Expr_const (Const_int 5)))])));
      (Decl_func
         ("test", { args = []; returns = None; body = [(Stmt_return [])] }));
      (Decl_func
         ("id",
          { args = [("a", Type_int)]; returns = (Some (Only_types [Type_int]));
            body = [(Stmt_return [(Expr_ident "a")])] }));
      (Decl_var
         (Long_decl_mult_init ((Some Type_int),
            [("f", (Expr_const (Const_int 0)))])));
      (Decl_func
         ("main",
          { args = []; returns = None;
            body =
            [(Stmt_defer ((Expr_ident "test"), []));
              (Stmt_go
                 ((Expr_ident "println"),
                  [(Expr_call ((Expr_ident "id"), [(Expr_const (Const_int 10))]))
                    ]))
              ]
            }))
      ] |}]
;;

let%expect_test "file with factorial func" =
  pp
    pp_file
    parse_file
    {|
  
  func fac(n int) int {
      if n == 1 {
          return 1  
      } else {
          return n * fac(n - 1)
      }
  }|};
  [%expect
    {|
    [(Decl_func
        ("fac",
         { args = [("n", Type_int)]; returns = (Some (Only_types [Type_int]));
           body =
           [Stmt_if {init = None;
              cond =
              (Expr_bin_oper (Bin_equal, (Expr_ident "n"),
                 (Expr_const (Const_int 1))));
              if_body = [(Stmt_return [(Expr_const (Const_int 1))])];
              else_body =
              (Some (Stmt_block
                       [(Stmt_return
                           [(Expr_bin_oper (Bin_multiply, (Expr_ident "n"),
                               (Expr_call
                                  ((Expr_ident "fac"),
                                   [(Expr_bin_oper (Bin_subtract,
                                       (Expr_ident "n"),
                                       (Expr_const (Const_int 1))))
                                     ]))
                               ))
                             ])
                         ]))}
             ]
           }))
      ] |}]
;;
