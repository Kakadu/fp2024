(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Printer

let%expect_test "ident" =
  print_endline (print_ident "therain17");
  [%expect {| therain17 |}]
;;

(********** type **********)

let%expect_test "type int" =
  print_endline (print_type Type_int);
  [%expect {| int |}]
;;

let%expect_test "type string" =
  print_endline (print_type Type_string);
  [%expect {| string |}]
;;

let%expect_test "type bool" =
  print_endline (print_type Type_bool);
  [%expect {| bool |}]
;;

let%expect_test "type simple array" =
  print_endline (print_type (Type_array (5, Type_int)));
  [%expect {| [5]int |}]
;;

let%expect_test "type array of arrays" =
  print_endline (print_type (Type_array (5, Type_array (5, Type_int))));
  [%expect {|[5][5]int|}]
;;

let%expect_test "type simple func" =
  print_endline (print_type (Type_func ([], [])));
  [%expect {| func() |}]
;;

let%expect_test "type complex func" =
  print_endline
    (print_type
       (Type_func
          ([ Type_bool; Type_func ([], []) ], [ Type_array (0, Type_string); Type_int ])));
  [%expect {| func(bool, func()) ([0]string, int) |}]
;;

let%expect_test "type bidirectional channel" =
  print_endline (print_type (Type_chan (Chan_bidirectional, Type_int)));
  [%expect {| chan int |}]
;;

let%expect_test "type bidirectional channel of receive-only channel" =
  print_endline
    (print_type (Type_chan (Chan_bidirectional, Type_chan (Chan_receive, Type_int))));
  [%expect {| chan (<-chan int) |}]
;;

let%expect_test "type receive-only channel" =
  print_endline (print_type (Type_chan (Chan_receive, Type_string)));
  [%expect {| <-chan string |}]
;;

let%expect_test "type send-only channel" =
  print_endline (print_type (Type_chan (Chan_send, Type_string)));
  [%expect {| chan<- string |}]
;;

(********** expr **********)

(*** Const ***)

let%expect_test "expr const int" =
  print_endline (print_expr (Expr_const (Const_int 10)));
  [%expect {| 10 |}]
;;

let%expect_test "expr const string" =
  print_endline (print_expr (Expr_const (Const_string "hello")));
  [%expect {| "hello" |}]
;;

let%expect_test "expr const empty string" =
  print_endline (print_expr (Expr_const (Const_string "")));
  [%expect {| "" |}]
;;

let%expect_test "expr const with escaped backslash" =
  print_endline (print_expr (Expr_const (Const_string "\\")));
  [%expect {| "\\" |}]
;;

let%expect_test "expr const with escaped quote" =
  print_endline (print_expr (Expr_const (Const_string "\"")));
  [%expect {| "\"" |}]
;;

let%expect_test "expr const with newline" =
  print_endline (print_expr (Expr_const (Const_string "\n")));
  [%expect {| "\n" |}]
;;

let%expect_test "expr const empty array" =
  print_endline (print_expr (Expr_const (Const_array (3, Type_int, []))));
  [%expect {| [3]int{} |}]
;;

let%expect_test "expr const array with init" =
  print_endline
    (print_expr
       (Expr_const
          (Const_array
             (3, Type_int, [ Expr_const (Const_int 1); Expr_const (Const_int 2) ]))));
  [%expect {| [3]int{1, 2} |}]
;;

let%expect_test "expr empty anon func" =
  print_endline
    (print_expr (Expr_const (Const_func { args = []; returns = None; body = [] })));
  [%expect {| func() {} |}]
;;

let%expect_test "expr anon func with one arg and one return value" =
  print_endline
    (print_expr
       (Expr_const
          (Const_func
             { args = [ "a", Type_int ]
             ; returns = Some (Only_types (Type_int, []))
             ; body = [ Stmt_return [ Expr_ident "a" ] ]
             })));
  [%expect {|
    func(a int) int {
        return a
    } |}]
;;

let%expect_test "expr anon func with mult args and return values" =
  print_endline
    (print_expr
       (Expr_const
          (Const_func
             { args = [ "a", Type_int; "b", Type_string ]
             ; returns = Some (Only_types (Type_int, [ Type_string ]))
             ; body = [ Stmt_return [ Expr_ident "a"; Expr_ident "b" ] ]
             })));
  [%expect {|
    func(a int, b string) (int, string) {
        return a, b
    } |}]
;;

let%expect_test "expr anon func with mult args and named return values" =
  print_endline
    (print_expr
       (Expr_const
          (Const_func
             { args = [ "a", Type_int; "b", Type_string ]
             ; returns =
                 Some (Ident_and_types (("res1", Type_int), [ "res2", Type_string ]))
             ; body =
                 [ Stmt_assign
                     (Assign_mult_expr
                        ( (Lvalue_ident "res1", Expr_ident "a")
                        , [ Lvalue_ident "res2", Expr_ident "b" ] ))
                 ; Stmt_return []
                 ]
             })));
  [%expect
    {|
    func(a int, b string) (res1 int, res2 string) {
        res1, res2 = a, b
        return
    } |}]
;;

(*** unary op ***)

let%expect_test "expr unary plus" =
  print_endline (print_expr (Expr_un_oper (Unary_plus, Expr_const (Const_int 5))));
  [%expect {| +5 |}]
;;

let%expect_test "expr unary minus" =
  print_endline (print_expr (Expr_un_oper (Unary_minus, Expr_const (Const_int 5))));
  [%expect {| -5 |}]
;;

let%expect_test "expr unary not" =
  print_endline (print_expr (Expr_un_oper (Unary_not, Expr_ident "t")));
  [%expect {| !t |}]
;;

let%expect_test "expr chan receive" =
  print_endline (print_expr (Expr_chan_receive (Expr_ident "c")));
  [%expect {| <-c |}]
;;

let%expect_test "expr chan receive from complex expr" =
  print_endline
    (print_expr
       (Expr_chan_receive (Expr_bin_oper (Bin_sum, Expr_ident "a", Expr_ident "b"))));
  [%expect {| <-(a + b) |}]
;;

let%expect_test "expr multiple unary operators" =
  print_endline
    (print_expr
       (Expr_un_oper
          ( Unary_minus
          , Expr_un_oper
              ( Unary_plus
              , Expr_un_oper
                  ( Unary_not
                  , Expr_un_oper
                      ( Unary_minus
                      , Expr_un_oper
                          ( Unary_minus
                          , Expr_un_oper
                              (Unary_not, Expr_un_oper (Unary_plus, Expr_ident "t")) ) )
                  ) ) )));
  [%expect {| -+!--!+t |}]
;;

(*** bin op ***)

let%expect_test "expr bin sum" =
  print_endline
    (print_expr (Expr_bin_oper (Bin_sum, Expr_const (Const_int 4), Expr_ident "i")));
  [%expect {| 4 + i |}]
;;

let%expect_test "expr bin subtraction" =
  print_endline
    (print_expr (Expr_bin_oper (Bin_subtract, Expr_ident "a", Expr_const (Const_int 5))));
  [%expect {| a - 5 |}]
;;

let%expect_test "expr bin multiplication" =
  print_endline
    (print_expr (Expr_bin_oper (Bin_multiply, Expr_ident "t", Expr_const (Const_int 5))));
  [%expect {| t * 5 |}]
;;

let%expect_test "expr bin division" =
  print_endline
    (print_expr (Expr_bin_oper (Bin_divide, Expr_ident "t", Expr_const (Const_int 5))));
  [%expect {| t / 5 |}]
;;

let%expect_test "expr bin equality" =
  print_endline
    (print_expr (Expr_bin_oper (Bin_equal, Expr_ident "t", Expr_const (Const_int 5))));
  [%expect {| t == 5 |}]
;;

let%expect_test "expr bin unequality" =
  print_endline
    (print_expr (Expr_bin_oper (Bin_not_equal, Expr_ident "t", Expr_const (Const_int 5))));
  [%expect {| t != 5 |}]
;;

let%expect_test "expr bin greater" =
  print_endline
    (print_expr (Expr_bin_oper (Bin_greater, Expr_ident "t", Expr_const (Const_int 5))));
  [%expect {| t > 5 |}]
;;

let%expect_test "expr bin greater or equal" =
  print_endline
    (print_expr
       (Expr_bin_oper (Bin_greater_equal, Expr_ident "t", Expr_const (Const_int 5))));
  [%expect {| t >= 5 |}]
;;

let%expect_test "expr bin less" =
  print_endline
    (print_expr (Expr_bin_oper (Bin_greater, Expr_ident "t", Expr_const (Const_int 5))));
  [%expect {| t > 5 |}]
;;

let%expect_test "expr bin less or equal" =
  print_endline
    (print_expr
       (Expr_bin_oper (Bin_less_equal, Expr_ident "t", Expr_const (Const_int 5))));
  [%expect {| t <= 5 |}]
;;

let%expect_test "expr bin and" =
  print_endline
    (print_expr (Expr_bin_oper (Bin_and, Expr_ident "t", Expr_const (Const_int 5))));
  [%expect {| t && 5 |}]
;;

let%expect_test "expr bin or" =
  print_endline
    (print_expr (Expr_bin_oper (Bin_or, Expr_ident "t", Expr_const (Const_int 5))));
  [%expect {| t || 5 |}]
;;

let%expect_test "expr arithmetic expression" =
  print_endline
    (print_expr
       (Expr_bin_oper
          ( Bin_divide
          , Expr_un_oper
              ( Unary_minus
              , Expr_bin_oper (Bin_sum, Expr_const (Const_int 5), Expr_const (Const_int 2))
              )
          , Expr_un_oper
              ( Unary_plus
              , Expr_un_oper
                  ( Unary_minus
                  , Expr_bin_oper
                      (Bin_sum, Expr_const (Const_int 2), Expr_const (Const_int 5)) ) ) )));
  [%expect {| -(5 + 2) / +-(2 + 5) |}]
;;

(*** func call ***)

let%expect_test "expr simple func call" =
  print_endline (print_expr (Expr_call (Expr_ident "a", [])));
  [%expect {| a() |}]
;;

let%expect_test "expr func call with multiple complex arguments" =
  print_endline
    (print_expr
       (Expr_call
          ( Expr_ident "three"
          , [ Expr_ident "abc"
            ; Expr_bin_oper (Bin_sum, Expr_const (Const_int 2), Expr_const (Const_int 3))
            ; Expr_call (Expr_ident "fac", [ Expr_const (Const_int 25) ])
            ] )));
  [%expect {| three(abc, 2 + 3, fac(25)) |}]
;;

let%expect_test "expr func call with array as a function" =
  print_endline
    (print_expr
       (Expr_call
          ( Expr_index (Expr_ident "funcs", Expr_const (Const_int 1))
          , [ Expr_ident "a"; Expr_ident "b" ] )));
  [%expect {| funcs[1](a, b) |}]
;;

let%expect_test "expr nested func call" =
  print_endline
    (print_expr (Expr_call (Expr_call (Expr_call (Expr_ident "a", []), []), [])));
  [%expect {| a()()() |}]
;;

(*** array index ***)

let%expect_test "expr index with constant array" =
  print_endline
    (print_expr
       (Expr_index
          ( Expr_const
              (Const_array
                 ( 3
                 , Type_int
                 , [ Expr_const (Const_int 1)
                   ; Expr_const (Const_int 2)
                   ; Expr_const (Const_int 3)
                   ] ))
          , Expr_const (Const_int 0) )));
  [%expect {| [3]int{1, 2, 3}[0] |}]
;;

let%expect_test "expr index with function call as an array" =
  print_endline
    (print_expr
       (Expr_index
          ( Expr_call (Expr_ident "get_array", [ Expr_ident "a"; Expr_ident "b" ])
          , Expr_const (Const_int 1) )));
  [%expect {| get_array(a, b)[1] |}]
;;

let%expect_test "expr nested indicies" =
  print_endline
    (print_expr
       (Expr_index
          ( Expr_index
              ( Expr_index (Expr_ident "a", Expr_const (Const_int 1))
              , Expr_const (Const_int 2) )
          , Expr_const (Const_int 3) )));
  [%expect {| a[1][2][3] |}]
;;

let%expect_test "expr check bin operators precedence" =
  print_endline
    (print_expr
       (Expr_bin_oper
          ( Bin_or
          , Expr_bin_oper
              ( Bin_greater_equal
              , Expr_bin_oper
                  ( Bin_sum
                  , Expr_const (Const_int 1)
                  , Expr_bin_oper
                      (Bin_multiply, Expr_const (Const_int 2), Expr_const (Const_int 3))
                  )
              , Expr_bin_oper
                  ( Bin_subtract
                  , Expr_un_oper (Unary_minus, Expr_const (Const_int 1))
                  , Expr_bin_oper
                      ( Bin_divide
                      , Expr_chan_receive (Expr_ident "a")
                      , Expr_const (Const_int 2) ) ) )
          , Expr_bin_oper (Bin_and, Expr_ident "true", Expr_call (Expr_ident "check", []))
          )));
  [%expect {| 1 + 2 * 3 >= -1 - <-a / 2 || true && check() |}]
;;

let%expect_test "expr check bin operators precedence with parens" =
  print_endline
    (print_expr
       (Expr_bin_oper
          ( Bin_multiply
          , Expr_bin_oper (Bin_sum, Expr_const (Const_int 1), Expr_const (Const_int 2))
          , Expr_un_oper
              ( Unary_plus
              , Expr_bin_oper
                  ( Bin_equal
                  , Expr_bin_oper
                      ( Bin_or
                      , Expr_const (Const_int 3)
                      , Expr_bin_oper
                          ( Bin_subtract
                          , Expr_const (Const_int 2)
                          , Expr_bin_oper
                              ( Bin_divide
                              , Expr_call (Expr_ident "a", [])
                              , Expr_const (Const_int 4) ) ) )
                  , Expr_bin_oper (Bin_and, Expr_ident "true", Expr_ident "false") ) ) )));
  [%expect {| (1 + 2) * +((3 || 2 - a() / 4) == (true && false)) |}]
;;

(********** stmt **********)

(*** break and continue ***)

let%expect_test "stmt break" =
  print_endline (print_stmt Stmt_break);
  [%expect {| break |}]
;;

let%expect_test "stmt continue" =
  print_endline (print_stmt Stmt_continue);
  [%expect {| continue |}]
;;

(*** chan send and receive ***)

let%expect_test "stmt chan send" =
  print_endline
    (print_stmt
       (Stmt_chan_send
          ("c", Expr_bin_oper (Bin_sum, Expr_ident "sum", Expr_const (Const_int 1)))));
  [%expect {| c <- sum + 1 |}]
;;

let%expect_test "stmt chan receive" =
  print_endline (print_stmt (Stmt_chan_receive (Expr_ident "c")));
  [%expect {| <-c |}]
;;

(*** incr and decr ***)

let%expect_test "stmt incr" =
  print_endline (print_stmt (Stmt_incr "a"));
  [%expect {| a++ |}]
;;

let%expect_test "stmt decr" =
  print_endline (print_stmt (Stmt_decr "a"));
  [%expect {| a-- |}]
;;

(*** return ***)

let%expect_test "stmt empty return" =
  print_endline (print_stmt (Stmt_return []));
  [%expect {| return |}]
;;

let%expect_test "stmt return with one expr" =
  print_endline (print_stmt (Stmt_return [ Expr_const (Const_int 5) ]));
  [%expect {| return 5 |}]
;;

let%expect_test "stmt return with multiple exprs" =
  print_endline
    (print_stmt
       (Stmt_return
          [ Expr_bin_oper
              ( Bin_sum
              , Expr_bin_oper
                  ( Bin_multiply
                  , Expr_un_oper (Unary_minus, Expr_const (Const_int 5))
                  , Expr_ident "_r" )
              , Expr_const (Const_int 8) )
          ; Expr_bin_oper
              ( Bin_and
              , Expr_un_oper (Unary_not, Expr_ident "a")
              , Expr_bin_oper (Bin_or, Expr_ident "b", Expr_ident "c") )
          ]));
  [%expect {| return -5 * _r + 8, !a && (b || c) |}]
;;

(*** func call, go, defer ***)

let%expect_test "stmt func call" =
  print_endline (print_stmt (Stmt_call (Expr_ident "a", [ Expr_ident "b" ])));
  [%expect {| a(b) |}]
;;

let%expect_test "stmt go" =
  print_endline (print_stmt (Stmt_go (Expr_ident "a", [ Expr_ident "b" ])));
  [%expect {| go a(b) |}]
;;

let%expect_test "stmt defer" =
  print_endline (print_stmt (Stmt_defer (Expr_ident "a", [ Expr_ident "b" ])));
  [%expect {| defer a(b) |}]
;;

(*** long decl ***)

let%expect_test "stmt long decl single var no init" =
  print_endline
    (print_stmt
       (Stmt_long_var_decl
          (Long_decl_no_init
             (Type_array (2, Type_array (3, Type_array (1, Type_bool))), "a", []))));
  [%expect {| var a [2][3][1]bool |}]
;;

let%expect_test "stmt long decl mult var no init" =
  print_endline
    (print_stmt (Stmt_long_var_decl (Long_decl_no_init (Type_string, "a", [ "b"; "c" ]))));
  [%expect {| var a, b, c string |}]
;;

let%expect_test "stmt long decl single var no type with init" =
  print_endline
    (print_stmt
       (Stmt_long_var_decl
          (Long_decl_mult_init (None, ("a", Expr_const (Const_int 5)), []))));
  [%expect {| var a = 5 |}]
;;

let%expect_test "stmt long decl multiple var no type with init" =
  print_endline
    (print_stmt
       (Stmt_long_var_decl
          (Long_decl_mult_init
             ( None
             , ("a", Expr_const (Const_int 5))
             , [ "b", Expr_ident "nil"; "c", Expr_const (Const_string "hi") ] ))));
  [%expect {| var a, b, c = 5, nil, "hi" |}]
;;

let%expect_test "stmt long decl one var with type with init" =
  print_endline
    (print_stmt
       (Stmt_long_var_decl
          (Long_decl_mult_init
             ( Some (Type_func ([], []))
             , ("a", Expr_const (Const_func { args = []; returns = None; body = [] }))
             , [] ))));
  [%expect {| var a func() = func() {} |}]
;;

let%expect_test "stmt long decl mult var with type with init" =
  print_endline
    (print_stmt
       (Stmt_long_var_decl
          (Long_decl_mult_init
             ( Some Type_int
             , ("a", Expr_const (Const_int 2))
             , [ "b", Expr_const (Const_int 3) ] ))));
  [%expect {| var a, b int = 2, 3 |}]
;;

let%expect_test "stmt long decl mult var no type with one init" =
  print_endline
    (print_stmt
       (Stmt_long_var_decl
          (Long_decl_one_init
             ( None
             , "a"
             , "b"
             , [ "c" ]
             , ( Expr_ident "get_three"
               , [ Expr_const (Const_int 1)
                 ; Expr_const (Const_int 2)
                 ; Expr_const (Const_int 3)
                 ] ) ))));
  [%expect {| var a, b, c  = get_three(1, 2, 3) |}]
;;

let%expect_test "stmt long decl mult var with type with one init" =
  print_endline
    (print_stmt
       (Stmt_long_var_decl
          (Long_decl_one_init
             ( Some (Type_chan (Chan_receive, Type_array (5, Type_int)))
             , "a"
             , "b"
             , [ "c" ]
             , (Expr_ident "get", []) ))));
  [%expect {| var a, b, c <-chan [5]int = get() |}]
;;

(*** short decl ***)

let%expect_test "stmt short single var decl" =
  print_endline
    (print_stmt
       (Stmt_short_var_decl (Short_decl_mult_init (("a", Expr_const (Const_int 7)), []))));
  [%expect {| a := 7 |}]
;;

let%expect_test "stmt short mult var decl" =
  print_endline
    (print_stmt
       (Stmt_short_var_decl
          (Short_decl_mult_init
             ( ("a", Expr_ident "true")
             , [ "b", Expr_const (Const_int 567)
               ; "c", Expr_const (Const_string "string")
               ] ))));
  [%expect {| a, b, c := true, 567, "string" |}]
;;

let%expect_test "stmt short var decl mult var and one init" =
  print_endline
    (print_stmt
       (Stmt_short_var_decl
          (Short_decl_one_init
             ( "a"
             , "b"
             , [ "c" ]
             , ( Expr_ident "three"
               , [ Expr_ident "abc"
                 ; Expr_bin_oper
                     (Bin_sum, Expr_const (Const_int 2), Expr_const (Const_int 3))
                 ; Expr_call (Expr_ident "fac", [ Expr_const (Const_int 25) ])
                 ] ) ))));
  [%expect {| a, b, c := three(abc, 2 + 3, fac(25)) |}]
;;

(*** assign ***)

let%expect_test "stmt assign one ident lvalue, one rvalue" =
  print_endline
    (print_stmt
       (Stmt_assign (Assign_mult_expr ((Lvalue_ident "a", Expr_const (Const_int 5)), []))));
  [%expect {| a = 5 |}]
;;

let%expect_test "stmt assign one lvalue that is an array index, one rvalue" =
  print_endline
    (print_stmt
       (Stmt_assign
          (Assign_mult_expr
             ( ( Lvalue_array_index
                   ( Lvalue_array_index (Lvalue_ident "a", Expr_ident "i")
                   , Expr_bin_oper
                       (Bin_sum, Expr_const (Const_int 2), Expr_const (Const_int 3)) )
               , Expr_const (Const_int 5) )
             , [] ))));
  [%expect {| a[i][2 + 3] = 5 |}]
;;

let%expect_test "stmt assign with mult lvalues and rvalues" =
  print_endline
    (print_stmt
       (Stmt_assign
          (Assign_mult_expr
             ( (Lvalue_ident "a", Expr_const (Const_int 5))
             , [ Lvalue_ident "b", Expr_ident "true"
               ; ( Lvalue_array_index
                     (Lvalue_ident "c", Expr_call (Expr_ident "get_index", []))
                 , Expr_const (Const_string "hello") )
               ] ))));
  [%expect {| a, b, c[get_index()] = 5, true, "hello" |}]
;;

let%expect_test "stmt assign mult lvalues and one rvalue" =
  print_endline
    (print_stmt
       (Stmt_assign
          (Assign_one_expr
             ( Lvalue_ident "a"
             , Lvalue_array_index (Lvalue_ident "b", Expr_const (Const_int 0))
             , [ Lvalue_ident "c" ]
             , (Expr_ident "get_three", []) ))));
  [%expect {| a, b[0], c = get_three() |}]
;;

(*** if ***)

let%expect_test "stmt simple if no init" =
  print_endline
    (print_stmt
       (Stmt_if { init = None; cond = Expr_ident "true"; if_body = []; else_body = None }));
  [%expect {| if true {} |}]
;;

let%expect_test "stmt if with init" =
  print_endline
    (print_stmt
       (Stmt_if
          { init =
              Some
                (Init_decl (Short_decl_mult_init (("k", Expr_const (Const_int 0)), [])))
          ; cond = Expr_bin_oper (Bin_equal, Expr_ident "k", Expr_ident "test")
          ; if_body = []
          ; else_body = None
          }));
  [%expect {| if k := 0; k == test {} |}]
;;

let%expect_test "stmt if with else that is a block" =
  print_endline
    (print_stmt
       (Stmt_if
          { init = None
          ; cond = Expr_ident "cond"
          ; if_body = []
          ; else_body = Some (Else_block [])
          }));
  [%expect {| if cond {} else {} |}]
;;

let%expect_test "stmt if with else that is another if" =
  print_endline
    (print_stmt
       (Stmt_if
          { init = None
          ; cond = Expr_ident "cond"
          ; if_body = []
          ; else_body =
              Some
                (Else_if
                   { init = None
                   ; cond = Expr_ident "cond2"
                   ; if_body = []
                   ; else_body = None
                   })
          }));
  [%expect {| if cond {} else if cond2 {} |}]
;;

(*** for ***)

let%expect_test "stmt empty for" =
  print_endline
    (print_stmt (Stmt_for { init = None; cond = None; post = None; body = [] }));
  [%expect {| for {} |}]
;;

let%expect_test "stmt for with only condition" =
  print_endline
    (print_stmt
       (Stmt_for
          { init = None
          ; cond =
              Some (Expr_bin_oper (Bin_greater, Expr_ident "a", Expr_const (Const_int 0)))
          ; post = None
          ; body = []
          }));
  [%expect {| for a > 0 {} |}]
;;

let%expect_test "stmt for with init, cond and post" =
  print_endline
    (print_stmt
       (Stmt_for
          { init =
              Some
                (Init_decl (Short_decl_mult_init (("i", Expr_const (Const_int 0)), [])))
          ; cond =
              Some (Expr_bin_oper (Bin_less, Expr_ident "i", Expr_const (Const_int 10)))
          ; post = Some (Init_incr "i")
          ; body = []
          }));
  [%expect {| for i := 0; i < 10; i++ {} |}]
;;

(*** block ***)

let%expect_test "stmt empty block" =
  print_endline (print_stmt (Stmt_block []));
  [%expect {| {} |}]
;;

let%expect_test "stmt block of one stmt" =
  print_endline
    (print_stmt
       (Stmt_block
          [ Stmt_short_var_decl
              (Short_decl_mult_init (("a", Expr_const (Const_int 5)), []))
          ]));
  [%expect {|
    {
        a := 5
    } |}]
;;

let%expect_test "stmt block of mult stmts" =
  print_endline
    (print_stmt
       (Stmt_block
          [ Stmt_short_var_decl
              (Short_decl_mult_init (("a", Expr_const (Const_int 5)), []))
          ; Stmt_incr "a"
          ; Stmt_call (Expr_ident "println", [ Expr_ident "a" ])
          ]));
  [%expect {|
    {
        a := 5
        a++
        println(a)
    } |}]
;;

(********** file **********)

let%expect_test "file with simple func decl" =
  print_endline
    (print_file
       [ Decl_func
           ( "main"
           , { args = [ "a", Type_int ]
             ; returns = Some (Only_types (Type_bool, []))
             ; body = []
             } )
       ]);
  [%expect {| func main(a int) bool {} |}]
;;

let%expect_test "file with multiple declarations" =
  print_endline
    (print_file
       [ Decl_var (Long_decl_no_init (Type_int, "x", []))
       ; Decl_func ("main", { args = []; returns = None; body = [] })
       ]);
  [%expect {|
    var x int

    func main() {} |}]
;;

let%expect_test "file with factorial func" =
  print_endline
    (print_file
       [ Decl_func
           ( "fac"
           , { args = [ "n", Type_int ]
             ; returns = Some (Only_types (Type_int, []))
             ; body =
                 [ Stmt_if
                     { init = None
                     ; cond =
                         Expr_bin_oper
                           (Bin_equal, Expr_ident "n", Expr_const (Const_int 1))
                     ; if_body = [ Stmt_return [ Expr_const (Const_int 1) ] ]
                     ; else_body =
                         Some
                           (Else_block
                              [ Stmt_return
                                  [ Expr_bin_oper
                                      ( Bin_multiply
                                      , Expr_ident "n"
                                      , Expr_call
                                          ( Expr_ident "fac"
                                          , [ Expr_bin_oper
                                                ( Bin_subtract
                                                , Expr_ident "n"
                                                , Expr_const (Const_int 1) )
                                            ] ) )
                                  ]
                              ])
                     }
                 ]
             } )
       ]);
  [%expect
    {|
    func fac(n int) int {
        if n == 1 {
            return 1
        } else {
            return n * fac(n - 1)
        }
    } |}]
;;
