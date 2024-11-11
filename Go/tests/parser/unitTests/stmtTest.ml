(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Parser.Stmt
open Pp

let pstmt = parse_stmt parse_block

(********** break, continue, go, defer and channel send **********)

let%expect_test "break stmt" =
  pp pp_stmt pstmt {|break|};
  [%expect {| Stmt_break |}]
;;

let%expect_test "continue stmt" =
  pp pp_stmt pstmt {|continue|};
  [%expect {| Stmt_continue |}]
;;

let%expect_test "stmt defer with func" =
  pp pp_stmt pstmt {|defer 
                      call(abc)|};
  [%expect {|
    (Stmt_defer ((Expr_ident "call"), [(Expr_ident "abc")])) |}]
;;

let%expect_test "stmt defer with expr that is not a func" =
  pp pp_stmt pstmt {|defer 2 + 2 * 5|};
  [%expect {|
    : Incorrect statement |}]
;;

let%expect_test "stmt go with func" =
  pp pp_stmt pstmt {|go 
                      call(abc)|};
  [%expect {|
    (Stmt_go ((Expr_ident "call"), [(Expr_ident "abc")])) |}]
;;

let%expect_test "stmt go with expr that is not a func" =
  pp pp_stmt pstmt {|go 2 + 2 * 5|};
  [%expect {|
    : Incorrect statement |}]
;;

let%expect_test "stmt chan send" =
  pp pp_stmt pstmt {|c <- sum + 1|};
  [%expect
    {|
    (Stmt_chan_send ("c",
       (Expr_bin_oper (Bin_sum, (Expr_ident "sum"), (Expr_const (Const_int 1))))
       )) |}]
;;

(********** incr and decr **********)

let%expect_test "incr stmt" =
  pp pp_stmt pstmt {|a++|};
  [%expect {| (Stmt_incr "a") |}]
;;

let%expect_test "incr stmt with ws_line" =
  pp pp_stmt pstmt {|a    /* some comment */   ++|};
  [%expect {| (Stmt_incr "a") |}]
;;

let%expect_test "incr stmt with blank ident" =
  pp pp_stmt pstmt {|_++|};
  [%expect {| (Stmt_incr "_") |}]
;;

let%expect_test "decr stmt" =
  pp pp_stmt pstmt {|a--|};
  [%expect {| (Stmt_decr "a") |}]
;;

let%expect_test "decr stmt with ws_line" =
  pp pp_stmt pstmt {|a    /* some comment */   --|};
  [%expect {| (Stmt_decr "a") |}]
;;

let%expect_test "decr stmt with blank ident" =
  pp pp_stmt pstmt {|_--|};
  [%expect {| (Stmt_decr "_") |}]
;;

(********** return **********)

let%expect_test "return without anything" =
  pp pp_stmt pstmt {|return|};
  [%expect {| (Stmt_return []) |}]
;;

let%expect_test "return with one expr" =
  pp pp_stmt pstmt {|return 5|};
  [%expect {| (Stmt_return [(Expr_const (Const_int 5))]) |}]
;;

let%expect_test "return with multiple exprs and ws" =
  pp
    pp_stmt
    pstmt
    {|return 3    ,   
             a  ,  // some comment 
             true /* RARAVARV */    ,  nil|};
  [%expect
    {|
    (Stmt_return
       [(Expr_const (Const_int 3)); (Expr_ident "a"); (Expr_ident "true");
         (Expr_ident "nil")]) |}]
;;

let%expect_test "return with multiple complex exprs" =
  pp pp_stmt pstmt {|return -5 * _r + 8, !a && (b || c)|};
  [%expect
    {|
    (Stmt_return
       [(Expr_bin_oper (Bin_sum,
           (Expr_bin_oper (Bin_multiply,
              (Expr_un_oper (Unary_minus, (Expr_const (Const_int 5)))),
              (Expr_ident "_r"))),
           (Expr_const (Const_int 8))));
         (Expr_bin_oper (Bin_and, (Expr_un_oper (Unary_not, (Expr_ident "a"))),
            (Expr_bin_oper (Bin_or, (Expr_ident "b"), (Expr_ident "c")))))
         ]) |}]
;;

(********** func call **********)

let%expect_test "stmt func call with one simple arg" =
  pp pp_stmt pstmt {|my_func(5)|};
  [%expect {| (Stmt_call ((Expr_ident "my_func"), [(Expr_const (Const_int 5))])) |}]
;;

let%expect_test "stmt func callmultiple args" =
  pp pp_stmt pstmt {|my_func(5, a, nil)|};
  [%expect
    {|
    (Stmt_call
       ((Expr_ident "my_func"),
        [(Expr_const (Const_int 5)); (Expr_ident "a"); (Expr_ident "nil")])) |}]
;;

let%expect_test "stmt func call with complex expressions and comments" =
  pp pp_stmt pstmt {|fac(   fac(2 + 2), 
  34 * 75,
  // aovnervo 
  !a)|};
  [%expect
    {|
    (Stmt_call
       ((Expr_ident "fac"),
        [(Expr_call
            ((Expr_ident "fac"),
             [(Expr_bin_oper (Bin_sum, (Expr_const (Const_int 2)),
                 (Expr_const (Const_int 2))))
               ]));
          (Expr_bin_oper (Bin_multiply, (Expr_const (Const_int 34)),
             (Expr_const (Const_int 75))));
          (Expr_un_oper (Unary_not, (Expr_ident "a")))])) |}]
;;

(********** assign **********)

let%expect_test "stmt assign one lvalue, one rvalue" =
  pp pp_stmt pstmt {|a = 5|};
  [%expect
    {|
    (Stmt_assign
       (Assign_mult_expr [((Lvalue_ident "a"), (Expr_const (Const_int 5)))])) |}]
;;

let%expect_test "stmt assign one lvalue that is an array index, one rvalue" =
  pp pp_stmt pstmt {|a[i][2 + 3] = 5|};
  [%expect
    {|
    (Stmt_assign
       (Assign_mult_expr
          [((Lvalue_array_index (
               (Lvalue_array_index ((Lvalue_ident "a"), (Expr_ident "i"))),
               (Expr_bin_oper (Bin_sum, (Expr_const (Const_int 2)),
                  (Expr_const (Const_int 3))))
               )),
            (Expr_const (Const_int 5)))])) |}]
;;

let%expect_test "stmt assign with mult equal number of lvalues and rvalues and ws" =
  pp
    pp_stmt
    pstmt
    {|a, 
  b , // comment
  c[get_index()] = 
  
  5, /* comment////// */true,
   "hello"|};
  [%expect
    {|
    (Stmt_assign
       (Assign_mult_expr
          [((Lvalue_ident "a"), (Expr_const (Const_int 5)));
            ((Lvalue_ident "b"), (Expr_ident "true"));
            ((Lvalue_array_index ((Lvalue_ident "c"),
                (Expr_call ((Expr_ident "get_index"), [])))),
             (Expr_const (Const_string "hello")))
            ])) |}]
;;

let%expect_test "stmt assign mult lvalues and one rvalue that is a func call" =
  pp pp_stmt pstmt {|a, b[0] ,c = get_three()|};
  [%expect
    {|
    (Stmt_assign
       (Assign_one_expr (
          [(Lvalue_ident "a");
            (Lvalue_array_index ((Lvalue_ident "b"), (Expr_const (Const_int 0))));
            (Lvalue_ident "c")],
          (Expr_call ((Expr_ident "get_three"), []))))) |}]
;;

let%expect_test "stmt assign mult lvalues and one rvalue that is not a func call" =
  pp pp_stmt pstmt {|a, b ,c = abc|};
  [%expect {| : Incorrect statement |}]
;;

let%expect_test "stmt assign mult unequal lvalues and rvalues" =
  pp pp_stmt pstmt {|a, b ,c = 2, 3, 4, 5 , 6|};
  [%expect {| : Incorrect statement |}]
;;

(********** long var decl **********)

let%expect_test "stmt long single var decl without init" =
  pp pp_stmt pstmt {|var a string|};
  [%expect {|
    (Stmt_long_var_decl (Long_decl_no_init (Type_string, ["a"]))) |}]
;;

let%expect_test "stmt long single var decl without init with mult array type" =
  pp pp_stmt pstmt {|var a [2][3][1]bool|};
  [%expect
    {|
    (Stmt_long_var_decl
       (Long_decl_no_init (
          (Type_array (2, (Type_array (3, (Type_array (1, Type_bool)))))),
          ["a"]))) |}]
;;

let%expect_test "stmt long single var decl no type" =
  pp pp_stmt pstmt {|var a = 5|};
  [%expect
    {|
    (Stmt_long_var_decl
       (Long_decl_mult_init (None, [("a", (Expr_const (Const_int 5)))]))) |}]
;;

let%expect_test "stmt long mult var decl no type" =
  pp pp_stmt pstmt {|var a, b, c = 5, nil, "hi"|};
  [%expect
    {|
    (Stmt_long_var_decl
       (Long_decl_mult_init (None,
          [("a", (Expr_const (Const_int 5))); ("b", (Expr_ident "nil"));
            ("c", (Expr_const (Const_string "hi")))]
          ))) |}]
;;

let%expect_test "stmt long single var decl with type" =
  pp pp_stmt pstmt {|var a func() = func() {}|};
  [%expect
    {|
    (Stmt_long_var_decl
       (Long_decl_mult_init ((Some (Type_func ([], []))),
          [("a",
            (Expr_const (Const_func { args = []; returns = None; body = [] })))]
          ))) |}]
;;

let%expect_test "stmt long mult var decl with type" =
  pp pp_stmt pstmt {|var a, b int = 2, 3|};
  [%expect
    {|
    (Stmt_long_var_decl
       (Long_decl_mult_init ((Some Type_int),
          [("a", (Expr_const (Const_int 2))); ("b", (Expr_const (Const_int 3)))]
          ))) |}]
;;

let%expect_test "stmt long mult var decl with type" =
  pp pp_stmt pstmt {|var a, b, c [2]int = [2]int{1, 2}, [2]int{}, [2]int{10, 20}|};
  [%expect
    {|
    (Stmt_long_var_decl
       (Long_decl_mult_init ((Some (Type_array (2, Type_int))),
          [("a",
            (Expr_const
               (Const_array (2, Type_int,
                  [(Expr_const (Const_int 1)); (Expr_const (Const_int 2))]))));
            ("b", (Expr_const (Const_array (2, Type_int, []))));
            ("c",
             (Expr_const
                (Const_array (2, Type_int,
                   [(Expr_const (Const_int 10)); (Expr_const (Const_int 20))]))))
            ]
          ))) |}]
;;

let%expect_test "stmt long mult var decl without type" =
  pp pp_stmt pstmt {|var a, b, c = 5, nil, "hi"|};
  [%expect
    {|
    (Stmt_long_var_decl
       (Long_decl_mult_init (None,
          [("a", (Expr_const (Const_int 5))); ("b", (Expr_ident "nil"));
            ("c", (Expr_const (Const_string "hi")))]
          ))) |}]
;;

let%expect_test "stmt long var decl mult lvalues and one rvalue that is a func call" =
  pp pp_stmt pstmt {|var a, b, c = get_three(1, 2, 3)|};
  [%expect
    {|
    (Stmt_long_var_decl
       (Long_decl_one_init (None, ["a"; "b"; "c"],
          (Expr_call
             ((Expr_ident "get_three"),
              [(Expr_const (Const_int 1)); (Expr_const (Const_int 2));
                (Expr_const (Const_int 3))]))
          ))) |}]
;;

let%expect_test "stmt long var decl mult lvalues and one rvalue that is not a func call" =
  pp pp_stmt pstmt {|var a, b, c = true|};
  [%expect {|
    : Incorrect statement |}]
;;

let%expect_test "stmt long var decl unequal lvalues and rvalues" =
  pp pp_stmt pstmt {|var a, b, c = 1, 2, 3, 4|};
  [%expect {|
    : Incorrect statement |}]
;;

(********** short var decl **********)

let%expect_test "stmt short single var decl" =
  pp pp_stmt pstmt {|a := 7|};
  [%expect
    {|
    (Stmt_short_var_decl
       (Short_decl_mult_init [("a", (Expr_const (Const_int 7)))])) |}]
;;

let%expect_test "stmt short mult var decl" =
  pp pp_stmt pstmt {|a, b, c := true, 567, "string"|};
  [%expect
    {|
    (Stmt_short_var_decl
       (Short_decl_mult_init
          [("a", (Expr_ident "true")); ("b", (Expr_const (Const_int 567)));
            ("c", (Expr_const (Const_string "string")))])) |}]
;;

let%expect_test "stmt short var decl mult lvalues and one rvalue that is a func call" =
  pp pp_stmt pstmt {|a, b, c := three(abc, 2 + 3, fac(25))|};
  [%expect
    {|
    (Stmt_short_var_decl
       (Short_decl_one_init (["a"; "b"; "c"],
          (Expr_call
             ((Expr_ident "three"),
              [(Expr_ident "abc");
                (Expr_bin_oper (Bin_sum, (Expr_const (Const_int 2)),
                   (Expr_const (Const_int 3))));
                (Expr_call ((Expr_ident "fac"), [(Expr_const (Const_int 25))]))]))
          ))) |}]
;;

let%expect_test "stmt short var decl mult lvalues and one rvalue that is not a func call" =
  pp pp_stmt pstmt {|a, b, c := abcdefg"|};
  [%expect {|
    : Incorrect statement |}]
;;

let%expect_test "stmt short var decl unequal lvalues and rvalues" =
  pp pp_stmt pstmt {|a, b, c := 1, 2, 3, 4|};
  [%expect {|
    : Incorrect statement |}]
;;

(********** block **********)

let%expect_test "stmt empty block" =
  pp pp_stmt pstmt {|{}|};
  [%expect {|
    (Stmt_block []) |}]
;;

let%expect_test "stmt block of one stmt" =
  pp pp_stmt pstmt {|{ a := 5 }|};
  [%expect
    {|
    (Stmt_block
       [(Stmt_short_var_decl
           (Short_decl_mult_init [("a", (Expr_const (Const_int 5)))]))
         ]) |}]
;;

let%expect_test "stmt block of mult stmts, separated by semicolon" =
  pp pp_stmt pstmt {|{ a := 5; a++; println(a) }|};
  [%expect
    {|
    (Stmt_block
       [(Stmt_short_var_decl
           (Short_decl_mult_init [("a", (Expr_const (Const_int 5)))]));
         (Stmt_incr "a");
         (Stmt_call ((Expr_ident "println"), [(Expr_ident "a")]))]) |}]
;;

let%expect_test "stmt block of mult stmts, separated by newlines" =
  pp
    pp_stmt
    pstmt
    {|{ var hi string = "hi"
    // string that says hi
      go get_int(hi)}|};
  [%expect
    {|
    (Stmt_block
       [(Stmt_long_var_decl
           (Long_decl_mult_init ((Some Type_string),
              [("hi", (Expr_const (Const_string "hi")))])));
         (Stmt_go ((Expr_ident "get_int"), [(Expr_ident "hi")]))]) |}]
;;

(********** if **********)

let%expect_test "stmt simple if" =
  pp pp_stmt pstmt {|if true {}|};
  [%expect
    {|
    Stmt_if {init = None; cond = (Expr_ident "true"); if_body = [];
      else_body = None} |}]
;;

let%expect_test "stmt if with init" =
  pp pp_stmt pstmt {|if k := 0; k == test {}|};
  [%expect
    {|
    Stmt_if {
      init =
      (Some (Stmt_short_var_decl
               (Short_decl_mult_init [("k", (Expr_const (Const_int 0)))])));
      cond = (Expr_bin_oper (Bin_equal, (Expr_ident "k"), (Expr_ident "test")));
      if_body = []; else_body = None} |}]
;;

let%expect_test "stmt if with empty init" =
  pp pp_stmt pstmt {|if ; call() {}|};
  [%expect
    {|
    Stmt_if {init = None; cond = (Expr_call ((Expr_ident "call"), []));
      if_body = []; else_body = None} |}]
;;

let%expect_test "stmt if with wrong init" =
  pp pp_stmt pstmt {|if var a = 5; cond {}|};
  [%expect
    {|
    : Incorrect statement |}]
;;

let%expect_test "stmt if with else that is a block" =
  pp pp_stmt pstmt {|if cond {} else {}|};
  [%expect
    {|
    Stmt_if {init = None; cond = (Expr_ident "cond"); if_body = [];
      else_body = (Some (Stmt_block []))} |}]
;;

let%expect_test "stmt if with else that is another if" =
  pp pp_stmt pstmt {|if cond {} else if cond2 {}|};
  [%expect
    {|
    Stmt_if {init = None; cond = (Expr_ident "cond"); if_body = [];
      else_body =
      (Some Stmt_if {init = None; cond = (Expr_ident "cond2"); if_body = [];
              else_body = None})} |}]
;;

let%expect_test "stmt if with wrong else" =
  pp pp_stmt pstmt {|if cond {} else do_smth()|};
  [%expect {|
    : Incorrect statement |}]
;;

(********** for **********)

let%expect_test "stmt empty for" =
  pp pp_stmt pstmt {|for {}|};
  [%expect {|
    Stmt_for {init = None; cond = None; post = None; body = []} |}]
;;

let%expect_test "stmt for with only conition" =
  pp pp_stmt pstmt {|for a > 0 {}|};
  [%expect
    {|
    Stmt_for {init = None;
      cond =
      (Some (Expr_bin_oper (Bin_greater, (Expr_ident "a"),
               (Expr_const (Const_int 0)))));
      post = None; body = []} |}]
;;

let%expect_test "stmt empty for with semicolons" =
  pp pp_stmt pstmt {|for ;; {}|};
  [%expect {|
    Stmt_for {init = None; cond = None; post = None; body = []} |}]
;;

let%expect_test "stmt simple for" =
  pp pp_stmt pstmt {|for i := 0; i < 10; i++ {}|};
  [%expect
    {|
    Stmt_for {
      init =
      (Some (Stmt_short_var_decl
               (Short_decl_mult_init [("i", (Expr_const (Const_int 0)))])));
      cond =
      (Some (Expr_bin_oper (Bin_less, (Expr_ident "i"),
               (Expr_const (Const_int 10)))));
      post = (Some (Stmt_incr "i")); body = []} |}]
;;

let%expect_test "stmt for with range and number" =
  pp pp_stmt pstmt {|for range 10 {}|};
  [%expect
    {|
    Stmt_for {
      init =
      (Some (Stmt_short_var_decl
               (Short_decl_mult_init [("i", (Expr_const (Const_int 0)))])));
      cond =
      (Some (Expr_bin_oper (Bin_less, (Expr_ident "i"),
               (Expr_const (Const_int 10)))));
      post = (Some (Stmt_incr "i")); body = []} |}]
;;

(********** range **********)

let%expect_test "stmt range with decl only index" =
  pp pp_stmt pstmt {|for i := range array {}|};
  [%expect
    {|
    (Stmt_range
       Range_decl {index = "i"; element = None; array = (Expr_ident "array");
         body = []}) |}]
;;

let%expect_test "stmt range with decl index and elem" =
  pp pp_stmt pstmt {|for i, elem := range array {}|};
  [%expect
    {|
    (Stmt_range
       Range_decl {index = "i"; element = (Some "elem");
         array = (Expr_ident "array"); body = []}) |}]
;;

let%expect_test "stmt range with decl blank index and elem" =
  pp pp_stmt pstmt {|for _, _ := range array {}|};
  [%expect
    {|
    (Stmt_range
       Range_decl {index = "_"; element = (Some "_");
         array = (Expr_ident "array"); body = []}) |}]
;;

let%expect_test "stmt range with assign only index" =
  pp pp_stmt pstmt {|for i = range array {}|};
  [%expect
    {|
    (Stmt_range
       Range_assign {index = "i"; element = None; array = (Expr_ident "array");
         body = []}) |}]
;;

let%expect_test "stmt range with assign index and elem" =
  pp pp_stmt pstmt {|for i, elem = range array {}|};
  [%expect
    {|
    (Stmt_range
       Range_assign {index = "i"; element = (Some "elem");
         array = (Expr_ident "array"); body = []}) |}]
;;

let%expect_test "stmt range with assign blank index and elem" =
  pp pp_stmt pstmt {|for _, _ = range array {}|};
  [%expect
    {|
    (Stmt_range
       Range_assign {index = "_"; element = (Some "_");
         array = (Expr_ident "array"); body = []}) |}]
;;

let%expect_test "stmt range with more than two idents" =
  pp pp_stmt pstmt {|for a, b, c, d = range array {}|};
  [%expect {| : Incorrect statement |}]
;;

let%expect_test "stmt range without idents" =
  pp pp_stmt pstmt {|for := range array {}|};
  [%expect {| : Incorrect statement |}]
;;

let%expect_test "stmt range with const array" =
  pp pp_stmt pstmt {|for i, elem := range [3]int{1, 2, 3} {}|};
  [%expect
    {|
    (Stmt_range
       Range_decl {index = "i"; element = (Some "elem");
         array =
         (Expr_const
            (Const_array (3, Type_int,
               [(Expr_const (Const_int 1)); (Expr_const (Const_int 2));
                 (Expr_const (Const_int 3))]
               )));
         body = []}) |}]
;;
