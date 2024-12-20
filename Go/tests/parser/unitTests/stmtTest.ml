(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Parse
open Pprinter.Printer
open Pp

(********** break, continue, go, defer, channel send and receive **********)

let%expect_test "break stmt" =
  pp print_stmt parse_stmt {|break|};
  [%expect {| break |}]
;;

let%expect_test "continue stmt" =
  pp print_stmt parse_stmt {|continue|};
  [%expect {| continue |}]
;;

let%expect_test "stmt defer with func" =
  pp print_stmt parse_stmt {|defer 
                      call(abc)|};
  [%expect {|
    defer call(abc) |}]
;;

let%expect_test "stmt defer with expr that is not a func" =
  pp print_stmt parse_stmt {|defer 2 + 2 * 5|};
  [%expect {|
    : syntax error |}]
;;

let%expect_test "stmt go with func" =
  pp print_stmt parse_stmt {|go 
                      call(abc)|};
  [%expect {|
    go call(abc) |}]
;;

let%expect_test "stmt go with expr that is not a func" =
  pp print_stmt parse_stmt {|go 2 + 2 * 5|};
  [%expect {|
    : syntax error |}]
;;

let%expect_test "stmt chan send" =
  pp print_stmt parse_stmt {|c <- sum + 1|};
  [%expect {|
    c <- sum + 1 |}]
;;

let%expect_test "stmt chan receive" =
  pp print_stmt parse_stmt {|<-c|};
  [%expect {|
    <-c |}]
;;

(********** incr and decr **********)

let%expect_test "incr stmt" =
  pp print_stmt parse_stmt {|a++|};
  [%expect {| a++ |}]
;;

let%expect_test "incr stmt with ws_line" =
  pp print_stmt parse_stmt {|a    /* some comment */   ++|};
  [%expect {| a++ |}]
;;

let%expect_test "incr stmt with blank ident" =
  pp print_stmt parse_stmt {|_++|};
  [%expect {| _++ |}]
;;

let%expect_test "decr stmt" =
  pp print_stmt parse_stmt {|a--|};
  [%expect {| a-- |}]
;;

let%expect_test "decr stmt with ws_line" =
  pp print_stmt parse_stmt {|a    /* some comment */   --|};
  [%expect {| a-- |}]
;;

let%expect_test "decr stmt with blank ident" =
  pp print_stmt parse_stmt {|_--|};
  [%expect {| _-- |}]
;;

(********** return **********)

let%expect_test "return without anything" =
  pp print_stmt parse_stmt {|return|};
  [%expect {| return |}]
;;

let%expect_test "return with one expr" =
  pp print_stmt parse_stmt {|return 5|};
  [%expect {| return 5 |}]
;;

let%expect_test "return with multiple exprs and ws" =
  pp
    print_stmt
    parse_stmt
    {|return 3    ,   
             a  ,  // some comment 
             true /* RARAVARV */    ,  nil|};
  [%expect {|
    return 3, a, true, nil |}]
;;

let%expect_test "return with multiple complex exprs" =
  pp print_stmt parse_stmt {|return -5 * _r + 8, !a && (b || c)|};
  [%expect {|
    return -5 * _r + 8, !a && (b || c) |}]
;;

(********** func call **********)

let%expect_test "stmt func call with one simple arg" =
  pp print_stmt parse_stmt {|my_func(5)|};
  [%expect {| my_func(5) |}]
;;

let%expect_test "stmt func callmultiple args" =
  pp print_stmt parse_stmt {|my_func(5, a, nil)|};
  [%expect {|
    my_func(5, a, nil) |}]
;;

let%expect_test "stmt func call with complex expressions and comments" =
  pp print_stmt parse_stmt {|fac(   fac(2 + 2), 
  34 * 75,
  // aovnervo 
  !a)|};
  [%expect {|
    fac(fac(2 + 2), 34 * 75, !a) |}]
;;

(********** assign **********)

let%expect_test "stmt assign one lvalue, one rvalue" =
  pp print_stmt parse_stmt {|a = 5|};
  [%expect {|
    a = 5 |}]
;;

let%expect_test "stmt assign one lvalue, no rvalue" =
  pp print_stmt parse_stmt {|a =|};
  [%expect {|
    : syntax error |}]
;;

let%expect_test "stmt assign no lvalue, one rvalue" =
  pp print_stmt parse_stmt {|= 5|};
  [%expect {|
    : syntax error |}]
;;

let%expect_test "stmt assign one lvalue that is an array index, one rvalue" =
  pp print_stmt parse_stmt {|a[i][2 + 3] = 5|};
  [%expect {|
    a[i][2 + 3] = 5 |}]
;;

let%expect_test "stmt assign with mult equal number of lvalues and rvalues and ws" =
  pp
    print_stmt
    parse_stmt
    {|a, 
  b , // comment
  c[get_index()] = 
  
  5, /* comment////// */true,
   "hello"|};
  [%expect {|
    a, b, c[get_index()] = 5, true, "hello" |}]
;;

let%expect_test "stmt assign mult lvalues and one rvalue that is a func call" =
  pp print_stmt parse_stmt {|a, b[0] ,c = get_three()|};
  [%expect {|
    a, b[0], c = get_three() |}]
;;

let%expect_test "stmt assign mult lvalues and one rvalue that is not a func call" =
  pp print_stmt parse_stmt {|a, b ,c = abc|};
  [%expect {| : syntax error |}]
;;

let%expect_test "stmt assign mult unequal lvalues and rvalues" =
  pp print_stmt parse_stmt {|a, b ,c = 2, 3, 4, 5 , 6|};
  [%expect {| : syntax error |}]
;;

(********** long var decl **********)

let%expect_test "stmt long single var decl without init" =
  pp print_stmt parse_stmt {|var a string|};
  [%expect {|
    var a string |}]
;;

let%expect_test "stmt long single var decl without init and type" =
  pp print_stmt parse_stmt {|var a|};
  [%expect {| : syntax error |}]
;;

let%expect_test "stmt long single var decl with type, without vars and init" =
  pp print_stmt parse_stmt {|var [0]int |};
  [%expect {| : syntax error |}]
;;

let%expect_test "stmt long single var decl without init with mult array type" =
  pp print_stmt parse_stmt {|var a [2][3][1]bool|};
  [%expect {|
    var a [2][3][1]bool |}]
;;

let%expect_test "stmt long single var decl no type" =
  pp print_stmt parse_stmt {|var a = 5|};
  [%expect {|
    var a = 5 |}]
;;

let%expect_test "stmt long decl no type and var" =
  pp print_stmt parse_stmt {|var = 5|};
  [%expect {|
    : syntax error |}]
;;

let%expect_test "stmt long mult var decl no type" =
  pp print_stmt parse_stmt {|var a, b, c = 5, nil, "hi"|};
  [%expect {|
    var a, b, c = 5, nil, "hi" |}]
;;

let%expect_test "stmt long single var decl with type" =
  pp print_stmt parse_stmt {|var a func() = func() {}|};
  [%expect {|
    var a func() = func() {} |}]
;;

let%expect_test "stmt long mult var decl with type" =
  pp print_stmt parse_stmt {|var a, b int = 2, 3|};
  [%expect {|
    var a, b int = 2, 3 |}]
;;

let%expect_test "stmt long mult var decl with type" =
  pp print_stmt parse_stmt {|var a, b, c [2]int = [2]int{1, 2}, [2]int{}, [2]int{10, 20}|};
  [%expect {|
    var a, b, c [2]int = [2]int{1, 2}, [2]int{}, [2]int{10, 20} |}]
;;

let%expect_test "stmt long mult var decl without type" =
  pp print_stmt parse_stmt {|var a, b, c = 5, nil, "hi"|};
  [%expect {|
    var a, b, c = 5, nil, "hi" |}]
;;

let%expect_test "stmt long var decl mult lvalues and one rvalue that is a func call" =
  pp print_stmt parse_stmt {|var a, b, c = get_three(1, 2, 3)|};
  [%expect {|
    var a, b, c  = get_three(1, 2, 3) |}]
;;

let%expect_test "stmt long var decl mult lvalues and one rvalue that is not a func call" =
  pp print_stmt parse_stmt {|var a, b, c = true|};
  [%expect {|
    : syntax error |}]
;;

let%expect_test "stmt long var decl unequal lvalues and rvalues" =
  pp print_stmt parse_stmt {|var a, b, c = 1, 2, 3, 4|};
  [%expect {|
    : syntax error |}]
;;

(********** short var decl **********)

let%expect_test "stmt short single var decl" =
  pp print_stmt parse_stmt {|a := 7|};
  [%expect {|
    a := 7 |}]
;;

let%expect_test "stmt short decl without init" =
  pp print_stmt parse_stmt {|a :=|};
  [%expect {|
    : syntax error |}]
;;

let%expect_test "stmt short decl with init, without var" =
  pp print_stmt parse_stmt {|:= 5|};
  [%expect {|
    : syntax error |}]
;;

let%expect_test "stmt short mult var decl" =
  pp print_stmt parse_stmt {|a, b, c := true, 567, "string"|};
  [%expect {|
    a, b, c := true, 567, "string" |}]
;;

let%expect_test "stmt short var decl mult lvalues and one rvalue that is a func call" =
  pp print_stmt parse_stmt {|a, b, c := three(abc, 2 + 3, fac(25))|};
  [%expect {|
    a, b, c := three(abc, 2 + 3, fac(25)) |}]
;;

let%expect_test "stmt short var decl mult lvalues and one rvalue that is not a func call" =
  pp print_stmt parse_stmt {|a, b, c := abcdefg"|};
  [%expect {|
    : syntax error |}]
;;

let%expect_test "stmt short var decl unequal lvalues and rvalues" =
  pp print_stmt parse_stmt {|a, b, c := 1, 2, 3, 4|};
  [%expect {|
    : syntax error |}]
;;

(********** block **********)

let%expect_test "stmt empty block" =
  pp print_stmt parse_stmt {|{}|};
  [%expect {|
    {} |}]
;;

let%expect_test "stmt block of one stmt" =
  pp print_stmt parse_stmt {|{ a := 5 }|};
  [%expect {|
    {
        a := 5
    } |}]
;;

let%expect_test "stmt block of mult stmts, separated by semicolon" =
  pp print_stmt parse_stmt {|{ a := 5; a++; println(a) }|};
  [%expect {|
    {
        a := 5
        a++
        println(a)
    } |}]
;;

let%expect_test "stmt block of mult stmts, separated by newlines" =
  pp
    print_stmt
    parse_stmt
    {|{ var hi string = "hi"
    // string that says hi
      go get_int(hi)}|};
  [%expect {|
    {
        var hi string = "hi"
        go get_int(hi)
    } |}]
;;

(********** if **********)

let%expect_test "stmt simple if" =
  pp print_stmt parse_stmt {|if true {}|};
  [%expect {|
    if true {} |}]
;;

let%expect_test "stmt if with empty init" =
  pp print_stmt parse_stmt {|if ; call() {}|};
  [%expect {|
    if call() {} |}]
;;

let%expect_test "stmt if with short decl init" =
  pp print_stmt parse_stmt {|if k := 0; k == test {}|};
  [%expect {|
    if k := 0; k == test {} |}]
;;

let%expect_test "stmt if with assign init" =
  pp print_stmt parse_stmt {|if k = 0; k == test {}|};
  [%expect {|
    if k = 0; k == test {} |}]
;;

let%expect_test "stmt if with incr init" =
  pp print_stmt parse_stmt {|if k++; k == test {}|};
  [%expect {|
    if k++; k == test {} |}]
;;

let%expect_test "stmt if with decr init" =
  pp print_stmt parse_stmt {|if k--; k == test {}|};
  [%expect {|
    if k--; k == test {} |}]
;;

let%expect_test "stmt if with call init" =
  pp print_stmt parse_stmt {|if run_test(); true {}|};
  [%expect {|
    if run_test(); true {} |}]
;;

let%expect_test "stmt if with chan send init" =
  pp print_stmt parse_stmt {|if c <- 5; true {}|};
  [%expect {|
    if c <- 5; true {} |}]
;;

let%expect_test "stmt if with chan receive init" =
  pp print_stmt parse_stmt {|if <-c; true {}|};
  [%expect {|
    if <-c; true {} |}]
;;

let%expect_test "stmt if with wrong init" =
  pp print_stmt parse_stmt {|if var a = 5; cond {}|};
  [%expect {|
    : syntax error |}]
;;

let%expect_test "stmt if with else that is a block" =
  pp print_stmt parse_stmt {|if cond {} else {}|};
  [%expect {|
    if cond {} else {} |}]
;;

let%expect_test "stmt if with else that is another if" =
  pp print_stmt parse_stmt {|if cond {} else if cond2 {}|};
  [%expect {|
    if cond {} else if cond2 {} |}]
;;

let%expect_test "stmt if with wrong else" =
  pp print_stmt parse_stmt {|if cond {} else do_smth()|};
  [%expect {|
    : syntax error |}]
;;

(********** for **********)

let%expect_test "stmt empty for" =
  pp print_stmt parse_stmt {|for {}|};
  [%expect {|
    for {} |}]
;;

let%expect_test "stmt for with only conition" =
  pp print_stmt parse_stmt {|for a > 0 {}|};
  [%expect {|
    for a > 0 {} |}]
;;

let%expect_test "stmt empty for with semicolons" =
  pp print_stmt parse_stmt {|for ;; {}|};
  [%expect {|
    for {} |}]
;;

let%expect_test "stmt default for with short decl in init and post" =
  pp print_stmt parse_stmt {|for i := 0;; j := i + 1 {}|};
  [%expect {|
    for i := 0;; j := i + 1 {} |}]
;;

let%expect_test "stmt default for with assign in init and post" =
  pp print_stmt parse_stmt {|for i = call();; i = i + 1 {}|};
  [%expect {|
    for i = call();; i = i + 1 {} |}]
;;

let%expect_test "stmt default for with call in init and post" =
  pp print_stmt parse_stmt {|for start();; finish() {}|};
  [%expect {|
    for start();; finish() {} |}]
;;

let%expect_test "stmt default for with incr in init and post" =
  pp print_stmt parse_stmt {|for i++;; i++ {}|};
  [%expect {|
    for i++;; i++ {} |}]
;;

let%expect_test "stmt default for with decr in init and post" =
  pp print_stmt parse_stmt {|for i--;; i-- {}|};
  [%expect {|
    for i--;; i-- {} |}]
;;

let%expect_test "stmt default for with chan send in init and post" =
  pp print_stmt parse_stmt {|for c <- 5;; c <- 5 {}|};
  [%expect {|
    for c <- 5;; c <- 5 {} |}]
;;

let%expect_test "stmt default for with chan receive in init and post" =
  pp print_stmt parse_stmt {|for <-c;; <-c {}|};
  [%expect {|
    for <-c;; <-c {} |}]
;;

let%expect_test "stmt default for with invalid stmt in init and post" =
  pp print_stmt parse_stmt {|for go call(); i > 0; return {}|};
  [%expect {|
    : syntax error |}]
;;

let%expect_test "stmt default for with valid init and invalid post" =
  pp print_stmt parse_stmt {|for call(); i > 0; break|};
  [%expect {|
    : syntax error  |}]
;;
