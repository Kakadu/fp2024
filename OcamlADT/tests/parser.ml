
open Ocamladt_lib.Parser
open Ocamladt_lib.Ast
(* open Angstrom *)
let test_programm str = print_endline(show_program(parse_str str))

(*let rec fact n = if n = 0 then 1 else n * fact (n - 1);;*)

(*good*)
let%expect_test "empty program" = 
    test_programm {||};
  [%expect{|
    Debug: let parser failed
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: compare parser failed
    Debug: pseval parser failed
    Debug: pany parser failed
    Debug: pvar parser failed
    Debug: value_binding parser failed
    Debug: psvalue parser SUCCEEDED
    [] |}]

    (*good*)
let%expect_test "double semicolons" = 
  test_programm {|;;|};
[%expect{|
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: pany parser failed
  Debug: pvar parser failed
  Debug: value_binding parser failed
  Debug: psvalue parser SUCCEEDED
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: pany parser failed
  Debug: pvar parser failed
  Debug: value_binding parser failed
  Debug: psvalue parser SUCCEEDED
  [(Str_value (Nonrecursive, []))] |}]  

  (*good*)
let%expect_test "(whitespaces)" =
test_programm {|       ;;|};
[%expect{|
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: pany parser failed
  Debug: pvar parser failed
  Debug: value_binding parser failed
  Debug: psvalue parser SUCCEEDED
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: pany parser failed
  Debug: pvar parser failed
  Debug: value_binding parser failed
  Debug: psvalue parser SUCCEEDED
  [(Str_value (Nonrecursive, []))] |}]

(*good*)
  let%expect_test "negative int constant" =
  test_programm {|-1;;|};
[%expect{|
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: compare parser SUCCEEDED
  Debug: pseval parser SUCCEEDED
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: pany parser failed
  Debug: pvar parser failed
  Debug: value_binding parser failed
  Debug: psvalue parser SUCCEEDED
  [(Str_eval (Exp_constant (Const_integer -1)))] |}]
  
(*good*)
let%expect_test "positive int constant" =
test_programm {|+1;;|};
[%expect{|
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: compare parser SUCCEEDED
  Debug: pseval parser SUCCEEDED
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: pany parser failed
  Debug: pvar parser failed
  Debug: value_binding parser failed
  Debug: psvalue parser SUCCEEDED
  [(Str_eval (Exp_constant (Const_integer 1)))] |}]

(*good*)
let%expect_test " nt constant" =
test_programm {|1;;|};
[%expect{|
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: compare parser SUCCEEDED
  Debug: pseval parser SUCCEEDED
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: pany parser failed
  Debug: pvar parser failed
  Debug: value_binding parser failed
  Debug: psvalue parser SUCCEEDED
  [(Str_eval (Exp_constant (Const_integer 1)))] |}]

(*good*)
let%expect_test "whitespace befor int constant" =
test_programm {|     1;;|};
[%expect{|
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: compare parser SUCCEEDED
  Debug: pseval parser SUCCEEDED
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: pany parser failed
  Debug: pvar parser failed
  Debug: value_binding parser failed
  Debug: psvalue parser SUCCEEDED
  [(Str_eval (Exp_constant (Const_integer 1)))] |}]

(*good*)
let%expect_test "negative zero" =
test_programm {|-0;;|};
[%expect{|
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: compare parser SUCCEEDED
  Debug: pseval parser SUCCEEDED
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: pany parser failed
  Debug: pvar parser failed
  Debug: value_binding parser failed
  Debug: psvalue parser SUCCEEDED
  [(Str_eval (Exp_constant (Const_integer 0)))] |}]

(*good*)
let%expect_test "positive zero" =
test_programm {|+0;;|};
[%expect{|
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: compare parser SUCCEEDED
  Debug: pseval parser SUCCEEDED
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: pany parser failed
  Debug: pvar parser failed
  Debug: value_binding parser failed
  Debug: psvalue parser SUCCEEDED
  [(Str_eval (Exp_constant (Const_integer 0)))] |}]

(*good*)
let%expect_test "zero" =
test_programm {|0;;|};
[%expect{|
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: compare parser SUCCEEDED
  Debug: pseval parser SUCCEEDED
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: pany parser failed
  Debug: pvar parser failed
  Debug: value_binding parser failed
  Debug: psvalue parser SUCCEEDED
  [(Str_eval (Exp_constant (Const_integer 0)))] |}]

(*bad?*)
let%expect_test "-111 5" =
  test_programm {|- 111 5;;|};
[%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 335, characters 2-29
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: pany parser failed
  Debug: pvar parser failed
  Debug: value_binding parser failed
  Debug: psvalue parser SUCCEEDED |}]

  (*bad?*)
  let%expect_test "5+" =
  test_programm {|5 +;;|};
[%expect.unreachable]
  [@@expect.uncaught_exn {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    (Failure ": end_of_input")
    Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
    Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
    Called from Tests__Parser.(fun) in file "tests/parser.ml", line 368, characters 2-25
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

    Trailing output
    ---------------
    Debug: let parser failed
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser SUCCEEDED
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser SUCCEEDED
    Debug: compare parser SUCCEEDED
    Debug: pseval parser SUCCEEDED |}]
   
(*good*)
let%expect_test "substraction" =
    test_programm {|5-11;;|};
  [%expect{|
    Debug: let parser failed
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser SUCCEEDED
    Debug: parens parser failed
    Debug: constint parser SUCCEEDED
    Debug: apply parser SUCCEEDED
    Debug: compare parser SUCCEEDED
    Debug: pseval parser SUCCEEDED
    Debug: let parser failed
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: compare parser failed
    Debug: pseval parser failed
    Debug: pany parser failed
    Debug: pvar parser failed
    Debug: value_binding parser failed
    Debug: psvalue parser SUCCEEDED
    [(Str_eval
        (Exp_apply ((Exp_constant (Const_integer 5)),
           (Exp_constant (Const_integer -11)))))
      ] |}]

      (*good*)
  let%expect_test "strange move" =
    test_programm {|5=5;;|};
  [%expect{|
    Debug: let parser failed
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser SUCCEEDED
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser SUCCEEDED
    Debug: parens parser failed
    Debug: constint parser SUCCEEDED
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser SUCCEEDED
    Debug: compare parser SUCCEEDED
    Debug: pseval parser SUCCEEDED
    Debug: let parser failed
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: compare parser failed
    Debug: pseval parser failed
    Debug: pany parser failed
    Debug: pvar parser failed
    Debug: value_binding parser failed
    Debug: psvalue parser SUCCEEDED
    [(Str_eval
        (Exp_apply ((Exp_ident "="),
           (Exp_tuple ((Exp_constant (Const_integer 5)),
              (Exp_constant (Const_integer 5)), []))
           )))
      ] |}]
  
    (*good*)
  let%expect_test "(assignment)" =
    test_programm {|x = 52;;|};
  [%expect{|
    Debug: let parser failed
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser SUCCEEDED
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser SUCCEEDED
    Debug: parens parser failed
    Debug: constint parser SUCCEEDED
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser SUCCEEDED
    Debug: compare parser SUCCEEDED
    Debug: pseval parser SUCCEEDED
    Debug: let parser failed
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: compare parser failed
    Debug: pseval parser failed
    Debug: pany parser failed
    Debug: pvar parser failed
    Debug: value_binding parser failed
    Debug: psvalue parser SUCCEEDED
    [(Str_eval
        (Exp_apply ((Exp_ident "="),
           (Exp_tuple ((Exp_ident "x"), (Exp_constant (Const_integer 52)), [])))))
      ] |}]
(*good*)
let%expect_test "multiplication" =
    test_programm {|5*5;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 522, characters 4-27
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: compare parser SUCCEEDED
  Debug: pseval parser SUCCEEDED |}]

(*good*)
      let%expect_test "operators with different priorities" =
      test_programm {|5-5*1;;|};
    [%expect.unreachable]
      [@@expect.uncaught_exn {|
        (* CR expect_test_collector: This test expectation appears to contain a backtrace.
           This is strongly discouraged as backtraces are fragile.
           Please change this test to not include a backtrace. *)

        (Failure ": end_of_input")
        Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
        Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
        Called from Tests__Parser.(fun) in file "tests/parser.ml", line 552, characters 6-31
        Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

        Trailing output
        ---------------
        Debug: let parser failed
        Debug: fun parser failed
        Debug: parens parser failed
        Debug: constint parser SUCCEEDED
        Debug: parens parser failed
        Debug: constint parser SUCCEEDED
        Debug: apply parser SUCCEEDED
        Debug: compare parser SUCCEEDED
        Debug: pseval parser SUCCEEDED |}]
    
(*good*)
let%expect_test "operators with different priorities" =
  test_programm {|5*5-1;;|};
[%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 579, characters 2-27
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: compare parser SUCCEEDED
  Debug: pseval parser SUCCEEDED |}]
      (*good*)
let%expect_test "parenthesis with operators with different priorities" =
    test_programm {|5*(5-1);;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 608, characters 4-31
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: compare parser SUCCEEDED
  Debug: pseval parser SUCCEEDED |}]

(*bad*)
let%expect_test "parenthesis4" =
  test_programm {|();;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 638, characters 2-24
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: let parser failed
  Debug: fun parser failed
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: compare parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: compare parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: pany parser failed
  Debug: pvar parser failed
  Debug: value_binding parser failed
  Debug: psvalue parser SUCCEEDED |}]

  let%expect_test "parenthesis3" =
  test_programm {|(5);;|};
  [%expect{|
    Debug: let parser failed
    Debug: fun parser failed
    Debug: let parser failed
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser SUCCEEDED
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser SUCCEEDED
    Debug: compare parser SUCCEEDED
    Debug: parens parser SUCCEEDED
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: apply parser failed
    Debug: let parser failed
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser SUCCEEDED
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser SUCCEEDED
    Debug: compare parser SUCCEEDED
    Debug: parens parser SUCCEEDED
    Debug: compare parser SUCCEEDED
    Debug: pseval parser SUCCEEDED
    Debug: let parser failed
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: compare parser failed
    Debug: pseval parser failed
    Debug: pany parser failed
    Debug: pvar parser failed
    Debug: value_binding parser failed
    Debug: psvalue parser SUCCEEDED
    [(Str_eval (Exp_constant (Const_integer 5)))] |}]
(*bad*)
let%expect_test "parenthesis1" =
  test_programm {|(5*(5-1));;|};
[%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 742, characters 2-31
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: let parser failed
  Debug: fun parser failed
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: compare parser SUCCEEDED
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: compare parser SUCCEEDED
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: pany parser failed
  Debug: pvar parser failed
  Debug: value_binding parser failed
  Debug: psvalue parser SUCCEEDED |}]

  (*bad*)
let%expect_test "parenthesis2" =
test_programm {|(5-1);;|};
[%expect{|
  Debug: let parser failed
  Debug: fun parser failed
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: apply parser SUCCEEDED
  Debug: compare parser SUCCEEDED
  Debug: parens parser SUCCEEDED
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: apply parser SUCCEEDED
  Debug: compare parser SUCCEEDED
  Debug: parens parser SUCCEEDED
  Debug: compare parser SUCCEEDED
  Debug: pseval parser SUCCEEDED
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: pany parser failed
  Debug: pvar parser failed
  Debug: value_binding parser failed
  Debug: psvalue parser SUCCEEDED
  [(Str_eval
      (Exp_apply ((Exp_constant (Const_integer 5)),
         (Exp_constant (Const_integer -1)))))
    ] |}]
(*bad*)
    let%expect_test "tuple" =
test_programm {|(5,1)|};
[%expect.unreachable]
    [@@expect.uncaught_exn {|
      (* CR expect_test_collector: This test expectation appears to contain a backtrace.
         This is strongly discouraged as backtraces are fragile.
         Please change this test to not include a backtrace. *)

      (Failure ": end_of_input")
      Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
      Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
      Called from Tests__Parser.(fun) in file "tests/parser.ml", line 846, characters 0-23
      Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

      Trailing output
      ---------------
      Debug: let parser failed
      Debug: fun parser failed
      Debug: let parser failed
      Debug: fun parser failed
      Debug: parens parser failed
      Debug: constint parser SUCCEEDED
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: ident parser failed
      Debug: apply parser failed
      Debug: parens parser failed
      Debug: constint parser SUCCEEDED
      Debug: compare parser SUCCEEDED
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: ident parser failed
      Debug: apply parser failed
      Debug: let parser failed
      Debug: fun parser failed
      Debug: parens parser failed
      Debug: constint parser SUCCEEDED
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: ident parser failed
      Debug: apply parser failed
      Debug: parens parser failed
      Debug: constint parser SUCCEEDED
      Debug: compare parser SUCCEEDED
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: ident parser failed
      Debug: compare parser failed
      Debug: pseval parser failed
      Debug: pany parser failed
      Debug: pvar parser failed
      Debug: value_binding parser failed
      Debug: psvalue parser SUCCEEDED |}]




      let%expect_test "in" =
      test_programm {|5 in 6;;|};
    [%expect.unreachable]
    [@@expect.uncaught_exn {|
      (* CR expect_test_collector: This test expectation appears to contain a backtrace.
         This is strongly discouraged as backtraces are fragile.
         Please change this test to not include a backtrace. *)
    
      (Failure ": end_of_input")
      Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
      Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
      Called from Tests__Parser.(fun) in file "tests/parser.ml", line 903, characters 6-32
      Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19
    
      Trailing output
      ---------------
      Debug: let parser failed
      Debug: fun parser failed
      Debug: parens parser failed
      Debug: constint parser SUCCEEDED
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: ident parser failed
      Debug: apply parser failed
      Debug: parens parser failed
      Debug: constint parser SUCCEEDED
      Debug: compare parser SUCCEEDED
      Debug: pseval parser SUCCEEDED |}]

let%expect_test "let assignment" =
  test_programm {|let x = 5 in 6;;|};
[%expect{|
  Debug: pany parser failed
  Debug: pvar parser SUCCEEDED
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: compare parser SUCCEEDED
  Debug: value_binding parser SUCCEEDED
  Debug: pany parser failed
  Debug: pvar parser SUCCEEDED
  Debug: value_binding parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: compare parser SUCCEEDED
  Debug: let parser SUCCEEDED
  Debug: pseval parser SUCCEEDED
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: pany parser failed
  Debug: pvar parser failed
  Debug: value_binding parser failed
  Debug: psvalue parser SUCCEEDED
  [(Str_eval
      (Exp_let (Nonrecursive,
         [{ pat = (Pat_var "x"); expr = (Exp_constant (Const_integer 5)) }],
         (Exp_constant (Const_integer 6)))))
    ] |}]

let%expect_test "let assignment with recursion" =
    test_programm {|let rec x = 5 in 6;;|};
  [%expect{|
    Debug: pany parser failed
    Debug: pvar parser SUCCEEDED
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser SUCCEEDED
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser SUCCEEDED
    Debug: compare parser SUCCEEDED
    Debug: value_binding parser SUCCEEDED
    Debug: pany parser failed
    Debug: pvar parser SUCCEEDED
    Debug: value_binding parser failed
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser SUCCEEDED
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser SUCCEEDED
    Debug: compare parser SUCCEEDED
    Debug: let parser SUCCEEDED
    Debug: pseval parser SUCCEEDED
    Debug: let parser failed
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: compare parser failed
    Debug: pseval parser failed
    Debug: pany parser failed
    Debug: pvar parser failed
    Debug: value_binding parser failed
    Debug: psvalue parser SUCCEEDED
    [(Str_eval
        (Exp_let (Recursive,
           [{ pat = (Pat_var "x"); expr = (Exp_constant (Const_integer 5)) }],
           (Exp_constant (Const_integer 6)))))
      ] |}]
(*bad*)
let%expect_test "let assignment with recursion" =
  test_programm {|let rec x = 5 in 7;;|};
[%expect{|
  Debug: pany parser failed
  Debug: pvar parser SUCCEEDED
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: compare parser SUCCEEDED
  Debug: value_binding parser SUCCEEDED
  Debug: pany parser failed
  Debug: pvar parser SUCCEEDED
  Debug: value_binding parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser SUCCEEDED
  Debug: compare parser SUCCEEDED
  Debug: let parser SUCCEEDED
  Debug: pseval parser SUCCEEDED
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: ident parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: pany parser failed
  Debug: pvar parser failed
  Debug: value_binding parser failed
  Debug: psvalue parser SUCCEEDED
  [(Str_eval
      (Exp_let (Recursive,
         [{ pat = (Pat_var "x"); expr = (Exp_constant (Const_integer 5)) }],
         (Exp_constant (Const_integer 7)))))
    ] |}]

  let%expect_test "apply" =
    test_programm {|f (x);;|};
  [%expect{|
    Debug: let parser failed
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser SUCCEEDED
    Debug: let parser failed
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser SUCCEEDED
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser SUCCEEDED
    Debug: compare parser SUCCEEDED
    Debug: parens parser SUCCEEDED
    Debug: apply parser SUCCEEDED
    Debug: compare parser SUCCEEDED
    Debug: pseval parser SUCCEEDED
    Debug: let parser failed
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: compare parser failed
    Debug: pseval parser failed
    Debug: pany parser failed
    Debug: pvar parser failed
    Debug: value_binding parser failed
    Debug: psvalue parser SUCCEEDED
    [(Str_eval (Exp_apply ((Exp_ident "f"), (Exp_ident "x"))))] |}]

  let%expect_test "apply and subtraction" =
    test_programm {|f (x-1);;|};
  [%expect{|
    Debug: let parser failed
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser SUCCEEDED
    Debug: let parser failed
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser SUCCEEDED
    Debug: parens parser failed
    Debug: constint parser SUCCEEDED
    Debug: apply parser SUCCEEDED
    Debug: compare parser SUCCEEDED
    Debug: parens parser SUCCEEDED
    Debug: apply parser SUCCEEDED
    Debug: compare parser SUCCEEDED
    Debug: pseval parser SUCCEEDED
    Debug: let parser failed
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: compare parser failed
    Debug: pseval parser failed
    Debug: pany parser failed
    Debug: pvar parser failed
    Debug: value_binding parser failed
    Debug: psvalue parser SUCCEEDED
    [(Str_eval
        (Exp_apply ((Exp_ident "f"),
           (Exp_apply ((Exp_ident "x"), (Exp_constant (Const_integer -1)))))))
      ] |}]
  
  let%expect_test "multiplication and apply" =
  test_programm {|x * (f (x-1));;|};
[%expect.unreachable]
  [@@expect.uncaught_exn {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    (Failure ": end_of_input")
    Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
    Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
    Called from Tests__Parser.(fun) in file "tests/parser.ml", line 1171, characters 2-35
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

    Trailing output
    ---------------
    Debug: let parser failed
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser SUCCEEDED
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser SUCCEEDED
    Debug: compare parser SUCCEEDED
    Debug: pseval parser SUCCEEDED |}]



    let%expect_test "funn" =
      test_programm {|fun x -> (fun z -> (y) ) ;;|};
    [%expect{|
      Debug: let parser failed
      Debug: pany parser failed
      Debug: pvar parser SUCCEEDED
      Debug: pany parser failed
      Debug: pvar parser failed
      Debug: pany parser failed
      Debug: pvar parser failed
      Debug: let parser failed
      Debug: pany parser failed
      Debug: pvar parser SUCCEEDED
      Debug: pany parser failed
      Debug: pvar parser failed
      Debug: pany parser failed
      Debug: pvar parser failed
      Debug: let parser failed
      Debug: fun parser failed
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: ident parser SUCCEEDED
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: ident parser failed
      Debug: apply parser failed
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: ident parser SUCCEEDED
      Debug: compare parser SUCCEEDED
      Debug: parens parser SUCCEEDED
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: ident parser failed
      Debug: apply parser failed
      Debug: let parser failed
      Debug: fun parser failed
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: ident parser SUCCEEDED
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: ident parser failed
      Debug: apply parser failed
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: ident parser SUCCEEDED
      Debug: compare parser SUCCEEDED
      Debug: parens parser SUCCEEDED
      Debug: compare parser SUCCEEDED
      Debug: fun parser SUCCEEDED
      Debug: parens parser SUCCEEDED
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: ident parser failed
      Debug: apply parser failed
      Debug: let parser failed
      Debug: pany parser failed
      Debug: pvar parser SUCCEEDED
      Debug: pany parser failed
      Debug: pvar parser failed
      Debug: pany parser failed
      Debug: pvar parser failed
      Debug: let parser failed
      Debug: fun parser failed
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: ident parser SUCCEEDED
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: ident parser failed
      Debug: apply parser failed
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: ident parser SUCCEEDED
      Debug: compare parser SUCCEEDED
      Debug: parens parser SUCCEEDED
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: ident parser failed
      Debug: apply parser failed
      Debug: let parser failed
      Debug: fun parser failed
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: ident parser SUCCEEDED
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: ident parser failed
      Debug: apply parser failed
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: ident parser SUCCEEDED
      Debug: compare parser SUCCEEDED
      Debug: parens parser SUCCEEDED
      Debug: compare parser SUCCEEDED
      Debug: fun parser SUCCEEDED
      Debug: parens parser SUCCEEDED
      Debug: compare parser SUCCEEDED
      Debug: fun parser SUCCEEDED
      Debug: pseval parser SUCCEEDED
      Debug: let parser failed
      Debug: fun parser failed
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: ident parser failed
      Debug: apply parser failed
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: ident parser failed
      Debug: compare parser failed
      Debug: pseval parser failed
      Debug: pany parser failed
      Debug: pvar parser failed
      Debug: value_binding parser failed
      Debug: psvalue parser SUCCEEDED
      [(Str_eval
          (Exp_fun ((Pat_var "x"), [],
             (Exp_fun ((Pat_var "z"), [], (Exp_ident "y"))))))
        ] |}]
    

let%expect_test "let and apply" =
  test_programm {|let f x = x in 5 ;;|};
[%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure 123)
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Angstrom__Parser.Monad.(>>|).(fun).succ' in file "lib/parser.ml", line 64, characters 61-66
  Called from Angstrom__Parser.parse_bigstring in file "lib/parser.ml", line 43, characters 52-93
  Called from Ocamladt_lib__Parser.parse in file "lib/parser.ml" (inlined), line 225, characters 16-56
  Called from Ocamladt_lib__Parser.parse_str in file "lib/parser.ml", line 228, characters 8-17
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 1326, characters 2-39
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: pany parser failed
  Debug: pvar parser SUCCEEDED
  Debug: value_binding parser failed
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser failed |}]

let%expect_test "let and apply v2" =
    test_programm {|let fact x = fact(x-1);;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure 123)
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Angstrom__Parser.Monad.(>>|).(fun).succ' in file "lib/parser.ml", line 64, characters 61-66
  Called from Angstrom__Parser.parse_bigstring in file "lib/parser.ml", line 43, characters 52-93
  Called from Ocamladt_lib__Parser.parse in file "lib/parser.ml" (inlined), line 225, characters 16-56
  Called from Ocamladt_lib__Parser.parse_str in file "lib/parser.ml", line 228, characters 8-17
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 1354, characters 4-46
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: pany parser failed
  Debug: pvar parser SUCCEEDED
  Debug: value_binding parser failed
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser failed |}]

let%expect_test "if then" =
    test_programm {|if 5 then 6;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure 123)
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Angstrom__Parser.Monad.(>>|).(fun).succ' in file "lib/parser.ml", line 64, characters 61-66
  Called from Angstrom__Parser.parse_bigstring in file "lib/parser.ml", line 43, characters 52-93
  Called from Ocamladt_lib__Parser.parse in file "lib/parser.ml" (inlined), line 225, characters 16-56
  Called from Ocamladt_lib__Parser.parse_str in file "lib/parser.ml", line 228, characters 8-17
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 1382, characters 4-35
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser failed |}]

let%expect_test "if statement. condition from fact" =
    test_programm {|if n = 0 then 1 else 7;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure 123)
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Angstrom__Parser.Monad.(>>|).(fun).succ' in file "lib/parser.ml", line 64, characters 61-66
  Called from Angstrom__Parser.parse_bigstring in file "lib/parser.ml", line 43, characters 52-93
  Called from Ocamladt_lib__Parser.parse in file "lib/parser.ml" (inlined), line 225, characters 16-56
  Called from Ocamladt_lib__Parser.parse_str in file "lib/parser.ml", line 228, characters 8-17
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 1407, characters 4-46
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser failed |}]

  let%expect_test "let and if" =
  test_programm {|let x = if n = 0 then 6 else 7 in 6;;|};
[%expect.unreachable]
  [@@expect.uncaught_exn {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    (Failure 123)
    Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
    Called from Angstrom__Parser.Monad.(>>|).(fun).succ' in file "lib/parser.ml", line 64, characters 61-66
    Called from Angstrom__Parser.parse_bigstring in file "lib/parser.ml", line 43, characters 52-93
    Called from Ocamladt_lib__Parser.parse in file "lib/parser.ml" (inlined), line 225, characters 16-56
    Called from Ocamladt_lib__Parser.parse_str in file "lib/parser.ml", line 228, characters 8-17
    Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
    Called from Tests__Parser.(fun) in file "tests/parser.ml", line 1432, characters 2-57
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

    Trailing output
    ---------------
    Debug: pany parser failed
    Debug: pvar parser SUCCEEDED
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: ident parser failed
    Debug: compare parser failed
    Debug: value_binding parser failed
    Debug: let parser failed
    Debug: fun parser failed
    Debug: parens parser failed
    Debug: constint parser failed |}]

let%expect_test "factorial" =
    test_programm {|let rec fact x = if x = 0 then 1 else x * fact(X-1);;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure 123)
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Angstrom__Parser.Monad.(>>|).(fun).succ' in file "lib/parser.ml", line 64, characters 61-66
  Called from Angstrom__Parser.parse_bigstring in file "lib/parser.ml", line 43, characters 52-93
  Called from Ocamladt_lib__Parser.parse in file "lib/parser.ml" (inlined), line 225, characters 16-56
  Called from Ocamladt_lib__Parser.parse_str in file "lib/parser.ml", line 228, characters 8-17
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 1469, characters 4-75
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: pany parser failed
  Debug: pvar parser SUCCEEDED
  Debug: value_binding parser failed
  Debug: let parser failed
  Debug: fun parser failed
  Debug: parens parser failed
  Debug: constint parser failed |}]
  

(* let%expect_test "let x = 5" =
    test_programm{|let x = 52;;|};
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19 |}] *)

(* let print_list to_string fr sc lst =
    Printf.printf "var %s" fr;
    Printf.printf "var2 %s" sc;
    Printf.printf "[";
    List.iteri (fun i x ->
      if i > 0 then Printf.printf "; ";
      Printf.printf "%s" (to_string x)
    ) lst;
    Printf.printf "]\n"  *)
(* 
let test_let str = parse_string ~consume:All pletexpr str

let%expect_test "let" =
        match test_let {|let|} with
        | Ok expr -> 
            print_endline (show_expression expr)
        | Error msg -> 
            print_endline ("Error: " ^ msg);
        [%expect {| Error: : string |}] *)