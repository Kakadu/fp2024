
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
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: compare parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed
    [] |}]

    (*good*)
let%expect_test "double semicolons" = 
  test_programm {|;;|};
[%expect{|
  Debug: let parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed
  Debug: let parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed
  [(Str_value (Nonrecursive, []))] |}]  

  (*good*)
let%expect_test "(whitespaces)" =
test_programm {|       ;;|};
[%expect{|
  Debug: let parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed
  Debug: let parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed
  [(Str_value (Nonrecursive, []))] |}]

(*good*)
  let%expect_test "negative int constant" =
  test_programm {|-1;;|};
[%expect{|
  Debug: let parser failed
  Debug: parens parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: let parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed
  [(Str_eval (Exp_constant (Const_integer -1)))] |}]
  
(*good*)
let%expect_test "positive int constant" =
test_programm {|+1;;|};
[%expect{|
Debug: let parser failed
Debug: parens parser failed
Debug: parens parser failed
Debug: constint parser failed
Debug: constchar parser failed
Debug: apply parser failed
Debug: parens parser failed
Debug: let parser failed
Debug: parens parser failed
Debug: constint parser failed
Debug: constchar parser failed
Debug: apply parser failed
Debug: parens parser failed
Debug: constint parser failed
Debug: constchar parser failed
Debug: compare parser failed
Debug: pseval parser failed
Debug: value_binding parser failed
[(Str_eval (Exp_constant (Const_integer 1)))] |}]

(*good*)
let%expect_test " nt constant" =
test_programm {|1;;|};
[%expect{|
Debug: let parser failed
Debug: parens parser failed
Debug: parens parser failed
Debug: constint parser failed
Debug: constchar parser failed
Debug: apply parser failed
Debug: parens parser failed
Debug: let parser failed
Debug: parens parser failed
Debug: constint parser failed
Debug: constchar parser failed
Debug: apply parser failed
Debug: parens parser failed
Debug: constint parser failed
Debug: constchar parser failed
Debug: compare parser failed
Debug: pseval parser failed
Debug: value_binding parser failed
[(Str_eval (Exp_constant (Const_integer 1)))] |}]

(*good*)
let%expect_test "whitespace befor int constant" =
test_programm {|     1;;|};
[%expect{|
Debug: let parser failed
Debug: parens parser failed
Debug: parens parser failed
Debug: constint parser failed
Debug: constchar parser failed
Debug: apply parser failed
Debug: parens parser failed
Debug: let parser failed
Debug: parens parser failed
Debug: constint parser failed
Debug: constchar parser failed
Debug: apply parser failed
Debug: parens parser failed
Debug: constint parser failed
Debug: constchar parser failed
Debug: compare parser failed
Debug: pseval parser failed
Debug: value_binding parser failed
[(Str_eval (Exp_constant (Const_integer 1)))] |}]

(*good*)
let%expect_test "negative zero" =
test_programm {|-0;;|};
[%expect{|
Debug: let parser failed
Debug: parens parser failed
Debug: parens parser failed
Debug: constint parser failed
Debug: constchar parser failed
Debug: apply parser failed
Debug: parens parser failed
Debug: let parser failed
Debug: parens parser failed
Debug: constint parser failed
Debug: constchar parser failed
Debug: apply parser failed
Debug: parens parser failed
Debug: constint parser failed
Debug: constchar parser failed
Debug: compare parser failed
Debug: pseval parser failed
Debug: value_binding parser failed
[(Str_eval (Exp_constant (Const_integer 0)))] |}]

(*good*)
let%expect_test "positive zero" =
test_programm {|+0;;|};
[%expect{|
Debug: let parser failed
Debug: parens parser failed
Debug: parens parser failed
Debug: constint parser failed
Debug: constchar parser failed
Debug: apply parser failed
Debug: parens parser failed
Debug: let parser failed
Debug: parens parser failed
Debug: constint parser failed
Debug: constchar parser failed
Debug: apply parser failed
Debug: parens parser failed
Debug: constint parser failed
Debug: constchar parser failed
Debug: compare parser failed
Debug: pseval parser failed
Debug: value_binding parser failed
[(Str_eval (Exp_constant (Const_integer 0)))] |}]

(*good*)
let%expect_test "zero" =
test_programm {|0;;|};
[%expect{|
Debug: let parser failed
Debug: parens parser failed
Debug: parens parser failed
Debug: constint parser failed
Debug: constchar parser failed
Debug: apply parser failed
Debug: parens parser failed
Debug: let parser failed
Debug: parens parser failed
Debug: constint parser failed
Debug: constchar parser failed
Debug: apply parser failed
Debug: parens parser failed
Debug: constint parser failed
Debug: constchar parser failed
Debug: compare parser failed
Debug: pseval parser failed
Debug: value_binding parser failed
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
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 252, characters 2-29
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: let parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed |}]

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
    Called from Tests__Parser.(fun) in file "tests/parser.ml", line 281, characters 2-25
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

    Trailing output
    ---------------
    Debug: let parser failed
    Debug: parens parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: apply parser failed
    Debug: parens parser failed |}]
   
(*good*)
let%expect_test "substraction" =
    test_programm {|5-11;;|};
  [%expect{|
    Debug: let parser failed
    Debug: parens parser failed
    Debug: parens parser failed
    Debug: let parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: compare parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed
    [(Str_eval
        (Exp_apply ((Exp_constant (Const_integer 5)),
           (Exp_constant (Const_integer -11)))))
      ] |}]

      (*good*)
  let%expect_test "strange move" =
    test_programm {|5=5;;|};
  [%expect{|
    Debug: let parser failed
    Debug: parens parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: parens parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: let parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: compare parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed
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
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: compare parser failed
    Debug: pseval parser failed
    Debug: let parser failed
    Debug: parens parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: value_binding parser failed
    Debug: let parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: compare parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed
    [(Str_value (Nonrecursive,
        [{ pat = (Pat_var "x"); expr = (Exp_constant (Const_integer 52)) }]))
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
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 400, characters 4-27
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: let parser failed
  Debug: parens parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed |}]

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
        Called from Tests__Parser.(fun) in file "tests/parser.ml", line 425, characters 6-31
        Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

        Trailing output
        ---------------
        Debug: let parser failed
        Debug: parens parser failed
        Debug: parens parser failed |}]
    
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
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 446, characters 2-27
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: let parser failed
  Debug: parens parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed |}]
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
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 470, characters 4-31
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: let parser failed
  Debug: parens parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed |}]

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
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 495, characters 2-24
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: let parser failed
  Debug: let parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: compare parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: let parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: compare parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed |}]

  let%expect_test "parenthesis3" =
  test_programm {|(5);;|};
  [%expect{|
    Debug: let parser failed
    Debug: let parser failed
    Debug: parens parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: apply parser failed
    Debug: let parser failed
    Debug: parens parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: let parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: compare parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed
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
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 576, characters 2-31
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: let parser failed
  Debug: let parser failed
  Debug: parens parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: let parser failed
  Debug: parens parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed |}]

  (*bad*)
let%expect_test "parenthesis2" =
test_programm {|(5-1);;|};
[%expect{|
  Debug: let parser failed
  Debug: let parser failed
  Debug: parens parser failed
  Debug: parens parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: let parser failed
  Debug: parens parser failed
  Debug: parens parser failed
  Debug: let parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed
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
      Called from Tests__Parser.(fun) in file "tests/parser.ml", line 649, characters 0-23
      Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

      Trailing output
      ---------------
      Debug: let parser failed
      Debug: let parser failed
      Debug: parens parser failed
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: constchar parser failed
      Debug: apply parser failed
      Debug: parens parser failed
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: constchar parser failed
      Debug: apply parser failed
      Debug: let parser failed
      Debug: parens parser failed
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: constchar parser failed
      Debug: apply parser failed
      Debug: parens parser failed
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: constchar parser failed
      Debug: compare parser failed
      Debug: pseval parser failed
      Debug: value_binding parser failed |}]




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
      Called from Tests__Parser.(fun) in file "tests/parser.ml", line 694, characters 6-32
      Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19
    
      Trailing output
      ---------------
      Debug: let parser failed
      Debug: parens parser failed
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: constchar parser failed
      Debug: apply parser failed
      Debug: parens parser failed |}]

let%expect_test "let assignment" =
  test_programm {|let x = 5 in 6;;|};
[%expect{|
  Debug: parens parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: value_binding parser failed
  Debug: parens parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: let parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed
  [(Str_eval
      (Exp_let (Nonrecursive,
         [{ pat = (Pat_var "x"); expr = (Exp_constant (Const_integer 5)) }],
         (Exp_constant (Const_integer 6)))))
    ] |}]

let%expect_test "let assignment with recursion" =
    test_programm {|let rec x = 5 in 6;;|};
  [%expect{|
    Debug: parens parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: value_binding parser failed
    Debug: parens parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: let parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: compare parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed
    [(Str_eval
        (Exp_let (Recursive,
           [{ pat = (Pat_var "x"); expr = (Exp_constant (Const_integer 5)) }],
           (Exp_constant (Const_integer 6)))))
      ] |}]

  let%expect_test "apply" =
    test_programm {|f x;;|};
  [%expect.unreachable]
  [@@expect.uncaught_exn {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    (Failure ": end_of_input")
    Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
    Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
    Called from Tests__Parser.(fun) in file "tests/parser.ml", line 784, characters 4-27
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

    Trailing output
    ---------------
    Debug: let parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: compare parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed |}]

  let%expect_test "(f (x-1))" =
    test_programm {|f (x-1);;|};
  [%expect.unreachable]
  [@@expect.uncaught_exn {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    (Failure ": end_of_input")
    Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
    Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
    Called from Tests__Parser.(fun) in file "tests/parser.ml", line 812, characters 4-31
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

    Trailing output
    ---------------
    Debug: let parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: compare parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed |}]
  
  let%expect_test "(x * f (x-1) )" =
  test_programm {|x * f (X-1);;|};
[%expect.unreachable]
  [@@expect.uncaught_exn {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    (Failure ": end_of_input")
    Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
    Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
    Called from Tests__Parser.(fun) in file "tests/parser.ml", line 840, characters 2-33
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

    Trailing output
    ---------------
    Debug: let parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: compare parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed |}]

let%expect_test "( let f x = x+5)" =
  test_programm {|let f x = x + 5;;|};
[%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 868, characters 2-37
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: value_binding parser failed
  Debug: let parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed |}]

let%expect_test "( let fact x = fact(x-1))" =
    test_programm {|let fact x = fact(x-1);;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 897, characters 4-46
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: value_binding parser failed
  Debug: let parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed |}]

let%expect_test "( if x=5 then 6)" =
    test_programm {|if x = 5 then 6;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 926, characters 4-39
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: let parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed |}]

let%expect_test "(if x = 5 then 6 else 7)" =
    test_programm {|if x = 5 then 6 else 7;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 954, characters 4-46
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: let parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed |}]

  let%expect_test "(let x = if 5=5 then 6 else 7)" =
  test_programm {|let x = if 5=5 then 6 else 7;;|};
[%expect.unreachable]
  [@@expect.uncaught_exn {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    (Failure ": end_of_input")
    Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
    Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
    Called from Tests__Parser.(fun) in file "tests/parser.ml", line 982, characters 2-50
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

    Trailing output
    ---------------
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: compare parser failed
    Debug: value_binding parser failed
    Debug: let parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: apply parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: compare parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed |}]

let%expect_test "(let rec fact x = if x = 0 then 1 else x * fact (x-1))" =
    test_programm {|let rec fact x = if x = 0 then 1 else x * fact(X-1);;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 1019, characters 4-75
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: value_binding parser failed
  Debug: let parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: apply parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: compare parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed |}]
  

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