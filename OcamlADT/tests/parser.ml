
open Ocamladt_lib.Parser
open Ocamladt_lib.Ast
(* open Angstrom *)
let test_programm str = print_endline(show_program(parse_str str))

(*let rec fact n = if n = 0 then 1 else n * fact (n - 1);;*)

(*good*)
let%expect_test "empty program" = 
    test_programm {||};
  [%expect{|
    Debug: identexp parser failed
    Debug: token_parser parser failed
    Debug: fun parser failed
    Debug: ps_eval parser failed
    [] |}]

    (*good*)
let%expect_test "double semicolons" = 
  test_programm {|;;|};
[%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 21, characters 2-22
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]  

  (*good*)
let%expect_test "(whitespaces)" =
test_programm {|       ;;|};
[%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 43, characters 0-27
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]

(*good*)
  let%expect_test "negative int constant" =
  test_programm {|-1;;|};
[%expect.unreachable]
  [@@expect.uncaught_exn {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    (Failure ": end_of_input")
    Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
    Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
    Called from Tests__Parser.(fun) in file "tests/parser.ml", line 65, characters 2-24
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

    Trailing output
    ---------------
    Debug: identexp parser failed
    Debug: token_parser parser failed
    Debug: fun parser failed
    Debug: ps_eval parser failed |}]
  
(*good*)
let%expect_test "positive int constant" =
test_programm {|+1;;|};
[%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 87, characters 0-22
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]

(*good*)
let%expect_test " nt constant" =
test_programm {|1;;|};
[%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 109, characters 0-21
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]

(*good*)
let%expect_test "whitespace befor int constant" =
test_programm {|     1;;|};
[%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 131, characters 0-26
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]

(*good*)
let%expect_test "negative zero" =
test_programm {|-0;;|};
[%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 153, characters 0-22
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]

(*good*)
let%expect_test "positive zero" =
test_programm {|+0;;|};
[%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 175, characters 0-22
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]

(*good*)
let%expect_test "zero" =
test_programm {|0;;|};
[%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 197, characters 0-21
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]

(*bad?, why? (i guess, no)*)
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
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 219, characters 2-29
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]

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
    Called from Tests__Parser.(fun) in file "tests/parser.ml", line 241, characters 2-25
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

    Trailing output
    ---------------
    Debug: identexp parser failed
    Debug: token_parser parser failed
    Debug: fun parser failed
    Debug: ps_eval parser failed |}]
   
(*good*)
let%expect_test "substraction" =
    test_programm {|5-11;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 263, characters 4-28
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]

      (*good*)
  let%expect_test "strange move" =
    test_programm {|5=5;;|};
  [%expect.unreachable]
  [@@expect.uncaught_exn {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    (Failure ": end_of_input")
    Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
    Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
    Called from Tests__Parser.(fun) in file "tests/parser.ml", line 285, characters 4-27
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

    Trailing output
    ---------------
    Debug: identexp parser failed
    Debug: token_parser parser failed
    Debug: fun parser failed
    Debug: ps_eval parser failed |}]
  
    (*good*)
  let%expect_test "(assignment)" =
    test_programm {|x = 52;;|};
  [%expect.unreachable]
  [@@expect.uncaught_exn {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    (Failure ": end_of_input")
    Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
    Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
    Called from Tests__Parser.(fun) in file "tests/parser.ml", line 307, characters 4-30
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

    Trailing output
    ---------------
    Debug: identexp parser SUCCEEDED
    Debug: ps_eval parser SUCCEEDED |}]
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
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 326, characters 4-27
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]

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
        Called from Tests__Parser.(fun) in file "tests/parser.ml", line 348, characters 6-31
        Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

        Trailing output
        ---------------
        Debug: identexp parser failed
        Debug: token_parser parser failed
        Debug: fun parser failed
        Debug: ps_eval parser failed |}]
    
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
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 370, characters 2-27
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]
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
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 391, characters 4-31
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]

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
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 413, characters 2-24
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]

  let%expect_test "parenthesis3" =
  test_programm {|(5);;|};
  [%expect.unreachable]
  [@@expect.uncaught_exn {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    (Failure ": end_of_input")
    Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
    Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
    Called from Tests__Parser.(fun) in file "tests/parser.ml", line 434, characters 2-25
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

    Trailing output
    ---------------
    Debug: identexp parser failed
    Debug: token_parser parser failed
    Debug: fun parser failed
    Debug: ps_eval parser failed |}]
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
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 455, characters 2-31
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]

  (*bad*)
let%expect_test "parenthesis2" =
test_programm {|(5-1);;|};
[%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 477, characters 0-25
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]
(* good fr *)
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
      Called from Tests__Parser.(fun) in file "tests/parser.ml", line 498, characters 0-23
      Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

      Trailing output
      ---------------
      Debug: identexp parser failed
      Debug: token_parser parser failed
      Debug: fun parser failed
      Debug: ps_eval parser failed |}]


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
        Called from Tests__Parser.(fun) in file "tests/parser.ml", line 520, characters 6-32
        Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

        Trailing output
        ---------------
        Debug: identexp parser failed
        Debug: token_parser parser failed
        Debug: fun parser failed
        Debug: ps_eval parser failed |}]

let%expect_test "let assignment" =
  test_programm {|let x = 5 in 6;;|};
[%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 541, characters 2-36
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]

let%expect_test "let assignment with recursion" =
    test_programm {|let rec x = 5 in 6;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 562, characters 4-42
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]
(*bad*)
let%expect_test "let assignment with recursion" =
  test_programm {|let rec x = 5 in 7;;|};
[%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 583, characters 2-40
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]

  let%expect_test "apply" =
    test_programm {|x;;|};
  [%expect{|
    Debug: identexp parser SUCCEEDED
    Debug: ps_eval parser SUCCEEDED
    Debug: identexp parser failed
    Debug: token_parser parser failed
    Debug: fun parser failed
    Debug: ps_eval parser failed
    [(Str_eval (Exp_ident "x"))] |}]

    let%expect_test "apply without space" =
    test_programm {|f(x);;|};
  [%expect.unreachable]
    [@@expect.uncaught_exn {|
      (* CR expect_test_collector: This test expectation appears to contain a backtrace.
         This is strongly discouraged as backtraces are fragile.
         Please change this test to not include a backtrace. *)

      (Failure ": end_of_input")
      Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
      Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
      Called from Tests__Parser.(fun) in file "tests/parser.ml", line 615, characters 4-28
      Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

      Trailing output
      ---------------
      Debug: identexp parser SUCCEEDED
      Debug: ps_eval parser SUCCEEDED |}]


let%expect_test "apply num to ident" =
    test_programm {|f (x-1);;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 635, characters 4-31
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser SUCCEEDED
  Debug: ps_eval parser SUCCEEDED |}]
  
      let%expect_test "simple fun" =
      test_programm {|fun x -> y;;|};
    [%expect{|
      Debug: identexp parser failed
      Debug: token_parser parser SUCCEEDED
      Debug: token_parser parser failed
      Debug: pattern_parser parser SUCCEEDED
      Debug: token_parser parser SUCCEEDED
      Debug: identexp parser SUCCEEDED
      Debug: fun parser SUCCEEDED
      Debug: ps_eval parser SUCCEEDED
      Debug: identexp parser failed
      Debug: token_parser parser failed
      Debug: fun parser failed
      Debug: ps_eval parser failed
      [(Str_eval (Exp_fun ((Pat_var "x"), (Exp_ident "y"))))] |}]

      let%expect_test "multi pattern fun" =
      test_programm {|fun x z -> y;;|};
    [%expect.unreachable]
      [@@expect.uncaught_exn {|
        (* CR expect_test_collector: This test expectation appears to contain a backtrace.
           This is strongly discouraged as backtraces are fragile.
           Please change this test to not include a backtrace. *)

        (Failure ": end_of_input")
        Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
        Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
        Called from Tests__Parser.(fun) in file "tests/parser.ml", line 671, characters 6-38
        Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

        Trailing output
        ---------------
        Debug: identexp parser failed
        Debug: token_parser parser SUCCEEDED
        Debug: token_parser parser failed
        Debug: pattern_parser parser SUCCEEDED
        Debug: token_parser parser failed
        Debug: fun parser failed
        Debug: ps_eval parser failed |}]

      let%expect_test "multi fun" =
      test_programm {|fun p -> fun x -> z;;|};
    [%expect{|
      Debug: identexp parser failed
      Debug: token_parser parser SUCCEEDED
      Debug: token_parser parser failed
      Debug: pattern_parser parser SUCCEEDED
      Debug: token_parser parser SUCCEEDED
      Debug: identexp parser failed
      Debug: token_parser parser SUCCEEDED
      Debug: token_parser parser failed
      Debug: pattern_parser parser SUCCEEDED
      Debug: token_parser parser SUCCEEDED
      Debug: identexp parser SUCCEEDED
      Debug: fun parser SUCCEEDED
      Debug: fun parser SUCCEEDED
      Debug: ps_eval parser SUCCEEDED
      Debug: identexp parser failed
      Debug: token_parser parser failed
      Debug: fun parser failed
      Debug: ps_eval parser failed
      [(Str_eval
          (Exp_fun ((Pat_var "p"), (Exp_fun ((Pat_var "x"), (Exp_ident "z"))))))
        ] |}]
    

  let%expect_test "apply and subtraction" =
    test_programm {|f (x-1);;|};
  [%expect.unreachable]
  [@@expect.uncaught_exn {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    (Failure ": end_of_input")
    Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
    Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
    Called from Tests__Parser.(fun) in file "tests/parser.ml", line 721, characters 4-31
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

    Trailing output
    ---------------
    Debug: identexp parser SUCCEEDED
    Debug: ps_eval parser SUCCEEDED |}]
  
      let%expect_test "let fun" =
      test_programm {|let f x = x*x;;|};
    [%expect.unreachable]
      [@@expect.uncaught_exn {|
        (* CR expect_test_collector: This test expectation appears to contain a backtrace.
           This is strongly discouraged as backtraces are fragile.
           Please change this test to not include a backtrace. *)

        (Failure ": end_of_input")
        Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
        Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
        Called from Tests__Parser.(fun) in file "tests/parser.ml", line 740, characters 6-39
        Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

        Trailing output
        ---------------
        Debug: identexp parser failed
        Debug: token_parser parser failed
        Debug: fun parser failed
        Debug: ps_eval parser failed |}]

  let%expect_test "multiplication and apply" =
  test_programm {|x * f x;;|};
[%expect.unreachable]
  [@@expect.uncaught_exn {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    (Failure ": end_of_input")
    Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
    Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
    Called from Tests__Parser.(fun) in file "tests/parser.ml", line 761, characters 2-29
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

    Trailing output
    ---------------
    Debug: identexp parser SUCCEEDED
    Debug: ps_eval parser SUCCEEDED |}]

let%expect_test "let and apply" =
  test_programm {|let f x = 5 in 6;;|};
[%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 780, characters 2-38
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]

let%expect_test "let and apply v2" =
    test_programm {|let fact x = fact(x-1);;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 801, characters 4-46
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]

let%expect_test "if then" =
    test_programm {|if 5 then 6;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 822, characters 4-35
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]

let%expect_test "if statement. condition from fact" =
    test_programm {|if n = 0 then 1 else 7;;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 843, characters 4-46
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]

  let%expect_test "let and if" =
  test_programm {|let x = if n = 0 then 6 else 7 in 6;;|};
[%expect.unreachable]
  [@@expect.uncaught_exn {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    (Failure ": end_of_input")
    Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
    Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
    Called from Tests__Parser.(fun) in file "tests/parser.ml", line 864, characters 2-57
    Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

    Trailing output
    ---------------
    Debug: identexp parser failed
    Debug: token_parser parser failed
    Debug: fun parser failed
    Debug: ps_eval parser failed |}]

let%expect_test "factorial" =
    test_programm {|let rec fact x = if x = 0 then 1 else x * fact(X-1);;|};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure ": end_of_input")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Tests__Parser.test_programm in file "tests/parser.ml", line 5, characters 50-65
  Called from Tests__Parser.(fun) in file "tests/parser.ml", line 885, characters 4-75
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Debug: identexp parser failed
  Debug: token_parser parser failed
  Debug: fun parser failed
  Debug: ps_eval parser failed |}]
  

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