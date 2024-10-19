
open Ocamladt_lib.Parser
open Ocamladt_lib.Ast
(* open Angstrom *)
let test_programm str = print_endline(show_program(parse_str str))

(*let rec fact n = if n = 0 then 1 else n * fact (n - 1);;*)

let%expect_test "empty_program" = 
    test_programm {||};
  [%expect{|
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: conststring parser failed
    Debug: mul_div parser failed
    Debug: add_sub parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed
    [] |}]

let%expect_test "double semicolons" = 
  test_programm {|;;|};
[%expect{|
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: conststring parser failed
  Debug: mul_div parser failed
  Debug: add_sub parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: conststring parser failed
  Debug: mul_div parser failed
  Debug: add_sub parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed
  [(Str_value (Nonrecursive, []))] |}]  

let%expect_test "(whitespaces)" =
test_programm {|       ;;|};
[%expect{|
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: conststring parser failed
  Debug: mul_div parser failed
  Debug: add_sub parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: conststring parser failed
  Debug: mul_div parser failed
  Debug: add_sub parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed
  [(Str_value (Nonrecursive, []))] |}]

  let%expect_test "-1" =
  test_programm {|1;;|};
[%expect{|
  Debug: parens parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: conststring parser failed
  Debug: mul_div parser failed
  Debug: add_sub parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed
  [(Str_eval (Exp_constant (Const_integer 1)))] |}]

let%expect_test "-111 5" =
  test_programm {|- 111 5;;|};
[%expect{|
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: conststring parser failed
  Debug: mul_div parser failed
  Debug: add_sub parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed
  [] |}]

  let%expect_test "5+" =
  test_programm {|5 +;;|};
[%expect{|
  Debug: parens parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: conststring parser failed
  Debug: mul_div parser failed
  [] |}]

let%expect_test "5-1" =
    test_programm {|5-11;;|};
  [%expect{|
    Debug: parens parser failed
    Debug: parens parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: conststring parser failed
    Debug: mul_div parser failed
    Debug: add_sub parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed
    [(Str_eval
        (Exp_apply ((Exp_ident "-"),
           (Exp_tuple ((Exp_constant (Const_integer 5)),
              (Exp_constant (Const_integer 11)), []))
           )))
      ] |}]

    let%expect_test "5" =
    test_programm {|5;;|};
    [%expect{|
      Debug: parens parser failed
      Debug: parens parser failed
      Debug: constint parser failed
      Debug: constchar parser failed
      Debug: conststring parser failed
      Debug: mul_div parser failed
      Debug: add_sub parser failed
      Debug: pseval parser failed
      Debug: value_binding parser failed
      [(Str_eval (Exp_constant (Const_integer 5)))] |}]

  let%expect_test "5-1" =
    test_programm {|5-11;;|};
  [%expect{|
    Debug: parens parser failed
    Debug: parens parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: conststring parser failed
    Debug: mul_div parser failed
    Debug: add_sub parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed
    [(Str_eval
        (Exp_apply ((Exp_ident "-"),
           (Exp_tuple ((Exp_constant (Const_integer 5)),
              (Exp_constant (Const_integer 11)), []))
           )))
      ] |}]

  let%expect_test "(5=5)" =
    test_programm {|5=5;;|};
  [%expect{|
    Debug: parens parser failed
    [] |}]
  
  let%expect_test "(x = 5)" =
    test_programm {|x=5;;|};
  [%expect{|
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: conststring parser failed
    Debug: mul_div parser failed
    Debug: add_sub parser failed
    Debug: pseval parser failed
    Debug: parens parser failed
    Debug: value_binding parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: conststring parser failed
    Debug: mul_div parser failed
    Debug: add_sub parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed
    [(Str_value (Nonrecursive,
        [{ pat = (Pat_var "x"); expr = (Exp_constant (Const_integer 5)) }]))
      ] |}]
  
let%expect_test "1-1" =
  test_programm {|1-1;;|};
[%expect{|
  Debug: parens parser failed
  Debug: parens parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: conststring parser failed
  Debug: mul_div parser failed
  Debug: add_sub parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed
  [(Str_eval
      (Exp_apply ((Exp_ident "-"),
         (Exp_tuple ((Exp_constant (Const_integer 1)),
            (Exp_constant (Const_integer 1)), []))
         )))
    ] |}]

let%expect_test "(1-1)" =
  test_programm {|(1-1);;|};
[%expect{|
  Debug: parens parser failed
  Debug: parens parser failed
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: conststring parser failed
  Debug: mul_div parser failed
  Debug: add_sub parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed
  [(Str_eval
      (Exp_apply ((Exp_ident "-"),
         (Exp_tuple ((Exp_constant (Const_integer 1)),
            (Exp_constant (Const_integer 1)), []))
         )))
    ] |}]

let%expect_test "( 5*5)" =
    test_programm {|5*5;;|};
  [%expect{|
    Debug: parens parser failed
    Debug: parens parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: conststring parser failed
    Debug: mul_div parser failed
    Debug: add_sub parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed
    [(Str_eval
        (Exp_apply ((Exp_ident "*"),
           (Exp_tuple ((Exp_constant (Const_integer 5)),
              (Exp_constant (Const_integer 5)), []))
           )))
      ] |}]
  
let%expect_test "(5*(5-1);;)" =
    test_programm {|5*(5-1);;|};
  [%expect{|
    Debug: parens parser failed
    Debug: parens parser failed
    Debug: parens parser failed
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: conststring parser failed
    Debug: mul_div parser failed
    Debug: add_sub parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed
    [(Str_eval
        (Exp_apply ((Exp_ident "*"),
           (Exp_tuple ((Exp_constant (Const_integer 5)),
              (Exp_apply ((Exp_ident "-"),
                 (Exp_tuple ((Exp_constant (Const_integer 5)),
                    (Exp_constant (Const_integer 1)), []))
                 )),
              []))
           )))
      ] |}]

let%expect_test "(5-1)" =
  test_programm {|(5-1)|};
[%expect{|
  Debug: parens parser failed
  Debug: parens parser failed
  [] |}]

let%expect_test "(let x = 5)" =
  test_programm {|let x = 5;;|};
[%expect{|
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: conststring parser failed
  Debug: mul_div parser failed
  Debug: add_sub parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed
  [] |}]

let%expect_test "(let rec x = 5)" =
    test_programm {|let rec x = 5;;|};
  [%expect{|
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: conststring parser failed
    Debug: mul_div parser failed
    Debug: add_sub parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed
    [] |}]

  let%expect_test "(f x)" =
    test_programm {|f x;;|};
  [%expect{|
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: conststring parser failed
    Debug: mul_div parser failed
    Debug: add_sub parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed
    [] |}]

  let%expect_test "(f (x-1))" =
    test_programm {|f (x-1);;|};
  [%expect{|
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: conststring parser failed
    Debug: mul_div parser failed
    Debug: add_sub parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed
    [] |}]
  
  let%expect_test "(x * f (x-1) )" =
  test_programm {|x * f (X-1);;|};
[%expect{|
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: conststring parser failed
  Debug: mul_div parser failed
  Debug: add_sub parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed
  [] |}]

let%expect_test "( let f x = x+5)" =
  test_programm {|let f x = x + 5;;|};
[%expect{|
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: conststring parser failed
  Debug: mul_div parser failed
  Debug: add_sub parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed
  [] |}]

let%expect_test "( let fact x = fact(x-1))" =
    test_programm {|let fact x = fact(x-1);;|};
  [%expect{|
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: conststring parser failed
    Debug: mul_div parser failed
    Debug: add_sub parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed
    [] |}]

let%expect_test "( if x=5 then 6)" =
    test_programm {|if x = 5 then 6;;|};
  [%expect{|
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: conststring parser failed
    Debug: mul_div parser failed
    Debug: add_sub parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed
    [] |}]

let%expect_test "(if x = 5 then 6 else 7)" =
    test_programm {|if x = 5 then 6 else 7;;|};
  [%expect{|
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: conststring parser failed
    Debug: mul_div parser failed
    Debug: add_sub parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed
    [] |}]

  let%expect_test "(let x = if 5=5 then 6 else 7)" =
  test_programm {|let x = if 5=5 then 6 else 7;;|};
[%expect{|
  Debug: parens parser failed
  Debug: constint parser failed
  Debug: constchar parser failed
  Debug: conststring parser failed
  Debug: mul_div parser failed
  Debug: add_sub parser failed
  Debug: pseval parser failed
  Debug: value_binding parser failed
  [] |}]

let%expect_test "(let rec fact x = if x = 0 then 1 else x * fact (x-1))" =
    test_programm {|let rec fact x = if x = 0 then 1 else x * fact(X-1);;|};
  [%expect{|
    Debug: parens parser failed
    Debug: constint parser failed
    Debug: constchar parser failed
    Debug: conststring parser failed
    Debug: mul_div parser failed
    Debug: add_sub parser failed
    Debug: pseval parser failed
    Debug: value_binding parser failed
    [] |}]
  

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