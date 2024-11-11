open Ocamladt_lib.Parser
open Ocamladt_lib.Ast
open Pprinter

let%expect_test "parse_const_int" =
  pp pp_expression pconst "5" ;
  [%expect {| (Exp_constant (Const_integer 5)) |}]

let%expect_test "parse_const_string" =
  pp pp_expression pconst "\"Hello world!\"" ;
  [%expect {| (Exp_constant (Const_string "Hello world!")) |}]

let%expect_test "parse_const_char" =
  pp pp_expression pconst "'m'" ;
  [%expect {| (Exp_constant (Const_char 'm')) |}]

