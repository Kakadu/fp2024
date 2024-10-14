(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlRV_lib.Parser

(* if-then-else tests *)
let%test _ = test_parse_pif "if x then y else z"
let%test _ = test_parse_pif "if 5 > 3 then true else false"
let%test _ = test_parse_pif "if a then b else c"
let%test _ = test_parse_pif "if x then 1 + 2 else 3"
let%test _ = test_parse_pif "if true then false else true"

(* unary operator tests *)
let%test _ = test_parse_unop "- 5"
let%test _ = test_parse_unop "- x"
let%test _ = test_parse_unop "+ 5"
let%test _ = test_parse_unop "+ x"
let%test _ = test_parse_unop "notx"

(* number tests *)
let%test _ = test_parse_pnumber "-5"
let%test _ = test_parse_pnumber "2134324"
let%test _ = test_parse_pnumber "-525"
let%test _ = test_parse_pnumber "true"
let%test _ = test_parse_pnumber "false"

(* binary operator tests *)
let%test _ = test_parse_binop_expr "5 + 5"
let%test _ = test_parse_binop_expr "5+5"
let%test _ = test_parse_binop_expr "2 - 3"
let%test _ = test_parse_binop_expr " 2 -2 -2"
let%test _ = test_parse_binop_expr "4 * 4";;

let b = test_parse "let rec fact n = if n <= 1 then 1 else n * fact (n - 1);;" in
Format.printf (if b then "ok\n" else "not ok\n")
