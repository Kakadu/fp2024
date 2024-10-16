(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCamlRV_lib.Parser

(* if-then-else tests *)
let%test _ = parse_to_bool "if x then y else z"
let%test _ = parse_to_bool "if 5 > 3 then true else false"
let%test _ = parse_to_bool "if a then b else c"
let%test _ = parse_to_bool "if x then 1 + 2 else 3"
let%test _ = parse_to_bool "if true then false else true"

(* unary operator tests *)
let%test _ = parse_to_bool "- 5"
let%test _ = parse_to_bool "- x"
let%test _ = parse_to_bool "+ 5"
let%test _ = parse_to_bool "+ x"
let%test _ = parse_to_bool "notx"

(* number tests *)
let%test _ = parse_to_bool "-5"
let%test _ = parse_to_bool "2134324"
let%test _ = parse_to_bool "-525"
let%test _ = parse_to_bool "true"
let%test _ = parse_to_bool "false"

(* binary operator tests *)
let%test _ = parse_to_bool "5 + 5"
let%test _ = parse_to_bool "5+5"
let%test _ = parse_to_bool "2 - 3"
let%test _ = parse_to_bool " 2 -2 -2"
let%test _ = parse_to_bool "4 * 4"

(* Factorial test *)
let%expect_test _ =
  parse_to_unit "let rec fact n = if n <= 1 then 1 else n * fact (n - 1)";
  [%expect
    {|
[(SValue (Rec,
    [((PLiteral (StringLiteral "fact")),
      (ExprFun ((PLiteral (StringLiteral "n")), [],
         (ExprIf (
            (ExprBinOperation (Lte, (ExprVariable "n"),
               (ExprLiteral (IntLiteral 1)))),
            (ExprLiteral (IntLiteral 1)),
            (Some (ExprBinOperation (Mul, (ExprVariable "n"),
                     (ExprApply ((ExprVariable "fact"),
                        (ExprBinOperation (Sub, (ExprVariable "n"),
                           (ExprLiteral (IntLiteral 1))))
                        ))
                     )))
            ))
         )))
      ]
    ))
  ]
|}]
;;
