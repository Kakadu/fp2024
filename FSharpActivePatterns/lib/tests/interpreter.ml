(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpActivePatterns.REPLUtils

let%expect_test "Even-Odd active pattern" =
  let program =
    {|
let (|MinusTwo|Not|) v =
  if v+2 = 0 then MinusTwo (v+10)
  else Not (v)

let res = match 1 with
  | MinusTwo val -> val
  | Not val -> val
  in print_int res|}
  in
  run_repl false None (Some program);
  [%expect
    {|
    1
    val MinusTwo : int -> int = <fun>
    val Not : int -> int = <fun> |}]
;;
