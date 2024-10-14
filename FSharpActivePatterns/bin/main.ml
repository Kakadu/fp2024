(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpActivePatterns.PrintAst
open FSharpActivePatterns.Parser

let () =
  let input = " f (4) (5)" in
  let result = parse input in
  print_construction result
;;
