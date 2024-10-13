(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpActivePatterns.Ast
open FSharpActivePatterns.PrintAst
open FSharpActivePatterns.Parser

let () =
  let input = "if ((n = 0) || (n = 1)) 
                  then 1 
                  else n" in
  let result = parse input in
  print_construction (Expr result)
;;
