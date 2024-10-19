(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpActivePatterns.PrintAst
open FSharpActivePatterns.Parser

let () =
  let input =
    "let factorial n =\n\
    \                  if ((n = 0)||(n = 1))\n\
    \                  then 1\n\
    \                  else n * factorial (n - 1)\n\
    \                in factorial (b)"
  in
  print_p_res (parse input)
;;
