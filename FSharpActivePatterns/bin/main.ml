(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpActivePatterns.PrintAst
open FSharpActivePatterns.Parser

let () =
  let input = " if (if true then true else false) then 2 else 4  " in
  print_p_res (parse input)
;;
