(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
 
  $ ../bin/main.exe
  | Let a =
  --| Const(Int: 10)
  | Rec Function(factorial):
    ARGS
  ----| Variable(n)
    BODY
  ----| If Then Else(
        CONDITION
  ------| Binary expr(
  ------| Logical Or
  --------| Binary expr(
  --------| Binary Equal
  ----------| Variable(n)
  ----------| Const(Int: 0)
  --------| Binary expr(
  --------| Binary Equal
  ----------| Variable(n)
  ----------| Const(Int: 1)
        THEN BRANCH
  --------| Const(Int: 1)
        ELSE BRANCH
  --------| Binary expr(
  --------| Binary Multiply
  ----------| Variable(n)
  ----------| Function Call:
              FUNCTION
  ------------| Variable(factorial)
              ARGS
  ------------| Binary expr(
  ------------| Binary Subtract
  --------------| Variable(n)
  --------------| Const(Int: 1)
  | Function Call:
    FUNCTION
  --| Variable(factorial)
    ARGS
  --| Variable(a)
