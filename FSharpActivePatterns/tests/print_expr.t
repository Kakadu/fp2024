(** Copyright 2021-2024, Ksenia Kotelnikova, Gleb Nasretdinov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
 
  $ ../bin/exec.exe
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
  ----------| Function Call(factorial):
              ARGS
  ------------| Binary expr(
  ------------| Binary Subtract
  --------------| Variable(n)
  --------------| Const(Int: 1)
  | Function Call(factorial):
    ARGS
  --| Variable(a)
