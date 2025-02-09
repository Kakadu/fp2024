Copyright 2024-2025, Damir Yunusov, Ilhom Kombaev
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../bin/REPL.exe -dparsetree <<EOF
  > 1
  Parsed result: (Pstr_eval (Pexp_constant (Pconst_int 1)))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > 1_000
  Parsed result: (Pstr_eval (Pexp_constant (Pconst_int 1000)))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > 1___1
  Parsed result: (Pstr_eval (Pexp_constant (Pconst_int 11)))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > 1_000_000
  Parsed result: (Pstr_eval (Pexp_constant (Pconst_int 1000000)))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > _
  Error: : not enough input

  $ ../bin/REPL.exe -dparsetree <<EOF
  > "Homka"
  Parsed result: (Pstr_eval (Pexp_constant (Pconst_string "Homka")))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > true
  Parsed result: (Pstr_eval (Pexp_constant (Pconst_boolean true)))
