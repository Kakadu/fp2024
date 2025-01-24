Copyright 2024-2025, Damir Yunusov, Ilhom Kombaev
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let _ = homka
  Parsed result: (Pstr_value (NonRecursive,
                    [{ pvb_pat = (Ppat_var "_");
                       pvb_expr = (Pexp_ident "homka") }
                      ]
                    ))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let homka = homka
  Parsed result: (Pstr_value (NonRecursive,
                    [{ pvb_pat = (Ppat_var "homka");
                       pvb_expr = (Pexp_ident "homka") }
                      ]
                    ))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let 122 = homka
  Error: : satisfy: '1'

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let "homka" = "homka"
  Error: : satisfy: '"'

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let _, _ = homka
  Error: : string


  $ ../bin/REPL.exe -dparsetree <<EOF
  > let x, y = homka
  Error: : string

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let (_, _, _) = homka
  Error: : satisfy: '('

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let "a" .. "z" = homka
  Error: : satisfy: '"'
