Copyright 2024-2025, Damir Yunusov, Ilhom Kombaev
SPDX-License-Identifier: LGPL-3.0-or-later

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let _ = homka
  Parsed result: (Pstr_value (NonRecursive,
                    [{ pvb_pat = Ppat_any; pvb_expr = (Pexp_ident "homka") }]))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let homka = homka
  Parsed result: (Pstr_value (NonRecursive,
                    [{ pvb_pat = (Ppat_var "homka");
                       pvb_expr = (Pexp_ident "homka") }
                      ]
                    ))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let 122 = homka
  Error: : Pattern name must be wildcard, variable or tuple of them

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let "homka" = "homka"
  Error: : Pattern name must be wildcard, variable or tuple of them

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let _, _ = homka
  Parsed result: (Pstr_value (NonRecursive,
                    [{ pvb_pat = (Ppat_tuple [Ppat_any; Ppat_any]);
                       pvb_expr = (Pexp_ident "homka") }
                      ]
                    ))


  $ ../bin/REPL.exe -dparsetree <<EOF
  > let x, y = homka
  Parsed result: (Pstr_value (NonRecursive,
                    [{ pvb_pat = (Ppat_tuple [(Ppat_var "x"); (Ppat_var "y")]);
                       pvb_expr = (Pexp_ident "homka") }
                      ]
                    ))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let (_, _, _) = homka
  Parsed result: (Pstr_value (NonRecursive,
                    [{ pvb_pat = (Ppat_tuple [Ppat_any; Ppat_any; Ppat_any]);
                       pvb_expr = (Pexp_ident "homka") }
                      ]
                    ))

  $ ../bin/REPL.exe -dparsetree <<EOF
  > let "a" .. "z" = homka
  Error: : Pattern name must be wildcard, variable or tuple of them
