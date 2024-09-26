(** Copyright 2024-2025, Friend-zva, RodionovMaxim05 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let () =
  print_endline
    {|"let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)"
        (Str_value(
          Recursive,
          {
            pat = (Pat_var "factorial");
            exp = (
              Exp_fun(
                Pat_var "n",
                Exp_ifelsethen(
                  Exp_apply(
                    Exp_apply(
                      Exp_ident "<=",
                      Exp_ident "n"
                    ),
                    Exp_constant(Const_integer "1")
                  ),
                  Exp_ident "1",
                  Some(Exp_apply(
                    Exp_apply(
                      Exp_ident "*",
                      Exp_ident "n"
                    ),
                    Exp_apply(
                      Exp_ident "factorial",
                      Exp_apply(
                        Exp_apply(
                          Exp_ident "-",
                          Exp_ident "n"
                        ),
                        Exp_constant(Const_integer "1")
                      )
                    )
                  ))
                )
              )
            )
          }
        ))
    |}
;;
