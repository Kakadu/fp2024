  $ ../bin/REPL.exe <<EOF
  > (fun x -> x) (fun x -> x) (fun x -> x) 5

  $ ../bin/REPL.exe < manytests/typed/001fac.ml
  24

  $ ../bin/REPL.exe < manytests/typed/002fac.ml
  24

  $ ../bin/REPL.exe < manytests/typed/003fib.ml
  3
  3

  $ ../bin/REPL.exe < manytests/typed/004manyargs.ml
  1111111111
  1
  10
  100

  $ ../bin/REPL.exe < manytests/typed/005fix.ml
  720

  $ ../bin/REPL.exe < manytests/typed/006partial.ml
  1122

  $ ../bin/REPL.exe < manytests/typed/006partial2.ml
  1
  2
  3
  7

  $ ../bin/REPL.exe < manytests/typed/006partial3.ml
  4
  8
  9

Argument eval order is undefined in OCaml, so in my language that also undefined 😼
  $ ../bin/REPL.exe < manytests/typed/007order.ml
  1
  2
  4
  -1
  103
  -555555
  10000

  $ ../bin/REPL.exe < manytests/typed/008ascription.ml
  8

  $ ../bin/REPL.exe < manytests/typed/009let_poly.ml

  $ cat manytests/typed/010sukharev.ml | ../bin/REPL.exe
  Error while interpret expression ((Pexp_match (
                                       (Pexp_construct ("Some",
                                          (Some (Pexp_ident "id")))),
                                       [{ pc_lhs =
                                          (Ppat_construct ("Some",
                                             (Some (Ppat_var "f"))));
                                          pc_rhs =
                                          (Pexp_let (NonRecursive,
                                             [{ pvb_pat = Ppat_any;
                                                pvb_expr =
                                                (Pexp_apply ((Pexp_ident "f"),
                                                   [(Pexp_constant
                                                       (Pconst_string "42"))
                                                     ]
                                                   ))
                                                }
                                               ],
                                             (Pexp_apply ((Pexp_ident "f"),
                                                [(Pexp_constant (Pconst_int 42))
                                                  ]
                                                ))
                                             ))
                                          };
                                         { pc_lhs =
                                           (Ppat_construct ("None", None));
                                           pc_rhs =
                                           (Pexp_constant (Pconst_int 0)) }
                                         ]
                                       )))

  $ ../bin/REPL.exe < manytests/typed/015tuples.ml
  1
  Error: No variable with name modd
  $ ../bin/REPL.exe < manytests/typed/016lists.ml
  Error: No variable with name print_int
