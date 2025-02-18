  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/001.ml
  Error: Unbound value fac

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/002if.ml
  Error: Can't unify (int) and (bool)

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/003occurs.ml
  Error: (OccursCheckFailed (9, (TArrow ((TVar 9), (TVar 12)))))

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/004let_poly.ml
  Error: Can't unify (int) and (bool)

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/005.ml
  Error: Can't unify (string) and (int)

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/015tuples.ml
  Error: (SomeError "Only variables are allowed as left-hand side of `let rec`")

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/016tuples_mismatch.ml
  Error: Can't unify ('a * 'b) and (int * int * int)

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/097fun_vs_list.ml
  Error: Can't unify ('a list) and ('a -> 'a)

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/097fun_vs_unit.ml
  Error: Can't unify (unit) and ('a -> 'a)

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/098rec_int.ml
  Error: (SomeError
            "This kind of expression is not allowed as right-hand side of 'let rec'")

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/099.ml
  Error: (SomeError "Only variables are allowed as left-hand side of `let rec`")
