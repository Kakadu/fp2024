  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/001.ml
  Error: (Unbound "fac")

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/002if.ml
  Error: Error when unify (int) and (bool)

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/003occurs.ml
  Error: (OccursCheckFailed (9, (TArrow ((TVar 9), (TVar 12)))))

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/004let_poly.ml
  Error: Error when unify (int) and (bool)

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/005.ml
  Error: Error when unify (string) and (int)

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/015tuples.ml
  Error: (SomeError "Only variables are allowed as left-hand side of `let rec`")

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/016tuples_mismatch.ml
  Error: Error when unify ('a * 'b) and (int * int * int)

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/097fun_vs_list.ml
  Error: Error when unify ('a list) and ('a -> 'a)

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/097fun_vs_unit.ml
  Error: Error when unify (unit) and ('a -> 'a)

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/098rec_int.ml
  val x : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/099.ml
  Error: (SomeError "Only variables are allowed as left-hand side of `let rec`")
