  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/001.ml
  Error: (Unbound "fac")

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/002if.ml
  Error: (UnificationFailed ((TBase BInt), (TBase BBool)))

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/003occurs.ml
  Error: (OccursCheckFailed (4, (TArrow ((TVar 4), (TVar 7)))))

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/004let_poly.ml
  Error: (UnificationFailed ((TBase BInt), (TBase BBool)))

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/005.ml
  Error: (UnificationFailed ((TBase BString), (TBase BInt)))

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/015tuples.ml
  Error: (SomeError "Only variables are allowed as left-hand side of `let rec`")

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/016tuples_mismatch.ml
  Error: (UnificationFailed ((TTuple ((TVar 3), (TVar 4), [])),
            (TTuple ((TBase BInt), (TBase BInt), [(TBase BInt)]))))

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/097fun_vs_list.ml
  Error: : end_of_input

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/097fun_vs_unit.ml
  Error: (UnificationFailed ((TBase BUnit), (TArrow ((TVar 3), (TVar 3)))))

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/098rec_int.ml
  val x : int
  

  $ ../bin/REPL.exe -dinferprogram < manytests/do_not_type/099.ml
  Error: (SomeError "Only variables are allowed as left-hand side of `let rec`")
