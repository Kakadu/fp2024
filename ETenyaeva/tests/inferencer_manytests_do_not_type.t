  $ cat ./manytests/do_not_type/001.ml | ../bin/REPL.exe --dinference
  Inferencer error: Unbound variable fac'.

  $ cat ./manytests/do_not_type/002if.ml | ../bin/REPL.exe --dinference
  Inferencer error: Failed to unify types: int and bool
  

  $ cat ./manytests/do_not_type/003occurs.ml | ../bin/REPL.exe --dinference
  Inferencer error: Occurs check failed. Type variable 'ty1 occurs inside 'ty1 -> 'ty3
  

  $ cat ./manytests/do_not_type/004let_poly.ml | ../bin/REPL.exe --dinference
  Inferencer error: Failed to unify types: int and bool
  

  $ cat ./manytests/do_not_type/005.ml | ../bin/REPL.exe --dinference
  Inferencer error: Failed to unify types: string and int
  

  $ cat ./manytests/do_not_type/015tuples.ml | ../bin/REPL.exe --dinference
  Inferencer error: Only variables are allowed as left-hand side of `let rec'

  $ cat ./manytests/do_not_type/016tuples_mismatch.ml | ../bin/REPL.exe --dinference
  Inferencer error: Failed to unify types: int * int * int and 'ty0 * 'ty1
  

  $ cat ./manytests/do_not_type/097fun_vs_list.ml | ../bin/REPL.exe --dinference
  Inferencer error: Failed to unify types: 'ty0 -> 'ty0 and 'ty2 list
  

  $ cat ./manytests/do_not_type/097fun_vs_unit.ml | ../bin/REPL.exe --dinference
  Inferencer error: Failed to unify types: 'ty0 -> 'ty0 and unit
  

  $ cat ./manytests/do_not_type/098rec_int.ml | ../bin/REPL.exe --dinference
  Inferencer error: This kind of expression is not allowed as right-hand side of `let rec'

  $ cat ./manytests/do_not_type/099.ml | ../bin/REPL.exe --dinference
  Inferencer error: Only variables are allowed as left-hand side of `let rec'
