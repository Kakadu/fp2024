Copyright 2024, Kostya Oreshin and Nikita Shchutskii
SPDX-License-Identifier: MIT
  $ ../lib/run_binding.exe -seed 67 -gen 1 -stop
  random seed: 67
  ================================================================================
  success (ran 1 tests)

  $ ../bin/REPL.exe manytests/do_not_type/001.hs -ptypes
  Undefined variable 'fac'

  $ ../bin/REPL.exe manytests/do_not_type/002if.hs -ptypes
  unification failed on Int and Bool

  $ ../bin/REPL.exe manytests/do_not_type/003occurs.hs -ptypes
  Occurs check failed

  $ ../bin/REPL.exe manytests/do_not_type/004_let_poly.hs -ptypes
  unification failed on Int and Bool

  $ ../bin/REPL.exe manytests/do_not_type/099.hs -ptypes
  unification failed on Maybe t7 and Ord t10 -> Ord t10 -> Bool
  unification failed on () and t12 -> t12
  [ 
  x:  Int
   ]

  $ ../bin/REPL.exe manytests/typed/001fac.hs

  $ ../bin/REPL.exe manytests/typed/002fac.hs

  $ ../bin/REPL.exe manytests/typed/003fib.hs

  $ ../bin/REPL.exe manytests/typed/004manyargs.hs

  $ ../bin/REPL.exe manytests/typed/005fix.hs

  $ ../bin/REPL.exe manytests/typed/006partial.hs

  $ ../bin/REPL.exe manytests/typed/006partial2.hs

  $ ../bin/REPL.exe manytests/typed/006partial3.hs

  $ ../bin/REPL.exe manytests/typed/007order.hs

  $ ../bin/REPL.exe manytests/typed/008ascription.hs

  $ ../bin/REPL.exe manytests/typed/009let_poly.hs -ptypes
  [ 
  temp:  (Int, Bool)
   ]


  $ ../bin/REPL.exe manytests/typed/010sukharev.hs -ptypes
  [ 
  _1: t7.  Int -> Int -> (Int, t7) -> Bool
  _2:  Int
  _3:  Maybe (Int, Bool)
  _4:  Int
  _42: t49.  t49 -> Bool
  _5: t46.  Int -> t46
  int_of_option: t40.  Maybe t40 -> Int
   ]

  $ ../bin/REPL.exe manytests/typed/015tuples.hs

  $ ../bin/REPL.exe manytests/typed/016lists.hs

  $ ../bin/REPL.exe <<-EOF
  >  fac0 self n = if n<2 then n else n* self (n-1) 
  >  fix f = f (fix f)
  >  fac = fix fac0
  >  main = print_int (fac 3)
  > EOF

# fibonacci
  $ ../bin/REPL.exe <<-EOF
  > iter f xs = case xs of [] -> (); h:tl -> let () = f h in iter f tl
  > take n xs = case xs of [] -> []; h:tl -> if n>0 then h : (take (n-1) tl) else []
  > tail xs = case xs of h:tl -> tl
  > zip_with f xs ys = case (xs,ys) of ([],[]) -> []; (h:tl, h2:tl2) -> (f h h2) : zip_with f tl tl2
  > fib = 0:1:(zip_with (+) fib (tail fib))
  > main = seq (iter print_int (take 10 fib)) 0
  > EOF


# TODO(Kakadu): It would be great to call read GHCi somewhere in the tests

  $ ../bin/REPL.exe <<-EOF
  > tree_example = (3; (4; (7; $; $); (11; $; $)); (8; (23; $; $); (1; $; $)))
  > min x y = if x > y then y else x
  > iter tree z = case tree of $ -> z; (v; l; r) -> let z1 = v:z; z2 = iter l z1; z3 = iter r z2 in z3
  > repmin t = r where (r, m) = repmin1 t m; repmin1 $ m = ($, 9999999); repmin1 (v; l; r) m = let (lt, lm) = repmin1 l m; (rt, rm) = repmin1 r m in ((m; lt; rt), min v (min lm rm))
  > old_values = iter tree_example []
  > new_tree = repmin tree_example
  > new_values = iter new_tree []
  > print_list xs = case xs of [] -> (); h:tl -> seq (print_int h) (print_list tl)
  > print_list old_values
  > print_list new_values
  > EOF

