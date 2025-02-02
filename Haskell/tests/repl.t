Copyright 2024, Kostya Oreshin and Nikita Shchutskii
SPDX-License-Identifier: MIT
  $ ../lib/run_binding.exe -seed 67 -gen 3 -stop
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
  24
  $ ../bin/REPL.exe manytests/typed/002fac.hs
  24

  $ ../bin/REPL.exe manytests/typed/003fib.hs
  3
  3
 
  $ ../bin/REPL.exe manytests/typed/004manyargs.hs
  1111111111

  $ ../bin/REPL.exe manytests/typed/005fix.hs
  720

  $ ../bin/REPL.exe manytests/typed/006partial.hs
  1122

  $ ../bin/REPL.exe manytests/typed/006partial2.hs
  1
  2
  3
  7

  $ ../bin/REPL.exe manytests/typed/006partial3.hs
  4
  8
  9

  $ ../bin/REPL.exe manytests/typed/007order.hs
  1
  2
  4
  -1
  103
  -555555
  10000

  $ ../bin/REPL.exe manytests/typed/008ascription.hs
  8

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

  $ ../bin/REPL.exe manytests/typed/016lists.hs
  1
  2
  3
  8


  $ ../bin/REPL.exe manytests/typed/015tuples.hs
  1
  1
  1
  1

  $ ../bin/REPL.exe <<-EOF
  >  fac0 self n = if n<2 then n else n * self (n-1) 
  >  fix f = f (fix f)
  >  fac = fix fac0
  >  main = print_int (fac 3)
  > EOF
  6

# fibonacci
  $ ../bin/REPL.exe <<-EOF
  > iter f xs = case xs of [] -> (); h:tl -> seq (f h) (iter f tl)
  > take n xs = case xs of [] -> []; h:tl -> if n>0 then h : (take (n-1) tl) else []
  > tail xs = case xs of h:tl -> tl
  > zip_with f xs ys = case (xs,ys) of ([],[]) -> []; (h:tl, h2:tl2) -> (f h h2) : zip_with f tl tl2
  > fib = 0:1:(zip_with (+) fib (tail fib))
  > main = seq (iter print_int (take 10 fib)) 0
  > EOF
  0
  1
  1
  2
  3
  5
  8
  13
  21
  34

# sieve of Eratosthenes
  $ ../bin/REPL.exe <<-EOF
  > filter p (x:xs) | p x = x : (filter p xs) | True = filter p xs
  > primes = sieve [2..] where sieve (x:xs) = x : sieve (filter (\\n -> n \`mod\` x /= 0) xs)
  > iter f xs = case xs of [] -> (); h:tl -> seq (f h) (iter f tl)
  > take n xs = case xs of [] -> []; h:tl -> if n > 0 then h : (take (n - 1) tl) else []
  > main = seq (iter print_int (take 10 primes)) 0
  > EOF
  2
  3
  5
  7
  11
  13
  17
  19
  23
  29

# TODO(Kakadu): It would be great to call read GHCi somewhere in the tests
# rep min value in tree
  $ ../bin/REPL.exe <<-EOF
  > tree_example = (3; (4; (7; $; $); (11; $; $)); (0; (23; $; $); (1; $; $)))
  > min x y = if x > y then y else x
  > iter f $ = (); iter f (v;l;r) =  seq (f v) (seq (iter f l) (iter f r))
  > repmin t = r where (r, m) = repmin1 t m; repmin1 $ m = ($, 9999999); repmin1 (v; l; r) m = let (lt, lm) = repmin1 l m; (rt, rm) = repmin1 r m in ((m; lt; rt), min v (min lm rm))
  > new_tree = repmin tree_example
  > main = seq (iter print_int tree_example) (iter print_int new_tree)
  > EOF
  3
  4
  7
  11
  0
  23
  1
  0
  0
  0
  0
  0
  0
  0

# 2+2
  $ ../bin/REPL.exe  <<-EOF
  > fac x (y,z) =  x + y + z
  > main = print_int (fac 6 (8 , 9))
  > EOF
  23
]

#func
  $ ../bin/REPL.exe  <<-EOF
  > f x Nothing = 0; f z (Just y) = z ^ y 
  > main = print_int (f 2 (Just 10))
  > EOF
  1024

#func_reord
  $ ../bin/REPL.exe  <<-EOF
  > f z (Just y) = z ^ y ; f x Nothing = 0
  > main = print_int (f 2 (Just 10))
  > EOF
  1024

# link to link
  $ ../bin/REPL.exe  <<-EOF
  > f x = x + 2
  > g n =  f n 
  > main = print_int (f 2)
  > EOF
  4


# unused var -- not evaled
  $ ../bin/REPL.exe  <<-EOF
  > (x,y) = (seq (print_int 0) 5,  seq (print_int 1) 6)
  > main = print_int x
  > EOF
  0
  5

# not_exh -- ignored
  $ ../bin/REPL.exe  <<-EOF
  > (x,Nothing) = (1, Just 2)
  > main = print_int 0
  > EOF
  0

# not_exh 
  $ ../bin/REPL.exe  <<-EOF
  > (x,Nothing) = (1, Just 2)
  > main = print_int x
  > EOF
  Not exhaustive paterns


# eval_once
  $ ../bin/REPL.exe  <<-EOF
  > x = seq (print_int 0) 88
  > f y = y + y
  > main = print_int (f x)
  > EOF
  0
  176

# eval_once (subpattern)
  $ ../bin/REPL.exe  <<-EOF
  > a@(_, y) = (12, seq (print_int 0) 88)
  > main = let (_,z) = a in print_int (y+z)
  > EOF
  0
  176

# link to part
  $ ../bin/REPL.exe  <<-EOF
  > a = ( 0, (seq (print_int 0) 2, seq (print_int 1) 3))
  > (_, (_, p)) = a
  > main =  print_int p
  > EOF
  1
  3

# pat_match with val
  $ ../bin/REPL.exe  <<-EOF
  > a = ((+) 5 6, 2)
  > (1, x) = a
  > main =  print_int x
  > EOF
  Not exhaustive paterns

#guards and where
  $ ../bin/REPL.exe  <<-EOF
  > f x | x < y = x + y | x > y = x - y where y = x * (x \`mod\` 3)
  > main =  print_int (f 2)
  > EOF
  6

#guards not exh
  $ ../bin/REPL.exe  <<-EOF
  > f x | x < y = x + y | x > y = x - y where y = x * (x \`mod\` 3)
  > main =  print_int (f 1)
  > EOF
  Not exhaustive paterns

#div by zero
  $ ../bin/REPL.exe  <<-EOF
  > f x  = 2 \`div\` x
  > y = 1 - 1
  > main =  print_int (f y)
  > EOF
  Division by zero

#negative exponent
  $ ../bin/REPL.exe  <<-EOF
  > main =  print_int (5 ^ (-1))
  > EOF
  Negative exponent

# eval_once pattern_match
  $ ../bin/REPL.exe  <<-EOF
  > x = seq (print_int 0) 88
  > f 12 = 0; f 88 = 1; f 100 = 2
  > main = print_int (f x)
  > EOF
  0
  1

# eval_once pattern_match
  $ ../bin/REPL.exe  <<-EOF
  > x = seq (print_int 0) 88
  > f 12 = 0; f 88 = 1; f 100 = 2
  > main = print_int (f x)
  > EOF
  0
  1
 
# eval_once pattern_match (case)
  $ ../bin/REPL.exe  <<-EOF
  > x = seq (print_int 0) 88
  > main = print_int (case x of 12 -> 0; z | z < 88 -> 2 | z >= 88 -> 1)
  > EOF
  0
  1

# class ord
  $ ../bin/REPL.exe  <<-EOF
  > bool_to_int True = 1; bool_to_int False = 0
  > x@(_, (2, _)) = (1, (2,3))
  > a =  (1, (3, 0)) > x
  > b = (0; $; $) <= (0; (-1; $;$); $)
  > c = [1..12] < 1:2:3:5:[]
  > d = Just (1, Nothing) < Just (1, Just $)
  > main = print_int (x) where x = bool_to_int (a && b && c && d)
  > EOF
  1


# eval_once list
  $ ../bin/REPL.exe  <<-EOF
  > lst = [(seq (print_int 0 ) 18), (seq (print_int 1 ) 19) ]
  > x:xs = lst
  > main = let y:ys = lst in seq (print_int x) (print_int y) 
  > EOF
  0
  18
  18


# lazy lists
  $ ../bin/REPL.exe  <<-EOF
  > (1:[], [True], x:y:tl) = ([1, 3 .. 2 ], [True .. ], [5, 3 .. ])
  > ([], z:_) =  ([5, 1 .. 6], tl)
  > False:False:False:False:False:False:_ = [False, False .. True]
  > main = print_int (x + y + z )
  > EOF
  9

  $ ../bin/REPL.exe  <<-EOF
  > main = let [x, y, z] = [1 .. 4 ] in print_int x
  > EOF
  Not exhaustive paterns

# imho tests written below are less interesting

# eval_full trees, tuples, etc
  $ ../bin/REPL.exe  <<-EOF
  > tr@(x; $; _) = (5; $; $)
  > tup@(True, Just y, q, z:[], _) = (True, Just 9, 2, ():[], 0 \`mod\` 0 )
  > main = seq tr tup
  > EOF
  Division by zero

# eval_full trees, tuples, etc (expr)
  $ ../bin/REPL.exe  <<-EOF
  > main = seq ((0, 0, 0 )) (seq ((Nothing;$;$)) (Just (9 ^ (-1))) )
  > EOF
  Negative exponent

# not_exh Nul

  $ ../bin/REPL.exe  <<-EOF
  > x = (0; $; $)
  > z@$ = x
  > main = z
  > EOF
  Not exhaustive paterns

# pm Nothing
  $ ../bin/REPL.exe  <<-EOF
  > f _ = Nothing
  > z = f 0
  > main = print_int (case z of Nothing -> 0 )
  > EOF
  0

# pm consts, node
  $ ../bin/REPL.exe  <<-EOF
  > ((x,y); $; $) = (((), True); $ ; $)
  > i = 1 - 2
  > ((), True, z@(-1), -1) = (x, y, i, -1)
  > main = print_int z
  > EOF
  -1

# not_exh bool
  $ ../bin/REPL.exe  <<-EOF
  > z@False = True
  > main = z
  > EOF
  Not exhaustive paterns

# not_exh NegativePInt
  $ ../bin/REPL.exe  <<-EOF
  > z@(-1) = 0
  > main = z
  > EOF
  Not exhaustive paterns

# pm lists
  $ ../bin/REPL.exe  <<-EOF
  > a@[_,_] = [0, 1]
  > [b,_] = [1,2]
  > d@[c, _] = a
  > e:f:xs = 1:[2,3]
  > main = print_int (b + c + e + f)
  > EOF
  4

# not exh cons
  $ ../bin/REPL.exe  <<-EOF
  > x:y = []
  > main = x
  > EOF
  Not exhaustive paterns

# not exh just
  $ ../bin/REPL.exe  <<-EOF
  > y = (\\ _ -> Nothing) 0
  > Just x = y
  > main = x
  > EOF
  Not exhaustive paterns

# pm with values
  $ ../bin/REPL.exe  <<-EOF
  > a = Just 5
  > c = (Nothing; $;$)
  > d = [() .. ]
  > (  Just y, (Nothing;$;_), _:_) = let x = ( a, c, d ) in seq x x
  > main = print_int y
  > EOF
  5

# pm (patpat_match)
  $ ../bin/REPL.exe  <<-EOF
  > a@($, $,  Nothing, Nothing, 1, 1, -1) = ($, $,  Nothing, Nothing, 1, 1,  -1)
  > ($, b@$,  Nothing, c@Nothing, 1, d@1, e@(-1)) = a
  > main = print_int (d + e)
  > EOF
  0

# class ord err
  $ ../bin/REPL.exe  <<-EOF
  > x = 5 > (if 0 \`div\` 0  == 0 then 1 else 1 )
  > main = print_int (if x then 0 else 1)
  > EOF
  Division by zero

  $ ../bin/REPL.exe  <<-EOF
  > x = Nothing > (if 0 \`div\` 0  == 0 then Nothing else Nothing )
  > main = print_int (if x then 0 else 1)
  > EOF
  Division by zero

  $ ../bin/REPL.exe  <<-EOF
  > x = $ > (if 0 \`div\` 0  == 0 then $ else $ )
  > main = print_int (if x then 0 else 1)
  > EOF
  Division by zero

  $ ../bin/REPL.exe  <<-EOF
  > x = [] > (if 0 \`div\` 0  == 0 then [] else [] )
  > main = print_int (if x then 0 else 1)
  > EOF
  Division by zero

  $ ../bin/REPL.exe  <<-EOF
  > x = ((\\ _ -> [] ) 0 ) > (if 0 \`div\` 0  == 0 then [] else [] )
  > main = print_int (if x then 0 else 1)
  > EOF
  Division by zero

  $ ../bin/REPL.exe  <<-EOF
  > x = [1..] > 1 : (if 0 \`div\` 0  == 0 then [] else [] )
  > main = print_int (if x then 0 else 1)
  > EOF
  Division by zero

# class ord 
  $ ../bin/REPL.exe  <<-EOF
  > not True = False; not False = True
  > a = let x@[-5, _] = [-5, 0] in x /= [0,0] &&  not (x /= x)  &&  seq x (x == x)
  > b = let x@(Nothing, _) = (Nothing, 0) in x <= (Just x, 0) || seq x (x > (Just x, 0)) && (Just x, 0) > (Nothing, 0) 
  > c = let x@($, _) = ($, 0) in x /= ( (x;$;$), 0) || seq x (x >= ((x;$;$), 0))  && ((x;$;$), 0) > ($, 0)
  > d = let x@([], _) = ([], 0) in x == ( [1,2], 0) || seq x (x < ([1,2], 0)) || ([1,2], 0) < ([], 0)
  > e = let x@([], _) = ([], 0) in x == ( 1:[], 0) || seq x (x < (1:[], 0)) || (1:[], 0) == ([], 0)
  > f = let y:ys = [1..] in let x@([], _) = ([], 0) in x == ( ys , 0) || seq x (x < (ys, 0)) && ([],0) < (ys, 0)
  > main = case a && b && c && d  && e && f of True -> print_int 0 
  > EOF
  0

  $ ../bin/REPL.exe  <<-EOF
  > a = let x = Just 3 in let y@(Nothing, _) = (Nothing, 0) in  x /= Just 2 && x > Nothing || (x,0) > y || seq y (y /= (x,0))
  > b = let x = (2;$;$) in let y@($, _) = ($, 0) in  x /= (1;$;$) && x > $ || (x,0) > y || seq y (y /= (x,0))
  > c = let x = [2] in let y@([], _) = ([], 0) in  x /= [1] && x > [] || (x,0) > y || seq y (y /= (x,0))
  > d = let x = [2..] in let y@([], _) = ([], 0) in  x /= [1] && x > [] || (x,0) > y || seq y (y /= (x,0))
  > e = let x = 1:[] in let y@([], _) = ([], 0) in  x /= [1] && x > [] || (x,0) > y || seq y (y /= (x,0))
  > main = case a && b && c && d && e of True -> print_int 0 
  > EOF
  0
 
