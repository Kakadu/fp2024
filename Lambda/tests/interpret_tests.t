Copyright 2021-2024, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Cram tests here. They run and compare program output to the expected output
https://dune.readthedocs.io/en/stable/tests.html#cram-tests
Use `dune promote` after you change things that should runned

If you need to put sample program and use it both in your interpreter and preinstalled one,
you could put it into separate file. Thise will need stanza `(cram (deps demo_input.txt))`
in the dune file

  $ ../bin/REPL.exe -cbv -dparsetree <<EOF
  > \f.x
  Parsed result: (Abs (f, (Var x)))
  Evaluated result: (λ _ . x)
  $ ../bin/REPL.exe -dparsetree <<EOF
  > garbage242
  Error: : end_of_input



  $ ../bin/REPL.exe -no -dparsetree <<EOF
  > (\x.\y.x)(\u.u)((\x. x x)(\x.x x))
  Parsed result: (App (
                    (App ((Abs (x, (Abs (y, (Var x))))), (Abs (u, (Var u))))),
                    (App ((Abs (x, (App ((Var x), (Var x))))),
                       (Abs (x, (App ((Var x), (Var x)))))))
                    ))
  Evaluated result: (λ u . u)
Below we redirect contents of the file to the evaluator
  $ ../bin/REPL.exe -dparsetree -stop-after parsing   < lam_1+1.txt
  Parsed result: (App (
                    (Abs (m,
                       (Abs (n,
                          (Abs (f,
                             (Abs (x,
                                (App ((Var m),
                                   (App ((Var f),
                                      (App ((Var n), (App ((Var f), (Var x)))))
                                      ))
                                   ))
                                ))
                             ))
                          ))
                       )),
                    (App ((Abs (f, (Abs (x, (App ((Var f), (Var x))))))),
                       (Abs (f, (Abs (x, (App ((Var f), (Var x)))))))))
                    ))

  $ ../bin/REPL.exe -ao   < lam_1+1.txt
  Evaluated result: (λ n f x _x -> ((f (n (f x))) _x))
  $ ../bin/REPL.exe -ao   < lam_2x1.txt
  Evaluated result: 2
Call by value doesn't reduce under abstraction
  $ ../bin/REPL.exe -cbv   < lam_2x1.txt
  Evaluated result: (λ z . (2 (1 z)))
  $ ../bin/REPL.exe -ao -small   < lam_3x2.txt
   -- ((λ y z -> ((λ f x -> (f (f (f x)))) (y z))) 2)
   -- ((λ y z x -> ((y z) ((y z) ((y z) x)))) 2)
   -- (λ z x -> ((2 z) ((2 z) ((2 z) x))))
   -- (λ z x -> ((λ x . (z (z x))) ((2 z) ((2 z) x))))
   -- (λ z x -> ((λ x . (z (z x))) ((λ x . (z (z x))) ((2 z) x))))
   -- (λ z x -> ((λ x . (z (z x))) ((λ x . (z (z x))) ((λ x . (z (z x))) x))))
   -- (λ z x -> ((λ x . (z (z x))) ((λ x . (z (z x))) (z (z x)))))
   -- (λ z x -> ((λ x . (z (z x))) (z (z (z (z x))))))
   -- (λ z x -> (z (z (z (z (z (z x)))))))
  Evaluated result: (λ z x -> (z (z (z (z (z (z x)))))))
  $ ../bin/REPL.exe -ao   < lam_zero.txt
  Evaluated result: ⊥
For 3! we use noral order reduction
  $ cat lam_fac3.txt
  (((λ f . ((λ x . (f (x x))) (λ x . (f (x x))))) (λ s . (λ n . ((((λ n . ((n (λ x . (λ x . (λ y . y)))) (λ x . (λ y . x)))) n) (λ f . (λ x . (f x)))) (((λ x . (λ y . (λ z . (x (y z))))) (s ((λ n . (λ f . (λ x . (((n (λ g . (λ h . (h (g f))))) (λ u . x)) (λ u . u))))) n))) n))))) (λ f . (λ x . (f (f (f x))))))
  $ ../bin/REPL.exe -no   < lam_fac3.txt
  Evaluated result: (λ z x -> (z (z (z (z (z (z x)))))))
