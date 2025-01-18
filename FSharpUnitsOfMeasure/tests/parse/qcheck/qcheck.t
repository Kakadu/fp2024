  $ ./qCheckRun.exe --seed 7
   seed: 7
  
  --- Failure --------------------------------------------------------------------
  
  Test anon_test_1 failed (2 shrink steps):
  
  AST:
  
  [(Str_item_def (Nonrecursive,
      (Bind (
         (Pattern_const
            (Const_unit_of_measure
               (Unit_of_measure ((Mnum_float 1.04931758405),
                  (Measure_prod (Measure_dimless, (Measure_ident "_5q1"))))))),
         (Expr_ident_or_op "x1DZ'"))),
      []))
    ]
  
  Pprinted:
  
  let 1.04931758405<1 * _5q1> = x1DZ';;
  
  
  
  ================================================================================
  failure (1 tests failed, 0 tests errored, ran 1 tests)
  [1]
