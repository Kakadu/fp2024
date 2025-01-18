  $ ./qCheckRun.exe --seed 1234567890
   seed: 1234567890
  
  --- Failure --------------------------------------------------------------------
  
  Test anon_test_1 failed (10 shrink steps):
  
  AST:
  
  [(Str_item_def (Nonrecursive,
      (Bind ((Pattern_const (Const_string "1LJ")), (Expr_ident_or_op ""))),
      [(Bind ((Pattern_const (Const_float 0.)), (Expr_ident_or_op "")))]))
    ]
  
  Pprinted:
  
  let "1LJ" =  and 0.000000 = ;;
  
  
  Parsed:
  
  : end_of_input
  
  
  ================================================================================
  failure (1 tests failed, 0 tests errored, ran 1 tests)
  [1]
