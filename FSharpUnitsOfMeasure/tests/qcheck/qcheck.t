  $ ./qCheckRun.exe --seed 111111111
   seed: 111111111
  
  --- Failure --------------------------------------------------------------------
  
  Test anon_test_1 failed (0 shrink steps):
  
  AST:
  
  [(Str_item_eval
      (Expr_match (
         (Expr_typed ((Expr_const (Const_char 'X')), (Type_ident "char"))),
         (Rule (Pattern_wild, (Expr_ident_or_op "_j531"))),
         [(Rule ((Pattern_ident_or_op "_O"), (Expr_const (Const_bool false))));
           (Rule ((Pattern_const (Const_string "$TNsq^]am")),
              (Expr_ident_or_op "a47")));
           (Rule ((Pattern_ident_or_op "t5"), (Expr_ident_or_op "gs")));
           (Rule ((Pattern_ident_or_op "__U4'"), (Expr_ident_or_op "g")));
           (Rule ((Pattern_ident_or_op "zqy'p"), (Expr_const (Const_bool true))
              ))
           ]
         )));
    (Str_item_def (Nonrecursive,
       (Bind ((Pattern_ident_or_op "_Sy"), (Expr_const (Const_string "<X&D("))
          )),
       [(Bind ((Pattern_ident_or_op "_QQf"), (Expr_ident_or_op "tOWs")));
         (Bind (Pattern_wild,
            (Expr_const
               (Const_unit_of_measure
                  (Unit_of_measure ((Mnum_float -3625.09046152),
                     (Measure_ident "b9o'5")))))
            ))
         ]
       ))
    ]
  
  Pprinted:
  
  match 'X' : char with _ -> _j531 | _O -> false | "$TNsq^]am" -> a47 | t5 -> gs | __U4' -> g | zqy'p -> true;;
  
  let _Sy = "<X&D(" and _QQf = tOWs and _ = -3625.090462<b9o'5>;;
  
  
  Parsed:
  
  : end_of_input
  
  
  ================================================================================
  failure (1 tests failed, 0 tests errored, ran 1 tests)
  [1]

скобочки в межры + сократить флоты
