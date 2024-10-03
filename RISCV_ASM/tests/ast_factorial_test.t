  $ ../examples/ast_factorial/ast_factorial.exe
  [(Label "factorial"); (Instruction (Mv (X15, X10)));
    (Instruction (Li (X10, 1))); (Instruction (Beq (X15, X0, (Label ".L5"))));
    (Label ".L2"); (Instruction (Mv (X14, X15)));
    (Instruction (Addiw (X15, X15, -1))); (Instruction (Mulw (X10, X14, X10)));
    (Instruction (Bne (X15, X0, (Label ".L2")))); (Label ".L5");
    (Instruction Ret)]
