  $ ../examples/ast_factorial/ast_factorial.exe
  [(LabelExpr "factorial"); (InstructionExpr (Mv (X15, X10)));
    (InstructionExpr (Li (X10, (Immediate32 1))));
    (InstructionExpr (Beq (X15, X0, (LabelAddress12 ".L5"))));
    (LabelExpr ".L2"); (InstructionExpr (Mv (X14, X15)));
    (InstructionExpr (Addiw (X15, X15, (Immediate12 -1))));
    (InstructionExpr (Mulw (X10, X14, X10)));
    (InstructionExpr (Bne (X15, X0, (LabelAddress12 ".L2"))));
    (LabelExpr ".L5"); (InstructionExpr Ret)]
  [(LabelExpr "factorial"); (InstructionExpr (Mv (X15, X10)));
    (InstructionExpr (Li (X10, (Immediate32 1))));
    (InstructionExpr (Beq (X15, X0, (LabelAddress12 ".L5"))));
    (LabelExpr ".L2"); (InstructionExpr (Mv (X14, X15)));
    (InstructionExpr (Addiw (X15, X15, (Immediate12 -1))));
    (InstructionExpr (Mulw (X10, X14, X10)));
    (InstructionExpr (Bne (X15, X0, (LabelAddress12 ".L2"))));
    (LabelExpr ".L5"); (InstructionExpr Ret)]
