  $ ../repl/REPL.exe -dparsetree -filepath="../examples/ast_factorial/factorial.s"
  [(DirectiveExpr (File "factorial.c")); (DirectiveExpr (Option "pic"));
    (DirectiveExpr
       (Attribute ("arch",
          (StrValue "rv64i2p1_m2p0_a2p1_f2p2_d2p2_c2p0_zicsr2p0_zifencei2p0"))));
    (DirectiveExpr (Attribute ("unaligned_access", (IntValue 0))));
    (DirectiveExpr (Attribute ("stack_align", (IntValue 16))));
    (DirectiveExpr Text); (DirectiveExpr (Align 1));
    (DirectiveExpr (Globl (LabelAddress12 "factorial")));
    (DirectiveExpr (TypeDir ("factorial", (Type "function"))));
    (LabelExpr "factorial"); (LabelExpr ".LFB23");
    (DirectiveExpr CfiStartproc); (InstructionExpr (Mv (X15, X10)));
    (InstructionExpr (Li (X10, (Immediate32 1))));
    (InstructionExpr (Beq (X15, X0, (LabelAddress12 ".L5"))));
    (LabelExpr ".L2"); (InstructionExpr (Mv (X14, X15)));
    (InstructionExpr (Addiw (X15, X15, (Immediate12 -1))));
    (InstructionExpr (Mulw (X10, X14, X10)));
    (InstructionExpr (Bne (X15, X0, (LabelAddress12 ".L2"))));
    (LabelExpr ".L5"); (InstructionExpr Ret); (DirectiveExpr CfiEndproc);
    (LabelExpr ".LFE23");
    (DirectiveExpr (Size ((LabelAddress12 "factorial"), ".-factorial")));
    (DirectiveExpr
       (Section (".rodata.str1.8", "aMS", (Type "progbits"), (Some 1))));
    (DirectiveExpr (Align 3)); (LabelExpr ".LC0");
    (DirectiveExpr (String "%d")); (DirectiveExpr (Align 3));
    (LabelExpr ".LC1"); (DirectiveExpr (String "%d\n"));
    (DirectiveExpr (Section (".text.startup", "ax", (Type "progbits"), None)));
    (DirectiveExpr (Align 1)); (DirectiveExpr (Globl (LabelAddress12 "main")));
    (DirectiveExpr (TypeDir ("main", (Type "function")))); (LabelExpr "main");
    (LabelExpr ".LFB24"); (DirectiveExpr CfiStartproc);
    (InstructionExpr (Addi (X2, X2, (Immediate12 -32))));
    (DirectiveExpr (CfiDefCfaOffset 32));
    (InstructionExpr (Sd (X8, X2, (ImmediateAddress12 (Immediate12 16)))));
    (DirectiveExpr (CfiOffset (8, -16)));
    (InstructionExpr (La (X8, (LabelAddress32 "__stack_chk_guard"))));
    (InstructionExpr (Ld (X15, X8, (ImmediateAddress12 (Immediate12 0)))));
    (InstructionExpr (Sd (X15, X2, (ImmediateAddress12 (Immediate12 8)))));
    (InstructionExpr (Li (X15, (Immediate32 0))));
    (InstructionExpr (Addi (X11, X2, (Immediate12 4))));
    (InstructionExpr (Lla (X10, (LabelAddress32 ".LC0"))));
    (InstructionExpr (Sd (X1, X2, (ImmediateAddress12 (Immediate12 24)))));
    (DirectiveExpr (CfiOffset (1, -8)));
    (InstructionExpr (Call "__isoc99_scanf@plt"));
    (InstructionExpr (Li (X15, (Immediate32 1))));
    (InstructionExpr (Bne (X10, X15, (LabelAddress12 ".L16"))));
    (InstructionExpr (Lw (X15, X2, (Immediate12 4))));
    (InstructionExpr (Mv (X12, X10)));
    (InstructionExpr (Beq (X15, X0, (LabelAddress12 ".L14"))));
    (LabelExpr ".L13"); (InstructionExpr (Mv (X14, X15)));
    (InstructionExpr (Addiw (X15, X15, (Immediate12 -1))));
    (InstructionExpr (Mulw (X12, X14, X12)));
    (InstructionExpr (Bne (X15, X0, (LabelAddress12 ".L13"))));
    (LabelExpr ".L14"); (InstructionExpr (Lla (X11, (LabelAddress32 ".LC1"))));
    (InstructionExpr (Li (X10, (Immediate32 2))));
    (InstructionExpr (Call "__printf_chk@plt"));
    (InstructionExpr (Li (X10, (Immediate32 0)))); (LabelExpr ".L12");
    (InstructionExpr (Ld (X14, X2, (ImmediateAddress12 (Immediate12 8)))));
    (InstructionExpr (Ld (X15, X8, (ImmediateAddress12 (Immediate12 0)))));
    (InstructionExpr (Xor (X15, X14, X15)));
    (InstructionExpr (Li (X14, (Immediate32 0))));
    (InstructionExpr (Bne (X15, X0, (LabelAddress12 ".L22"))));
    (InstructionExpr (Ld (X1, X2, (ImmediateAddress12 (Immediate12 24)))));
    (DirectiveExpr CfiRememberState); (DirectiveExpr (CfiRestore 1));
    (InstructionExpr (Ld (X8, X2, (ImmediateAddress12 (Immediate12 16)))));
    (DirectiveExpr (CfiRestore 8));
    (InstructionExpr (Addi (X2, X2, (Immediate12 32))));
    (DirectiveExpr (CfiDefCfaOffset 0)); (InstructionExpr (Jr X1));
    (LabelExpr ".L16"); (DirectiveExpr CfiRestoreState);
    (InstructionExpr (Li (X10, (Immediate32 -1))));
    (InstructionExpr (J (LabelAddress20 ".L12"))); (LabelExpr ".L22");
    (InstructionExpr (Call "__stack_chk_fail@plt"));
    (DirectiveExpr CfiEndproc); (LabelExpr ".LFE24");
    (DirectiveExpr (Size ((LabelAddress12 "main"), ".-main")));
    (DirectiveExpr (Ident "GCC: (Ubuntu 13.2.0-23ubuntu4) 13.2.0"));
    (DirectiveExpr (Section (".note.GNU-stack", "", (Type "progbits"), None)))]
