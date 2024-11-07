  $ ../repl/REPL.exe -dparsetree -filepath="../examples/factorial/factorial.s"
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
    (DirectiveExpr CfiStartproc);
    (InstructionExpr (Mv ((Register 15), (Register 10))));
    (InstructionExpr (Li ((Register 10), (ImmediateAddress32 1))));
    (InstructionExpr
       (Beq ((Register 15), (Register 0), (LabelAddress12 ".L5"))));
    (LabelExpr ".L2"); (InstructionExpr (Mv ((Register 14), (Register 15))));
    (InstructionExpr
       (Addiw ((Register 15), (Register 15), (ImmediateAddress12 -1))));
    (InstructionExpr (Mulw ((Register 10), (Register 14), (Register 10))));
    (InstructionExpr
       (Bne ((Register 15), (Register 0), (LabelAddress12 ".L2"))));
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
    (InstructionExpr
       (Addi ((Register 2), (Register 2), (ImmediateAddress12 -32))));
    (DirectiveExpr (CfiDefCfaOffset 32));
    (InstructionExpr (Sd ((Register 8), (Register 2), (ImmediateAddress12 16))));
    (DirectiveExpr (CfiOffset (8, -16)));
    (InstructionExpr (La ((Register 8), (LabelAddress32 "__stack_chk_guard"))));
    (InstructionExpr (Ld ((Register 15), (Register 8), (ImmediateAddress12 0))));
    (InstructionExpr (Sd ((Register 15), (Register 2), (ImmediateAddress12 8))));
    (InstructionExpr (Li ((Register 15), (ImmediateAddress32 0))));
    (InstructionExpr
       (Addi ((Register 11), (Register 2), (ImmediateAddress12 4))));
    (InstructionExpr (Lla ((Register 10), (LabelAddress32 ".LC0"))));
    (InstructionExpr (Sd ((Register 1), (Register 2), (ImmediateAddress12 24))));
    (DirectiveExpr (CfiOffset (1, -8)));
    (InstructionExpr (Call "__isoc99_scanf@plt"));
    (InstructionExpr (Li ((Register 15), (ImmediateAddress32 1))));
    (InstructionExpr
       (Bne ((Register 10), (Register 15), (LabelAddress12 ".L16"))));
    (InstructionExpr (Lw ((Register 15), (Register 2), (ImmediateAddress12 4))));
    (InstructionExpr (Mv ((Register 12), (Register 10))));
    (InstructionExpr
       (Beq ((Register 15), (Register 0), (LabelAddress12 ".L14"))));
    (LabelExpr ".L13"); (InstructionExpr (Mv ((Register 14), (Register 15))));
    (InstructionExpr
       (Addiw ((Register 15), (Register 15), (ImmediateAddress12 -1))));
    (InstructionExpr (Mulw ((Register 12), (Register 14), (Register 12))));
    (InstructionExpr
       (Bne ((Register 15), (Register 0), (LabelAddress12 ".L13"))));
    (LabelExpr ".L14");
    (InstructionExpr (Lla ((Register 11), (LabelAddress32 ".LC1"))));
    (InstructionExpr (Li ((Register 10), (ImmediateAddress32 2))));
    (InstructionExpr (Call "__printf_chk@plt"));
    (InstructionExpr (Li ((Register 10), (ImmediateAddress32 0))));
    (LabelExpr ".L12");
    (InstructionExpr (Ld ((Register 14), (Register 2), (ImmediateAddress12 8))));
    (InstructionExpr (Ld ((Register 15), (Register 8), (ImmediateAddress12 0))));
    (InstructionExpr (Xor ((Register 15), (Register 14), (Register 15))));
    (InstructionExpr (Li ((Register 14), (ImmediateAddress32 0))));
    (InstructionExpr
       (Bne ((Register 15), (Register 0), (LabelAddress12 ".L22"))));
    (InstructionExpr (Ld ((Register 1), (Register 2), (ImmediateAddress12 24))));
    (DirectiveExpr CfiRememberState); (DirectiveExpr (CfiRestore 1));
    (InstructionExpr (Ld ((Register 8), (Register 2), (ImmediateAddress12 16))));
    (DirectiveExpr (CfiRestore 8));
    (InstructionExpr
       (Addi ((Register 2), (Register 2), (ImmediateAddress12 32))));
    (DirectiveExpr (CfiDefCfaOffset 0)); (InstructionExpr (Jr (Register 1)));
    (LabelExpr ".L16"); (DirectiveExpr CfiRestoreState);
    (InstructionExpr (Li ((Register 10), (ImmediateAddress32 -1))));
    (InstructionExpr (J (LabelAddress20 ".L12"))); (LabelExpr ".L22");
    (InstructionExpr (Call "__stack_chk_fail@plt"));
    (DirectiveExpr CfiEndproc); (LabelExpr ".LFE24");
    (DirectiveExpr (Size ((LabelAddress12 "main"), ".-main")));
    (DirectiveExpr (Ident "GCC: (Ubuntu 13.2.0-23ubuntu4) 13.2.0"));
    (DirectiveExpr (Section (".note.GNU-stack", "", (Type "progbits"), None)))]
