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
    (DirectiveExpr CfiStartproc); (InstructionExpr (Mv (A5, A0)));
    (InstructionExpr (Li (A0, (ImmediateAddress32 1))));
    (InstructionExpr (Beq (A5, Zero, (LabelAddress12 ".L5"))));
    (LabelExpr ".L2"); (InstructionExpr (Mv (A4, A5)));
    (InstructionExpr (Addiw (A5, A5, (ImmediateAddress12 -1))));
    (InstructionExpr (Mulw (A0, A4, A0)));
    (InstructionExpr (Bne (A5, Zero, (LabelAddress12 ".L2"))));
    (LabelExpr ".L5"); (InstructionExpr Ret); (DirectiveExpr CfiEndproc);
    (LabelExpr ".LFE23");
    (DirectiveExpr (Size ((LabelAddress12 "factorial"), ".-factorial")));
    (DirectiveExpr
       (Section (".rodata.str1.8", (Some "aMS"), (Some (Type "progbits")),
          (Some 1))));
    (DirectiveExpr (Align 3)); (LabelExpr ".LC0");
    (DirectiveExpr (StringDir "%d")); (DirectiveExpr (Align 3));
    (LabelExpr ".LC1"); (DirectiveExpr (StringDir "%d\n"));
    (DirectiveExpr
       (Section (".text.startup", (Some "ax"), (Some (Type "progbits")), None)));
    (DirectiveExpr (Align 1)); (DirectiveExpr (Globl (LabelAddress12 "main")));
    (DirectiveExpr (TypeDir ("main", (Type "function")))); (LabelExpr "main");
    (LabelExpr ".LFB24"); (DirectiveExpr CfiStartproc);
    (InstructionExpr (Addi (Sp, Sp, (ImmediateAddress12 -32))));
    (DirectiveExpr (CfiDefCfaOffset 32));
    (InstructionExpr (Sd (S0, Sp, (ImmediateAddress12 16))));
    (DirectiveExpr (CfiOffset (8, -16)));
    (InstructionExpr (La (S0, (LabelAddress32 "__stack_chk_guard"))));
    (InstructionExpr (Ld (A5, S0, (ImmediateAddress12 0))));
    (InstructionExpr (Sd (A5, Sp, (ImmediateAddress12 8))));
    (InstructionExpr (Li (A5, (ImmediateAddress32 0))));
    (InstructionExpr (Addi (A1, Sp, (ImmediateAddress12 4))));
    (InstructionExpr (Lla (A0, (LabelAddress32 ".LC0"))));
    (InstructionExpr (Sd (Ra, Sp, (ImmediateAddress12 24))));
    (DirectiveExpr (CfiOffset (1, -8)));
    (InstructionExpr (Call "__isoc99_scanf@plt"));
    (InstructionExpr (Li (A5, (ImmediateAddress32 1))));
    (InstructionExpr (Bne (A0, A5, (LabelAddress12 ".L16"))));
    (InstructionExpr (Lw (A5, Sp, (ImmediateAddress12 4))));
    (InstructionExpr (Mv (A2, A0)));
    (InstructionExpr (Beq (A5, Zero, (LabelAddress12 ".L14"))));
    (LabelExpr ".L13"); (InstructionExpr (Mv (A4, A5)));
    (InstructionExpr (Addiw (A5, A5, (ImmediateAddress12 -1))));
    (InstructionExpr (Mulw (A2, A4, A2)));
    (InstructionExpr (Bne (A5, Zero, (LabelAddress12 ".L13"))));
    (LabelExpr ".L14"); (InstructionExpr (Lla (A1, (LabelAddress32 ".LC1"))));
    (InstructionExpr (Li (A0, (ImmediateAddress32 2))));
    (InstructionExpr (Call "__printf_chk@plt"));
    (InstructionExpr (Li (A0, (ImmediateAddress32 0)))); (LabelExpr ".L12");
    (InstructionExpr (Ld (A4, Sp, (ImmediateAddress12 8))));
    (InstructionExpr (Ld (A5, S0, (ImmediateAddress12 0))));
    (InstructionExpr (Xor (A5, A4, A5)));
    (InstructionExpr (Li (A4, (ImmediateAddress32 0))));
    (InstructionExpr (Bne (A5, Zero, (LabelAddress12 ".L22"))));
    (InstructionExpr (Ld (Ra, Sp, (ImmediateAddress12 24))));
    (DirectiveExpr CfiRememberState); (DirectiveExpr (CfiRestore 1));
    (InstructionExpr (Ld (S0, Sp, (ImmediateAddress12 16))));
    (DirectiveExpr (CfiRestore 8));
    (InstructionExpr (Addi (Sp, Sp, (ImmediateAddress12 32))));
    (DirectiveExpr (CfiDefCfaOffset 0)); (InstructionExpr (Jr Ra));
    (LabelExpr ".L16"); (DirectiveExpr CfiRestoreState);
    (InstructionExpr (Li (A0, (ImmediateAddress32 -1))));
    (InstructionExpr (J (LabelAddress20 ".L12"))); (LabelExpr ".L22");
    (InstructionExpr (Call "__stack_chk_fail@plt"));
    (DirectiveExpr CfiEndproc); (LabelExpr ".LFE24");
    (DirectiveExpr (Size ((LabelAddress12 "main"), ".-main")));
    (DirectiveExpr (Ident "GCC: (Ubuntu 13.2.0-23ubuntu4) 13.2.0"));
    (DirectiveExpr
       (Section (".note.GNU-stack", (Some ""), (Some (Type "progbits")), None)))
    ]
