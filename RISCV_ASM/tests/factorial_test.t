  $ ../repl/REPL.exe -dparsetree -filepath="../examples/factorial/factorial.s"
  [(DirectiveExpr (Globl (LabelAddress12 "_start")));
    (DirectiveExpr (Section (".data", None))); (LabelExpr "buffer");
    (DirectiveExpr (Space 32)); (DirectiveExpr (Section (".text", None)));
    (LabelExpr "_start"); (InstructionExpr (Li (A7, (ImmediateAddress32 63))));
    (InstructionExpr (Li (A0, (ImmediateAddress32 0))));
    (InstructionExpr (La (A1, (LabelAddress32 "buffer"))));
    (InstructionExpr (Li (A2, (ImmediateAddress32 32))));
    (InstructionExpr Ecall);
    (InstructionExpr (La (T0, (LabelAddress32 "buffer"))));
    (InstructionExpr (Mv (A0, Zero)));
    (InstructionExpr (Li (T2, (ImmediateAddress32 10))));
    (LabelExpr "convert_string_to_number");
    (InstructionExpr (Lb (T1, T0, (ImmediateAddress12 0))));
    (InstructionExpr (Beqz (T1, (LabelAddress12 "end_convert"))));
    (InstructionExpr (Addi (T1, T1, (ImmediateAddress12 -48))));
    (InstructionExpr (Bltz (T1, (LabelAddress12 "end_convert"))));
    (InstructionExpr (Bge (T1, T2, (LabelAddress12 "end_convert"))));
    (InstructionExpr (Mul (A0, A0, T2))); (InstructionExpr (Add (A0, A0, T1)));
    (InstructionExpr (Addi (T0, T0, (ImmediateAddress12 1))));
    (InstructionExpr (J (LabelAddress20 "convert_string_to_number")));
    (LabelExpr "end_convert");
    (InstructionExpr (Addi (T1, Zero, (ImmediateAddress12 1))));
    (LabelExpr "loop"); (InstructionExpr (Beqz (A0, (LabelAddress12 "exit"))));
    (InstructionExpr (Mul (T1, T1, A0)));
    (InstructionExpr (Addi (A0, A0, (ImmediateAddress12 -1))));
    (InstructionExpr (J (LabelAddress20 "loop"))); (LabelExpr "exit");
    (InstructionExpr (La (T0, (LabelAddress32 "buffer"))));
    (InstructionExpr (Addi (T2, Zero, (ImmediateAddress12 10))));
    (InstructionExpr (Mv (A0, T1)));
    (InstructionExpr (Addi (T3, Zero, (ImmediateAddress12 0))));
    (LabelExpr "convert_loop"); (InstructionExpr (Rem (T4, A0, T2)));
    (InstructionExpr (Div (A0, A0, T2)));
    (InstructionExpr (Addi (T4, T4, (ImmediateAddress12 48))));
    (InstructionExpr (Sb (T4, T0, (ImmediateAddress12 0))));
    (InstructionExpr (Addi (T0, T0, (ImmediateAddress12 1))));
    (InstructionExpr (Addi (T3, T3, (ImmediateAddress12 1))));
    (InstructionExpr (Bnez (A0, (LabelAddress12 "convert_loop"))));
    (InstructionExpr (La (T0, (LabelAddress32 "buffer"))));
    (InstructionExpr (Mv (T4, T0))); (InstructionExpr (Add (T5, T0, T3)));
    (InstructionExpr (Addi (T5, T5, (ImmediateAddress12 -1))));
    (LabelExpr "reverse_loop");
    (InstructionExpr (Bge (T4, T5, (LabelAddress12 "end_reverse"))));
    (InstructionExpr (Lb (T6, T4, (ImmediateAddress12 0))));
    (InstructionExpr (Lb (T1, T5, (ImmediateAddress12 0))));
    (InstructionExpr (Sb (T1, T4, (ImmediateAddress12 0))));
    (InstructionExpr (Sb (T6, T5, (ImmediateAddress12 0))));
    (InstructionExpr (Addi (T4, T4, (ImmediateAddress12 1))));
    (InstructionExpr (Addi (T5, T5, (ImmediateAddress12 -1))));
    (InstructionExpr (J (LabelAddress20 "reverse_loop")));
    (LabelExpr "end_reverse"); (InstructionExpr (Add (T0, T0, T3)));
    (InstructionExpr (Li (A7, (ImmediateAddress32 64))));
    (InstructionExpr (Li (A0, (ImmediateAddress32 1))));
    (InstructionExpr (La (A1, (LabelAddress32 "buffer"))));
    (InstructionExpr (Mv (A2, T3))); (InstructionExpr Ecall);
    (InstructionExpr (Li (A7, (ImmediateAddress32 93))));
    (InstructionExpr (Li (A0, (ImmediateAddress32 0))));
    (InstructionExpr Ecall)]
  $ riscv64-unknown-elf-as -o ../examples/factorial/factorial.o ../examples/factorial/factorial.s
  $ riscv64-unknown-elf-ld -o ../examples/factorial/factorial ../examples/factorial/factorial.o
  $ qemu-riscv64 ../examples/factorial/factorial <<EOF
  > 8
  40320
  $ ../repl/REPL.exe -eval -filepath="../examples/factorial/factorial.s" <<EOF
  > 8
  40320
