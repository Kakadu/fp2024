(** Copyright 2024, Vyacheslav Kochergin, Roman Mukovenkov, Yuliana Ementyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Integer Registers *)
type register =
  | X0 (** zero - Zero Constant *)
  | X1 (** ra - Return Address *)
  | X2 (** sp - Stack Pointer *)
  | X3 (** gp - Global Pointer *)
  | X4 (** tp - Thread Pointer *)
  | X5 (** t0 - Temporary *)
  | X6 (** t1 - Temporary *)
  | X7 (** t2 - Temporary *)
  | X8 (** s0/fp - Saved/Frame Pointer *)
  | X9 (** s1 - Saved Register *)
  | X10 (** a0 - Function Argument or Return Value *)
  | X11 (** a1 - Function Argument or Return Value *)
  | X12 (** a2 - Function Argument *)
  | X13 (** a3 - Function Argument *)
  | X14 (** a4 - Function Argument *)
  | X15 (** a5 - Function Argument *)
  | X16 (** a6 - Function Argument *)
  | X17 (** a7 - Function Argument *)
  | X18 (** s2 - Saved Register *)
  | X19 (** s3 - Saved Register *)
  | X20 (** s4 - Saved Register *)
  | X21 (** s5 - Saved Register *)
  | X22 (** s6 - Saved Register *)
  | X23 (** s7 - Saved Register *)
  | X24 (** s8 - Saved Register *)
  | X25 (** s9 - Saved Register *)
  | X26 (** s10 - Saved Register *)
  | X27 (** s11 - Saved Register *)
  | X28 (** t3 - Temporary *)
  | X29 (** t4 - Temporary *)
  | X30 (** t5 - Temporary *)
  | X31 (** t6 - Temporary *)
  | Zero (** a.k.a. X0 *)
  | Ra (** a.k.a. X1 *)
  | Sp (** a.k.a. X2 *)
  | Gp (** a.k.a. X3 *)
  | Tp (** a.k.a. X4 *)
  | T0 (** a.k.a. X5 *)
  | T1 (** a.k.a. X6 *)
  | T2 (** a.k.a. X7 *)
  | S0 (** a.k.a. X8 *)
  | Fp (** a.k.a. X8 *)
  | S1 (** a.k.a. X9 *)
  | A0 (** a.k.a. X10 *)
  | A1 (** a.k.a. X11 *)
  | A2 (** a.k.a. X12 *)
  | A3 (** a.k.a. X13 *)
  | A4 (** a.k.a. X14 *)
  | A5 (** a.k.a. X15 *)
  | A6 (** a.k.a. X16 *)
  | A7 (** a.k.a. X17 *)
  | S2 (** a.k.a. X18 *)
  | S3 (** a.k.a. X19 *)
  | S4 (** a.k.a. X20 *)
  | S5 (** a.k.a. X21 *)
  | S6 (** a.k.a. X22 *)
  | S7 (** a.k.a. X23 *)
  | S8 (** a.k.a. X24 *)
  | S9 (** a.k.a. X25 *)
  | S10 (** a.k.a. X26 *)
  | S11 (** a.k.a. X27 *)
  | T3 (** a.k.a. X28 *)
  | T4 (** a.k.a. X29 *)
  | T5 (** a.k.a. X30 *)
  | T6 (** a.k.a. X31s *)
[@@deriving eq, show { with_path = false }, qcheck]

(** Label Type *)
type label = (string[@gen Generators.gen_my_label])
[@@deriving eq, show { with_path = false }, qcheck]

(** Address12 Type to Jump to *)
type address12 =
  | ImmediateAddress12 of (int[@gen QCheck.Gen.(-2048 -- 2047)])
  (** Immediate12 to Jump to*)
  | LabelAddress12 of label (** Label to Jump to *)
[@@deriving eq, show { with_path = false }, qcheck]

(** Address20 Type to Jump to *)
type address20 =
  | ImmediateAddress20 of (int[@gen QCheck.Gen.(-524288 -- 524287)])
  (** Immediate20 to Jump to*)
  | LabelAddress20 of label (** Label to Jump to *)
[@@deriving eq, show { with_path = false }, qcheck]

(** Address32 Type to Jump to *)
type address32 =
  | ImmediateAddress32 of (int[@gen QCheck.Gen.(-2147483648 -- 2147483647)])
  (** Immediate32 to Jump to *)
  | LabelAddress32 of label (** Label to Jump to *)
[@@deriving eq, show { with_path = false }, qcheck]

type instruction =
  | Add of register * register * register (** Addition. rd = rs1 + rs2 *)
  | Sub of register * register * register (** Subtraction. rd = rs1 - rs2 *)
  | Xor of register * register * register (** Exclusive OR. rd = rs1 ^ rs2 *)
  | Or of register * register * register (** OR. rd = rs1 | rs2 *)
  | And of register * register * register (** AND. rd = rs1 & rs2 *)
  | Sll of register * register * register (** Shift Left Logical. rd = rs1 << rs2 *)
  | Srl of register * register * register (** Shift Right Logical. rd = rs1 >> rs2 *)
  | Sra of register * register * register (** Shift Right Arithmetic. rd = rs1 >> rs2 *)
  | Slt of register * register * register (** Set Less Than. rd = (rs1 < rs2) ? 1 : 0 *)
  | Sltu of register * register * register (** Set Less Than (Unsigned) *)
  | Addi of register * register * address12 (** Addition of Immediate. rd = rs1 + imm *)
  | Xori of register * register * address12 (** XOR with Immediate. rd = rs1 ^ imm *)
  | Ori of register * register * address12 (** OR with Immediate. rd = rs1 | imm *)
  | Andi of register * register * address12 (** AND with Immediate. rd = rs1 & imm *)
  | Slli of register * register * address12
  (** Shift Left Logical with Immediate. rd = rs1 << shamt[0:4] *)
  | Srli of register * register * address12
  (** Shift Right Logical with Immediate. rd = rs1 >> shamt[0:4] logical *)
  | Srai of register * register * address12
  (** Shift Right Arithmetic with Immediate. rd = rs1 >> shamt[0:4] arithmetical *)
  | Slti of register * register * address12
  (** Set Less Than Imm. rd = (rs1 < imm) ? 1 : 0 *)
  | Sltiu of register * register * address12 (** Set Less Than Imm (Unsigned) *)
  | Lb of register * register * address12 (** Load Byte. rd = M[rs1 + imm][0:7] *)
  | Lh of register * register * address12 (** Load Half. rd = M[rs1 + imm][0:15] *)
  | Lw of register * register * address12 (** Load Word. rd = M[rs1 + imm][0:31] *)
  | Lbu of register * register * address12 (** Load Byte Unsigned *)
  | Lhu of register * register * address12 (** Load Half Unsigned *)
  | Sb of register * register * address12 (** Store Byte. M[rs1 + imm][0:7] = rs2[0:7] *)
  | Sh of register * register * address12
  (** Store Half. M[rs1 + imm][0:15] = rs2[0:15] *)
  | Sw of register * register * address12
  (** Store Word. M[rs1 + imm][0:31] = rs2[0:31] *)
  | Beq of register * register * address12
  (** Branch ==. if (rs1 == rs2) PC += imm. PC is a program counter *)
  | Beqz of register * address12
  (** Branch == 0. if (rs1 == 0) PC += imm. PC is a program counter *)
  | Bne of register * register * address12 (** Branch !=. if (rs1 != rs2) PC += imm. *)
  | Bnez of register * address12 (** Branch != 0. if (rs1 != 0) PC += imm. *)
  | Blt of register * register * address12 (** Branch <. if (rs1 < rs2) PC += imm. *)
  | Bltz of register * address12 (** Branch < 0. if (rs1 < 0) PC += imm. *)
  | Bgt of register * register * address12 (** Branch >. if (rs1 > rs2) PC += imm. *)
  | Bgtz of register * address12 (** Branch > 0. if (rs1 > 0) PC += imm. *)
  | Bge of register * register * address12 (** Branch >=. if (rs1 >= rs2) PC += imm. *)
  | Bltu of register * register * address12
  (** Branch < (Unsigned). if (rs1 < rs2) PC += imm. *)
  | Bgeu of register * register * address12
  (** Branch >= (Unsigned). if (rs1 >= rs2) PC += imm. *)
  | Jal of register * address20
  (** Jump and Link. rd = PC + 4; PC += imm. 4 bytes = 32 bits - instuction size *)
  | Jalr of register * register * address12
  (** Jump and Link register. rd = PC + 4, PC = rs1 + imm *)
  | Jr of register (** Jump Reg. jalr x0, rs1, 0 *)
  | J of address20 (** Jump. jal x0, 2 * offset *)
  | Lui of register * address20 (** Load Upper Immediate. rd = imm << 12 *)
  | Auipc of register * address20 (** Add Upper Immediate to PC. rd = PC + (imm << 12) *)
  | Ecall (** EnvironmentCall - a syscall *)
  | Call of (string[@gen Generators.gen_my_string]) (** call. - a syscall *)
  | Mul of register * register * register (** Multiply. rd = (rs1 * rs2)[31:0] *)
  | Mulh of register * register * register (** Multiply High. rd = (rs1 * rs2)[63:32] *)
  | Mulhsu of register * register * register
  (** Multiply High (Signed * Unsigned). rd = (rs1 * rs2)[63:32] *)
  | Mulhu of register * register * register
  (** Multiply High (Unsigned * Unsigned). rd = (rs1 * rs2)[63:32] *)
  | Div of register * register * register (** Division. rd = rs1 / rs2 *)
  | Divu of register * register * register (** Division (Unsigned). rd = rs1 / rs2 *)
  | Rem of register * register * register (** Remainder. rd = rs1 % rs2 *)
  | Remu of register * register * register (** Remainder (Unsigned). rd = rs1 % rs2 *)
  | Lwu of register * register * address12
  (** Load Word (Unsigned). rd = M[rs1 + imm][0:31] *)
  | Ld of register * register * address12
  (** Load Doubleword (Unsigned). rd = M[rs1 + imm][0:63] *)
  | La of register * address32
  (** Load Address. auipc rd, symbol[31:12]; addi rd, rd, symbol[11:0] *)
  | Lla of register * address32
  (** Load Local Address. auipc rd, %pcrel_hi(symbol); addi rd, rd, %pcrel_lo(label) *)
  | Sd of register * register * address12
  (** Store Doubleword. M[rs1 + imm][0:63] = rs2[0:63] *)
  | Addiw of register * register * address12
  (** Addition of Immediate Word. rd = (rs1 + imm)[31:0] *)
  | Slliw of register * register * address12
  (** Shift Left Logical with Immediate Word. rd = (rs1 << shamt)[31:0] *)
  | Srliw of register * register * address12
  (** Shift Right Logical with Immediate Word. rd = (rs1 >> shamt)[31:0] *)
  | Sraiw of register * register * address12
  (** Shift Right Arithmetic with Immediate Word. rd = (rs1 >> shamt)[31:0] *)
  | Addw of register * register * register (** Add Word. rd = (rs1 + rs2)[31:0] *)
  | Subw of register * register * register (** Add Word. rd = (rs1 - rs2)[31:0] *)
  | Sllw of register * register * register
  (** Shifl Left Logical Word. rd = (rs1 << rs2)[31:0] *)
  | Srlw of register * register * register
  (** Shifl Right Logical Word. rd = (rs1 >> rs2)[31:0] *)
  | Sraw of register * register * register
  (** Shifl Right Arithmetical Word. rd = (rs1 >> rs2)[31:0] *)
  | Mulw of register * register * register (** Multiply Word. rd = (rs1 * rs2)[31:0] *)
  | Divw of register * register * register (** Division Word. rd = (rs1 / rs2)[31:0] *)
  | Divuw of register * register * register
  (** Division Word (Unsigned). rd = (rs1 / rs2)[31:0] *)
  | Remw of register * register * register (** Remainder Word. rd = (rs1 % rs2)[31:0] *)
  | Remwu of register * register * register
  (** Remainder Word (Unsigned). rd = (rs1 % rs2)[31:0] *)
  | Mv of register * register (** Copy from rs1 to rd. addi rd, rs1, 0 *)
  | Li of register * address32
  (** Load Immediate. lui rd, immediate20; addi rd, rd, immediate12 *)
  | Ret (** Return. Jalr x0, x1, 0 *)
[@@deriving eq, show { with_path = false }, qcheck]

(** Attribute can either take in a string or an int as its value *)
type string_or_int_value =
  | StrValue of (string[@gen Generators.gen_my_string]) (** A string value *)
  | IntValue of (int[@gen QCheck.Gen.(-2147483648 -- 2147483647)]) (** An integer value*)
[@@deriving eq, show { with_path = false }, qcheck]

(** Types that are assigned to symbols for the logic of the compiler*)
type type_dir = Type of (string[@gen Generators.gen_my_string])
[@@deriving eq, show { with_path = false }, qcheck]

(** Compiler directive (most of them are not needed while interpreting) *)
type directive =
  | File of (string[@gen Generators.gen_my_string]) (** .file string *)
  | Option of (string[@gen Generators.gen_my_string]) (** .option argument *)
  | Attribute of (string[@gen Generators.gen_my_string]) * string_or_int_value
  (** .attribute tag, value *)
  | Text (** .text subsection *)
  | Align of (int[@gen QCheck.Gen.(-2147483648 -- 2147483647)])
  (** .align abs-expr, abs-expr, abs-expr *)
  | Globl of address12 (** .globl symbol *)
  | TypeDir of (string[@gen Generators.gen_my_string]) * type_dir
  (** .type assigns type to a symbol *)
  | CfiStartproc (** .cfi_startproc *)
  | CfiEndproc (** .cfi_endproc *)
  | Size of address12 * (string[@gen Generators.gen_my_string]) (** .size *)
  | Section of
      (string[@gen Generators.gen_my_string])
      * (string[@gen Generators.gen_my_string])
      * type_dir
      * int option (** .section name *)
  | String of (string[@gen Generators.gen_my_string]) (** .string "str" *)
  | CfiDefCfaOffset of (int[@gen QCheck.Gen.(-2147483648 -- 2147483647)])
  (** .cfi_def_cfa_offset int*)
  | CfiOffset of
      (int[@gen QCheck.Gen.(-2147483648 -- 2147483647)])
      * (int[@gen QCheck.Gen.(-2147483648 -- 2147483647)]) (** .cfi_offset int, int *)
  | CfiRememberState (** .cfi_remember_state *)
  | CfiRestore of (int[@gen QCheck.Gen.(-2147483648 -- 2147483647)])
  (** .cfi_restore int *)
  | Ident of (string[@gen Generators.gen_my_string]) (** .ident string *)
  | CfiRestoreState (** .cfi_restore_state *)
  | Word of address32 (** .word stores data in memory*)
[@@deriving eq, show { with_path = false }, qcheck]

(** Expression in AST *)
type expr =
  | InstructionExpr of instruction (** Instruction *)
  | LabelExpr of label (** Label *)
  | DirectiveExpr of directive (** Directive *)
[@@deriving eq, show { with_path = false }, qcheck]

(** AST is Presented by a List of Expressions *)
type ast = expr list [@@deriving eq, show { with_path = false }, qcheck]
