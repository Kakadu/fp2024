(** Copyright 2024-2025, Vyacheslav Kochergin, Roman Mukovenkov, Yuliana Ementyan *)

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
  | T6 (** a.k.a. X31 *)
[@@deriving eq, show { with_path = false }, qcheck]

(** Vector Registers *)
type vector_register =
  | V0 (** Vector Register 0 *)
  | V1 (** Vector Register 1 *)
  | V2 (** Vector Register 2 *)
  | V3 (** Vector Register 3 *)
  | V4 (** Vector Register 4 *)
  | V5 (** Vector Register 5 *)
  | V6 (** Vector Register 6 *)
  | V7 (** Vector Register 7 *)
  | V8 (** Vector Register 8 *)
  | V9 (** Vector Register 9 *)
  | V10 (** Vector Register 10 *)
  | V11 (** Vector Register 11 *)
  | V12 (** Vector Register 12 *)
  | V13 (** Vector Register 13 *)
  | V14 (** Vector Register 14 *)
  | V15 (** Vector Register 15 *)
  | V16 (** Vector Register 16 *)
  | V17 (** Vector Register 17 *)
  | V18 (** Vector Register 18 *)
  | V19 (** Vector Register 19 *)
  | V20 (** Vector Register 20 *)
  | V21 (** Vector Register 21 *)
  | V22 (** Vector Register 22 *)
  | V23 (** Vector Register 23 *)
  | V24 (** Vector Register 24 *)
  | V25 (** Vector Register 25 *)
  | V26 (** Vector Register 26 *)
  | V27 (** Vector Register 27 *)
  | V28 (** Vector Register 28 *)
  | V29 (** Vector Register 29 *)
  | V30 (** Vector Register 30 *)
  | V31 (** Vector Register 31 *)
[@@deriving eq, show { with_path = false }, qcheck]

(** Float Registers *)
type float_register =
  | F0 (** ft0 - Temporary *)
  | F1 (** ft1 - Temporary *)
  | F2 (** ft2 - Temporary *)
  | F3 (** ft3 - Temporary *)
  | F4 (** ft4 - Temporary *)
  | F5 (** ft5 - Temporary *)
  | F6 (** ft6 - Temporary *)
  | F7 (** ft7 - Temporary *)
  | F8 (** fs0 - Saved Register *)
  | F9 (** fs1 - Saved Register *)
  | F10 (** fa0 - Function Argument or Return Value *)
  | F11 (** fa1 - Function Argument or Return Value *)
  | F12 (** fa2 - Function Argument *)
  | F13 (** fa3 - Function Argument *)
  | F14 (** fa4 - Function Argument *)
  | F15 (** fa5 - Function Argument *)
  | F16 (** fa6 - Function Argument *)
  | F17 (** fa7 - Function Argument *)
  | F18 (** fs2 - Saved Register *)
  | F19 (** fs3 - Saved Register *)
  | F20 (** fs4 - Saved Register *)
  | F21 (** fs5 - Saved Register *)
  | F22 (** fs6 - Saved Register *)
  | F23 (** fs7 - Saved Register *)
  | F24 (** fs8 - Saved Register *)
  | F25 (** fs9 - Saved Register *)
  | F26 (** fs10 - Saved Register *)
  | F27 (** fs11 - Saved Register *)
  | F28 (** t8 - Temporary *)
  | F29 (** t9 - Temporary *)
  | F30 (** t10 - Temporary *)
  | F31 (** t11 - Temporary *)
  | Ft0 (** a.k.a F0 *)
  | Ft1 (** a.k.a F1 *)
  | Ft2 (** a.k.a F2 *)
  | Ft3 (** a.k.a F3 *)
  | Ft4 (** a.k.a F4 *)
  | Ft5 (** a.k.a F5 *)
  | Ft6 (** a.k.a F6 *)
  | Ft7 (** a.k.a F7 *)
  | Fs0 (** a.k.a F8 *)
  | Fs1 (** a.k.a F9 *)
  | Fa0 (** a.k.a F10 *)
  | Fa1 (** a.k.a F11 *)
  | Fa2 (** a.k.a F12 *)
  | Fa3 (** a.k.a F13 *)
  | Fa4 (** a.k.a F14 *)
  | Fa5 (** a.k.a F15 *)
  | Fa6 (** a.k.a F16 *)
  | Fa7 (** a.k.a F17 *)
  | Fs2 (** a.k.a F18 *)
  | Fs3 (** a.k.a F19 *)
  | Fs4 (** a.k.a F20 *)
  | Fs5 (** a.k.a F21 *)
  | Fs6 (** a.k.a F22 *)
  | Fs7 (** a.k.a F23 *)
  | Fs8 (** a.k.a F24 *)
  | Fs9 (** a.k.a F25 *)
  | Fs10 (** a.k.a F26 *)
  | Fs11 (** a.k.a F27 *)
  | Ft8 (** a.k.a F28 *)
  | Ft9 (** a.k.a F29 *)
  | Ft10 (** a.k.a F30 *)
  | Ft11 (** a.k.a F31 *)
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
  (** Store Vector to Memory. vse32.v vs, (rs1) *)
  | FmaddS of float_register * float_register * float_register * float_register
  (** Fused Mul-Add Single precision. rd = rs1 * rs2 + rs3 *)
  | FmsubS of float_register * float_register * float_register * float_register
  (** Fused Mul-Sub Single precision. rd = rs1 * rs2 - rs3 *)
  | FnmsubS of float_register * float_register * float_register * float_register
  (** Fused Negative Mul-Sub Single precision. rd = -rs1 * rs2 + rs3 *)
  | FnmaddS of float_register * float_register * float_register * float_register
  (** Fused Negative Mul-Sub Single precision. rd = -rs1 * rs2 - rs3 *)
  | FaddS of float_register * float_register * float_register
  (** Addition Single precision. rd = rs1 + rs2 *)
  | FsubS of float_register * float_register * float_register
  (** Subtraction Single precision. rd = rs1 - rs2 *)
  | FmulS of float_register * float_register * float_register
  (** Multiplication Single precision. rd = rs1 * rs2 *)
  | FdivS of float_register * float_register * float_register
  (** Division Single precision. rd = rs1 / rs2 *)
  | FsqrtS of float_register * float_register
  (** Square root Single precision. rd = sqrt(rs1) *)
  | FsgnjS of float_register * float_register * float_register
  (** Sign Injection Single precision. rd = [rs2[31], rs1[30:0]]. Sign bit from rs2, other bits from rs1 *)
  | FsgnjnS of float_register * float_register * float_register
  (** Sign Injection Negative Single precision. rd = [~rs2[31], rs1[30:0]] *)
  | FsgnjxS of float_register * float_register * float_register
  (** Sign Injection Xor Single precision. rd = [rs1[31] ^ rs2[31], rs1[30:0]] *)
  | FminS of float_register * float_register * float_register
  (** Min Single precision. rd = min(rs1, rs2) *)
  | FmaxS of float_register * float_register * float_register
  (** Max Single precision. rd = max(rs1, rs2) *)
  | FcvtWS of register * float_register
  (** Convert single precision float to signed 32-bit integer *)
  | FcvtWuS of register * float_register
  (** Convert single precision float to unsigned 32-bit integer *)
  | FmvXW of register * float_register
  (** Move single precision float to lower 32 bits of integer register *)
  | FeqS of register * float_register * float_register
  (** Equality Single precision. Result stored in integer register. rd = (rs1 == rs2) *)
  | FltS of register * float_register * float_register
  (** Less Single precision. rd = (rs1 < rs2) *)
  | FleS of register * float_register * float_register
  (** Less or Equal Single precision. rd = (rs1 <= rs2) *)
  | FclassS of register * float_register (** Classification of Single precision float *)
  | FcvtSW of float_register * register
  (** Converts 32-bit signed integer to Single precision float *)
  | FcvtSWu of float_register * register
  (** Converts 32-bit unsigned integer to Single precision float *)
  | FmvWX of float_register * register
  (** Move single precision float from lower 32 bits of integer register to float register *)
  | FmaddD of float_register * float_register * float_register * float_register
  (** Fused Mul-Add Single precision. rd = rs1 * rs2 + rs3 *)
  | FmsubD of float_register * float_register * float_register * float_register
  (** Fused Mul-Sub Single precision. rd = rs1 * rs2 - rs3 *)
  | FnmsubD of float_register * float_register * float_register * float_register
  (** Fused Negative Mul-Sub Single precision. rd = -rs1 * rs2 + rs3 *)
  | FnmaddD of float_register * float_register * float_register * float_register
  (** Fused Negative Mul-Sub Single precision. rd = -rs1 * rs2 - rs3 *)
  | FaddD of float_register * float_register * float_register
  (** Addition Single precision. rd = rs1 + rs2 *)
  | FsubD of float_register * float_register * float_register
  (** Subtraction Single precision. rd = rs1 - rs2 *)
  | FmulD of float_register * float_register * float_register
  (** Multiplication Single precision. rd = rs1 * rs2 *)
  | FdivD of float_register * float_register * float_register
  (** Division Single precision. rd = rs1 / rs2 *)
  | FsqrtD of float_register * float_register
  (** Square root Single precision. rd = sqrt(rs1) *)
  | FsgnjD of float_register * float_register * float_register
  (** Sign Injection Single precision. rd = [rs2[63], rs1[62:0]]. Sign bit from rs2, other bits from rs1 *)
  | FsgnjnD of float_register * float_register * float_register
  (** Sign Injection Negative Single precision. rd = [~rs2[63], rs1[62:0]] *)
  | FsgnjxD of float_register * float_register * float_register
  (** Sign Injection Xor Single precision. rd = [rs1[63] ^ rs2[63], rs1[62:0]] *)
  | FminD of float_register * float_register * float_register
  (** Min Single precision. rd = min(rs1, rs2) *)
  | FmaxD of float_register * float_register * float_register
  (** Max Single precision. rd = max(rs1, rs2) *)
  | FcvtSD of float_register * float_register
  (** Converts double floating-point register into a floating-point number *)
  | FcvtDS of float_register * float_register
  (** Converts single floating-point register into a double floating-point number *)
  | FeqD of register * float_register * float_register
  (** Equality Single precision. Result stored in integer register. rd = (rs1 == rs2) *)
  | FltD of register * float_register * float_register
  (** Less Single precision. rd = (rs1 < rs2) *)
  | FleD of register * float_register * float_register
  (** Less or Equal Single precision. rd = (rs1 <= rs2) *)
  | FclassD of register * float_register
  | FcvtWD of register * float_register
  (** Converts a double-precision floating-point number to a signed 32-bit integer *)
  | FcvtWuD of register * float_register
  (** Converts a double-precision floating-point number to a unsigned 32-bit integer *)
  | FcvtDW of float_register * register
  (** Converts a 32-bit signed integer into a double-precision floating-point number *)
  | FcvtDWu of float_register * register
  (** Converts a 32-bit unsigned integer into a double-precision floating-point number *)
  | Flw of float_register * float_register * address12
  (** Load a single-precision floating-point value from memory into floating-point register. f[rd] = M[x[rs1] + sext(offset)][31:0] *)
  | Fsw of float_register * float_register * address12
  (** Store a single-precision value from floating-point register rs2 to memory. M[x[rs1] + sext(offset)] = f[rs2][31:0] *)
  | Fld of float_register * float_register * address12
  (** Load a double-precision floating-point value from memory into floating-point register rd. f[rd] = M[x[rs1] + sext(offset)][63:0] *)
  | Fsd of float_register * float_register * address12
  (** Store a double-precision value from the floating-point registers to memory. M[x[rs1] + sext(offset)] = f[rs2][63:0] *)
  | FcvtLS of register * float_register
  (** Convert single-precision floating-point to 64-bit integer *)
  | FcvtLuS of register * float_register
  (** Convert single-precision floating-point to unsigned 64-bit integer *)
  | FcvtSL of float_register * register
  (** Convert 64-bit integer to single-precision floating-point *)
  | FcvtSLu of float_register * register
  (** Convert 64-bit unsigned integer to single-precision floating-point *)
  | FcvtLD of register * float_register
  (** Convert double-precision floating-point to 64-bit integer *)
  | FcvtLuD of register * float_register
  (** Convert double-precision floating-point to unsigned 64-bit integer *)
  | FcvtDL of float_register * register
  (** Convert 64-bit integer to double-precision floating-point *)
  | FcvtDLu of float_register * register
  (** Convert 64-bit unsigned integer to double-precision floating-point *)
  | Adduw of register * register * register
  (** Add unsigned word. rd = ZEXT(rs1 + rs2)[31:0]*)
  | Sh1add of register * register * register
  (** Shift left by 1 and add. rd = rs2 + (rs1 << 1) *)
  | Sh1adduw of register * register * register
  (** Shift unsigned word left by 1 and add. rd = rs2 + (ZEXT(rs1) << 1) *)
  | Sh2add of register * register * register
  (** Shift left by 2 and add. rd = rs2 + (rs1 << 2) *)
  | Sh2adduw of register * register * register
  (** Shift unsigned word left by 2 and add. rd = rs2 + (ZEXT(rs1) << 2) *)
  | Sh3add of register * register * register
  (** Shift left by 3 and add. rd = rs2 + (rs1 << 3) *)
  | Sh3adduw of register * register * register
  (** Shift unsigned word left by 3 and add. rd = rs2 + (ZEXT(rs1) << 3) *)
  | Andn of register * register * register
  (** AND with inverted operand. rd = rs1 & ~rs2 *)
  | Orn of register * register * register (** OR with inverted operand. rd = rs1 | ~rs2 *)
  | Xnor of register * register * register (** Exclusive NOR. ~(rs1 ^ rs2) *)
  | Vle32v of vector_register * register * address12
  (** Load Vector from Memory. vle32.v vd, (rs1) *)
  | Vse32v of vector_register * register * address12
  (** Store Vector to Memory. vse32.v vs, (rs1) *)
  | Vaddvv of vector_register * vector_register * vector_register
  (** Vector Addition. Vadd.vv vd, vs1, vs2 *)
  | Vaddvx of vector_register * vector_register * register
  (** Vector Addition with Scalar. vadd.vx vd, vs1, rs2 *)
  | Vsubvv of vector_register * vector_register * vector_register
  (** Vector Subtraction. Vsub.vv vd, vs1, vs2 *)
  | Vsubvx of vector_register * vector_register * register
  (** Vector Subtraction with Scalar. vsub.vx vd, vs1, rs2 *)
  | Vmulvv of vector_register * vector_register * vector_register
  (** Vector Multiplication. Vmul.vv vd, vs1, vs2 *)
  | Vmulvx of vector_register * vector_register * register
  (** Vector Multiplication with Scalar. vmul.vx vd, vs1, rs2 *)
  | Vdivvv of vector_register * vector_register * vector_register
  (** Vector Division. Vdiv.vv vd, vs1, vs2 *)
  | Vdivvx of vector_register * vector_register * register
  (** Vector Division with Scalar. vdiv.vx vd, vs1, rs2 *)
  | Vandvv of vector_register * vector_register * vector_register
  (** Vector Logical AND. Vand.vv vd, vs1, vs2 *)
  | Vandvx of vector_register * vector_register * register
  (** Vector Logical AND with Scalar. vand.vx vd, vs1, rs2 *)
  | Vorvv of vector_register * vector_register * vector_register
  (** Vector Logical OR. Vor.vv vd, vs1, vs2 *)
  | Vorvx of vector_register * vector_register * register
  (** Vector Logical OR with Scalar. vor.vx vd, vs1, rs2 *)
  | Vxorvv of vector_register * vector_register * vector_register
  (** Vector Logical XOR. Vxor.vv vd, vs1, vs2 *)
  | Vxorvx of vector_register * vector_register * register
  (** Vector Logical XOR with Scalar. vxor.vx vd, vs1, rs2 *)
  | Vminvv of vector_register * vector_register * vector_register
  (** Vector Minimum. Vmin.vv vd, vs1, vs2 *)
  | Vminvx of vector_register * vector_register * register
  (** Vector Minimum with Scalar. vmin.vx vd, vs1, rs2 *)
  | Vmaxvv of vector_register * vector_register * vector_register
  (** Vector Maximum. Vmax.vv vd, vs1, vs2 *)
  | Vmaxvx of vector_register * vector_register * register
  (** Vector Maximum with Scalar. vmax.vx vd, vs1, rs2 *)
  | Vmseqvv of vector_register * vector_register * vector_register
  (** Vector Equals. Vmseq.vv vd, vs1, vs2 *)
  | Vmseqvx of vector_register * vector_register * register
  (** Vector Equals with Scalar. vmseq.vx vd, vs1, rs2 *)
  | Vsetvli of register * register
  (** RVV Configuration. rd = new vl, rs1 = AVL (application vector length) *)
  | Vredsumvs of vector_register * vector_register * vector_register
  (** vd[0] =  sum( vs1[0] , vs2[*] ), where [*] denotes all active elements *)
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
  | Text (** .text subsection *)
  | Globl of address12 (** .globl symbol *)
  | TypeDir of (string[@gen Generators.gen_my_string]) * type_dir
  (** .type assigns type to a symbol *)
  | Section of
      (string[@gen Generators.gen_my_string])
      * ((string[@gen Generators.gen_my_string]) * (type_dir * int option) option) option
  (** .section name *)
  | StringDir of (string[@gen Generators.gen_my_string]) (** .string "str" *)
  | Word of Int32.t (** .word stores data in memory*)
  | Space of int (** .space allocates unitialized space for data in memory *)
[@@deriving eq, show { with_path = false }, qcheck]

(** Expression in AST *)
type expr =
  | InstructionExpr of instruction (** Instruction *)
  | LabelExpr of label (** Label *)
  | DirectiveExpr of directive (** Directive *)
[@@deriving eq, show { with_path = false }, qcheck]

(** AST is Presented by a List of Expressions *)
type ast = expr list [@@deriving eq, show { with_path = false }, qcheck]
