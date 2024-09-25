(** integer registers *)
type int_register = 
    | X0 (** zero - zero constant *)
    | X1 (** ra - return address *)
    | X2 (** sp - stack pointer *)
    | X3 (** gp - global pointer *)
    | X4 (** tp - thread pointer *)
    | X5 (** t0 - temporary *)
    | X6 (** t1 - temporary *)
    | X7 (** t2 - temporary *)
    | X8 (** s0/fp - saved/frame pointer *)
    | X9 (** s1 - saved register *)
    | X10 (** a0 - function argument or return value *)
    | X11 (** a1 - function argument or return value *)
    | X12 (** a2 - function argument *)
    | X13 (** a3 - function arg0ter *)
    | X24 (** s8 - saved register *)
    | X25 (** s9 - saved register *)
    | X26 (** s10 - saved register *)
    | X27 (** s11 - saved register *)
    | X28 (** t3 - temporary *)
    | X29 (** t4 - temporary *)
    | X30 (** t5 - temporary *)
    | X31 (** t6 - temporary *)

(** floating point registers *)
type float_register = 
    | F0 (** ft0 - temporary *)
    | F1 (** ft1 - temporary *)
    | F2 (** ft2 - temporary *)
    | F3 (** ft3 - temporary *)
    | F4 (** ft4 - temporary *)
    | F5 (** ft5 - temporary *)
    | F6 (** ft6 - temporary *)
    | F7 (** ft7 - temporary *)
    | F8 (** fs0 - saved register *)
    | F9 (** fs1 - saved register *)
    | F10 (** fa0 - function argument or return value *)
    | F11 (** fa1 - function argument or return value *)
    | F12 (** fa2 - function argument *)
    | F13 (** fa3 - function argument *)
    | F14 (** fa4 - function argument *)
    | F15 (** fa5 - function argument *)
    | F16 (** fa6 - function argument *)
    | F17 (** fa7 - function argument *)
    | F18 (** fs2 - saved register *)
    | F19 (** fs3 - saved register *)
    | F20 (** fs4 - saved register *)
    | F21 (** fs5 - saved register *)
    | F22 (** fs6 - saved register *)
    | F23 (** fs7 - saved register *)
    | F24 (** fs8 - saved register *)
    | F25 (** fs9 - saved register *)
    | F26 (** fs10 - saved register *)
    | F27 (** fs11 - saved register *)
    | F28 (** ft8 - temporary *)
    | F29 (** ft9 - temporary *)
    | F30 (** ft10 - temporary *)
    | F31 (** ft11 - temporary *)

type immediate (** immediate value, stored in opcode, not in registers or other memory *)

type int_instruction =
    (** RV32I instructions *)
    | Add of int_register * int_register * int_register (** Addition. rd = rs1 + rs2 *)
    | Sub of int_register * int_register * int_register (** Subtraction. rd = rs1 - rs2 *)
    | Xor of int_register * int_register * int_register (** Exclusive OR. rd = rs1 ^ rs2 *)
    | Or of int_register * int_register * int_register (** OR. rd = rs1 | rs2 *)
    | And of int_register * int_register * int_register (** AND. rd = rs1 & rs2 *)
    | Sll of int_register * int_register * int_register (** Shift Left Logical. rd = rs1 << rs2 *)
    | Srl of int_register * int_register * int_register (** Shift Right Logical. rd = rs1 >> rs2 *)
    | Sra of int_register * int_register * int_register (** Shift Right Arithmetic. rd = rs1 >> rs2 *)
    | Slt of int_register * int_register * int_register (** Set Less Than. rd = (rs1 < rs2) ? 1 : 0 *)
    | Sltu of int_register * int_register * int_register (** Set Less Than (Unsigned) *)
    | Addi of int_register * int_register * immediate (** Addition of Immediate. rd = rs1 + imm *)
    | Xori of int_register * int_register * immediate (** XOR with Immediate. rd = rs1 ^ imm *)
    | Ori of int_register * int_register * immediate (** OR with Immediate. rd = rs1 | imm *)
    | Andi of int_register * int_register * immediate (** AND with Immediate. rd = rs1 & imm *)
    | Slli of int_register * int_register * immediate (** Shift Left Logical with Immediate. rd = rs1 << imm[0:4] *)
    | Srli of int_register * int_register * immediate (** Shift Right Logical with Immediate. rd = rs1 >> imm[0:4] logical *)
    | Srai of int_register * int_register * immediate (** Shift Right Arithmetic with Immediate. rd = rs1 >> imm[0:4] arithmetical *)
    | Slti of int_register * int_register * immediate (** Set Less Than Imm. rd = (rs1 < imm) ? 1 : 0 *)
    | Sltiu of int_register * int_register * immediate (** Set Less Than Imm (Unsigned) *)
    | Lb of int_register * int_register * immediate (** Load Byte. rd = M[rs1 + imm][0:7] *)
    | Lh of int_register * int_register * immediate (** Load Half. rd = M[rs1 + imm][0:15] *)
    | Lw of int_register * int_register * immediate (** Load Word. rd = M[rs1 + imm][0:31] *)
    | Lbu of int_register * int_register * immediate (** Load Byte Unsigned *)
    | Lhu of int_register * int_register * immediate (** Load Half Unsigned *)
    | Sb of int_register * immediate * int_register (** Store Byte. M[rs1 + imm][0:7] = rs2[0:7] *)
    | Sh of int_register * immediate * int_register (** Store Half. M[rs1 + imm][0:15] = rs2[0:15] *)
    | Sw of int_register * immediate * int_register (** Store Word. M[rs1 + imm][0:31] = rs2[0:31] *)
    | Beq of int_register * int_register * immediate (** Branch ==. if (rs1 == rs2) PC += imm. PC is a program counter *)
    | Bne of int_register * int_register * immediate (** Branch !=. if (rs1 != rs2) PC += imm. *)
    | Blt of int_register * int_register * immediate (** Branch <. if (rs1 < rs2) PC += imm. *)
    | Bge of int_register * int_register * immediate (** Branch >=. if (rs1 >= rs2) PC += imm. *)
    | Bltu of int_register * int_register * immediate (** Branch < (Unsigned). if (rs1 < rs2) PC += imm. *)
    | Bgeu of int_register * int_register * immediate (** Branch >= (Unsigned). if (rs1 >= rs2) PC += imm. *)
    | Jal of int_register * immediate (** Jump and Link. rd = PC + 4; PC += imm. 4 bytes = 32 bits - instuction size *)
    | Jalr of int_register * int_register * immediate (** Jump and Link Register. rd = PC + 4; PC = rs1 + imm *)
    | Lui of int_register * immediate (** Load Upper Immediate. rd = imm << 12 *)
    | Auipc of int_register * immediate (** Add Upper Immediate to PC. rd = PC + (imm << 12) *)
    (** RV32M instructions *)
    | Mul of int_register * int_register * int_register (** Multiply. rd = (rs1 * rs2)[31:0] *)
    | Mulh of int_register * int_register * int_register (** Multiply High. rd = (rs1 * rs2)[63:32] *)
    | Mulhsu of int_register * int_register * int_register (** Multiply High (Signed * Unsigned). rd = (rs1 * rs2)[63:32] *)
    | Mulhu of int_register * int_register * int_register (** Multiply High (Unsigned * Unsigned). rd = (rs1 * rs2)[63:32] *)
    | Div of int_register * int_register * int_register (** Division. rd = rs1 / rs2 *)
    | Divu of int_register * int_register * int_register (** Division (Unsigned). rd = rs1 / rs2 *)
    | Rem of int_register * int_register * int_register (** Remainder. rd = rs1 % rs2 *)
    | Remu of int_register * int_register * int_register (** Remainder (Unsigned). rd = rs1 % rs2 *)
    (** RV64I instructions *)
    | Lwu of int_register * int_register * immediate (** Load Word (Unsigned). rd = M[rs1 + imm][0:31] *)
    | Ld of int_register * int_register * immediate (** Load Doubleword (Unsigned). rd = M[rs1 + imm][0:63] *)
    | Sd of int_register * int_register * immediate (** Store Doubleword. M[rs1 + imm][0:63] = rs2[0:63] *)
    | Addiw  of int_register * int_register * immediate (** Addition of Immediate Word. rd = (rs1 + imm)[31:0] *)
    | Slliw of int_register * int_register * immediate (** Shift Left Logical with Immediate Word. rd = (rs1 << imm)[31:0] *)
    | Srliw of int_register * int_register * immediate (** Shift Right Logical with Immediate Word. rd = (rs1 >> imm)[31:0] *)
    | Sraiw of int_register * int_register * immediate (** Shift Right Arithmetic with Immediate Word. rd = (rs1 >> imm)[31:0] *)
    | Addw of int_register * int_register * int_register (** Add Word. rd = (rs1 + rs2)[31:0] *)
    | Subw of int_register * int_register * int_register (** Add Word. rd = (rs1 - rs2)[31:0] *)
    | Sllw of int_register * int_register * int_register (** Shifl Left Logical Word. rd = (rs1 << rs2)[31:0] *)
    | Srlw of int_register * int_register * int_register (** Shifl Right Logical Word. rd = (rs1 >> rs2)[31:0] *)
    | Sraw of int_register * int_register * int_register (** Shifl Right Arithmetical Word. rd = (rs1 >> rs2)[31:0] *)
    (** RV64M instructions *)
    | Mulw of int_register * int_register * int_register (** Multiply Word. rd = (rs1 * rs2)[31:0] *)
    | Divw of int_register * int_register * int_register (** Division Word. rd = (rs1 / rs2)[31:0] *)
    | Divuw of int_register * int_register * int_register (** Division Word (Unsigned). rd = (rs1 / rs2)[31:0] *)
    | Remw of int_register * int_register * int_register (** Remainder Word. rd = (rs1 % rs2)[31:0] *)
    | Remwu of int_register * int_register * int_register (** Remainder Word (Unsigned). rd = (rs1 % rs2)[31:0] *)

type label (** labels to jump on *)

type comment (** comments in code *)

type expr = 
    | IntInstruction of int_instruction
    | Label of label
    | Comment of comment

type ast = expr list
