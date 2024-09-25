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
    | Addi of int_register * int_register * immediate (** Addition of immediate. rd = rs1 + imm *)
    | Xori of int_register * int_register * immediate (** XOR with immediate. rd = rs1 ^ imm *)
    | Ori of int_register * int_register * immediate (** OR with immediate. rd = rs1 | imm *)
    | Andi of int_register * int_register * immediate (** AND with immediate. rd = rs1 & imm *)
    | Slli of int_register * int_register * immediate (** Shift Left Logical with immediate. rd = rs1 << imm[0:4] *)
    | Srli of int_register * int_register * immediate (** Shift Right Logical with immediate. rd = rs1 >> imm[0:4] logical *)
    | Srai of int_register * int_register * immediate (** Shift Right Arithmetic with immediate. rd = rs1 >> imm[0:4] arithmetical *)
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

type label (** labels to jump on *)

type comment (** comments in code *)

type expr = 
    | IntInstruction of int_instruction
    | Label of label
    | Comment of comment

type ast = expr list