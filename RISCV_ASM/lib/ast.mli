(** Copyright 2024, Vyacheslav Kochergin and Roman Mukovenkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** integer registers *)
type register =
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

(** immediate 12-bit type *)
type immediate12

(** immediate 20-bit type *)
type immediate20

(** immediate 32-bit type*)
type immediate32

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
  | Addi of register * register * immediate12 (** Addition of Immediate. rd = rs1 + imm *)
  | Xori of register * register * immediate12 (** XOR with Immediate. rd = rs1 ^ imm *)
  | Ori of register * register * immediate12 (** OR with Immediate. rd = rs1 | imm *)
  | Andi of register * register * immediate12 (** AND with Immediate. rd = rs1 & imm *)
  | Slli of register * register * immediate12
  (** Shift Left Logical with Immediate. rd = rs1 << shamt[0:4] *)
  | Srli of register * register * immediate12
  (** Shift Right Logical with Immediate. rd = rs1 >> shamt[0:4] logical *)
  | Srai of register * register * immediate12
  (** Shift Right Arithmetic with Immediate. rd = rs1 >> shamt[0:4] arithmetical *)
  | Slti of register * register * immediate12
  (** Set Less Than Imm. rd = (rs1 < imm) ? 1 : 0 *)
  | Sltiu of register * register * immediate12 (** Set Less Than Imm (Unsigned) *)
  | Lb of register * register * immediate12 (** Load Byte. rd = M[rs1 + imm][0:7] *)
  | Lh of register * register * immediate12 (** Load Half. rd = M[rs1 + imm][0:15] *)
  | Lw of register * register * immediate12 (** Load Word. rd = M[rs1 + imm][0:31] *)
  | Lbu of register * register * immediate12 (** Load Byte Unsigned *)
  | Lhu of register * register * immediate12 (** Load Half Unsigned *)
  | Sb of register * immediate12 * register
  (** Store Byte. M[rs1 + imm][0:7] = rs2[0:7] *)
  | Sh of register * immediate12 * register
  (** Store Half. M[rs1 + imm][0:15] = rs2[0:15] *)
  | Sw of register * immediate12 * register
  (** Store Word. M[rs1 + imm][0:31] = rs2[0:31] *)
  | Beq of register * register * immediate12
  (** Branch ==. if (rs1 == rs2) PC += imm. PC is a program counter *)
  | Bne of register * register * immediate12 (** Branch !=. if (rs1 != rs2) PC += imm. *)
  | Blt of register * register * immediate12 (** Branch <. if (rs1 < rs2) PC += imm. *)
  | Bge of register * register * immediate12 (** Branch >=. if (rs1 >= rs2) PC += imm. *)
  | Bltu of register * register * immediate12
  (** Branch < (Unsigned). if (rs1 < rs2) PC += imm. *)
  | Bgeu of register * register * immediate12
  (** Branch >= (Unsigned). if (rs1 >= rs2) PC += imm. *)
  | Jal of register * immediate20
  (** Jump and Link. rd = PC + 4; PC += imm. 4 bytes = 32 bits - instuction size *)
  | Jalr of register * register * immediate12
  (** Jump and Link Register. rd = PC + 4; PC = rs1 + imm *)
  | Lui of register * immediate20 (** Load Upper Immediate. rd = imm << 12 *)
  | Auipc of register * immediate20
  (** Add Upper Immediate to PC. rd = PC + (imm << 12) *)
  | Ecall (** EnvironmentCall - a syscall *)
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
  | Lwu of register * register * immediate12
  (** Load Word (Unsigned). rd = M[rs1 + imm][0:31] *)
  | Ld of register * register * immediate12
  (** Load Doubleword (Unsigned). rd = M[rs1 + imm][0:63] *)
  | Sd of register * register * immediate12
  (** Store Doubleword. M[rs1 + imm][0:63] = rs2[0:63] *)
  | Addiw of register * register * immediate12
  (** Addition of Immediate Word. rd = (rs1 + imm)[31:0] *)
  | Slliw of register * register * immediate12
  (** Shift Left Logical with Immediate Word. rd = (rs1 << shamt)[31:0] *)
  | Srliw of register * register * immediate12
  (** Shift Right Logical with Immediate Word. rd = (rs1 >> shamt)[31:0] *)
  | Sraiw of register * register * immediate12
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
  | Mv of register * register (** Copy from rs1 to rd. addi rd, rs1, 0*)
  | Li of register * immediate32 (**Load Immediate. lui x5, immediate20; addi x5, x5, immediate 12*)
  

(** Expression in AST *)
type expr =
  | Instruction of instruction (** Instruction *)
  | Label of string (** Label *)
  | Comment of string (** Comment *)

(** AST is presented by a list of expressions *)
type ast = expr list
