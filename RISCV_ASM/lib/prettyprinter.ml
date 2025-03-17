(** Copyright 2024-2025, Vyacheslav Kochergin, Roman Mukovenkov, Yuliana Ementyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

let pp_register ppf = function
  | X0 -> Format.fprintf ppf "x0"
  | X1 -> Format.fprintf ppf "x1"
  | X2 -> Format.fprintf ppf "x2"
  | X3 -> Format.fprintf ppf "x3"
  | X4 -> Format.fprintf ppf "x4"
  | X5 -> Format.fprintf ppf "x5"
  | X6 -> Format.fprintf ppf "x6"
  | X7 -> Format.fprintf ppf "x7"
  | X8 -> Format.fprintf ppf "x8"
  | X9 -> Format.fprintf ppf "x9"
  | X10 -> Format.fprintf ppf "x10"
  | X11 -> Format.fprintf ppf "x11"
  | X12 -> Format.fprintf ppf "x12"
  | X13 -> Format.fprintf ppf "x13"
  | X14 -> Format.fprintf ppf "x14"
  | X15 -> Format.fprintf ppf "x15"
  | X16 -> Format.fprintf ppf "x16"
  | X17 -> Format.fprintf ppf "x17"
  | X18 -> Format.fprintf ppf "x18"
  | X19 -> Format.fprintf ppf "x19"
  | X20 -> Format.fprintf ppf "x20"
  | X21 -> Format.fprintf ppf "x21"
  | X22 -> Format.fprintf ppf "x22"
  | X23 -> Format.fprintf ppf "x23"
  | X24 -> Format.fprintf ppf "x24"
  | X25 -> Format.fprintf ppf "x25"
  | X26 -> Format.fprintf ppf "x26"
  | X27 -> Format.fprintf ppf "x27"
  | X28 -> Format.fprintf ppf "x28"
  | X29 -> Format.fprintf ppf "x29"
  | X30 -> Format.fprintf ppf "x30"
  | X31 -> Format.fprintf ppf "x31"
  | Zero -> Format.fprintf ppf "zero"
  | Ra -> Format.fprintf ppf "ra"
  | Sp -> Format.fprintf ppf "sp"
  | Gp -> Format.fprintf ppf "gp"
  | Tp -> Format.fprintf ppf "tp"
  | T0 -> Format.fprintf ppf "t0"
  | T1 -> Format.fprintf ppf "t1"
  | T2 -> Format.fprintf ppf "t2"
  | S0 -> Format.fprintf ppf "s0"
  | Fp -> Format.fprintf ppf "fp"
  | S1 -> Format.fprintf ppf "s1"
  | A0 -> Format.fprintf ppf "a0"
  | A1 -> Format.fprintf ppf "a1"
  | A2 -> Format.fprintf ppf "a2"
  | A3 -> Format.fprintf ppf "a3"
  | A4 -> Format.fprintf ppf "a4"
  | A5 -> Format.fprintf ppf "a5"
  | A6 -> Format.fprintf ppf "a6"
  | A7 -> Format.fprintf ppf "a7"
  | S2 -> Format.fprintf ppf "s2"
  | S3 -> Format.fprintf ppf "s3"
  | S4 -> Format.fprintf ppf "s4"
  | S5 -> Format.fprintf ppf "s5"
  | S6 -> Format.fprintf ppf "s6"
  | S7 -> Format.fprintf ppf "s7"
  | S8 -> Format.fprintf ppf "s8"
  | S9 -> Format.fprintf ppf "s9"
  | S10 -> Format.fprintf ppf "s10"
  | S11 -> Format.fprintf ppf "s11"
  | T3 -> Format.fprintf ppf "t3"
  | T4 -> Format.fprintf ppf "t4"
  | T5 -> Format.fprintf ppf "t5"
  | T6 -> Format.fprintf ppf "t6"
;;

let pp_vector_register ppf = function
  | V0 -> Format.fprintf ppf "v0"
  | V1 -> Format.fprintf ppf "v1"
  | V2 -> Format.fprintf ppf "v2"
  | V3 -> Format.fprintf ppf "v3"
  | V4 -> Format.fprintf ppf "v4"
  | V5 -> Format.fprintf ppf "v5"
  | V6 -> Format.fprintf ppf "v6"
  | V7 -> Format.fprintf ppf "v7"
  | V8 -> Format.fprintf ppf "v8"
  | V9 -> Format.fprintf ppf "v9"
  | V10 -> Format.fprintf ppf "v10"
  | V11 -> Format.fprintf ppf "v11"
  | V12 -> Format.fprintf ppf "v12"
  | V13 -> Format.fprintf ppf "v13"
  | V14 -> Format.fprintf ppf "v14"
  | V15 -> Format.fprintf ppf "v15"
  | V16 -> Format.fprintf ppf "v16"
  | V17 -> Format.fprintf ppf "v17"
  | V18 -> Format.fprintf ppf "v18"
  | V19 -> Format.fprintf ppf "v19"
  | V20 -> Format.fprintf ppf "v20"
  | V21 -> Format.fprintf ppf "v21"
  | V22 -> Format.fprintf ppf "v22"
  | V23 -> Format.fprintf ppf "v23"
  | V24 -> Format.fprintf ppf "v24"
  | V25 -> Format.fprintf ppf "v25"
  | V26 -> Format.fprintf ppf "v26"
  | V27 -> Format.fprintf ppf "v27"
  | V28 -> Format.fprintf ppf "v28"
  | V29 -> Format.fprintf ppf "v29"
  | V30 -> Format.fprintf ppf "v30"
  | V31 -> Format.fprintf ppf "v31"
;;

let pp_float_register ppf = function
  | F0 -> Format.fprintf ppf "f0"
  | F1 -> Format.fprintf ppf "f1"
  | F2 -> Format.fprintf ppf "f2"
  | F3 -> Format.fprintf ppf "f3"
  | F4 -> Format.fprintf ppf "f4"
  | F5 -> Format.fprintf ppf "f5"
  | F6 -> Format.fprintf ppf "f6"
  | F7 -> Format.fprintf ppf "f7"
  | F8 -> Format.fprintf ppf "f8"
  | F9 -> Format.fprintf ppf "f9"
  | F10 -> Format.fprintf ppf "f10"
  | F11 -> Format.fprintf ppf "f11"
  | F12 -> Format.fprintf ppf "f12"
  | F13 -> Format.fprintf ppf "f13"
  | F14 -> Format.fprintf ppf "f14"
  | F15 -> Format.fprintf ppf "f15"
  | F16 -> Format.fprintf ppf "f16"
  | F17 -> Format.fprintf ppf "f17"
  | F18 -> Format.fprintf ppf "f18"
  | F19 -> Format.fprintf ppf "f19"
  | F20 -> Format.fprintf ppf "f20"
  | F21 -> Format.fprintf ppf "f21"
  | F22 -> Format.fprintf ppf "f22"
  | F23 -> Format.fprintf ppf "f23"
  | F24 -> Format.fprintf ppf "f24"
  | F25 -> Format.fprintf ppf "f25"
  | F26 -> Format.fprintf ppf "f26"
  | F27 -> Format.fprintf ppf "f27"
  | F28 -> Format.fprintf ppf "f28"
  | F29 -> Format.fprintf ppf "f29"
  | F30 -> Format.fprintf ppf "f30"
  | F31 -> Format.fprintf ppf "f31"
  | Ft0 -> Format.fprintf ppf "ft0"
  | Ft1 -> Format.fprintf ppf "ft1"
  | Ft2 -> Format.fprintf ppf "ft2"
  | Ft3 -> Format.fprintf ppf "ft3"
  | Ft4 -> Format.fprintf ppf "ft4"
  | Ft5 -> Format.fprintf ppf "ft5"
  | Ft6 -> Format.fprintf ppf "ft6"
  | Ft7 -> Format.fprintf ppf "ft7"
  | Fs0 -> Format.fprintf ppf "fs0"
  | Fs1 -> Format.fprintf ppf "fs1"
  | Fa0 -> Format.fprintf ppf "fa0"
  | Fa1 -> Format.fprintf ppf "fa1"
  | Fa2 -> Format.fprintf ppf "fa2"
  | Fa3 -> Format.fprintf ppf "fa3"
  | Fa4 -> Format.fprintf ppf "fa4"
  | Fa5 -> Format.fprintf ppf "fa5"
  | Fa6 -> Format.fprintf ppf "fa6"
  | Fa7 -> Format.fprintf ppf "fa7"
  | Fs2 -> Format.fprintf ppf "fs2"
  | Fs3 -> Format.fprintf ppf "fs3"
  | Fs4 -> Format.fprintf ppf "fs4"
  | Fs5 -> Format.fprintf ppf "fs5"
  | Fs6 -> Format.fprintf ppf "fs6"
  | Fs7 -> Format.fprintf ppf "fs7"
  | Fs8 -> Format.fprintf ppf "fs8"
  | Fs9 -> Format.fprintf ppf "fs9"
  | Fs10 -> Format.fprintf ppf "fs10"
  | Fs11 -> Format.fprintf ppf "fs11"
  | Ft8 -> Format.fprintf ppf "ft8"
  | Ft9 -> Format.fprintf ppf "ft9"
  | Ft10 -> Format.fprintf ppf "ft10"
  | Ft11 -> Format.fprintf ppf "ft11"
;;

type address =
  | Address12 of address12
  | Address20 of address20
  | Address32 of address32

let pp_instruction_3reg_helper ppf mnemonic r1 r2 r3 =
  Format.fprintf ppf "%s %a, %a, %a" mnemonic pp_register r1 pp_register r2 pp_register r3
;;

let pp_instruction_3vreg_helper ppf mnemonic v1 v2 v3 =
  Format.fprintf
    ppf
    "%s %a, %a, %a"
    mnemonic
    pp_vector_register
    v1
    pp_vector_register
    v2
    pp_vector_register
    v3
;;

let pp_address ppf = function
  | Address12 (LabelAddress12 str) -> Format.fprintf ppf "%s" str
  | Address20 (LabelAddress20 str) -> Format.fprintf ppf "%s" str
  | Address32 (LabelAddress32 str) -> Format.fprintf ppf "%s" str
  | Address12 (ImmediateAddress12 imm) -> Format.fprintf ppf "%d" imm
  | Address20 (ImmediateAddress20 imm) -> Format.fprintf ppf "%d" imm
  | Address32 (ImmediateAddress32 imm) -> Format.fprintf ppf "%d" imm
;;

let pp_instruction_3f_reg_helper ppf mnemonic r1 r2 r3 =
  Format.fprintf
    ppf
    "%s %a, %a, %a"
    mnemonic
    pp_float_register
    r1
    pp_float_register
    r2
    pp_float_register
    r3
;;

let pp_instruction_2f_1_reg_helper ppf mnemonic r1 r2 r3 =
  Format.fprintf
    ppf
    "%s %a, %a, %a"
    mnemonic
    pp_register
    r1
    pp_float_register
    r2
    pp_float_register
    r3
;;

let pp_instruction_4f_reg_helper ppf mnemonic r1 r2 r3 r4 =
  Format.fprintf
    ppf
    "%s %a, %a, %a, %a"
    mnemonic
    pp_float_register
    r1
    pp_float_register
    r2
    pp_float_register
    r3
    pp_float_register
    r4
;;

let pp_address ppf = function
  | Address12 (LabelAddress12 str) -> Format.fprintf ppf "%s" str
  | Address20 (LabelAddress20 str) -> Format.fprintf ppf "%s" str
  | Address32 (LabelAddress32 str) -> Format.fprintf ppf "%s" str
  | Address12 (ImmediateAddress12 imm) -> Format.fprintf ppf "%d" imm
  | Address20 (ImmediateAddress20 imm) -> Format.fprintf ppf "%d" imm
  | Address32 (ImmediateAddress32 imm) -> Format.fprintf ppf "%d" imm
;;

let pp_instruction_2reg_1imm_helper ppf mnemonic r1 r2 addr =
  Format.fprintf ppf "%s %a,%a,%a" mnemonic pp_register r1 pp_register r2 pp_address addr
;;

let pp_instruction_2vreg_1reg_helper ppf mnemonic v1 v2 r3 =
  Format.fprintf
    ppf
    "%s %a, %a, %a"
    mnemonic
    pp_vector_register
    v1
    pp_vector_register
    v2
    pp_register
    r3
;;

let pp_instruction_2reg_1offset_helper ppf mnemonic r1 r2 addr =
  Format.fprintf ppf "%s %a,%a(%a)" mnemonic pp_register r1 pp_address addr pp_register r2
;;

let pp_instruction_1vreg_1reg_1offset_helper ppf mnemonic v1 r2 addr =
  Format.fprintf
    ppf
    "%s %a,%a(%a)"
    mnemonic
    pp_vector_register
    v1
    pp_address
    addr
    pp_register
    r2
;;

let pp_instruction_2freg_1offset_helper ppf mnemonic r1 r2 addr =
  Format.fprintf
    ppf
    "%s %a,%a(%a)"
    mnemonic
    pp_float_register
    r1
    pp_address
    addr
    pp_float_register
    r2
;;

let pp_instruction ppf = function
  | Add (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "add" rd rs1 rs2
  | Sub (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "sub" rd rs1 rs2
  | Xor (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "xor" rd rs1 rs2
  | Or (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "or" rd rs1 rs2
  | And (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "and" rd rs1 rs2
  | Sll (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "sll" rd rs1 rs2
  | Srl (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "srl" rd rs1 rs2
  | Sra (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "sra" rd rs1 rs2
  | Slt (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "slt" rd rs1 rs2
  | Sltu (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "sltu" rd rs1 rs2
  | Mul (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "mul" rd rs1 rs2
  | Mulh (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "mulh" rd rs1 rs2
  | Mulhsu (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "mulhsu" rd rs1 rs2
  | Mulhu (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "mulhu" rd rs1 rs2
  | Div (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "div" rd rs1 rs2
  | Divu (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "divu" rd rs1 rs2
  | Rem (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "rem" rd rs1 rs2
  | Remu (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "remu" rd rs1 rs2
  | Addw (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "addw" rd rs1 rs2
  | Subw (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "subw" rd rs1 rs2
  | Sllw (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "sllw" rd rs1 rs2
  | Srlw (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "srlw" rd rs1 rs2
  | Sraw (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "sraw" rd rs1 rs2
  | Mulw (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "mulw" rd rs1 rs2
  | Divw (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "divw" rd rs1 rs2
  | Divuw (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "divuw" rd rs1 rs2
  | Remw (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "remw" rd rs1 rs2
  | Remwu (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "remwu" rd rs1 rs2
  | Lwu (rd, rs1, imm) ->
    pp_instruction_2reg_1offset_helper ppf "lwu" rd rs1 (Address12 imm)
  | Addi (rd, rs1, imm) ->
    pp_instruction_2reg_1imm_helper ppf "addi" rd rs1 (Address12 imm)
  | Xori (rd, rs1, imm) ->
    pp_instruction_2reg_1imm_helper ppf "xori" rd rs1 (Address12 imm)
  | Ori (rd, rs1, imm) -> pp_instruction_2reg_1imm_helper ppf "ori" rd rs1 (Address12 imm)
  | Andi (rd, rs1, imm) ->
    pp_instruction_2reg_1imm_helper ppf "andi" rd rs1 (Address12 imm)
  | Slli (rd, rs1, imm) ->
    pp_instruction_2reg_1imm_helper ppf "slli" rd rs1 (Address12 imm)
  | Srli (rd, rs1, imm) ->
    pp_instruction_2reg_1imm_helper ppf "srli" rd rs1 (Address12 imm)
  | Srai (rd, rs1, imm) ->
    pp_instruction_2reg_1imm_helper ppf "srai" rd rs1 (Address12 imm)
  | Slti (rd, rs1, imm) ->
    pp_instruction_2reg_1imm_helper ppf "slti" rd rs1 (Address12 imm)
  | Sltiu (rd, rs1, imm) ->
    pp_instruction_2reg_1imm_helper ppf "sltiu" rd rs1 (Address12 imm)
  | Lb (rd, rs1, imm) ->
    pp_instruction_2reg_1offset_helper ppf "lb" rd rs1 (Address12 imm)
  | Lh (rd, rs1, imm) ->
    pp_instruction_2reg_1offset_helper ppf "lh" rd rs1 (Address12 imm)
  | Lw (rd, rs1, imm) ->
    pp_instruction_2reg_1offset_helper ppf "lw" rd rs1 (Address12 imm)
  | Lbu (rd, rs1, imm) ->
    pp_instruction_2reg_1offset_helper ppf "lbu" rd rs1 (Address12 imm)
  | Lhu (rd, rs1, imm) ->
    pp_instruction_2reg_1offset_helper ppf "lhu" rd rs1 (Address12 imm)
  | Sb (rd, rs1, imm) ->
    pp_instruction_2reg_1offset_helper ppf "sb" rd rs1 (Address12 imm)
  | Sh (rd, rs1, imm) ->
    pp_instruction_2reg_1offset_helper ppf "sh" rd rs1 (Address12 imm)
  | Sw (rd, rs1, imm) ->
    pp_instruction_2reg_1offset_helper ppf "sw" rd rs1 (Address12 imm)
  | Beq (rd, rs1, imm) -> pp_instruction_2reg_1imm_helper ppf "beq" rd rs1 (Address12 imm)
  | Beqz (rs1, imm) ->
    Format.fprintf ppf "beqz %a,%a" pp_register rs1 pp_address (Address12 imm)
  | Bne (rd, rs1, imm) -> pp_instruction_2reg_1imm_helper ppf "bne" rd rs1 (Address12 imm)
  | Bnez (rs1, imm) ->
    Format.fprintf ppf "bnez %a,%a" pp_register rs1 pp_address (Address12 imm)
  | Blt (rd, rs1, imm) -> pp_instruction_2reg_1imm_helper ppf "blt" rd rs1 (Address12 imm)
  | Bltz (rs1, imm) ->
    Format.fprintf ppf "bltz %a,%a" pp_register rs1 pp_address (Address12 imm)
  | Bgt (rd, rs1, imm) -> pp_instruction_2reg_1imm_helper ppf "bgt" rd rs1 (Address12 imm)
  | Bgtz (rs1, imm) ->
    Format.fprintf ppf "bgtz %a,%a" pp_register rs1 pp_address (Address12 imm)
  | Bge (rd, rs1, imm) -> pp_instruction_2reg_1imm_helper ppf "bge" rd rs1 (Address12 imm)
  | Bltu (rd, rs1, imm) ->
    pp_instruction_2reg_1imm_helper ppf "bltu" rd rs1 (Address12 imm)
  | Bgeu (rd, rs1, imm) ->
    pp_instruction_2reg_1imm_helper ppf "bgeu" rd rs1 (Address12 imm)
  | Jalr (rd, rs1, imm) ->
    pp_instruction_2reg_1imm_helper ppf "jalr" rd rs1 (Address12 imm)
  | Ld (rd, rs1, imm) ->
    pp_instruction_2reg_1offset_helper ppf "ld" rd rs1 (Address12 imm)
  | Sd (rd, rs1, imm) ->
    pp_instruction_2reg_1offset_helper ppf "sd" rd rs1 (Address12 imm)
  | Addiw (rd, rs1, imm) ->
    pp_instruction_2reg_1imm_helper ppf "addiw" rd rs1 (Address12 imm)
  | Slliw (rd, rs1, imm) ->
    pp_instruction_2reg_1imm_helper ppf "slliw" rd rs1 (Address12 imm)
  | Srliw (rd, rs1, imm) ->
    pp_instruction_2reg_1imm_helper ppf "srliw" rd rs1 (Address12 imm)
  | Sraiw (rd, rs1, imm) ->
    pp_instruction_2reg_1imm_helper ppf "sraiw" rd rs1 (Address12 imm)
  | Jal (rd, imm) ->
    Format.fprintf ppf "jal %a, %a" pp_register rd pp_address (Address20 imm)
  | Jr rs1 -> Format.fprintf ppf "jr %a" pp_register rs1
  | J imm -> Format.fprintf ppf "j %a" pp_address (Address20 imm)
  | Lui (rd, imm) ->
    Format.fprintf ppf "lui %a,%a" pp_register rd pp_address (Address20 imm)
  | Auipc (rd, imm) ->
    Format.fprintf ppf "auipc %a,%a" pp_register rd pp_address (Address20 imm)
  | Ecall -> Format.fprintf ppf "ecall"
  | Call str -> Format.fprintf ppf "call %s" str
  | La (rd, imm) ->
    Format.fprintf ppf "la %a,%a" pp_register rd pp_address (Address32 imm)
  | Lla (rd, imm) ->
    Format.fprintf ppf "lla %a,%a" pp_register rd pp_address (Address32 imm)
  | Mv (rd, rs1) -> Format.fprintf ppf "mv %a,%a" pp_register rd pp_register rs1
  | Li (rd, imm) ->
    Format.fprintf ppf "li %a,%a" pp_register rd pp_address (Address32 imm)
  | Ret -> Format.fprintf ppf "ret"
  | FmaddS (rd, rs1, rs2, rs3) ->
    pp_instruction_4f_reg_helper ppf "fmadd.s" rd rs1 rs2 rs3
  | FmsubS (rd, rs1, rs2, rs3) ->
    pp_instruction_4f_reg_helper ppf "fmsub.s" rd rs1 rs2 rs3
  | FnmsubS (rd, rs1, rs2, rs3) ->
    pp_instruction_4f_reg_helper ppf "fnmsub.s" rd rs1 rs2 rs3
  | FnmaddS (rd, rs1, rs2, rs3) ->
    pp_instruction_4f_reg_helper ppf "fnmadd.s" rd rs1 rs2 rs3
  | FaddS (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fadd.s" rd rs1 rs2
  | FsubS (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fsub.s" rd rs1 rs2
  | FmulS (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fmul.s" rd rs1 rs2
  | FdivS (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fdiv.s" rd rs1 rs2
  | FsqrtS (rd, rs1) ->
    Format.fprintf ppf "fsqrt.s %a,%a" pp_float_register rd pp_float_register rs1
  | FsgnjS (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fsgnj.s" rd rs1 rs2
  | FsgnjnS (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fsgnjn.s" rd rs1 rs2
  | FsgnjxS (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fsgnjx.s" rd rs1 rs2
  | FminS (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fmin.s" rd rs1 rs2
  | FmaxS (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fmax.s" rd rs1 rs2
  | FcvtWS (rd, rs1) ->
    Format.fprintf ppf "fcvt.w.s %a,%a" pp_register rd pp_float_register rs1
  | FcvtWuS (rd, rs1) ->
    Format.fprintf ppf "fcvt.wu.s %a,%a" pp_register rd pp_float_register rs1
  | FmvXW (rd, rs1) ->
    Format.fprintf ppf "fmv.x.w %a,%a" pp_register rd pp_float_register rs1
  | FeqS (rd, rs1, rs2) -> pp_instruction_2f_1_reg_helper ppf "feq.s" rd rs1 rs2
  | FltS (rd, rs1, rs2) -> pp_instruction_2f_1_reg_helper ppf "flt.s" rd rs1 rs2
  | FleS (rd, rs1, rs2) -> pp_instruction_2f_1_reg_helper ppf "fle.s" rd rs1 rs2
  | FclassS (rd, rs1) ->
    Format.fprintf ppf "fclass.s %a,%a" pp_register rd pp_float_register rs1
  | FcvtSW (rd, rs1) ->
    Format.fprintf ppf "fcvt.s.w %a,%a" pp_float_register rd pp_register rs1
  | FcvtSWu (rd, rs1) ->
    Format.fprintf ppf "fcvt.s.wu %a,%a" pp_float_register rd pp_register rs1
  | FmvWX (rd, rs1) ->
    Format.fprintf ppf "fmv.w.x %a,%a" pp_float_register rd pp_register rs1
  | FmaddD (rd, rs1, rs2, rs3) ->
    pp_instruction_4f_reg_helper ppf "fmadd.d" rd rs1 rs2 rs3
  | FmsubD (rd, rs1, rs2, rs3) ->
    pp_instruction_4f_reg_helper ppf "fmsub.d" rd rs1 rs2 rs3
  | FnmsubD (rd, rs1, rs2, rs3) ->
    pp_instruction_4f_reg_helper ppf "fnmsub.d" rd rs1 rs2 rs3
  | FnmaddD (rd, rs1, rs2, rs3) ->
    pp_instruction_4f_reg_helper ppf "fnmadd.d" rd rs1 rs2 rs3
  | FaddD (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fadd.d" rd rs1 rs2
  | FsubD (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fsub.d" rd rs1 rs2
  | FmulD (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fmul.d" rd rs1 rs2
  | FdivD (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fdiv.d" rd rs1 rs2
  | FsqrtD (rd, rs1) ->
    Format.fprintf ppf "fsqrt.d %a,%a" pp_float_register rd pp_float_register rs1
  | FsgnjD (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fsgnj.d" rd rs1 rs2
  | FsgnjnD (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fsgnjn.d" rd rs1 rs2
  | FsgnjxD (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fsgnjx.d" rd rs1 rs2
  | FminD (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fmin.d" rd rs1 rs2
  | FmaxD (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fmax.d" rd rs1 rs2
  | FcvtSD (rd, rs1) ->
    Format.fprintf ppf "fcvt.s.d %a,%a" pp_float_register rd pp_float_register rs1
  | FcvtDS (rd, rs1) ->
    Format.fprintf ppf "fcvt.d.s %a,%a" pp_float_register rd pp_float_register rs1
  | FeqD (rd, rs1, rs2) -> pp_instruction_2f_1_reg_helper ppf "feq.d" rd rs1 rs2
  | FltD (rd, rs1, rs2) -> pp_instruction_2f_1_reg_helper ppf "flt.d" rd rs1 rs2
  | FleD (rd, rs1, rs2) -> pp_instruction_2f_1_reg_helper ppf "fle.d" rd rs1 rs2
  | FcvtWD (rd, rs1) ->
    Format.fprintf ppf "fcvt.w.d %a,%a" pp_register rd pp_float_register rs1
  | FcvtWuD (rd, rs1) ->
    Format.fprintf ppf "fcvt.wu.d %a,%a" pp_register rd pp_float_register rs1
  | FclassD (rd, rs1) ->
    Format.fprintf ppf "fclass.d %a,%a" pp_register rd pp_float_register rs1
  | FcvtDW (rd, rs1) ->
    Format.fprintf ppf "fcvt.d.w %a,%a" pp_float_register rd pp_register rs1
  | FcvtDWu (rd, rs1) ->
    Format.fprintf ppf "fcvt.d.wu %a,%a" pp_float_register rd pp_register rs1
  | Flw (rd, rs1, imm) ->
    pp_instruction_2freg_1offset_helper ppf "flw" rd rs1 (Address12 imm)
  | Fsw (rd, rs1, imm) ->
    pp_instruction_2freg_1offset_helper ppf "fsw" rd rs1 (Address12 imm)
  | Fld (rd, rs1, imm) ->
    pp_instruction_2freg_1offset_helper ppf "fld" rd rs1 (Address12 imm)
  | Fsd (rd, rs1, imm) ->
    pp_instruction_2freg_1offset_helper ppf "fsd" rd rs1 (Address12 imm)
  | FcvtLS (rd, rs1) ->
    Format.fprintf ppf "fcvt.l.s %a,%a" pp_register rd pp_float_register rs1
  | FcvtLuS (rd, rs1) ->
    Format.fprintf ppf "fcvt.lu.s %a,%a" pp_register rd pp_float_register rs1
  | FcvtSL (rd, rs1) ->
    Format.fprintf ppf "fcvt.s.l %a,%a" pp_float_register rd pp_register rs1
  | FcvtSLu (rd, rs1) ->
    Format.fprintf ppf "fcvt.s.lu %a,%a" pp_float_register rd pp_register rs1
  | FcvtLD (rd, rs1) ->
    Format.fprintf ppf "fcvt.l.d %a,%a" pp_register rd pp_float_register rs1
  | FcvtLuD (rd, rs1) ->
    Format.fprintf ppf "fcvt.lu.d %a,%a" pp_register rd pp_float_register rs1
  | FcvtDL (rd, rs1) ->
    Format.fprintf ppf "fcvt.d.l %a,%a" pp_float_register rd pp_register rs1
  | FcvtDLu (rd, rs1) ->
    Format.fprintf ppf "fcvt.d.lu %a,%a" pp_float_register rd pp_register rs1
  | Adduw (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "add.uw" rd rs1 rs2
  | Sh1add (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "sh1add" rd rs1 rs2
  | Sh1adduw (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "sh1add.uw" rd rs1 rs2
  | Sh2add (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "sh2add" rd rs1 rs2
  | Sh2adduw (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "sh2add.uw" rd rs1 rs2
  | Sh3add (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "sh3add" rd rs1 rs2
  | Sh3adduw (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "sh3add.uw" rd rs1 rs2
  | Andn (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "andn" rd rs1 rs2
  | Orn (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "orn" rd rs1 rs2
  | Xnor (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "xnor" rd rs1 rs2
  | Vle32v (vd, rs1, imm) ->
    pp_instruction_1vreg_1reg_1offset_helper ppf "vle32.v" vd rs1 (Address12 imm)
  | Vse32v (vs, rs1, imm) ->
    pp_instruction_1vreg_1reg_1offset_helper ppf "vse32.v" vs rs1 (Address12 imm)
  | Vaddvv (vd, vs1, vs2) -> pp_instruction_3vreg_helper ppf "vadd.vv" vd vs1 vs2
  | Vaddvx (vd, vs1, rs2) -> pp_instruction_2vreg_1reg_helper ppf "vadd.vx" vd vs1 rs2
  | Vsubvv (vd, vs1, vs2) -> pp_instruction_3vreg_helper ppf "vsub.vv" vd vs1 vs2
  | Vsubvx (vd, vs1, rs2) -> pp_instruction_2vreg_1reg_helper ppf "vsub.vx" vd vs1 rs2
  | Vmulvv (vd, vs1, vs2) -> pp_instruction_3vreg_helper ppf "vmul.vv" vd vs1 vs2
  | Vmulvx (vd, vs1, rs2) -> pp_instruction_2vreg_1reg_helper ppf "vmul.vx" vd vs1 rs2
  | Vdivvv (vd, vs1, vs2) -> pp_instruction_3vreg_helper ppf "vdiv.vv" vd vs1 vs2
  | Vdivvx (vd, vs1, rs2) -> pp_instruction_2vreg_1reg_helper ppf "vdiv.vx" vd vs1 rs2
  | Vandvv (vd, vs1, vs2) -> pp_instruction_3vreg_helper ppf "vand.vv" vd vs1 vs2
  | Vandvx (vd, vs1, rs2) -> pp_instruction_2vreg_1reg_helper ppf "vand.vx" vd vs1 rs2
  | Vorvv (vd, vs1, vs2) -> pp_instruction_3vreg_helper ppf "vor.vv" vd vs1 vs2
  | Vorvx (vd, vs1, rs2) -> pp_instruction_2vreg_1reg_helper ppf "vor.vx" vd vs1 rs2
  | Vxorvv (vd, vs1, vs2) -> pp_instruction_3vreg_helper ppf "vxor.vv" vd vs1 vs2
  | Vxorvx (vd, vs1, rs2) -> pp_instruction_2vreg_1reg_helper ppf "vxor.vx" vd vs1 rs2
  | Vminvv (vd, vs1, vs2) -> pp_instruction_3vreg_helper ppf "vmin.vv" vd vs1 vs2
  | Vminvx (vd, vs1, rs2) -> pp_instruction_2vreg_1reg_helper ppf "vmin.vx" vd vs1 rs2
  | Vmaxvv (vd, vs1, vs2) -> pp_instruction_3vreg_helper ppf "vmax.vv" vd vs1 vs2
  | Vmaxvx (vd, vs1, rs2) -> pp_instruction_2vreg_1reg_helper ppf "vmax.vx" vd vs1 rs2
  | Vmseqvv (vd, vs1, vs2) -> pp_instruction_3vreg_helper ppf "vmseq.vv" vd vs1 vs2
  | Vmseqvx (vd, vs1, rs2) -> pp_instruction_2vreg_1reg_helper ppf "vmseq.vx" vd vs1 rs2
  | Vsetvli (r1, r2) ->
    Format.fprintf ppf "vsetvli %a,%a, e32" pp_register r1 pp_register r2
  | Vredsumvs (vd, vs1, vs2) -> pp_instruction_3vreg_helper ppf "vredsum.vs" vd vs1 vs2
;;

let pp_type_dir ppf = function
  | Type str -> Format.fprintf ppf "%s" str
;;

let pp_directive ppf = function
  | Text -> Format.fprintf ppf ".text"
  | Globl imm -> Format.fprintf ppf ".globl %a" pp_address (Address12 imm)
  | TypeDir (str, str_type) -> Format.fprintf ppf ".type %s,@%a" str pp_type_dir str_type
  | Section (str1, None) -> Format.fprintf ppf ".section %s" str1
  | Section (str1, Some (str2, None)) -> Format.fprintf ppf ".section %s,%S" str1 str2
  | Section (str1, Some (str2, Some (typ, None))) ->
    Format.fprintf ppf ".section %s,%S,@%a" str1 str2 pp_type_dir typ
  | Section (str1, Some (str2, Some (typ, Some i))) ->
    Format.fprintf ppf ".section %s,%S,@%a,%d" str1 str2 pp_type_dir typ i
  | StringDir str -> Format.fprintf ppf ".string %S" (String.escaped str)
  | Word imm -> Format.fprintf ppf ".word %d" (Int32.to_int imm)
  | Space imm -> Format.fprintf ppf ".space %d" imm
;;

let pp_label ppf label = Format.fprintf ppf "%s:" label

let pp_expr ppf = function
  | InstructionExpr instruction -> pp_instruction ppf instruction
  | LabelExpr label -> pp_label ppf label
  | DirectiveExpr directive -> pp_directive ppf directive
;;

let pp_ast ppf (ast : ast) =
  Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "\n") pp_expr ppf ast
;;

let print_ast ast = pp_ast Format.std_formatter ast
