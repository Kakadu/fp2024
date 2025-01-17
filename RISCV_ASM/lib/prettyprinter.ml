(** Copyright 2024, Vyacheslav Kochergin Roman Mukovenkov, Yuliana Ementyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

let pp_register = function
  | X0 -> Printf.sprintf "x0"
  | X1 -> Printf.sprintf "x1"
  | X2 -> Printf.sprintf "x2"
  | X3 -> Printf.sprintf "x3"
  | X4 -> Printf.sprintf "x4"
  | X5 -> Printf.sprintf "x5"
  | X6 -> Printf.sprintf "x6"
  | X7 -> Printf.sprintf "x7"
  | X8 -> Printf.sprintf "x8"
  | X9 -> Printf.sprintf "x9"
  | X10 -> Printf.sprintf "x10"
  | X11 -> Printf.sprintf "x11"
  | X12 -> Printf.sprintf "x12"
  | X13 -> Printf.sprintf "x13"
  | X14 -> Printf.sprintf "x14"
  | X15 -> Printf.sprintf "x15"
  | X16 -> Printf.sprintf "x16"
  | X17 -> Printf.sprintf "x17"
  | X18 -> Printf.sprintf "x18"
  | X19 -> Printf.sprintf "x19"
  | X20 -> Printf.sprintf "x20"
  | X21 -> Printf.sprintf "x21"
  | X22 -> Printf.sprintf "x22"
  | X23 -> Printf.sprintf "x23"
  | X24 -> Printf.sprintf "x24"
  | X25 -> Printf.sprintf "x25"
  | X26 -> Printf.sprintf "x26"
  | X27 -> Printf.sprintf "x27"
  | X28 -> Printf.sprintf "x28"
  | X29 -> Printf.sprintf "x29"
  | X30 -> Printf.sprintf "x30"
  | X31 -> Printf.sprintf "x31"
  | Zero -> Printf.sprintf "zero"
  | Ra -> Printf.sprintf "ra"
  | Sp -> Printf.sprintf "sp"
  | Gp -> Printf.sprintf "gp"
  | Tp -> Printf.sprintf "tp"
  | T0 -> Printf.sprintf "t0"
  | T1 -> Printf.sprintf "t1"
  | T2 -> Printf.sprintf "t2"
  | S0 -> Printf.sprintf "s0"
  | Fp -> Printf.sprintf "fp"
  | S1 -> Printf.sprintf "s1"
  | A0 -> Printf.sprintf "a0"
  | A1 -> Printf.sprintf "a1"
  | A2 -> Printf.sprintf "a2"
  | A3 -> Printf.sprintf "a3"
  | A4 -> Printf.sprintf "a4"
  | A5 -> Printf.sprintf "a5"
  | A6 -> Printf.sprintf "a6"
  | A7 -> Printf.sprintf "a7"
  | S2 -> Printf.sprintf "s2"
  | S3 -> Printf.sprintf "s3"
  | S4 -> Printf.sprintf "s4"
  | S5 -> Printf.sprintf "s5"
  | S6 -> Printf.sprintf "s6"
  | S7 -> Printf.sprintf "s7"
  | S8 -> Printf.sprintf "s8"
  | S9 -> Printf.sprintf "s9"
  | S10 -> Printf.sprintf "s10"
  | S11 -> Printf.sprintf "s11"
  | T3 -> Printf.sprintf "t3"
  | T4 -> Printf.sprintf "t4"
  | T5 -> Printf.sprintf "t5"
  | T6 -> Printf.sprintf "t6"
;;

let pp_float_register = function
  | F0 -> Printf.sprintf "f0"
  | F1 -> Printf.sprintf "f1"
  | F2 -> Printf.sprintf "f2"
  | F3 -> Printf.sprintf "f3"
  | F4 -> Printf.sprintf "f4"
  | F5 -> Printf.sprintf "f5"
  | F6 -> Printf.sprintf "f6"
  | F7 -> Printf.sprintf "f7"
  | F8 -> Printf.sprintf "f8"
  | F9 -> Printf.sprintf "f9"
  | F10 -> Printf.sprintf "f10"
  | F11 -> Printf.sprintf "f11"
  | F12 -> Printf.sprintf "f12"
  | F13 -> Printf.sprintf "f13"
  | F14 -> Printf.sprintf "f14"
  | F15 -> Printf.sprintf "f15"
  | F16 -> Printf.sprintf "f16"
  | F17 -> Printf.sprintf "f17"
  | F18 -> Printf.sprintf "f18"
  | F19 -> Printf.sprintf "f19"
  | F20 -> Printf.sprintf "f20"
  | F21 -> Printf.sprintf "f21"
  | F22 -> Printf.sprintf "f22"
  | F23 -> Printf.sprintf "f23"
  | F24 -> Printf.sprintf "f24"
  | F25 -> Printf.sprintf "f25"
  | F26 -> Printf.sprintf "f26"
  | F27 -> Printf.sprintf "f27"
  | F28 -> Printf.sprintf "f28"
  | F29 -> Printf.sprintf "f29"
  | F30 -> Printf.sprintf "f30"
  | F31 -> Printf.sprintf "f31"
  | Ft0 -> Printf.sprintf "ft0"
  | Ft1 -> Printf.sprintf "ft1"
  | Ft2 -> Printf.sprintf "ft2"
  | Ft3 -> Printf.sprintf "ft3"
  | Ft4 -> Printf.sprintf "ft4"
  | Ft5 -> Printf.sprintf "ft5"
  | Ft6 -> Printf.sprintf "ft6"
  | Ft7 -> Printf.sprintf "ft7"
  | Fs0 -> Printf.sprintf "fs0"
  | Fs1 -> Printf.sprintf "fs1"
  | Fa0 -> Printf.sprintf "fa0"
  | Fa1 -> Printf.sprintf "fa1"
  | Fa2 -> Printf.sprintf "fa2"
  | Fa3 -> Printf.sprintf "fa3"
  | Fa4 -> Printf.sprintf "fa4"
  | Fa5 -> Printf.sprintf "fa5"
  | Fa6 -> Printf.sprintf "fa6"
  | Fa7 -> Printf.sprintf "fa7"
  | Fs2 -> Printf.sprintf "fs2"
  | Fs3 -> Printf.sprintf "fs3"
  | Fs4 -> Printf.sprintf "fs4"
  | Fs5 -> Printf.sprintf "fs5"
  | Fs6 -> Printf.sprintf "fs6"
  | Fs7 -> Printf.sprintf "fs7"
  | Fs8 -> Printf.sprintf "fs8"
  | Fs9 -> Printf.sprintf "fs9"
  | Fs10 -> Printf.sprintf "fs10"
  | Fs11 -> Printf.sprintf "fs11"
  | Ft8 -> Printf.sprintf "ft8"
  | Ft9 -> Printf.sprintf "ft9"
  | Ft10 -> Printf.sprintf "ft10"
  | Ft11 -> Printf.sprintf "ft11"
;;

type address =
  | Address12 of address12
  | Address20 of address20
  | Address32 of address32

let pp_instruction_3reg_helper ppf mnemonic r1 r2 r3 =
  Format.fprintf
    ppf
    "%s %s, %s, %s"
    mnemonic
    (pp_register r1)
    (pp_register r2)
    (pp_register r3)
;;

let pp_instruction_3f_reg_helper ppf mnemonic r1 r2 r3 =
  Format.fprintf
    ppf
    "%s %s, %s, %s"
    mnemonic
    (pp_float_register r1)
    (pp_float_register r2)
    (pp_float_register r3)
;;

let pp_instruction_2f_1_reg_helper ppf mnemonic r1 r2 r3 =
  Format.fprintf
    ppf
    "%s %s, %s, %s"
    mnemonic
    (pp_register r1)
    (pp_float_register r2)
    (pp_float_register r3)
;;

let pp_instruction_4f_reg_helper ppf mnemonic r1 r2 r3 r4 =
  Format.fprintf
    ppf
    "%s %s, %s, %s, %s"
    mnemonic
    (pp_float_register r1)
    (pp_float_register r2)
    (pp_float_register r3)
    (pp_float_register r4)
;;

let pp_address = function
  | Address12 (LabelAddress12 str) -> Format.sprintf "%s" str
  | Address20 (LabelAddress20 str) -> Format.sprintf "%s" str
  | Address32 (LabelAddress32 str) -> Format.sprintf "%s" str
  | Address12 (ImmediateAddress12 imm) -> Format.sprintf "%d" imm
  | Address20 (ImmediateAddress20 imm) -> Format.sprintf "%d" imm
  | Address32 (ImmediateAddress32 imm) -> Format.sprintf "%d" imm
;;

let pp_instruction_2reg_1imm_helper ppf mnemonic r1 r2 addr =
  Format.fprintf
    ppf
    "%s %s,%s,%s"
    mnemonic
    (pp_register r1)
    (pp_register r2)
    (pp_address addr)
;;

let pp_instruction_2reg_1offset_helper ppf mnemonic r1 r2 addr =
  Format.fprintf
    ppf
    "%s %s,%s(%s)"
    mnemonic
    (pp_register r1)
    (pp_address addr)
    (pp_register r2)
;;

let pp_instruction_2freg_1offset_helper ppf mnemonic r1 r2 addr =
  Format.fprintf
    ppf
    "%s %s,%s(%s)"
    mnemonic
    (pp_float_register r1)
    (pp_address addr)
    (pp_float_register r2)
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
  | Lwu (rd, rs1, imm) -> pp_instruction_2reg_1imm_helper ppf "lwu" rd rs1 (Address12 imm)
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
  | Lhu (rd, rs1, imm) ->
    pp_instruction_2reg_1offset_helper ppf "lhu" rd rs1 (Address12 imm)
  | Sb (rd, rs1, imm) ->
    pp_instruction_2reg_1offset_helper ppf "sb" rd rs1 (Address12 imm)
  | Sh (rd, rs1, imm) ->
    pp_instruction_2reg_1offset_helper ppf "sh" rd rs1 (Address12 imm)
  | Sw (rd, rs1, imm) ->
    pp_instruction_2reg_1offset_helper ppf "sw" rd rs1 (Address12 imm)
  | Beq (rd, rs1, imm) -> pp_instruction_2reg_1imm_helper ppf "beq" rd rs1 (Address12 imm)
  | Bne (rd, rs1, imm) -> pp_instruction_2reg_1imm_helper ppf "bne" rd rs1 (Address12 imm)
  | Blt (rd, rs1, imm) -> pp_instruction_2reg_1imm_helper ppf "blt" rd rs1 (Address12 imm)
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
    Format.fprintf ppf "jal %s, %s" (pp_register rd) (pp_address (Address20 imm))
  | Jr rs1 -> Format.fprintf ppf "jr %s" (pp_register rs1)
  | J imm -> Format.fprintf ppf "j %s" (pp_address (Address20 imm))
  | Lui (rd, imm) ->
    Format.fprintf ppf "lui %s,%s" (pp_register rd) (pp_address (Address20 imm))
  | Auipc (rd, imm) ->
    Format.fprintf ppf "auipc %s,%s" (pp_register rd) (pp_address (Address20 imm))
  | Ecall -> Format.fprintf ppf "ecall"
  | Call str -> Format.fprintf ppf "call %s" str
  | La (rd, imm) ->
    Format.fprintf ppf "la %s,%s" (pp_register rd) (pp_address (Address32 imm))
  | Lla (rd, imm) ->
    Format.fprintf ppf "lla %s,%s" (pp_register rd) (pp_address (Address32 imm))
  | Mv (rd, rs1) -> Format.fprintf ppf "mv %s,%s" (pp_register rd) (pp_register rs1)
  | Li (rd, imm) ->
    Format.fprintf ppf "li %s,%s" (pp_register rd) (pp_address (Address32 imm))
  | Ret -> Format.fprintf ppf "ret"
  | FmaddS (rd, rs1, rs2, rs3) ->
    pp_instruction_4f_reg_helper ppf "fmadd.s" rd rs1 rs2 rs3
  | FmsubS (rd, rs1, rs2, rs3) ->
    pp_instruction_4f_reg_helper ppf "fmsub.s" rd rs1 rs2 rs3
  | FnmsubS (rd, rs1, rs2, rs3) ->
    pp_instruction_4f_reg_helper ppf "fnmsub.s" rd rs1 rs2 rs3
  | FnmaddS (rd, rs1, rs2, rs3) ->
    pp_instruction_4f_reg_helper ppf "fmadd.s" rd rs1 rs2 rs3
  | FaddS (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fadd.s" rd rs1 rs2
  | FsubS (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fsub.s" rd rs1 rs2
  | FmulS (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fmul.s" rd rs1 rs2
  | FdivS (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fdiv.s" rd rs1 rs2
  | FsqrtS (rd, rs1) ->
    Format.fprintf ppf "fsqrt.s %s,%s" (pp_float_register rd) (pp_float_register rs1)
  | FsgnjS (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fsgnj.s" rd rs1 rs2
  | FsgnjnS (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fsgnjn.s" rd rs1 rs2
  | FsgnjxS (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fsgnjx.s" rd rs1 rs2
  | FminS (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fmin.s" rd rs1 rs2
  | FmaxS (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fmax.s" rd rs1 rs2
  | FcvtWS (rd, rs1) ->
    Format.fprintf ppf "fcvt.w.s %s,%s" (pp_register rd) (pp_float_register rs1)
  | FcvtWuS (rd, rs1) ->
    Format.fprintf ppf "fcvt.wu.s %s,%s" (pp_register rd) (pp_float_register rs1)
  | FmvXW (rd, rs1) ->
    Format.fprintf ppf "fmv.x.w %s,%s" (pp_register rd) (pp_float_register rs1)
  | FeqS (rd, rs1, rs2) -> pp_instruction_2f_1_reg_helper ppf "feq.s" rd rs1 rs2
  | FltS (rd, rs1, rs2) -> pp_instruction_2f_1_reg_helper ppf "flt.s" rd rs1 rs2
  | FleS (rd, rs1, rs2) -> pp_instruction_2f_1_reg_helper ppf "fle.s" rd rs1 rs2
  | FclassS (rd, rs1) ->
    Format.fprintf ppf "fclass.s %s,%s" (pp_register rd) (pp_float_register rs1)
  | FcvtSW (rd, rs1) ->
    Format.fprintf ppf "fcvt.s.w %s,%s" (pp_float_register rd) (pp_register rs1)
  | FcvtSWu (rd, rs1) ->
    Format.fprintf ppf "fcvt.s.wu %s,%s" (pp_float_register rd) (pp_register rs1)
  | FmvWX (rd, rs1) ->
    Format.fprintf ppf "fmv.v.x %s,%s" (pp_float_register rd) (pp_register rs1)
  | FmaddD (rd, rs1, rs2, rs3) ->
    pp_instruction_4f_reg_helper ppf "fmadd.d" rd rs1 rs2 rs3
  | FmsubD (rd, rs1, rs2, rs3) ->
    pp_instruction_4f_reg_helper ppf "fmsub.d" rd rs1 rs2 rs3
  | FnmsubD (rd, rs1, rs2, rs3) ->
    pp_instruction_4f_reg_helper ppf "fnmsub.d" rd rs1 rs2 rs3
  | FnmaddD (rd, rs1, rs2, rs3) ->
    pp_instruction_4f_reg_helper ppf "fmadd.d" rd rs1 rs2 rs3
  | FaddD (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fadd.d" rd rs1 rs2
  | FsubD (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fsub.d" rd rs1 rs2
  | FmulD (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fmul.d" rd rs1 rs2
  | FdivD (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fdiv.d" rd rs1 rs2
  | FsqrtD (rd, rs1) ->
    Format.fprintf ppf "fsqrt.d %s,%s" (pp_float_register rd) (pp_float_register rs1)
  | FsgnjD (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fsgnj.d" rd rs1 rs2
  | FsgnjnD (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fsgnjn.d" rd rs1 rs2
  | FsgnjxD (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fsgnjx.d" rd rs1 rs2
  | FminD (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fmin.d" rd rs1 rs2
  | FmaxD (rd, rs1, rs2) -> pp_instruction_3f_reg_helper ppf "fmax.d" rd rs1 rs2
  | FcvtSD (rd, rs1) ->
    Format.fprintf ppf "fcvt.s.d %s,%s" (pp_float_register rd) (pp_float_register rs1)
  | FcvtDS (rd, rs1) ->
    Format.fprintf ppf "fcvt.d.s %s,%s" (pp_float_register rd) (pp_float_register rs1)
  | FeqD (rd, rs1, rs2) -> pp_instruction_2f_1_reg_helper ppf "feq.d" rd rs1 rs2
  | FltD (rd, rs1, rs2) -> pp_instruction_2f_1_reg_helper ppf "flt.d" rd rs1 rs2
  | FleD (rd, rs1, rs2) -> pp_instruction_2f_1_reg_helper ppf "fle.d" rd rs1 rs2
  | FcvtWD (rd, rs1) ->
    Format.fprintf ppf "fcvt.w.d %s,%s" (pp_register rd) (pp_float_register rs1)
  | FcvtWuD (rd, rs1) ->
    Format.fprintf ppf "fcvt.wu.d %s,%s" (pp_register rd) (pp_float_register rs1)
  | FclassD (rd, rs1) ->
    Format.fprintf ppf "fclass.d %s,%s" (pp_register rd) (pp_float_register rs1)
  | FcvtDW (rd, rs1) ->
    Format.fprintf ppf "fcvt.b.w %s,%s" (pp_float_register rd) (pp_register rs1)
  | FcvtDWu (rd, rs1) ->
    Format.fprintf ppf "fcvt.b.wu %s,%s" (pp_float_register rd) (pp_register rs1)
  | Flw (rd, rs1, imm) ->
    pp_instruction_2freg_1offset_helper ppf "flw" rd rs1 (Address12 imm)
  | Fsw (rd, rs1, imm) ->
    pp_instruction_2freg_1offset_helper ppf "fsw" rd rs1 (Address12 imm)
  | Fld (rd, rs1, imm) ->
    pp_instruction_2freg_1offset_helper ppf "fld" rd rs1 (Address12 imm)
  | Fsd (rd, rs1, imm) ->
    pp_instruction_2freg_1offset_helper ppf "fsd" rd rs1 (Address12 imm)
  | FcvtLS (rd, rs1) ->
    Format.fprintf ppf "fcvt.l.s %s,%s" (pp_register rd) (pp_float_register rs1)
  | FcvtLuS (rd, rs1) ->
    Format.fprintf ppf "fcvt.lu.s %s,%s" (pp_register rd) (pp_float_register rs1)
  | FcvtSL (rd, rs1) ->
    Format.fprintf ppf "fcvt.s.l %s,%s" (pp_float_register rd) (pp_register rs1)
  | FcvtSLu (rd, rs1) ->
    Format.fprintf ppf "fcvt.s.lu %s,%s" (pp_float_register rd) (pp_register rs1)
  | FcvtLD (rd, rs1) ->
    Format.fprintf ppf "fcvt.l.d %s,%s" (pp_register rd) (pp_float_register rs1)
  | FcvtLuD (rd, rs1) ->
    Format.fprintf ppf "fcvt.lu.d %s,%s" (pp_register rd) (pp_float_register rs1)
  | FcvtDL (rd, rs1) ->
    Format.fprintf ppf "fcvt.d.l %s,%s" (pp_float_register rd) (pp_register rs1)
  | FcvtDLu (rd, rs1) ->
    Format.fprintf ppf "fcvt.d.lu %s,%s" (pp_float_register rd) (pp_register rs1)
  | _ -> Format.fprintf ppf "_Unknown instruction_"
;;

let pp_str_or_int = function
  | StrValue str -> Format.sprintf {|%S|} str
  | IntValue imm -> Format.sprintf "%d" imm
;;

let pp_type_dir = function
  | Type str -> Format.sprintf "%s" str
;;

let pp_directive ppf = function
  | File str -> Format.fprintf ppf {|.file %S|} str
  | Option str -> Format.fprintf ppf ".option %s" str
  | Attribute (str, str_or_int) ->
    Format.fprintf ppf ".attribute %s, %s" str (pp_str_or_int str_or_int)
  | Text -> Format.fprintf ppf ".text"
  | Align imm -> Format.fprintf ppf ".align %d" imm
  | Globl imm -> Format.fprintf ppf ".globl %s" (pp_address (Address12 imm))
  | TypeDir (str, str_type) ->
    Format.fprintf ppf ".type %s,@%s" str (pp_type_dir str_type)
  | CfiStartproc -> Format.fprintf ppf ".cfi_startproc"
  | CfiEndproc -> Format.fprintf ppf ".cfi_endproc"
  | Size (imm, str) -> Format.fprintf ppf ".size %s,%s" (pp_address (Address12 imm)) str
  | Section (str1, str2, str_type, none_or_int) ->
    (match none_or_int with
     | Some imm ->
       Format.fprintf ppf {|.section %s,%S,@%s,%d|} str1 str2 (pp_type_dir str_type) imm
     | None -> Format.fprintf ppf {|.section %s,%S,@%s|} str1 str2 (pp_type_dir str_type))
  | String str -> Format.fprintf ppf {|.string %S|} (String.escaped str)
  | CfiDefCfaOffset imm -> Format.fprintf ppf ".cfi_def_cfa_offset %d" imm
  | CfiOffset (imm1, imm2) -> Format.fprintf ppf ".cfi_offset %d,%d" imm1 imm2
  | CfiRememberState -> Format.fprintf ppf ".cfi_remember_state"
  | CfiRestore imm -> Format.fprintf ppf ".cfi_restore %d" imm
  | Ident str -> Format.fprintf ppf {|.ident %S|} str
  | CfiRestoreState -> Format.fprintf ppf ".cfi_restore_state"
;;

let pp_expr ppf = function
  | InstructionExpr instruction -> pp_instruction ppf instruction
  | LabelExpr label -> Format.fprintf ppf "%s:" label
  | DirectiveExpr directive -> pp_directive ppf directive
;;

let pp_ast ppf (ast : ast) =
  List.iter
    (fun expr ->
      pp_expr ppf expr;
      Format.fprintf ppf "\n")
    ast
;;

let print_ast ast = pp_ast Format.std_formatter ast
