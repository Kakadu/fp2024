(** Copyright 2024, Vyacheslav Kochergin, Roman Mukovenkov, Yuliana Ementyan *)

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
  | Subi (rd, rs1, imm) ->
    pp_instruction_2reg_1imm_helper ppf "subi" rd rs1 (Address12 imm)
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
  | Adduw (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "add.uw" rd rs1 rs2
  | Sh1add (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "sh1add" rd rs1 rs2
  | Sh1adduw (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "sh1add.uw" rd rs1 rs2
  | Sh2add (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "sh2add" rd rs1 rs2
  | Sh2adduw (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "sh2add.uw" rd rs1 rs2
  | Sh3add (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "sh3add" rd rs1 rs2
  | Sh3adduw (rd, rs1, rs2) -> pp_instruction_3reg_helper ppf "sh3add.uw" rd rs1 rs2
  | Vle32v (vd, rs1, imm) -> pp_instruction_1vreg_1reg_1offset_helper ppf "vle32.vv" vd rs1 (Address12 imm)
  | Vse32v (vs, rs1, imm) -> pp_instruction_1vreg_1reg_1offset_helper ppf "vse32.vv" vs rs1 (Address12 imm)
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
;;

let pp_str_or_int ppf = function
  | StrValue str -> Format.fprintf ppf "%S" str
  | IntValue imm -> Format.fprintf ppf "%d" imm
;;

let pp_type_dir ppf = function
  | Type str -> Format.fprintf ppf "%s" str
;;

let pp_directive ppf = function
  | File str -> Format.fprintf ppf ".file %S" str
  | Option str -> Format.fprintf ppf ".option %s" str
  | Attribute (str, str_or_int) ->
    Format.fprintf ppf ".attribute %s, %a" str pp_str_or_int str_or_int
  | Text -> Format.fprintf ppf ".text"
  | Align imm -> Format.fprintf ppf ".align %d" imm
  | Globl imm -> Format.fprintf ppf ".globl %a" pp_address (Address12 imm)
  | TypeDir (str, str_type) -> Format.fprintf ppf ".type %s,@%a" str pp_type_dir str_type
  | CfiStartproc -> Format.fprintf ppf ".cfi_startproc"
  | CfiEndproc -> Format.fprintf ppf ".cfi_endproc"
  | Size (imm, str) -> Format.fprintf ppf ".size %a,%s" pp_address (Address12 imm) str
  | Section (str1, None) -> Format.fprintf ppf ".section %s" str1
  | Section (str1, Some (str2, None)) -> Format.fprintf ppf ".section %s,%S" str1 str2
  | Section (str1, Some (str2, Some (typ, None))) ->
    Format.fprintf ppf ".section %s,%S,@%a" str1 str2 pp_type_dir typ
  | Section (str1, Some (str2, Some (typ, Some i))) ->
    Format.fprintf ppf ".section %s,%S,@%a,%d" str1 str2 pp_type_dir typ i
  | StringDir str -> Format.fprintf ppf ".string %S" (String.escaped str)
  | CfiDefCfaOffset imm -> Format.fprintf ppf ".cfi_def_cfa_offset %d" imm
  | CfiOffset (imm1, imm2) -> Format.fprintf ppf ".cfi_offset %d,%d" imm1 imm2
  | CfiRememberState -> Format.fprintf ppf ".cfi_remember_state"
  | CfiRestore imm -> Format.fprintf ppf ".cfi_restore %d" imm
  | Ident str -> Format.fprintf ppf ".ident %S" str
  | CfiRestoreState -> Format.fprintf ppf ".cfi_restore_state"
  | Word imm -> Format.fprintf ppf ".word %d" imm
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
