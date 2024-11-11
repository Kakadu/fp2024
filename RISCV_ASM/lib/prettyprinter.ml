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
  | _ -> Format.fprintf ppf "_Unknown instruction_"
;;

let pp_str_or_int = function
  | StrValue str -> Format.sprintf "\"%s\"" str
  | IntValue imm -> Format.sprintf "%d" imm
;;

let pp_type_dir = function
  | Type str -> Format.sprintf "%s" str
;;

let pp_directive ppf = function
  | File str -> Format.fprintf ppf {|.file "%s"|} str
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
       Format.fprintf ppf {|.section %s,"%s",@%s,%d|} str1 str2 (pp_type_dir str_type) imm
     | None ->
       Format.fprintf ppf {|.section %s,"%s",@%s|} str1 str2 (pp_type_dir str_type))
  | String str -> Format.fprintf ppf {|.string "%s"|} (String.escaped str)
  | CfiDefCfaOffset imm -> Format.fprintf ppf ".cfi_def_cfa_offset %d" imm
  | CfiOffset (imm1, imm2) -> Format.fprintf ppf ".cfi_offset %d,%d" imm1 imm2
  | CfiRememberState -> Format.fprintf ppf ".cfi_remember_state"
  | CfiRestore imm -> Format.fprintf ppf ".cfi_restore %d" imm
  | Ident str -> Format.fprintf ppf {|.ident "%s"|} str
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
