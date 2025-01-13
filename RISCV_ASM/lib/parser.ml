(** Copyright 2024, Vyacheslav Kochergin, Roman Mukovenkov, Yuliana Ementyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

let parse_number =
  let sign = option "" (string "-") in
  let digits =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  in
  lift2 ( ^ ) sign digits >>= fun num_str -> return (int_of_string num_str)
;;

let ws =
  take_while (function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false)
;;

let parse_string =
  take_while1 (function
    | ' ' | '\t' | '\n' | '\r' | ',' -> false
    | _ -> true)
;;

let parse_escaped_char = char '\\' *> (char 'n' *> return '\n')

let parse_regular_char =
  satisfy (function
    | '"' | '\\' -> false
    | _ -> true)
;;

let char_list_to_string char_list =
  let len = List.length char_list in
  String.init len (List.nth char_list)
;;

let parse_quoted_string =
  char '"' *> many (parse_escaped_char <|> parse_regular_char)
  <* char '"'
  >>| char_list_to_string
;;

let parse_type = char '@' *> parse_string >>= fun str -> return (Type str)

let parse_number_or_quoted_string =
  peek_char
  >>= function
  | Some '"' -> parse_quoted_string >>= fun str -> return (StrValue str)
  | Some '0' .. '9' | Some '-' -> parse_number >>= fun num -> return (IntValue num)
  | _ -> fail "Expected number or quoted string"
;;

let parse_label_string =
  take_while1 (function
    | ' ' | '\t' | '\n' | '\r' | ',' | ':' | '(' -> false
    | _ -> true)
;;

let ws_opt p = ws *> p <* ws

let parse_register =
  ws_opt
    (choice
       [ string "x10" *> return X10
       ; string "x11" *> return X11
       ; string "x12" *> return X12
       ; string "x13" *> return X13
       ; string "x14" *> return X14
       ; string "x15" *> return X15
       ; string "x16" *> return X16
       ; string "x17" *> return X17
       ; string "x18" *> return X18
       ; string "x19" *> return X19
       ; string "x20" *> return X20
       ; string "x21" *> return X21
       ; string "x22" *> return X22
       ; string "x23" *> return X23
       ; string "x24" *> return X24
       ; string "x25" *> return X25
       ; string "x26" *> return X26
       ; string "x27" *> return X27
       ; string "x28" *> return X28
       ; string "x29" *> return X29
       ; string "x30" *> return X30
       ; string "x31" *> return X31
       ; string "x0" *> return X0
       ; string "x1" *> return X1
       ; string "x2" *> return X2
       ; string "x3" *> return X3
       ; string "x4" *> return X4
       ; string "x5" *> return X5
       ; string "x6" *> return X6
       ; string "x7" *> return X7
       ; string "x8" *> return X8
       ; string "x9" *> return X9
       ; string "zero" *> return Zero
       ; string "ra" *> return Ra
       ; string "sp" *> return Sp
       ; string "gp" *> return Gp
       ; string "tp" *> return Tp
       ; string "t0" *> return T0
       ; string "t1" *> return T1
       ; string "t2" *> return T2
       ; string "s0" *> return S0
       ; string "fp" *> return Fp
       ; string "s10" *> return S10
       ; string "s11" *> return S11
       ; string "s1" *> return S1
       ; string "a0" *> return A0
       ; string "a1" *> return A1
       ; string "a2" *> return A2
       ; string "a3" *> return A3
       ; string "a4" *> return A4
       ; string "a5" *> return A5
       ; string "a6" *> return A6
       ; string "a7" *> return A7
       ; string "s2" *> return S2
       ; string "s3" *> return S3
       ; string "s4" *> return S4
       ; string "s5" *> return S5
       ; string "s6" *> return S6
       ; string "s7" *> return S7
       ; string "s8" *> return S8
       ; string "s9" *> return S9
       ; string "t3" *> return T3
       ; string "t4" *> return T4
       ; string "t5" *> return T5
       ; string "t6" *> return T6
       ])
;;

let parse_immediate12 = ws_opt (lift (fun imm -> ImmediateAddress12 imm) parse_number)
let parse_immediate20 = ws_opt (lift (fun imm -> ImmediateAddress20 imm) parse_number)
let parse_immediate32 = ws_opt (lift (fun imm -> ImmediateAddress32 imm) parse_number)

let parse_label_address12 =
  ws_opt (lift (fun str -> LabelAddress12 str) parse_label_string)
;;

let parse_label_address20 =
  ws_opt (lift (fun str -> LabelAddress20 str) parse_label_string)
;;

let parse_label_address32 =
  ws_opt (lift (fun str -> LabelAddress32 str) parse_label_string)
;;

let parse_label_expr =
  ws_opt (lift (fun str -> LabelExpr str) (parse_label_string <* ws_opt (char ':')))
;;

let parse_address12 = ws_opt (choice [ parse_immediate12; parse_label_address12 ])
let parse_address20 = ws_opt (choice [ parse_immediate20; parse_label_address20 ])
let parse_address32 = ws_opt (choice [ parse_immediate32; parse_label_address32 ])

let parse_string_with_spaces str =
  string str
  *> (peek_char
      >>= function
      | Some (' ' | '\n' | '\t') -> return ()
      | None -> return ()
      | _ -> fail "")
;;

let parse_directive =
  ws_opt
    (choice
       [ parse_string_with_spaces ".file"
         *> ws_opt (lift (fun str -> DirectiveExpr (File str)) parse_quoted_string)
       ; parse_string_with_spaces ".option"
         *> ws_opt (lift (fun str -> DirectiveExpr (Option str)) parse_string)
       ; parse_string_with_spaces ".attribute"
         *> ws_opt
              (lift2
                 (fun tag value -> DirectiveExpr (Attribute (tag, value)))
                 parse_string
                 (ws_opt (char ',') *> parse_number_or_quoted_string))
       ; parse_string_with_spaces ".text" *> return (DirectiveExpr Text)
       ; parse_string_with_spaces ".align"
         *> ws_opt (lift (fun int -> DirectiveExpr (Align int)) parse_number)
       ; parse_string_with_spaces ".globl"
         *> ws_opt (lift (fun label -> DirectiveExpr (Globl label)) parse_address12)
       ; parse_string_with_spaces ".type"
         *> ws_opt
              (lift2
                 (fun str type_str -> DirectiveExpr (TypeDir (str, type_str)))
                 parse_string
                 (ws_opt (char ',') *> parse_type))
       ; parse_string_with_spaces ".cfi_startproc" *> return (DirectiveExpr CfiStartproc)
       ; parse_string_with_spaces ".cfi_endproc" *> return (DirectiveExpr CfiEndproc)
       ; parse_string_with_spaces ".cfi_remember_state"
         *> return (DirectiveExpr CfiRememberState)
       ; parse_string_with_spaces ".cfi_restore_state"
         *> return (DirectiveExpr CfiRestoreState)
       ; parse_string_with_spaces ".size"
         *> ws_opt
              (lift2
                 (fun label size -> DirectiveExpr (Size (label, size)))
                 (* FIXME: Size can actually be an expression with +, -, * and operands, . (dot) is current address*)
                 parse_address12
                 (ws_opt (char ',') *> parse_string))
       ; parse_string_with_spaces ".section"
         *> ws_opt
              (lift4
                 (fun section_name section_type section_flags section_index ->
                   DirectiveExpr
                     (Section (section_name, section_type, section_flags, section_index)))
                 parse_string
                 (ws_opt (char ',') *> ws_opt parse_quoted_string)
                 (ws_opt (char ',') *> ws_opt parse_type)
                 (peek_char
                  >>= function
                  | Some ',' ->
                    ws_opt (char ',') *> ws_opt (lift (fun i -> Some i) parse_number)
                  | _ -> return None))
       ; parse_string_with_spaces ".string"
         *> ws_opt (lift (fun str -> DirectiveExpr (String str)) parse_quoted_string)
       ; parse_string_with_spaces ".cfi_def_cfa_offset"
         *> ws_opt (lift (fun int -> DirectiveExpr (CfiDefCfaOffset int)) parse_number)
       ; parse_string_with_spaces ".cfi_offset"
         *> ws_opt
              (lift2
                 (fun int1 int2 -> DirectiveExpr (CfiOffset (int1, int2)))
                 parse_number
                 (ws_opt (char ',') *> ws_opt parse_number))
       ; parse_string_with_spaces ".cfi_restore"
         *> ws_opt (lift (fun int -> DirectiveExpr (CfiRestore int)) parse_number)
       ; parse_string_with_spaces ".ident"
         *> ws_opt (lift (fun str -> DirectiveExpr (Ident str)) parse_quoted_string)
       ])
;;

let parse_instruction =
  ws_opt
    (choice
       [ parse_string_with_spaces "add"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Add (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "sub"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Sub (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "xor"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Xor (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "or"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Or (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "and"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (And (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "sll"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Sll (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "srl"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Srl (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "sra"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Sra (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "slt"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Slt (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "sltu"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Sltu (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "addi"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Addi (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; parse_string_with_spaces "xori"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Xori (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; parse_string_with_spaces "ori"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Ori (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; parse_string_with_spaces "andi"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Andi (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; parse_string_with_spaces "slli"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Slli (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; parse_string_with_spaces "srli"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Srli (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; parse_string_with_spaces "srai"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Srai (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; parse_string_with_spaces "slti"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Slti (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; parse_string_with_spaces "sltiu"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Sltiu (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; parse_string_with_spaces "lb"
         *> lift3
              (fun r1 addr12 r2 -> InstructionExpr (Lb (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_address12)
              (char '(' *> parse_register <* char ')')
       ; parse_string_with_spaces "lh"
         *> lift3
              (fun r1 addr12 r2 -> InstructionExpr (Lh (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_address12)
              (char '(' *> parse_register <* char ')')
       ; parse_string_with_spaces "lw"
         *> lift3
              (fun r1 addr12 r2 -> InstructionExpr (Lw (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_address12)
              (char '(' *> parse_register <* char ')')
       ; parse_string_with_spaces "lbu"
         *> lift3
              (fun r1 addr12 r2 -> InstructionExpr (Lbu (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_address12)
              (char '(' *> parse_register <* char ')')
       ; parse_string_with_spaces "lhu"
         *> lift3
              (fun r1 addr12 r2 -> InstructionExpr (Lhu (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_address12)
              (char '(' *> parse_register <* char ')')
       ; parse_string_with_spaces "sb"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Sb (r1, addr12, r2)))
              parse_register
              (char ',' *> parse_address12)
              (char '(' *> parse_register <* char ')')
       ; parse_string_with_spaces "sh"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Sh (r1, addr12, r2)))
              parse_register
              (char ',' *> parse_address12)
              (char '(' *> parse_register <* char ')')
       ; parse_string_with_spaces "sw"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Sw (r1, addr12, r2)))
              parse_register
              (char ',' *> parse_address12)
              (char '(' *> parse_register <* char ')')
       ; parse_string_with_spaces "beq"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Beq (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; parse_string_with_spaces "beqz"
         *> ws_opt
              (lift2
                (fun r1 addr12 -> InstructionExpr (Beqz (r1, addr12)))
                parse_register
                (char ',' *> parse_address12))
       ; parse_string_with_spaces "bne"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Bne (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; parse_string_with_spaces "bnez"
         *> ws_opt
              (lift2
                (fun r1 addr12 -> InstructionExpr (Bnez (r1, addr12)))
                parse_register
                (char ',' *> parse_address12))
       ; parse_string_with_spaces "blt"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Blt (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; parse_string_with_spaces "bltz"
         *> ws_opt
              (lift2
                (fun r1 addr12 -> InstructionExpr (Bltz (r1, addr12)))
                parse_register
                (char ',' *> parse_address12))
       ; parse_string_with_spaces "bgt"
         *> lift3
            (fun r1 r2 addr12 -> InstructionExpr (Bgt (r1, r2, addr12)))
            parse_register
            (char ',' *> parse_register)
            (char ',' *> parse_address12)
       ; parse_string_with_spaces "bgtz"
         *> ws_opt
              (lift2
                (fun r1 addr12 -> InstructionExpr (Bgtz (r1, addr12)))
                parse_register
                (char ',' *> parse_address12))
       ; parse_string_with_spaces "bge"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Bge (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; parse_string_with_spaces "bltu"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Bltu (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; parse_string_with_spaces "bgeu"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Bgeu (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; parse_string_with_spaces "jal"
         *> lift2
              (fun r1 addr20 -> InstructionExpr (Jal (r1, addr20)))
              parse_register
              (char ',' *> parse_address20)
       ; parse_string_with_spaces "jalr"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Jalr (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; parse_string_with_spaces "jr"
         *> lift (fun r1 -> InstructionExpr (Jr r1)) parse_register
       ; parse_string_with_spaces "j"
         *> lift (fun addr20 -> InstructionExpr (J addr20)) parse_address20
       ; parse_string_with_spaces "lui"
         *> lift2
              (fun r1 addr20 -> InstructionExpr (Lui (r1, addr20)))
              parse_register
              (char ',' *> parse_address20)
       ; parse_string_with_spaces "li"
         *> lift2
              (fun r1 addr32 -> InstructionExpr (Li (r1, addr32)))
              parse_register
              (char ',' *> parse_address32)
       ; parse_string_with_spaces "auipc"
         *> lift2
              (fun r1 addr20 -> InstructionExpr (Auipc (r1, addr20)))
              parse_register
              (char ',' *> parse_address20)
       ; parse_string_with_spaces "ecall" *> return (InstructionExpr Ecall)
       ; parse_string_with_spaces "call"
         *> ws_opt (lift (fun str -> InstructionExpr (Call str)) parse_string)
       ; parse_string_with_spaces "mul"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Mul (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "mulh"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Mulh (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "mulhsu"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Mulhsu (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "mulhu"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Mulhu (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "div"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Div (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "divu"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Divu (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "rem"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Rem (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "remu"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Remu (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "lwu"
         *> lift3
              (fun r1 addr12 r2 -> InstructionExpr (Lwu (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_address12)
              (char '(' *> parse_register <* char ')')
       ; parse_string_with_spaces "ld"
         *> lift3
              (fun r1 addr12 r2 -> InstructionExpr (Ld (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_address12)
              (char '(' *> parse_register <* char ')')
       ; parse_string_with_spaces "la"
         *> ws_opt
              (lift2
                 (fun r1 addr32 -> InstructionExpr (La (r1, addr32)))
                 parse_register
                 (char ',' *> parse_address32))
       ; parse_string_with_spaces "lla"
         *> ws_opt
              (lift2
                 (fun r1 addr32 -> InstructionExpr (Lla (r1, addr32)))
                 parse_register
                 (char ',' *> parse_address32))
       ; parse_string_with_spaces "sd"
         *> lift3
              (fun r1 addr12 r2 -> InstructionExpr (Sd (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_address12)
              (char '(' *> parse_register <* char ')')
       ; parse_string_with_spaces "addiw"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Addiw (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; parse_string_with_spaces "slliw"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Slliw (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; parse_string_with_spaces "srliw"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Srliw (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; parse_string_with_spaces "sraiw"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Sraiw (r1, r2, addr12)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; parse_string_with_spaces "addw"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Addw (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "subw"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Subw (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "sllw"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Sllw (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "srlw"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Srlw (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "sraw"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Sraw (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "mulw"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Mulw (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "divw"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Divw (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "divuw"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Divuw (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "remw"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Remw (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "remwu"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Remwu (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "mv"
         *> lift2
              (fun r1 r2 -> InstructionExpr (Mv (r1, r2)))
              parse_register
              (char ',' *> parse_register)
       ; parse_string_with_spaces "ret" *> return (InstructionExpr Ret)
       ])
;;

let parse_expr = ws_opt (choice [ parse_directive; parse_instruction; parse_label_expr ])

let parse_ast =
  ws_opt (many parse_expr)
  <* (end_of_input
      <|> (Angstrom.pos
           >>= fun pos ->
           Angstrom.peek_char
           >>= function
           | None -> return ()
           | Some c ->
             fail (Printf.sprintf "Unexpected character: '%c' at position %d" c pos)))
;;
