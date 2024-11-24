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

let parse_vector_register =
  ws_opt
    (choice
       [ string "v10" *> return V10
       ; string "v11" *> return V11
       ; string "v12" *> return V12
       ; string "v13" *> return V13
       ; string "v14" *> return V14
       ; string "v15" *> return V15
       ; string "v16" *> return V16
       ; string "v17" *> return V17
       ; string "v18" *> return V18
       ; string "v19" *> return V19
       ; string "v20" *> return V20
       ; string "v21" *> return V21
       ; string "v22" *> return V22
       ; string "v23" *> return V23
       ; string "v24" *> return V24
       ; string "v25" *> return V25
       ; string "v26" *> return V26
       ; string "v27" *> return V27
       ; string "v28" *> return V28
       ; string "v29" *> return V29
       ; string "v30" *> return V30
       ; string "v31" *> return V31
       ; string "v0" *> return V0
       ; string "v1" *> return V1
       ; string "v2" *> return V2
       ; string "v3" *> return V3
       ; string "v4" *> return V4
       ; string "v5" *> return V5
       ; string "v6" *> return V6
       ; string "v7" *> return V7
       ; string "v8" *> return V8
       ; string "v9" *> return V9
       ])
;;

let parse_float_register =
  ws_opt
    (choice
       [ string "f10" *> return F10
       ; string "f11" *> return F11
       ; string "f12" *> return F12
       ; string "f13" *> return F13
       ; string "f14" *> return F14
       ; string "f15" *> return F15
       ; string "f16" *> return F16
       ; string "f17" *> return F17
       ; string "f18" *> return F18
       ; string "f19" *> return F19
       ; string "f20" *> return F20
       ; string "f21" *> return F21
       ; string "f22" *> return F22
       ; string "f23" *> return F23
       ; string "f24" *> return F24
       ; string "f25" *> return F25
       ; string "f26" *> return F26
       ; string "f27" *> return F27
       ; string "f28" *> return F28
       ; string "f29" *> return F29
       ; string "f30" *> return F30
       ; string "f31" *> return F31
       ; string "f0" *> return F0
       ; string "f1" *> return F1
       ; string "f2" *> return F2
       ; string "f3" *> return F3
       ; string "f4" *> return F4
       ; string "f5" *> return F5
       ; string "f6" *> return F6
       ; string "f7" *> return F7
       ; string "f8" *> return F8
       ; string "f9" *> return F9
       ; string "ft10" *> return Ft10
       ; string "ft11" *> return Ft11
       ; string "ft0" *> return Ft0
       ; string "ft0" *> return Ft1
       ; string "ft2" *> return Ft2
       ; string "ft3" *> return Ft3
       ; string "ft4" *> return Ft4
       ; string "ft5" *> return Ft5
       ; string "ft6" *> return Ft6
       ; string "ft7" *> return Ft7
       ; string "ft8" *> return Ft8
       ; string "ft9" *> return Ft9
       ; string "fa0" *> return Fa0
       ; string "fa1" *> return Fa1
       ; string "fa2" *> return Fa2
       ; string "fa3" *> return Fa3
       ; string "fa4" *> return Fa4
       ; string "fa5" *> return Fa5
       ; string "fa6" *> return Fa6
       ; string "fa7" *> return Fa7
       ; string "fs10" *> return Fs10
       ; string "fs11" *> return Fs11
       ; string "fs0" *> return Fs0
       ; string "fs1" *> return Fs1
       ; string "fs2" *> return Fs2
       ; string "fs3" *> return Fs3
       ; string "fs4" *> return Fs4
       ; string "fs5" *> return Fs5
       ; string "fs6" *> return Fs6
       ; string "fs7" *> return Fs7
       ; string "fs8" *> return Fs8
       ; string "fs9" *> return Fs9
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

let parse_section_subargs =
  lift2
    (fun section_arg3 i -> section_arg3, i)
    parse_type
    (peek_char
     >>= function
     | Some ',' -> ws_opt (char ',') *> ws_opt (parse_number >>| fun i -> Some i)
     | _ -> return None)
;;

let parse_section_args =
  lift2
    (fun section_arg2 section_subargs -> section_arg2, section_subargs)
    parse_quoted_string
    (ws_opt
       (peek_char
        >>= function
        | Some ',' ->
          ws_opt (char ',') *> ws_opt (parse_section_subargs >>| fun t -> Some t)
        | _ -> return None))
;;

let parse_section =
  parse_string_with_spaces ".section"
  *> ws_opt
       (lift2
          (fun section_arg1 section_args ->
            DirectiveExpr (Section (section_arg1, section_args)))
          parse_string
          (ws_opt
             (peek_char
              >>= function
              | Some ',' ->
                ws_opt (char ',') *> ws_opt (parse_section_args >>| fun s -> Some s)
              | _ -> return None)))
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
       ; parse_section
       ; parse_string_with_spaces ".string"
         *> ws_opt (lift (fun str -> DirectiveExpr (StringDir str)) parse_quoted_string)
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
       ; parse_string_with_spaces ".word"
         *> ws_opt (lift (fun int -> DirectiveExpr (Word int)) parse_number)
       ; parse_string_with_spaces ".space"
         *> ws_opt (lift (fun int -> DirectiveExpr (Space int)) parse_number)
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
       ; parse_string_with_spaces "subi"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Subi (r1, r2, addr12)))
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
       ; parse_string_with_spaces "add.uw"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Adduw (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "sh1add"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Sh1add (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "sh1add.uw"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Sh1adduw (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "sh2add"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Sh2add (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "sh2add.uw"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Sh2adduw (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "sh3add"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Sh3add (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "sh3add.uw"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Sh3adduw (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "vle32.v"
         *> lift3
              (fun vd addr12 rs1 -> InstructionExpr (Vle32v (vd, rs1, addr12)))
              parse_vector_register
              (char ',' *> parse_address12)
              (char '(' *> parse_register <* char ')')
       ; parse_string_with_spaces "vse32.v"
         *> lift3
              (fun vs addr12 rs1 -> InstructionExpr (Vse32v (vs, rs1, addr12)))
              parse_vector_register
              (char ',' *> parse_address12)
              (char '(' *> parse_register <* char ')')
       ; parse_string_with_spaces "vadd.vv"
         *> lift3
              (fun vd vs1 vs2 -> InstructionExpr (Vaddvv (vd, vs1, vs2)))
              parse_vector_register
              (char ',' *> parse_vector_register)
              (char ',' *> parse_vector_register)
       ; parse_string_with_spaces "vadd.vx"
         *> lift3
              (fun vd vs1 rs2 -> InstructionExpr (Vaddvx (vd, vs1, rs2)))
              parse_vector_register
              (char ',' *> parse_vector_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "vsub.vv"
         *> lift3
              (fun vd vs1 vs2 -> InstructionExpr (Vsubvv (vd, vs1, vs2)))
              parse_vector_register
              (char ',' *> parse_vector_register)
              (char ',' *> parse_vector_register)
       ; parse_string_with_spaces "vsub.vx"
         *> lift3
              (fun vd vs1 rs2 -> InstructionExpr (Vsubvx (vd, vs1, rs2)))
              parse_vector_register
              (char ',' *> parse_vector_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "vmul.vv"
         *> lift3
              (fun vd vs1 vs2 -> InstructionExpr (Vmulvv (vd, vs1, vs2)))
              parse_vector_register
              (char ',' *> parse_vector_register)
              (char ',' *> parse_vector_register)
       ; parse_string_with_spaces "vmul.vx"
         *> lift3
              (fun vd vs1 rs2 -> InstructionExpr (Vmulvx (vd, vs1, rs2)))
              parse_vector_register
              (char ',' *> parse_vector_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "vdiv.vv"
         *> lift3
              (fun vd vs1 vs2 -> InstructionExpr (Vdivvv (vd, vs1, vs2)))
              parse_vector_register
              (char ',' *> parse_vector_register)
              (char ',' *> parse_vector_register)
       ; parse_string_with_spaces "vdiv.vx"
         *> lift3
              (fun vd vs1 rs2 -> InstructionExpr (Vdivvx (vd, vs1, rs2)))
              parse_vector_register
              (char ',' *> parse_vector_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "vand.vv"
         *> lift3
              (fun vd vs1 vs2 -> InstructionExpr (Vandvv (vd, vs1, vs2)))
              parse_vector_register
              (char ',' *> parse_vector_register)
              (char ',' *> parse_vector_register)
       ; parse_string_with_spaces "vand.vx"
         *> lift3
              (fun vd vs1 rs2 -> InstructionExpr (Vandvx (vd, vs1, rs2)))
              parse_vector_register
              (char ',' *> parse_vector_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "vor.vv"
         *> lift3
              (fun vd vs1 vs2 -> InstructionExpr (Vorvv (vd, vs1, vs2)))
              parse_vector_register
              (char ',' *> parse_vector_register)
              (char ',' *> parse_vector_register)
       ; parse_string_with_spaces "vor.vx"
         *> lift3
              (fun vd vs1 rs2 -> InstructionExpr (Vorvx (vd, vs1, rs2)))
              parse_vector_register
              (char ',' *> parse_vector_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "vxor.vv"
         *> lift3
              (fun vd vs1 vs2 -> InstructionExpr (Vxorvv (vd, vs1, vs2)))
              parse_vector_register
              (char ',' *> parse_vector_register)
              (char ',' *> parse_vector_register)
       ; parse_string_with_spaces "vxor.vx"
         *> lift3
              (fun vd vs1 rs2 -> InstructionExpr (Vxorvx (vd, vs1, rs2)))
              parse_vector_register
              (char ',' *> parse_vector_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "vmax.vv"
         *> lift3
              (fun vd vs1 vs2 -> InstructionExpr (Vmaxvv (vd, vs1, vs2)))
              parse_vector_register
              (char ',' *> parse_vector_register)
              (char ',' *> parse_vector_register)
       ; parse_string_with_spaces "vmax.vx"
         *> lift3
              (fun vd vs1 rs2 -> InstructionExpr (Vmaxvx (vd, vs1, rs2)))
              parse_vector_register
              (char ',' *> parse_vector_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "vmin.vv"
         *> lift3
              (fun vd vs1 vs2 -> InstructionExpr (Vminvv (vd, vs1, vs2)))
              parse_vector_register
              (char ',' *> parse_vector_register)
              (char ',' *> parse_vector_register)
       ; parse_string_with_spaces "vmin.vx"
         *> lift3
              (fun vd vs1 rs2 -> InstructionExpr (Vminvx (vd, vs1, rs2)))
              parse_vector_register
              (char ',' *> parse_vector_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "vmseq.vv"
         *> lift3
              (fun vd vs1 vs2 -> InstructionExpr (Vmseqvv (vd, vs1, vs2)))
              parse_vector_register
              (char ',' *> parse_vector_register)
              (char ',' *> parse_vector_register)
       ; parse_string_with_spaces "vmseq.vx"
         *> lift3
              (fun vd vs1 rs2 -> InstructionExpr (Vmseqvx (vd, vs1, rs2)))
              parse_vector_register
              (char ',' *> parse_vector_register)
              (char ',' *> parse_register)
       ; parse_string_with_spaces "fmadd.s"
         *> lift4
              (fun r1 r2 r3 r4 -> InstructionExpr (FmaddS (r1, r2, r3, r4)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fmsub.s"
         *> lift4
              (fun r1 r2 r3 r4 -> InstructionExpr (FmaddS (r1, r2, r3, r4)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fnmsub.s"
         *> lift4
              (fun r1 r2 r3 r4 -> InstructionExpr (FnmsubS (r1, r2, r3, r4)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fnmadd.s"
         *> lift4
              (fun r1 r2 r3 r4 -> InstructionExpr (FnmaddS (r1, r2, r3, r4)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fadd.s"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FaddS (r1, r2, r3)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fsub.s"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FsubS (r1, r2, r3)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fmul.s"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FmulS (r1, r2, r3)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fdiv.s"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FdivS (r1, r2, r3)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fsqrt.s"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FsqrtS (r1, r2)))
              parse_float_register
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fsgnj.s"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FsgnjS (r1, r2, r3)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fsgnjn.s"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FsgnjnS (r1, r2, r3)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fsgnjx.s"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FsgnjxS (r1, r2, r3)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fmin.s"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FminS (r1, r2, r3)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fmax.s"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FmaxS (r1, r2, r3)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fcvt.w.s"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FcvtWS (r1, r2)))
              parse_register
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fcvt.wu.s"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FcvtWuS (r1, r2)))
              parse_register
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fmv.x.w"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FmvXW (r1, r2)))
              parse_register
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "feq.s"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FeqS (r1, r2, r3)))
              parse_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "flt.s"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FltS (r1, r2, r3)))
              parse_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fle.s"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FleS (r1, r2, r3)))
              parse_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fclass.s"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FclassS (r1, r2)))
              parse_register
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fcvt.s.w"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FcvtSW (r1, r2)))
              parse_float_register
              (char ',' *> parse_register)
       ; parse_string_with_spaces "fcvt.s.wu"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FcvtSWu (r1, r2)))
              parse_float_register
              (char ',' *> parse_register)
       ; parse_string_with_spaces "fmv.w.x"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FmvWX (r1, r2)))
              parse_float_register
              (char ',' *> parse_register)
       ; parse_string_with_spaces "fmadd.d"
         *> lift4
              (fun r1 r2 r3 r4 -> InstructionExpr (FmaddD (r1, r2, r3, r4)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fmsub.d"
         *> lift4
              (fun r1 r2 r3 r4 -> InstructionExpr (FmsubD (r1, r2, r3, r4)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fnmadd.d"
         *> lift4
              (fun r1 r2 r3 r4 -> InstructionExpr (FnmaddD (r1, r2, r3, r4)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fnmsub.d"
         *> lift4
              (fun r1 r2 r3 r4 -> InstructionExpr (FnmsubD (r1, r2, r3, r4)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fadd.d"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FaddD (r1, r2, r3)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fsub.d"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FsubD (r1, r2, r3)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fmul.d"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FmulD (r1, r2, r3)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fdiv.d"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FdivD (r1, r2, r3)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fsqrt.d"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FsqrtD (r1, r2)))
              parse_float_register
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fsgnj.d"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FsgnjD (r1, r2, r3)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fsgnjn.d"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FsgnjnD (r1, r2, r3)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fsgnjx.d"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FsgnjxD (r1, r2, r3)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fmin.d"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FminD (r1, r2, r3)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fmax.d"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FmaxD (r1, r2, r3)))
              parse_float_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fcvt.s.d"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FcvtSD (r1, r2)))
              parse_float_register
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fcvt.d.s"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FcvtDS (r1, r2)))
              parse_float_register
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "feq.d"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FeqD (r1, r2, r3)))
              parse_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "flt.d"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FltD (r1, r2, r3)))
              parse_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fle.d"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (FleD (r1, r2, r3)))
              parse_register
              (char ',' *> parse_float_register)
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fcvt.w.d"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FcvtWD (r1, r2)))
              parse_register
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fcvt.wu.d"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FcvtWuD (r1, r2)))
              parse_register
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fclass.d"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FclassD (r1, r2)))
              parse_register
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fcvt.d.w"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FcvtDW (r1, r2)))
              parse_float_register
              (char ',' *> parse_register)
       ; parse_string_with_spaces "fcvt.d.wu"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FcvtDWu (r1, r2)))
              parse_float_register
              (char ',' *> parse_register)
       ; parse_string_with_spaces "flw"
         *> lift3
              (fun r1 addr12 r2 -> InstructionExpr (Flw (r1, r2, addr12)))
              parse_float_register
              (char ',' *> parse_address12)
              (char '(' *> parse_float_register <* char ')')
       ; parse_string_with_spaces "fsw"
         *> lift3
              (fun r1 r2 addr12 -> InstructionExpr (Fsw (r1, addr12, r2)))
              parse_float_register
              (char ',' *> parse_address12)
              (char '(' *> parse_float_register <* char ')')
       ; parse_string_with_spaces "fld"
         *> lift3
              (fun r1 addr12 r2 -> InstructionExpr (Fld (r1, r2, addr12)))
              parse_float_register
              (char ',' *> parse_address12)
              (char '(' *> parse_float_register <* char ')')
       ; parse_string_with_spaces "fsd"
         *> lift3
              (fun r1 addr12 r2 -> InstructionExpr (Fsd (r1, r2, addr12)))
              parse_float_register
              (char ',' *> parse_address12)
              (char '(' *> parse_float_register <* char ')')
       ; parse_string_with_spaces "fcvt.l.s"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FcvtLS (r1, r2)))
              parse_register
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fcvt.lu.s"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FcvtLuS (r1, r2)))
              parse_register
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fcvt.s.l"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FcvtSL (r1, r2)))
              parse_float_register
              (char ',' *> parse_register)
       ; parse_string_with_spaces "fcvt.s.lu"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FcvtSLu (r1, r2)))
              parse_float_register
              (char ',' *> parse_register)
       ; parse_string_with_spaces "fcvt.l.d"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FcvtLD (r1, r2)))
              parse_register
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fcvt.lu.d"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FcvtLuD (r1, r2)))
              parse_register
              (char ',' *> parse_float_register)
       ; parse_string_with_spaces "fcvt.d.l"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FcvtDL (r1, r2)))
              parse_float_register
              (char ',' *> parse_register)
       ; parse_string_with_spaces "fcvt.d.lu"
         *> lift2
              (fun r1 r2 -> InstructionExpr (FcvtDLu (r1, r2)))
              parse_float_register
              (char ',' *> parse_register)
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
