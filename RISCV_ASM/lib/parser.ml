(** Copyright 2024, Vyacheslav Kochergin and Roman Mukovenkov*)

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
    | ' ' | '\t' | '\n' | '\r' | ',' | ':' -> false
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
       ; string "zero" *> return X0
       ; string "ra" *> return X1
       ; string "sp" *> return X2
       ; string "gp" *> return X3
       ; string "tp" *> return X4
       ; string "t0" *> return X5
       ; string "t1" *> return X6
       ; string "t2" *> return X7
       ; string "s0" *> return X8
       ; string "fp" *> return X8
       ; string "s10" *> return X26
       ; string "s11" *> return X27
       ; string "s1" *> return X9
       ; string "a0" *> return X10
       ; string "a1" *> return X11
       ; string "a2" *> return X12
       ; string "a3" *> return X13
       ; string "a4" *> return X14
       ; string "a5" *> return X15
       ; string "a6" *> return X16
       ; string "a7" *> return X17
       ; string "s2" *> return X18
       ; string "s3" *> return X19
       ; string "s4" *> return X20
       ; string "s5" *> return X21
       ; string "s6" *> return X22
       ; string "s7" *> return X23
       ; string "s8" *> return X24
       ; string "s9" *> return X25
       ; string "t3" *> return X28
       ; string "t4" *> return X29
       ; string "t5" *> return X30
       ; string "t6" *> return X31
       ])
;;

let parse_immediate12 = ws_opt (lift (fun imm -> Immediate12 imm) parse_number)
let parse_immediate20 = ws_opt (lift (fun imm -> Immediate20 imm) parse_number)
let parse_immediate32 = ws_opt (lift (fun imm -> Immediate32 imm) parse_number)

let parse_immediate_address32 =
  ws_opt (lift (fun imm -> ImmediateAddress32 imm) parse_immediate32)
;;

let parse_label_address32 = ws_opt (lift (fun str -> LabelAddress32 str) parse_string)

let parse_immediate_address12 =
  ws_opt (lift (fun imm -> ImmediateAddress12 imm) parse_immediate12)
;;

let parse_label_address12 = ws_opt (lift (fun str -> LabelAddress12 str) parse_string)

let parse_immediate_address20 =
  ws_opt (lift (fun imm -> ImmediateAddress20 imm) parse_immediate20)
;;

let parse_label_address20 = ws_opt (lift (fun str -> LabelAddress20 str) parse_string)

let parse_label_expr =
  ws_opt (lift (fun str -> LabelExpr str) (parse_string <* ws_opt (char ':')))
;;

let parse_address12 = ws_opt (choice [ parse_immediate_address12; parse_label_address12 ])
let parse_address20 = ws_opt (choice [ parse_immediate_address20; parse_label_address20 ])
let parse_address32 = ws_opt (choice [ parse_immediate_address32; parse_label_address32 ])

let parse_directive =
  ws_opt
    (choice
       [ string ".file"
         *> ws_opt (lift (fun str -> DirectiveExpr (File str)) parse_quoted_string)
       ; string ".option"
         *> ws_opt (lift (fun str -> DirectiveExpr (Option str)) parse_string)
       ; string ".attribute"
         *> ws_opt
              (lift2
                 (fun tag value -> DirectiveExpr (Attribute (tag, value)))
                 parse_string
                 (ws_opt (char ',') *> parse_number_or_quoted_string))
       ; string ".text" *> return (DirectiveExpr Text)
       ; string ".align"
         *> ws_opt (lift (fun int -> DirectiveExpr (Align int)) parse_number)
       ; string ".globl"
         *> ws_opt (lift (fun label -> DirectiveExpr (Globl label)) parse_label_address12)
       ; string ".type"
         *> ws_opt
              (lift2
                 (fun str type_str -> DirectiveExpr (TypeDir (str, type_str)))
                 parse_string
                 (ws_opt (char ',') *> parse_type))
       ; string ".cfi_startproc" *> return (DirectiveExpr CfiStartproc)
       ; string ".cfi_endproc" *> return (DirectiveExpr CfiEndproc)
       ; string ".cfi_remember_state" *> return (DirectiveExpr CfiRememberState)
       ; string ".cfi_restore_state" *> return (DirectiveExpr CfiRestoreState)
       ; string ".size"
         *> ws_opt
              (lift2
                 (fun label size -> DirectiveExpr (Size (label, size)))
                 (* FIXME: Size can actually be an expression with +, -, * and operands, . (dot) is current address*)
                 parse_label_address12
                 (ws_opt (char ',') *> parse_string))
       ; string ".section"
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
       ; string ".string"
         *> ws_opt (lift (fun str -> DirectiveExpr (String str)) parse_quoted_string)
       ; string ".cfi_def_cfa_offset"
         *> ws_opt (lift (fun int -> DirectiveExpr (CfiDefCfaOffset int)) parse_number)
       ; string ".cfi_offset"
         *> ws_opt
              (lift2
                 (fun int1 int2 -> DirectiveExpr (CfiOffset (int1, int2)))
                 parse_number
                 (ws_opt (char ',') *> ws_opt parse_number))
       ; string ".cfi_restore"
         *> ws_opt (lift (fun int -> DirectiveExpr (CfiRestore int)) parse_number)
       ; string ".ident"
         *> ws_opt (lift (fun str -> DirectiveExpr (Ident str)) parse_quoted_string)
       ])
;;

let parse_instruction =
  ws_opt
    (choice
       [ string "add"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Add (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; string "li"
         *> lift2
              (fun r1 imm32 -> InstructionExpr (Li (r1, imm32)))
              parse_register
              (char ',' *> parse_immediate32)
       ; string "mv"
         *> lift2
              (fun r1 r2 -> InstructionExpr (Mv (r1, r2)))
              parse_register
              (char ',' *> parse_register)
       ; string "beq"
         *> lift3
              (fun r1 r2 adr -> InstructionExpr (Beq (r1, r2, adr)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; string "addiw"
         *> lift3
              (fun r1 r2 imm -> InstructionExpr (Addiw (r1, r2, imm)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_immediate12)
       ; string "mulw"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Mulw (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; string "bne"
         *> lift3
              (fun r1 r2 adr -> InstructionExpr (Bne (r1, r2, adr)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_address12)
       ; string "ld"
         *> lift3
              (fun r1 imm r2 -> InstructionExpr (Ld (r1, r2, imm)))
              parse_register
              (char ',' *> parse_immediate_address12)
              (char '(' *> parse_register <* char ')')
       ; string "sd"
         *> lift3
              (fun r1 imm r2 -> InstructionExpr (Sd (r1, r2, imm)))
              parse_register
              (char ',' *> parse_immediate_address12)
              (char '(' *> parse_register <* char ')')
       ; string "lw"
         *> lift3
              (fun r1 imm r2 -> InstructionExpr (Lw (r1, r2, imm)))
              parse_register
              (char ',' *> parse_immediate12)
              (char '(' *> parse_register <* char ')')
       ; string "addi"
         *> lift3
              (fun r1 r2 imm -> InstructionExpr (Addi (r1, r2, imm)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_immediate12)
       ; string "xor"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Xor (r1, r2, r3)))
              parse_register
              (char ',' *> parse_register)
              (char ',' *> parse_register)
       ; string "ret" *> return (InstructionExpr Ret)
       ; string "jr" *> lift (fun r1 -> InstructionExpr (Jr r1)) parse_register
       ; string "j" *> lift (fun adr -> InstructionExpr (J adr)) parse_address20
       ; string "call"
         *> ws_opt (lift (fun str -> InstructionExpr (Call str)) parse_string)
       ; string "la"
         *> ws_opt
              (lift2
                 (fun r1 adr -> InstructionExpr (La (r1, adr)))
                 parse_register
                 (char ',' *> parse_address32))
       ; string "lla"
         *> ws_opt
              (lift2
                 (fun r1 adr -> InstructionExpr (Lla (r1, adr)))
                 parse_register
                 (char ',' *> parse_address32))
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
