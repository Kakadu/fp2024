(** Copyright 2024, Vyacheslav Kochergin and Roman Mukovenkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

let is_not_colon = function
  | ':' -> false
  | _ -> true
;;

let is_digit = function
  | '1' .. '9' -> true
  | _ -> false
;;

let ws =
  take_while (function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false)
;;

let ws_opt p = ws *> p <* ws

let parse_register =
  ws_opt
    (choice
       [ string "x0" *> return X0
       ; string "x1" *> return X1
       ; string "x2" *> return X2
       ; string "x3" *> return X3
       ; string "x4" *> return X4
       ; string "x5" *> return X5
       ; string "x6" *> return X6
       ; string "x7" *> return X7
       ; string "x8" *> return X8
       ; string "x9" *> return X9
       ; string "x10" *> return X10
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
       ; string "zero" *> return X0
       ; string "ra" *> return X1
       ; string "sp" *> return X2
       ; string "gp" *> return X3
       ; string "tp" *> return X4
       ; string "t0" *> return X5
       ; string "t1" *> return X6
       ; string "s0" *> return X7
       ; string "fp" *> return X7
       ; string "s1" *> return X8
       ; string "a0" *> return X9
       ; string "a1" *> return X10
       ; string "a2" *> return X11
       ; string "a3" *> return X12
       ; string "a4" *> return X13
       ; string "a5" *> return X14
       ; string "a6" *> return X15
       ; string "a7" *> return X16
       ; string "s2" *> return X17
       ; string "s3" *> return X18
       ; string "s4" *> return X19
       ; string "s5" *> return X20
       ; string "s6" *> return X21
       ; string "s7" *> return X22
       ; string "s8" *> return X23
       ; string "s9" *> return X24
       ; string "s10" *> return X25
       ; string "s11" *> return X26
       ; string "t3" *> return X27
       ; string "t4" *> return X28
       ; string "t5" *> return X29
       ; string "t6" *> return X30
       ; string "t7" *> return X31
       ])
;;

let parse_immediate12 =
  ws_opt (take_while1 is_digit >>= fun str -> return (Immediate12 (int_of_string str)))
;;

let parse_immediate_address12 =
  ws_opt (lift (fun imm -> ImmediateAddress12 imm) parse_immediate12)
;;

let parse_label_address12 =
    ws_opt (take_while1 is_not_colon <* char ':' >>= fun str -> return (LabelAddress12 str))
  ;;
  
  let parse_label_expr =
    ws_opt (take_while1 is_not_colon <* char ':' >>= fun str -> return (LabelExpr str))
;;

let parse_address12 = ws_opt (choice [ parse_immediate_address12; parse_label_address12 ])

let parse_instruction =
  ws_opt
    (choice
       [ string "add"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Add (r1, r2, r3)))
              parse_register
              (string "," *> parse_register)
              (string "," *> parse_register)
       ; string "mv"
         *> lift2
              (fun r1 r2 -> InstructionExpr (Mv (r1, r2)))
              parse_register
              (string "," *> parse_register)
       ; string "beq"
         *> lift3
              (fun r1 r2 adr -> InstructionExpr (Beq (r1, r2, adr)))
              parse_register
              (string "," *> parse_register)
              parse_address12
       ; string "addiw"
         *> lift3
              (fun r1 r2 imm -> InstructionExpr (Addiw (r1, r2, imm)))
              parse_register
              (string "," *> parse_register)
              (string "," *> parse_immediate12)
       ; string "mulw"
         *> lift3
              (fun r1 r2 r3 -> InstructionExpr (Mulw (r1, r2, r3)))
              parse_register
              (string "," *> parse_register)
              (string "," *> parse_register)
       ; string "bne"
         *> lift3
              (fun r1 r2 adr -> InstructionExpr (Bne (r1, r2, adr)))
              parse_register
              (string "," *> parse_register)
              (string "," *> parse_address12)
       ; string "ret" *> return (InstructionExpr Ret)
       ])
;;

let parse_expr = ws_opt (choice [ parse_instruction; parse_label_expr ])
let parse_ast =
    ws_opt (many parse_expr)
    <* (end_of_input <|> (Angstrom.peek_char >>= function
            | None -> return ()  (* Handle graceful end of input *)
            | Some c -> fail (Printf.sprintf "Unexpected character: '%c'" c)))