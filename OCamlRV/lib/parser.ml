(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Base

let is_letter = function
  | 'a' .. 'z' -> true
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_keyword = function
  | "let"
  | "rec"
  | "in"
  | "fun"
  | "match"
  | "with"
  | "if"
  | "then"
  | "else"
  | "true"
  | "false" -> true
  | _ -> false
;;

let ws = take_while Char.is_whitespace
let token s = ws *> string s

(* Number of parser *)
let pnumber =
  let* sign = choice [ token "-"; token "+"; token "" ] in
  let* digits = take_while1 is_digit in
  return (int_of_string (sign ^ digits))
;;

(* Parser of literals *)
let pliteral =
  choice
    [ (let* n = pnumber in
       return (ExprLiteral (IntLiteral n)))
    ; token "true" *> return (ExprLiteral (BoolLiteral true))
    ; token "false" *> return (ExprLiteral (BoolLiteral false))
    ; token "()" *> return (ExprLiteral UnitLiteral)
    ; token "nil" *> return (ExprLiteral NilLiteral)
    ]
;;

(* Parser of patterns *)
let ppattern =
  let* p = take_while1 is_letter in
  return p
;;

(* Parser of identifiers *)
let id =
  let* first_char = satisfy is_letter in
  let* rest = take_while (fun c -> is_letter c || is_digit c || Char.equal c '_') in
  return (String.make 1 first_char ^ rest)
;;

let is_idc c = Char.is_alphanum c || Char.equal c '_'

(* Parser of variables *)
let pvariable =
  let* fst =
    ws
    *> satisfy (function
      | 'a' .. 'z' | '_' -> true
      | _ -> false)
  in
  let* rest = take_while is_idc in
  match String.of_char fst ^ rest with
  | "_" -> fail "Wildcard can't be used as indetifier"
  | s when is_keyword s -> fail "Keyword can't be used as identifier"
  | _ as name -> return (ExprVariable name)
;;

(* Parser of binary operators *)
let pbinop =
  choice
    [ token "<>" *> return Neq
    ; token "&&" *> return And
    ; token "||" *> return Or
    ; token ">=" *> return Gte
    ; token "<=" *> return Lte
    ; token "+" *> return Add
    ; token "-" *> return Sub
    ; token "*" *> return Mul
    ; token "/" *> return Div
    ; token "<" *> return Lt
    ; token ">" *> return Gt
    ; token "=" *> return Eq
    ]
;;

(* Parser of unary operators *)
let punop pexpression =
  let* op = choice [ token "+"; token "-"; token "not" ] in
  let* expr = pexpression in
  match op with
  | "+" -> return (ExprUnOperation (UnaryPlus, expr))
  | "-" -> return (ExprUnOperation (UnaryMinus, expr))
  | "not" -> return (ExprUnOperation (UnaryNeg, expr))
  | _ -> fail "unsupported unary operator"
;;

(* Parser of if-then-else *)
let pif pexpression =
  let* _ = token "if" in
  let* cond = pexpression in
  let* _ = token "then" in
  let* then_expr = pexpression in
  let* _ = token "else" in
  let* else_expr = pexpression in
  return (ExprIf (cond, then_expr, Some else_expr))
;;

let parse_rec_flag = ws *> option NonRec (token "rec" *> return Rec)

let primary_expr pexpression =
  choice [ pliteral; pif pexpression; punop pexpression; pvariable ]
;;

let papply pexpression =
  let* expr1 = primary_expr pexpression in
  ws
  *>
  let* expr2 = pexpression in
  return (ExprApply (expr1, expr2))
;;

(* Parser of binary expressions *)
let pexpr_with_binop pexpression =
  let* _ = ws *> option None (token "(" *> return None) in
  ws
  *>
  let* left = primary_expr pexpression in
  let* op_opt = option None (pbinop >>| fun op -> Some op) in
  ws
  *>
  match op_opt with
  | None -> return left
  | Some op ->
    ws
    *>
    let* right = pexpression in
    let* _ = ws *> option None (token ")" *> return None) in
    return (ExprBinOperation (op, left, right))
;;

(* Main parser of expressions *)
let pexpression = fix (fun p -> choice [ papply p; pexpr_with_binop p; primary_expr p ])

(* Main parser of structures *)
let pstructure =
  let psvalue =
    let* _ = token "let" in
    let* rec_flag = parse_rec_flag in
    ws
    *>
    let* id = id in
    ws
    *>
    let* p = ppattern in
    ws
    *>
    let* _ = token "=" in
    ws
    *>
    let* expr = pexpression in
    return
      (SValue
         ( rec_flag
         , [ PLiteral (StringLiteral id), ExprFun (PLiteral (StringLiteral p), [], expr) ]
         ))
  in
  let pseval = pexpression >>| fun e -> SEval e in
  choice [ psvalue; pseval ]
;;

let structure : structure t = sep_by (token ";;") pstructure
let parse s = parse_string ~consume:Prefix structure s

let test_parse input =
  match parse input with
  | Ok _ -> true
  | Error _ -> false
;;

let check_parse input =
  match parse input with
  | Ok structure -> Format.printf "%s\n" (show_structure structure)
  | Error err -> Format.printf "%s\n" err
;;
