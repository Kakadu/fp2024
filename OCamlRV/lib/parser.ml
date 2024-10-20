(** Copyright 2024-2025, Viacheslav Sidorov and Danila Rudnev-Stepanyan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Base

let is_id c = Char.is_alphanum c || Char.equal c '_' || Char.equal c '\''

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

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

(*--------------------------- Literals ---------------------------*)

let integer =
  let* sign = choice [ token "-"; token "+"; token "" ] in
  let* digits = take_while1 Char.is_digit in
  return (Int.of_string (sign ^ digits))
;;

let pinteger = integer >>| fun i -> IntLiteral i

let pbool =
  let t = token "true" *> return (BoolLiteral true) in
  let f = token "false" *> return (BoolLiteral false) in
  choice [ t; f ]
;;

let pstring =
  token "\""
  *> take_while (function
    | '"' -> false
    | _ -> true)
  <* char '"'
  >>| fun s -> StringLiteral s
;;

let punit = token "()" *> return UnitLiteral
let pnil = token "[]" *> return NilLiteral
let pliteral = choice [ pinteger; pbool; pstring; punit; pnil ]

(*--------------------------- Patterns ---------------------------*)

let ppany = token "_" *> return PAny
let ppliteral = pliteral >>| fun a -> PLiteral a

let variable =
  let* fst =
    ws
    *> satisfy (function
      | 'a' .. 'z' | '_' -> true
      | _ -> false)
  in
  let* rest = take_while is_id in
  match String.of_char fst ^ rest with
  | "_" -> fail "Wildcard can't be used as indetifier"
  | s when is_keyword s -> fail "Keyword can't be used as identifier"
  | name -> return name
;;

let ppvariable = variable >>| fun v -> PVar v
let pparens p = token "(" *> p <* token ")"
let pattern = fix (fun pat -> choice [ ppany; ppliteral; ppvariable; pparens pat ])

(* need to add parsers for PTuple; PCons; PPoly *)

(*--------------------------- Expressions ---------------------------*)

let ebinop op e1 e2 = ExprBinOperation (op, e1, e2)
let eapply e1 e2 = ExprApply (e1, e2)
let elet f b e = ExprLet (f, b, e)
let efun p e = ExprFun (p, e)
let eif e1 e2 e3 = ExprIf (e1, e2, e3)
let pevar = variable >>| fun v -> ExprVariable v
let peliteral = pliteral >>| fun l -> ExprLiteral l
let padd = token "+" *> return (ebinop Add)
let psub = token "-" *> return (ebinop Sub)
let pmul = token "*" *> return (ebinop Mul)
let pdiv = token "/" *> return (ebinop Div)

let pcmp =
  choice
    [ token "=" *> return (ebinop Eq)
    ; token "<>" *> return (ebinop Neq)
    ; token "<=" *> return (ebinop Lte)
    ; token ">=" *> return (ebinop Gte)
    ; token "<" *> return (ebinop Lt)
    ; token ">" *> return (ebinop Gt)
    ; token "&&" *> return (ebinop And)
    ; token "||" *> return (ebinop Or)
    ]
;;

let get_rec_keyword =
  let* r = ws *> take_while1 is_id in
  if String.compare r "rec" = 0 then return true else fail "There in no 'rec' keyword."
;;

let parse_rec_flag =
  let* is_rec = get_rec_keyword <|> return false in
  if is_rec then return Rec else return NonRec
;;

let efunf ps e = List.fold_right ps ~f:efun ~init:e

let pelet pe =
  lift3
    elet
    (token "let" *> parse_rec_flag)
    (both pattern (lift2 efunf (many pattern <* token "=") pe))
    (token "in" *> pe)
;;

let pefun pe =
  lift2 efun (token "fun" *> pattern) (lift2 efunf (many pattern <* token "->") pe)
;;

let peif pe =
  fix (fun peif ->
    lift3
      eif
      (token "if" *> (peif <|> pe))
      (token "then" *> (peif <|> pe))
      (option None (token "else" *> (peif <|> pe) >>| Option.some)))
;;

let expr =
  fix (fun expr ->
    let term = choice [ pevar; peliteral; pparens expr ] in
    let apply = chainl1 term (return eapply) in
    let ops1 = chainl1 apply (pmul <|> pdiv) in
    let ops2 = chainl1 ops1 (padd <|> psub) in
    let cmp = chainl1 ops2 pcmp in
    let ife = peif cmp <|> cmp in
    choice [ pelet expr; pefun expr; ife ])
;;

(*--------------------------- Structure ---------------------------*)

let pstructure =
  let pseval = expr >>| fun e -> SEval e in
  let psvalue =
    lift2
      (fun f b -> SValue (f, b))
      (token "let" *> parse_rec_flag)
      (both pattern (lift2 efunf (many pattern <* token "=") expr))
  in
  choice [ pseval; psvalue ]
;;

let structure : structure t = sep_by (token ";;") pstructure
let parse s = parse_string ~consume:Prefix structure s

let parse_to_string input =
  match parse input with
  | Ok structure -> show_structure structure
  | Error err -> err
;;
