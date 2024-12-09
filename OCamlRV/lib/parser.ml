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
  | "and"
  | "not"
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

let rec annot_list t =
  let* base = t in
  let* _ = ws *> string "list" in
  annot_list (return (AList base)) <|> return (AList base)
;;

let annot_alone =
  choice
    [ token "int" *> return AInt
    ; token "string" *> return AString
    ; token "bool" *> return ABool
    ; token "unit" *> return AUnit
    ]
;;

let parse_type_annotation =
  let alone = annot_alone in
  let list_type = annot_list alone <|> alone in
  list_type
;;

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
let brackets p = token "[" *> p <* token "]"

let pptuple ppattern =
  let* el1 = ws *> ppattern in
  let* el2 = token "," *> ws *> ppattern in
  let* rest = many (token "," *> ppattern) in
  return (PTuple (el1, el2, rest))
;;

let pp_option_none = ws *> token "None" *> return (POption None)
let pp_option_some pe = ws *> token "Some" *> pe >>| fun x -> POption (Some x)
let pp_option pe = choice [ pp_option_none; pp_option_some pe ]

let ppcons pe =
  let* e1 = pe in
  let* rest = many (token "::" *> pe) in
  let rec helper = function
    | [] -> e1
    | [ x ] -> x
    | x :: xs -> PCons (x, helper xs)
  in
  return (helper (e1 :: rest))
;;

let pattern =
  let pattern_with_type ppat =
    let* pat = ws *> token "(" *> ppat in
    let* constr = ws *> token ":" *> ws *> parse_type_annotation <* ws <* token ")" in
    return (PType (pat, constr))
  in
  fix (fun pat ->
    let term =
      choice
        [ ppany
        ; ppliteral
        ; ppvariable
        ; pparens pat
        ; pp_option pat
        ; pattern_with_type pat
        ]
    in
    let cons = ppcons term in
    let tuple = pptuple term <|> cons in
    tuple)
;;

(*--------------------------- Expressions ---------------------------*)

let ebinop op e1 e2 = ExprBinOperation (op, e1, e2)
let punary_neg = token "-" *> return UnaryMinus
let punary_not = token "not" *> return UnaryNeg
let punary_add = token "+" *> return UnaryPlus
let punary_op = choice [ punary_neg; punary_not; punary_add ]
let peunop pe = lift2 (fun op e -> ExprUnOperation (op, e)) punary_op pe
let eapply e1 e2 = ExprApply (e1, e2)
let elet f b bl e = ExprLet (f, b, bl, e)
let efun p e = ExprFun (p, e)
let eif e1 e2 e3 = ExprIf (e1, e2, e3)
let pevar = variable >>| fun v -> ExprVariable v
let peliteral = pliteral >>| fun l -> ExprLiteral l

let ematch e = function
  | [] -> ExprOption None (* unreachable *)
  | [ x ] -> ExprMatch (e, x, [])
  | x :: xs -> ExprMatch (e, x, xs)
;;

let grd = token "|"

let pematch pe =
  let pexpr = token "match" *> pe <* token "with" <* option "" grd in
  let pcase = lift2 (fun p e -> p, e) (pattern <* token "->") pe in
  lift2 ematch pexpr (sep_by1 grd pcase)
;;

let petuple pe =
  let* el1 = ws *> pe in
  let* el2 = token "," *> ws *> pe in
  let* rest = many (token "," *> pe) in
  return (ExprTuple (el1, el2, rest))
;;

let pelist pe =
  brackets @@ sep_by1 (token ";") pe
  >>| function
  | [] -> ExprLiteral NilLiteral
  | [ x ] -> ExprList (x, [])
  | x :: xs -> ExprList (x, xs)
;;

let pecons pe =
  let* e1 = pe in
  let* rest = many (token "::" *> pe) in
  let rec helper = function
    | [] -> e1
    | [ x ] -> x
    | x :: xs -> ExprCons (x, helper xs)
  in
  return (helper (e1 :: rest))
;;

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
let pbinding pe = both pattern (lift2 efunf (many pattern <* token "=") pe)

let pelet pe =
  lift4
    elet
    (token "let" *> parse_rec_flag)
    (pbinding pe)
    (many (token "and" *> pbinding pe))
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

let p_option_none = ws *> token "None" *> return (ExprOption None)
let p_option_some pe = ws *> token "Some" *> pe >>| fun x -> ExprOption (Some x)
let p_option pe = choice [ p_option_none; p_option_some pe ]

let expr =
  fix (fun expr ->
    let term = choice [ pevar; peliteral; pelist expr; pparens expr ] in
    let apply = chainl1 term (return eapply) in
    let cons = pecons apply in
    let ife = peif expr <|> cons in
    let opt = p_option ife <|> ife in
    let ops1 = chainl1 opt (pmul <|> pdiv) in
    let ops2 = chainl1 ops1 (padd <|> psub) in
    let unops = ops2 <|> peunop ops2 in
    let cmp = chainl1 unops pcmp in
    let tuple = petuple cmp <|> cmp in
    choice [ tuple; pelet expr; pematch expr; pefun expr ])
;;

(*--------------------------- Structure ---------------------------*)

let pstructure =
  let pseval = expr >>| fun e -> SEval e in
  let psvalue =
    lift3
      (fun f b bl -> SValue (f, b, bl))
      (token "let" *> parse_rec_flag)
      (pbinding expr)
      (many (token "and" *> pbinding expr))
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
