(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom
open Base
open Char

(*
   |░▒▓██████▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓██████▓▒░░▒▓███████▓▒░░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▒ ░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░░▒▓██████▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░▒▓████████▓▒░▒▓███████▓▒░ ░▒▓██████▓▒░
   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░  ░▒▓█▓▒░
   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░  ░▒▓█▓▒░
   ░▒▓█▓▒░░▒▓█▓▒░░▒▓██████▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░▒▓████████▓▒░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░  ░▒▓█▓▒░
*)

let is_not_keyword = function
  | "let"
  | "if"
  | "then"
  | "else"
  | "in"
  | "fun"
  | "true"
  | "false"
  | "rec"
  | "and"
  | "function"
  | "match"
  | "with" -> false
  | _ -> true
;;

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let pass_ws = skip_while is_whitespace

(** Parser that matches string literals an 's' skipping all whitespaces before *)
let token s = pass_ws *> string s

let pparenth stmt = token "(" *> stmt <* token ")"

let plettersdig =
  satisfy (function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false)
;;

let ptowhitespace = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

(* add check for keyword + prefix and infix sym + Some *)
let pident_cap =
  let first_char_str =
    satisfy (function
      | 'A' .. 'Z' -> true
      | _ -> false)
  in
  let rem_string = lift (fun rest -> rest) (take_while ptowhitespace) in
  lift2 (fun fc rs -> String.make 1 fc ^ rs) first_char_str rem_string
;;

let pident_lc =
  let first_char_str =
    satisfy (function
      | 'a' .. 'z' -> true
      | _ -> false)
  in
  let rem_string = lift (fun rest -> rest) (take_while ptowhitespace) in
  lift2 (fun fc rs -> String.make 1 fc ^ rs) first_char_str rem_string
;;

(*
   |░▒▓██████▓▒░ ░▒▓██████▓▒░░▒▓███████▓▒░ ░▒▓███████▓▒░▒▓████████▓▒░▒▓██████▓▒░░▒▓███████▓▒░▒▓████████▓▒░
   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░         ░▒▓█▓▒░  ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░         ░▒▓█▓▒░  ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░░▒▓██████▓▒░   ░▒▓█▓▒░  ░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░      ░▒▓█▓▒░  ░▒▓█▓▒░  ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░
   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░      ░▒▓█▓▒░  ░▒▓█▓▒░  ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░
   |░▒▓██████▓▒░ ░▒▓██████▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓███████▓▒░   ░▒▓█▓▒░  ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░

   ░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓███████▓▒░░▒▓███████▓▒░░▒▓████████▓▒░░▒▓███████▓▒░▒▓███████▓▒░▒▓█▓▒░░▒▓██████▓▒░░▒▓███████▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░     ░▒▓█▓▒░      ░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░     ░▒▓█▓▒░      ░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓██████▓▒░  ░▒▓██████▓▒░░▒▓███████▓▒░░▒▓███████▓▒░░▒▓██████▓▒░  ░▒▓██████▓▒░░▒▓██████▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░             ░▒▓█▓▒░     ░▒▓█▓▒░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░             ░▒▓█▓▒░     ░▒▓█▓▒░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓████████▓▒░▒▓███████▓▒░▒▓███████▓▒░░▒▓█▓▒░░▒▓██████▓▒░░▒▓█▓▒░░▒▓█▓▒░
*)
let pconstintexpr =
  let* number = int_of_string <$> take_while1 is_digit in
  return @@ Exp_constant (Const_integer number)
;;

let pconstcharexpr =
  let* _ = token "'" in
  let* c = satisfy (fun code -> code >= ' ' && code <= '~') in
  let* _ = token "'" in
  return (Exp_constant (Const_char c))
;;

let pconststringexpr =
  token "\""
  *> lift
       (fun str -> Exp_constant (Const_string str))
       (take_while1 (function
         | '"' -> false
         | _ -> true))
  <* token "\""
;;

let pconst = pconstcharexpr <|> pconstintexpr <|> pconststringexpr

(*                   Arithm utils + ident parser *)
let lchain p op =
  let rec loop acc =
    (let* f = op in
     let* y = p in
     loop (f acc y))
    <|> return acc
  in
  let* x = p in
  loop x
;;

let rchain p op =
  let rec loop acc =
    (let* f = op in
     let* y = p in
     let new_acc = f acc y in
     loop new_acc)
    <|> return acc
  in
  let* x = p in
  loop x
;;

(*
   ░▒▓███████▓▒░ ░▒▓██████▓▒░▒▓████████▓▒░▒▓████████▓▒░▒▓████████▓▒░▒▓███████▓▒░░▒▓███████▓▒░
   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░      ░▒▓█▓▒░   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░      ░▒▓█▓▒░   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓███████▓▒░░▒▓████████▓▒░ ░▒▓█▓▒░      ░▒▓█▓▒░   ░▒▓██████▓▒░ ░▒▓███████▓▒░░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░      ░▒▓█▓▒░   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░      ░▒▓█▓▒░   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░      ░▒▓█▓▒░   ░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
*)
let pany = token "_" *> return Pat_any

let ptuplepat ppattern =
  let* el1 = pass_ws *> ppattern in
  let* el2 = token "," *> pass_ws *> ppattern in
  let* rest = pass_ws *> sep_by (token ",") ppattern in
  return (Pat_tuple (el1, el2, rest))
;;

let pvar =
  pident_lc
  >>= fun ident ->
  if is_not_keyword ident
  then return (Pat_var ident)
  else fail "Found a keyword instead of an variable"
;;

let ppattern =
  fix (fun ppattern ->
    let poprnd = choice [ pany; pvar; pparenth ppattern ] in
    ptuplepat poprnd <|> poprnd)
;;

(*
   ░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓███████▓▒░░▒▓███████▓▒░░▒▓████████▓▒░░▒▓███████▓▒░▒▓███████▓▒░▒▓█▓▒░░▒▓██████▓▒░░▒▓███████▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░     ░▒▓█▓▒░      ░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░     ░▒▓█▓▒░      ░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓██████▓▒░  ░▒▓██████▓▒░░▒▓███████▓▒░░▒▓███████▓▒░░▒▓██████▓▒░  ░▒▓██████▓▒░░▒▓██████▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░             ░▒▓█▓▒░     ░▒▓█▓▒░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░             ░▒▓█▓▒░     ░▒▓█▓▒░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░
   ░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░▒▓████████▓▒░▒▓███████▓▒░▒▓███████▓▒░░▒▓█▓▒░░▒▓██████▓▒░░▒▓█▓▒░░▒▓█▓▒░
*)

let pidentexpr =
  pident_lc
  >>= fun ident ->
  if is_not_keyword ident
  then return (Exp_ident ident)
  else fail "Found a keyword instead of an identifier"
;;

let pcase pexpr =
  let* pat = pass_ws *> ppattern in
  (*todo: pass ws rework*)
  let* expr = token "->" *> pexpr in
  return { left = pat; right = expr }
;;

let ppatternmatching pexpr =
  let* casefs = option "" (token "|") *> pcase pexpr in
  let* casetl = option "" (token "|") *> (sep_by (token "|") @@ pcase pexpr) in
  (*todo: remove option *)
  return (casefs, casetl)
;;

let pfunction pexpr =
  let* cases = token "function" *> ppatternmatching pexpr in
  return @@ Exp_function cases
;;

let pmatch pexpr =
  let* expr = token "match" *> pexpr in
  let* cases = token "with" *> ppatternmatching pexpr in
  return @@ Exp_match (expr, cases)
;;

let pletbinding pexpr =
  let psimple =
    let* pat = pass_ws *> ppattern in
    let* expr = token "=" *> pass_ws *> pexpr in
    return { pat; expr }
  in
  let pfun =
    let* value_name = pass_ws *> pvar in
    let* parameterfs = pass_ws *> (pass_ws *> ppattern) in
    let* parametertl = pass_ws *> many (pass_ws *> ppattern) in
    let* expr = token "=" *> pass_ws *> pexpr in
    return { pat = value_name; expr = Exp_fun ((parameterfs, parametertl), expr) }
  in
  choice [ psimple; pfun ]
;;

let plet pexpr =
  let precflag = token "rec" *> return Recursive <|> return Nonrecursive in
  let* recflag = token "let" *> precflag in
  let* bindingfs = pletbinding pexpr in
  let* bindingtl = many (token "and" *> pletbinding pexpr) in
  let* expr = token "in" *> pass_ws *> pexpr in
  return @@ Exp_let (recflag, (bindingfs, bindingtl), expr)
;;

let ptupleexpr pexpr =
  let* el1 = pass_ws *> pexpr in
  let* el2 = token "," *> pass_ws *> pexpr in
  let* rest = pass_ws *> sep_by (token ",") pexpr in
  return (Exp_tuple (el1, el2, rest))
;;

let pifexpr pexpr =
  lift3
    (fun condition thenexpr elseexpr -> Exp_if (condition, thenexpr, elseexpr))
    (token "if" *> pass_ws *> pexpr)
    (token "then" *> pass_ws *> pexpr)
    (option None (token "else" *> pass_ws *> pexpr >>| fun x -> Some x))
;;

(* let pconstructexpr = *)

let papplyexpr pexpr = lchain pexpr (return (fun ex1 ex2 -> Exp_apply (ex1, (ex2, []))))

let pfunexpr pexpr =
  lift3
    (fun first_pattern rest_patterns body_expr ->
      Exp_fun ((first_pattern, rest_patterns), body_expr))
    (token "fun" *> pass_ws *> ppattern)
    (many (pass_ws *> ppattern))
    (token "->" *> pass_ws *> pexpr)
;;

let rec parseprefop pexpr pop =
  (let* f = pop in
   let* expr = parseprefop pexpr pop in
   return @@ f expr)
  <|> pexpr
;;

(* let* token = choice [token "+"; token "-"] in
  let* expr = pass_ws *> pexpr in
  return @@ Exp_apply(Exp_ident(token),(expr,[])) *)

let parsebinop binoptoken =
  token binoptoken
  *> return (fun e1 e2 -> Exp_apply (Exp_ident binoptoken, (Exp_tuple (e1, e2, []), [])))
;;

let padd = parsebinop "+"
let psub = parsebinop "-"
let pdiv = parsebinop "/"
let pmul = parsebinop "*"

let pcompops =
  choice
    [ parsebinop ">"
    ; parsebinop "<"
    ; parsebinop ">="
    ; parsebinop "<="
    ; parsebinop "<>"
    ; parsebinop "="
    ]
;;

let plogops = choice [ parsebinop "&&"; parsebinop "||" ]

let pexpr =
  fix (fun pexpr ->
    let poprnd =
      pass_ws
      *> choice
           [ pidentexpr
           ; pconstintexpr
           ; pconstcharexpr
           ; pconststringexpr
           ; pparenth pexpr
           ; pfunction pexpr
           ; pfunexpr pexpr
           ; plet pexpr
           ; pifexpr pexpr
           ; pmatch pexpr
           ]
    in
    let papply = papplyexpr poprnd in
    let prefop =
      parseprefop
        papply
        (choice [ token "+"; token "-" ]
         >>| fun id expr -> Exp_apply (Exp_ident id, (expr, [])))
      <|> papply
    in
    let pmuldiv = lchain prefop (pmul <|> pdiv) in
    let paddsub = lchain pmuldiv (padd <|> psub) in
    let pcompare = lchain paddsub pcompops in
    let plogop = rchain pcompare plogops in
    ptupleexpr plogop <|> plogop)
;;

(*
   |░▒▓███████▓▒░▒▓████████▓▒░▒▓███████▓▒░░▒▓█▓▒░░▒▓█▓▒░░▒▓██████▓▒░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓███████▓▒░░▒▓████████▓▒░
   ░▒▓█▓▒░         ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
   ░▒▓█▓▒░         ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░        ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
   |░▒▓██████▓▒░   ░▒▓█▓▒░   ░▒▓███████▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░        ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓███████▓▒░░▒▓██████▓▒░
   |      ░▒▓█▓▒░  ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░        ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
   |      ░▒▓█▓▒░  ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
   ░▒▓███████▓▒░   ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░░▒▓██████▓▒░ ░▒▓██████▓▒░  ░▒▓█▓▒░    ░▒▓██████▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓████████▓▒░

   ░▒▓█▓▒░▒▓████████▓▒░▒▓████████▓▒░▒▓██████████████▓▒░ ░▒▓███████▓▒░
   ░▒▓█▓▒░  ░▒▓█▓▒░   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
   ░▒▓█▓▒░  ░▒▓█▓▒░   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
   ░▒▓█▓▒░  ░▒▓█▓▒░   ░▒▓██████▓▒░ ░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░░▒▓██████▓▒░
   ░▒▓█▓▒░  ░▒▓█▓▒░   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░      ░▒▓█▓▒░
   ░▒▓█▓▒░  ░▒▓█▓▒░   ░▒▓█▓▒░      ░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░      ░▒▓█▓▒░
   ░▒▓█▓▒░  ░▒▓█▓▒░   ░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓███████▓▒░
*)

let pseval = lift (fun expr -> Str_eval expr) pexpr

let pstrlet =
  let precflag = token "rec" *> return Recursive <|> return Nonrecursive in
  let* recflag = token "let" *> precflag in
  let* bindingfs = pletbinding pexpr in
  let* bindingtl = many (token "and" *> pletbinding pexpr) in
  return @@ Str_value (recflag, (bindingfs, bindingtl))
;;

let psvalue = pstrlet (*<|> prsadt*)
let pstr_item = pseval <|> psvalue

let pstructure =
  let psemicolon = token ";;" in
  (*change on default token*)
  many (pstr_item <* psemicolon <* pass_ws)
;;

let parse str = parse_string ~consume:All pstructure str

let parse_str str =
  match parse str with
  | Ok str -> str
  | Error msg -> failwith msg
;;
