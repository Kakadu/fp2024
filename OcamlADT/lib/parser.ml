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

let debug_parser name p = p
(* >>= (fun result ->
   Stdlib.Printf.printf "Debug: %s parser SUCCEEDED\n" name;
   return result)
   <|> (return ()
   >>= fun () ->
   Stdlib.Printf.printf "Debug: %s parser failed\n" name;
   fail (Printf.sprintf "%s parser failed" name)) *)

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let pass_ws = skip_while is_whitespace

(** Parser that matches string literals an 's' skipping all whitespaces before *)
let token s = debug_parser "token_parser" (pass_ws *> string s)

let pparens stmt = token "(" *> stmt <* token ")"

let pdsemicolon =
  let* str_part =
    take_while (function
      | ';' -> false
      | _ -> true)
  in
  let* semi_part = peek_char in
  (* Peek to see if we have encountered `;` *)
  match semi_part with
  | Some ';' ->
    let* _ = string ";;" in
    (* Ensure we consume both semicolons *)
    return str_part
  | _ -> fail "Expected ;;"
;;

let pletters =
  satisfy (function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false)
;;

let ptowhitespace = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let pident =
  lift2 (fun first rest -> String.make 1 first ^ rest) pletters (take_while ptowhitespace)
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
  let parse_sign = choice [ token "+"; token "-"; token "" ] in
  let parse_number =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  in
  lift2
    (fun sign n -> Exp_constant (Const_integer (int_of_string (sign ^ n))))
    parse_sign
    parse_number
;;

let pconstcharexpr =
  let* _ = token "'" in
  let* c = satisfy (fun code -> code >= ' ' && code <= '~') in
  let* _ = token "'" in
  return (Exp_constant (Const_char c))
;;

let pconst = 
  pconstcharexpr <|> pconstintexpr <|> pconststringexpr

let pconststringexpr =
  token "\""
  *> lift
       (fun str -> Exp_constant (Const_string str))
       (take_while1 (function
         | '"' -> false
         | _ -> true))
;;

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
  pident
  >>= fun ident ->
  if is_not_keyword ident
  then return (Pat_var ident)
  else fail "Found a keyword instead of an variable"
;;

let ppattern =
  let simplevar = choice [ pany; pvar ] in
  ptuplepat simplevar <|> simplevar
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
  pident
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

let parsebinop binoptoken =
  pass_ws
  *> token binoptoken
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
           [ debug_parser "identexp" pidentexpr
           ; debug_parser "constint" pconstintexpr
           ; debug_parser "constchar" pconstcharexpr
           ; debug_parser "conststring" pconststringexpr
           ; debug_parser "parens" (pparens pexpr)
           ; debug_parser "function" (pfunction pexpr)
           ; debug_parser "fun" (pfunexpr pexpr)
           ; debug_parser "let" (plet pexpr)
           ; debug_parser "if_then_else" (pifexpr pexpr)
           ; debug_parser "match" (pmatch pexpr)
           ]
    in
    let papply = debug_parser "apply" (papplyexpr poprnd) in
    let expr = debug_parser "mul_div" (lchain papply (pmul <|> pdiv)) in
    let expr = debug_parser "add_sub" (lchain expr (padd <|> psub)) in
    let expr = debug_parser "compare" (lchain expr pcompops) in
    let expr = rchain expr plogops in
    (* let expr = debug_parser "if_then_else" (pifexpr expr) <|> expr in *)
    debug_parser "tuple" (ptupleexpr expr) <|> expr)
;;

(*
   ░▒▓███████▓▒░▒▓████████▓▒░▒▓███████▓▒░░▒▓█▓▒░░▒▓█▓▒░░▒▓██████▓▒░▒▓████████▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓███████▓▒░░▒▓████████▓▒░
   ░▒▓█▓▒░         ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░ ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
   ░▒▓█▓▒░         ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░        ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░
   ░▒▓██████▓▒░   ░▒▓█▓▒░   ░▒▓███████▓▒░░▒▓█▓▒░░▒▓█▓▒░▒▓█▓▒░        ░▒▓█▓▒░   ░▒▓█▓▒░░▒▓█▓▒░▒▓███████▓▒░░▒▓██████▓▒░
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

let pseval = debug_parser "ps_eval" (lift (fun expr -> Str_eval expr) pexpr)

(** It applies Str_eval to output of expression parser *)
(* let pstr_item =
   pseval <|> psvalue *)

let pstrlet =
  let precflag = token "rec" *> return Recursive <|> return Nonrecursive in
  let* recflag = token "let" *> precflag in
  let* bindingfs = pletbinding pexpr in
  let* bindingtl = sep_by (token "and") (pletbinding pexpr) in
  return @@ Str_value (recflag, (bindingfs, bindingtl))
;;

let psvalue = pstrlet (*<|> prsadt*)
let pstr_item = pseval <|> psvalue

let pstructure =
  let psemicolon = token ";;" in
  (*change on default token*)
  many (pstr_item <* psemicolon <* pass_ws)
;;

let parse str = parse_string ~consume:Prefix pstructure str

let parse_str str =
  match parse str with
  | Ok str -> str
  | Error msg -> failwith msg
;;
