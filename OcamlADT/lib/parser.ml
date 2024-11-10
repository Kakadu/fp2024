(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom
open Base
open Char

(*                   Auxiliary parsers                     *)

let is_not_keyword = function
  | "let" | "if" | "then" | "else" | "in" | "fun" | "true" | "false" | "rec" -> false
  | _ -> true
  
let debug_parser name p =
  (p >>= fun result ->
    Stdlib.Printf.printf "Debug: %s parser SUCCEEDED\n" name;
    return result) <|> (return () >>= fun () ->
    Stdlib.Printf.printf "Debug: %s parser failed\n" name;
    fail (Printf.sprintf "%s parser failed" name))

let is_whitespace = function
| ' ' | '\t' | '\n' | '\r' -> true
| _ -> false
let pass_ws = skip_while is_whitespace

(** Parser that matches string literals an 's' skipping all whitespaces before *)
let token s = pass_ws *> string s
let pparens stmt = token "(" *> stmt <* token ")"
let pdsemicolon = 
  let* str_part = take_while (function ';' -> false | _ -> true) in  
  let* semi_part = peek_char in  (* Peek to see if we have encountered `;` *)
  match semi_part with
  | Some ';' ->
    let* _ = string ";;" in  (* Ensure we consume both semicolons *)
    return str_part
  | _ -> fail "Expected ;;"
let pletters = satisfy (function 'a'..'z' | 'A'..'Z' | '_' -> true | _ -> false)
let ptowhitespace = function 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true | _ -> false


let pident =
  lift2 (fun first rest -> (String.make 1 first) ^ rest)
    pletters
    (take_while ptowhitespace)


(*                   Constant expressions                         *)
let pconstintexpr =
  let parse_sign = choice [ token "+"; token "-"; token ""] in
  let parse_number = take_while1 (function '0' .. '9' -> true | _ -> false) in
  lift2 (fun sign n -> Exp_constant (Const_integer (int_of_string (sign ^ n))))
    parse_sign
    parse_number


let pconstcharexpr =   
  let* _ = token "'" in 
  let* c = satisfy (fun code -> code >= ' ' && code <= '~') in 
  let* _ = token "'" in 
  return (Exp_constant(Const_char c))

let pconststringexpr = 
  token "\"" *> lift 
    (fun str -> Exp_constant(Const_string str)) 
    (take_while1 (function '"' -> false | _ -> true))
  <* token "\""

let pconst = 
  pconstcharexpr <|> pconstintexpr <|> pconststringexpr

(*                   Arithm utils + ident parser                         *)
let lchain p op =
  let rec loop acc =
    (let* f = op in
     let* y = p in
     loop (f acc y))
    <|> return acc
  in
  let* x = p in
  loop x

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

let pidentexpr =
  pident >>= fun ident ->
  if is_not_keyword ident then
    return (Exp_ident ident)
  else
    fail "Found a keyword instead of an identifier"

(*                   Patterns                         *)
let pany = token "_" *> return Pat_any

let pvar =
  pident >>= fun ident ->
  if is_not_keyword ident then
    return (Pat_var ident)
  else
    fail "Found a keyword instead of an variable"

let ppattern =
    pany <|> pvar (* <|> pconstant <|> ptuple <|> pconstruct, will be added in future, not necessary for fact *)
  

(*                   Expressions                         *)


let pvalue_binding pexpr =
  lift2 (fun pat expr -> {pat;expr}) ppattern (token "=" *> pass_ws *> pexpr)
;;
let prec_flag = token "rec" *> return Recursive <|> return Nonrecursive
;;
let pletexpr pexpr =
  lift3 (fun rec_flag value_binding expr -> Exp_let(rec_flag, value_binding, expr))
  (token "let" *> pass_ws *> prec_flag) (pass_ws *> many1(pvalue_binding pexpr))  (token "in" *> pass_ws *> pexpr)
;;

(*rewrite*) 
let ptupleexpr =
  let* _ = token "(" in
  let* expression1 = pidentexpr in
  let* _ = token "," in
  let* expression2 = pidentexpr in
  let* _ = token "," in
  let* expressiontl = sep_by (char ',') pidentexpr in
  let* _ = token ")" in
  return (Exp_tuple(expression1, expression2, expressiontl))
;;

let pifexpr pexpr =
  lift3 (fun condition thenexpr elseexpr -> Exp_if(condition, thenexpr, elseexpr))
  (token "if" *> pass_ws *> pexpr) (token "then" *> pass_ws *> pexpr ) 
  (option None (token "else" *> pass_ws *> pexpr >>| fun x -> Some x))
;;

let papplyexpr pexpr = lchain pexpr (return (fun ex1 ex2 -> Exp_apply(ex1, [ex2])))
;;

(*let papplyexpr pexpr =
  let rec collect_args acc =
    let* arg = pexpr in
    let* args = option [] (collect_args (arg :: acc)) in
    return (arg :: args)
  in
  let papplychain = 
    let term = pidentexpr <|> pexpr in
    (* We ensure to only apply if we are not dealing with a binary operation *)
    lift2 (fun f args -> Exp_apply(f, args)) term (collect_args [])
  in 
  (* We apply lchain to handle the chain of applications *)
  lchain papplychain pexpr
;;*)

let pfunexpr pexpr = 
  lift3 
    (fun first_pattern rest_patterns body_expr -> 
      Exp_fun (first_pattern, rest_patterns, body_expr))
    (token "fun" *> ppattern)
    (many ppattern)
    (token "->" *> pexpr)
;;

let parsebinop binoptoken =
  pass_ws *> token binoptoken *> return (fun e1 e2 -> Exp_apply (Exp_ident binoptoken, [Exp_tuple (e1, e2, [])]))
;;

let padd = parsebinop "+"
let psub = parsebinop "-"
let pdiv = parsebinop "/"
let pmul = parsebinop "*"
let pcompops = 
  choice 
    [
      parsebinop ">";
      parsebinop "<";
      parsebinop ">=";
      parsebinop "<=";
      parsebinop "<>";
      parsebinop "=";
    ]

let plogops = 
  choice 
  [
    parsebinop "||";
    parsebinop "&&";
  ]

let pexpr = fix (fun expr ->
(* let expr = choice [pparens expr; pconstintexpr; pconstcharexpr; pconststringexpr; ] in *)
  let expr = choice [
    pidentexpr;
    (pparens expr);
    pconstintexpr;
    pconstcharexpr;
    pconststringexpr;
  ] in
  let expr = (papplyexpr expr) <|> expr in
  let expr = (lchain expr (pmul <|> pdiv)) in
  let expr = (lchain expr (padd <|> psub)) in
  let expr = (lchain expr pcompops) in
  let expr = rchain expr plogops in
  let expr =  (pifexpr expr) <|> expr in 
  let expr =  ptupleexpr <|> expr in  
  let expr =  (pletexpr expr) <|> expr in
  let expr =  (pfunexpr expr) <|> expr in  
  expr)
;;

(*                   Structure items                         *)

let pseval =  (lift (fun expr -> Str_eval(expr)) pexpr)

let psvalue = 
  (let check_no_in =
    let* next = peek_char in
    match next with
    | Some 'i' -> 
      let* _ = string "in" in
      fail "Unexpected 'in' found after let binding"
    | _ -> return () 
  in
  lift2 (fun rec_flag value_bindings ->
    Str_value (rec_flag, value_bindings))
    (token "let" *> pass_ws *> prec_flag)
    (many1 (pvalue_binding pexpr))
  <* check_no_in)
;;

(** It applies Str_eval to output of expression parser *)
(* let pstr_item =
  pseval <|> psvalue *)

let pstr_item =
  pseval <|> psvalue

let pstructure =
  let psemicolon = token ";;" in
  many (pstr_item <* psemicolon <* pass_ws)

let parse str = parse_string ~consume:All pstructure str

let parse_str str = 
  match parse str with
  | Ok str -> str
  | Error msg -> failwith msg
