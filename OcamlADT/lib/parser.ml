(** Copyright 2024-2025, Rodion Suvorov, Mikhail Gavrilenko *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom
open Base
open Char

(*                   Auxiliary parsers                     *)
  
let debug_parser name p =
  p  <|> (return () >>= fun () ->
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

let pidentexpr = lift (fun ident -> Exp_ident ident) pident

(*                   Patterns                         *)
let pany = token "_" *> return Pat_any
let pvar = lift (fun ident -> Pat_var ident) pident
let ppattern =
  pany <|> pvar (* <|> pconstant <|> ptuple <|> pconstruct, will be added in future, not necessary for fact *)

(*                   Exptessions                         *)


let pvalue_binding pexpr =
  let parse_pattern_and_expr =
    lift2 (fun pat expr -> {pat;expr}) ppattern (token "=" *> pexpr)
  in
  debug_parser "value_binding" parse_pattern_and_expr
;;
let prec_flag = token "rec" *> return Recursive <|> return Nonrecursive
;;
(*rewrite*)
let pletexpr pexpr =
  lift3 (fun rec_flag value_binding expr -> Exp_let(rec_flag, value_binding, expr))
  (token "let" *> pass_ws *> prec_flag) (pass_ws *> many1(pvalue_binding pexpr)) (pass_ws *> token "in" *> pexpr)
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

let pifexpr pexpr =
  let* _ = token "if" in
  let* condition = pexpr in
  let* _ = token "then" in
  let* expr = pexpr in
  let* alternative = option None (
    let* _ = token "else" in
    let* expr = pexpr in
    return (Some expr)
  ) in
  return (Exp_if(condition, expr, alternative))

let papplyexpr pexpr =
  lift2 (fun fexpr sexpr -> Exp_apply(fexpr, sexpr)) pexpr pexpr
;;

let pfunexpr pexpr = 
  lift3 
    (fun first_pattern rest_patterns body_expr -> 
      Exp_fun (first_pattern, rest_patterns, body_expr))
    (token "fun" *> ppattern)
    (many ppattern)
    (token "->" *> pexpr)
;;

let parsebinop binoptoken =
  let parse_operator = pass_ws *> token binoptoken in
  lift (fun op e1 e2 -> Exp_apply (Exp_ident op, Exp_tuple (e1, e2, []))) parse_operator
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
    debug_parser "parens" (pparens expr);
    debug_parser "constint" pconstintexpr;
    debug_parser "constchar" pconstcharexpr;
    (* debug_parser "conststring" pconststringexpr; *)
  ] in
  let expr = debug_parser "apply" (papplyexpr expr) <|> expr in
  (* let expr = debug_parser "mul_div" (lchain expr (pmul <|> pdiv)) in *)
  (* let expr = debug_parser "add_sub" (lchain expr (padd <|> psub)) in *)
  let expr = debug_parser "compare" (lchain expr pcompops) in
  (* let expr = rchain expr plogops in  *)
  (* let expr = debug_parser "if_then_else" (pifexpr expr) <|> expr in *)
  (* let expr = debug_parser "tuple" ptupleexpr <|> expr in  *)
  let expr = debug_parser "let" (pletexpr expr) <|> expr in
  (* let expr = debug_parser "fun" (pfunexpr expr) <|> expr in  *)
  expr)
;;

(*                   Structure items                         *)

let pseval = lift (fun expr -> Str_eval(expr)) pexpr

let psvalue = 
  lift2 (fun rec_flag 
  value_binding -> Str_value (rec_flag, value_binding))
  prec_flag (many (pvalue_binding pexpr))
;;    

(** It applies Str_eval to output of expression parser *)
(* let pstr_item =
  pseval <|> psvalue *)

  let pstr_item =
    debug_parser "pseval" pseval
    <|> debug_parser "psvalue" psvalue

(** It applies Str_eval to output of expression parser *)

let pstructure =
  let psemicolon = token ";;" in
  many (pstr_item <* psemicolon)

let parse str = parse_string ~consume:All pstructure str

let parse_str str = 
  match parse str with
  | Ok str -> str
  | Error msg -> failwith msg
