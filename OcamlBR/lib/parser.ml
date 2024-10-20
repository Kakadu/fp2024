(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Base
open Ast

(*---------------------Check conditions---------------------*)

let is_keyword = function
  | "let" | "in" | "fun" | "rec" | "if" | "then" | "else" | "true" | "false" -> true
  | _ -> false
;;

(*---------------------Control characters---------------------*)

let pwhitespace = take_while Char.is_whitespace
let pstoken s = pwhitespace *> string s
let ptoken s = pwhitespace *> s
let pparens p = pstoken "(" *> p <* pstoken ")"

(*-------------------------Constants/Variables-------------------------*)

let pint =
  let sign = choice [ pstoken "-"; pstoken "+"; pstoken "" ] in
  let rest = take_while1 Char.is_digit in
  let whole = lift2 ( ^ ) sign rest in
  whole
  >>= fun str ->
  match Stdlib.int_of_string_opt str with
  | Some n -> return (Int n)
  | None -> fail "Integer value exceeds the allowable range for the int type"
;;

let pbool =
  choice [ pstoken "true" *> return true; pstoken "false" *> return false ]
  >>| fun x -> Bool x
;;

let pstr = char '"' *> take_till (Char.equal '"') <* char '"' >>| fun x -> String x
let punit = pstoken "()" *> return Unit
let const = choice [ pint; pbool; pstr; punit ]

let varname =
  ptoken
    (take_while (fun ch -> Char.is_digit ch || Char.equal ch '\'')
     >>= function
     | "" ->
       take_while1 (fun ch -> Char.is_alpha ch || Char.is_digit ch || Char.equal ch '_')
       >>= fun str ->
       if is_keyword str
       then fail "Variable name conflicts with a keyword"
       else return str
     | _ -> fail "Variable name must not start with a digit")
;;

let pvar = varname >>| fun x -> PVar x
let pconst = const >>| fun x -> PConst x
let ppattern = choice [ pconst; pvar ]

(*------------------Binary operators-----------------*)

let pbinop op token =
  pwhitespace *> pstoken token *> return (fun e1 e2 -> Ebin_op (op, e1, e2))
;;

let add = pbinop Add "+"
let sub = pbinop Sub "-"
let mult = pbinop Mult "*"
let div = pbinop Div "/"

let relation =
  choice
    [ pbinop Eq "="
    ; pbinop Neq "<>"
    ; pbinop Lt "<"
    ; pbinop Gt ">"
    ; pbinop Lte "<="
    ; pbinop Gte ">="
    ]
;;

(*------------------------Expressions----------------------*)

let chain e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

let plet pexpr =
  let rec pbody pexpr =
    ppattern
    >>= function
    | PVar id -> pbody pexpr <|> (pstoken "=" *> pexpr >>| fun e -> Efun ([ id ], e))
    | _ -> fail "Only variable patterns are supported"
  in
  pstoken "let"
  *> lift4
       (fun r id e1 e2 -> Elet (r, id, e1, e2))
       (pstoken "rec " *> return Recursive <|> return Non_recursive)
       (pparens varname <|> varname)
       (pstoken "=" *> pexpr <|> pbody pexpr)
       (pstoken "in" *> pexpr <|> return (Econst Unit))
;;

let pEconst = const >>| fun x -> Econst x
let pEvar = varname >>| fun x -> Evar x
let pEapp e = chain e (return (fun e1 e2 -> Efun_application (e1, e2)))

let pbranch pexpr =
  lift3
    (fun e1 e2 e3 -> Eif_then_else (e1, e2, e3))
    (pstoken "if" *> pexpr)
    (pstoken "then" *> pexpr)
    (pstoken "else" *> pexpr >>| (fun e3 -> Some e3) <|> return None)
;;

let pexpr =
  fix (fun expr ->
    let atom_expr = choice [ pEconst; pEvar; pparens expr ] in
    (* parsing of nested if expressions and fallback to simpler atomic expressions
       if the conditional expression parsing fails *)
    let ite_expr = pbranch (expr <|> atom_expr) <|> atom_expr in
    (* parsing function applications, where the left side can be
       an if expression or a simpler atomic expression *)
    let app_expr = pEapp (ite_expr <|> atom_expr) <|> ite_expr in
    let factor_expr = chain app_expr (mult <|> div) in
    let sum_expr = chain factor_expr (add <|> sub) in
    let rel_expr = chain sum_expr relation in
    rel_expr)
;;

let parse_structure =
  let parse_structure_item = choice [ plet pexpr; pexpr ] in
  let semicolons = pstoken ";;" in
  let items = sep_by semicolons parse_structure_item in
  items <* pwhitespace
;;

let parse_expr str = parse_string ~consume:All parse_structure str
