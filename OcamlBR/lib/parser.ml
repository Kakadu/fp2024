(** Copyright 2024, Sofya Kozyreva, Maksim Shipilov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Base
open Ast
open Typedtree

(*---------------------Check conditions---------------------*)

let is_keyword = function
  | "let"
  | "in"
  | "fun"
  | "rec"
  | "if"
  | "then"
  | "else"
  | "true"
  | "false"
  | "Some"
  | "None"
  | "and"
  | "match"
  | "with"
  | "print_int" -> true
  | _ -> false
;;

(*---------------------Control characters---------------------*)

let pwhitespace = take_while Char.is_whitespace
let pws1 = take_while1 Char.is_whitespace
let pstoken s = pwhitespace *> string s
let ptoken s = pwhitespace *> s
let pparens p = pstoken "(" *> p <* pstoken ")"
let psqparens p = pstoken "[" *> p <* pstoken "]"

(*-------------------------Constants/Variables-------------------------*)

let pint =
  pwhitespace *> take_while1 Char.is_digit
  >>= fun str ->
  match Stdlib.int_of_string_opt str with
  | Some n -> return (Int n)
  | None -> fail "Integer value exceeds the allowable range for the int type"
;;

let pbool =
  choice [ pstoken "true" *> return true; pstoken "false" *> return false ]
  >>| fun x -> Bool x
;;

let pstr =
  pwhitespace *> char '"' *> take_till (Char.equal '"') <* char '"' >>| fun x -> String x
;;

let punit = pstoken "()" *> return Unit
let const = choice [ pint; pbool; pstr; punit ]

let varname =
  ptoken
    (let* first_char =
       take_while1 (fun ch -> Char.is_lowercase ch || Char.equal ch '_')
     in
     let* rest =
       take_while (fun ch ->
         Char.is_alpha ch || Char.is_digit ch || Char.equal ch '_' || Char.equal ch '\'')
     in
     match first_char, rest with
     | _, _ when is_keyword (first_char ^ rest) ->
       fail "Variable name conflicts with a keyword"
     | "_", "" -> fail "Variable cannot be called _"
     | _ -> return (first_char ^ rest))
;;

let patomic_type =
  choice
    [ pstoken "int" *> return (TPrim "int")
    ; pstoken "string" *> return (TPrim "string")
    ; pstoken "bool" *> return (TPrim "bool")
    ]
;;

let plist_type ptype = ptype >>= fun t -> pstoken "list" *> return (TList t)

let ptuple_type ptype =
  let star = pstoken "*" in
  pparens
    (lift3
       (fun t1 t2 rest -> TTuple (t1, t2, rest))
       ptype
       (star *> ptype)
       (many (star *> ptype)))
;;

let rec pfun_type ptype =
  ptype
  >>= fun left ->
  pstoken "->" *> pfun_type ptype
  >>= (fun right -> return (TArrow (left, right)))
  <|> return left
;;

let ptype =
  let some_type =
    fix (fun typ ->
      let atom = patomic_type <|> pparens typ in
      let list = plist_type atom <|> atom in
      let tuple = ptuple_type list <|> list in
      let func = pfun_type tuple <|> tuple in
      func)
  in
  pstoken ":" *> (some_type >>| fun t -> Some t) <|> return None
;;

let pident = lift2 (fun t v -> Id (t, v)) varname ptype
let pat_var = pident >>| fun x -> PVar x
let pat_const = const >>| fun x -> PConst x
let pat_any = pstoken "_" *> return PAny

let rec pat_cons ppattern =
  let cons =
    ppattern
    >>= fun head ->
    pstoken "::" *> pat_cons ppattern
    >>= (fun tail -> return (PCons (head, tail)))
    <|> return head
  in
  pparens cons <|> cons
;;

let pat_tuple pat =
  let commas = pstoken "," in
  let tuple =
    lift3
      (fun p1 p2 rest -> PTuple (p1, p2, rest))
      pat
      (commas *> pat)
      (many (commas *> pat))
    <* pwhitespace
  in
  pparens tuple <|> tuple
;;

let pat_list pat =
  let semicols = pstoken ";" in
  psqparens (sep_by semicols pat >>| fun patterns -> PList patterns)
;;

let pat_option pat =
  lift
    (fun e -> POption e)
    (pstoken "Some" *> pat >>| (fun e -> Some e) <|> (pstoken "None" >>| fun _ -> None))
;;

let ppattern =
  fix (fun pat ->
    let patom = pat_const <|> pat_var <|> pat_any in
    let pptuple = pat_tuple (patom <|> pat) <|> patom in
    let pplist = pat_list (pptuple <|> pat) <|> pptuple in
    let poption = pat_option pplist <|> pplist in
    let pcons = pat_cons poption in
    pcons)
;;

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
    ; pbinop Lte "<="
    ; pbinop Gte ">="
    ; pbinop Lt "<"
    ; pbinop Gt ">"
    ]
;;

let logic = choice [ pbinop And "&&"; pbinop Or "||" ]

(*------------------Unary operators-----------------*)

let punop op token = pwhitespace *> pstoken token *> return (fun e1 -> Eun_op (op, e1))
let negation = punop Not "not" <* pws1
let neg_sign = punop Negative "-"
let pos_sign = punop Positive "+"

(*------------------------Expressions----------------------*)

let chain e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

let un_chain e op =
  fix (fun self -> op >>= (fun unop -> self >>= fun e -> return (unop e)) <|> e)
;;

let rec pbody pexpr =
  ppattern
  >>= function
  | PVar id ->
    many ppattern
    >>= fun patterns ->
    pbody pexpr <|> (pstoken "=" *> pexpr >>| fun e -> Efun (PVar id, patterns, e))
  | _ -> fail "Only variable patterns are supported"
;;

let pvalue_binding pexpr =
  lift2
    (fun id e -> Evalue_binding (id, e))
    (pparens pident <|> pident)
    (pstoken "=" *> pexpr <|> pbody pexpr)
;;

let plet pexpr =
  pstoken "let"
  *> lift4
       (fun r id id_list e2 -> Elet (r, id, id_list, e2))
       (pstoken "rec" *> (pws1 *> return Recursive) <|> return Non_recursive)
       (pvalue_binding pexpr)
       (many (pstoken "and" *> pvalue_binding pexpr))
       (pstoken "in" *> pexpr)
;;

let pEfun pexpr =
  lift3
    (fun arg args body -> Efun (arg, args, body))
    (pstoken "fun" *> ppattern)
    (many ppattern)
    (pstoken "->" *> pexpr)
;;

let pElist pexpr =
  let semicols = pstoken ";" in
  psqparens (sep_by semicols pexpr <* (semicols <|> pwhitespace) >>| fun x -> Elist x)
;;

let pEtuple pexpr =
  let commas = pstoken "," in
  pparens
    (lift3
       (fun e1 e2 rest -> Etuple (e1, e2, rest))
       pexpr
       (commas *> pexpr)
       (many (commas *> pexpr))
     <* pwhitespace)
;;

let pEconst = const >>| fun x -> Econst x
let pEvar = pident >>| fun x -> Evar x
let pEapp e = chain e (return (fun e1 e2 -> Efun_application (e1, e2)))

let pEoption pexpr =
  lift
    (fun e -> Eoption e)
    (pstoken "Some" *> pexpr >>| (fun e -> Some e) <|> (pstoken "None" >>| fun _ -> None))
;;

let pbranch pexpr =
  lift3
    (fun e1 e2 e3 -> Eif_then_else (e1, e2, e3))
    (pstoken "if" *> pexpr)
    (pstoken "then" *> pexpr)
    (pstoken "else" *> pexpr >>| (fun e3 -> Some e3) <|> return None)
;;

let pEmatch pexpr =
  let parse_case =
    lift2
      (fun pat exp -> Ecase (pat, exp))
      (ppattern <* pstoken "->")
      (pwhitespace *> pexpr)
  in
  lift3
    (fun e case case_l -> Ematch (e, case, case_l))
    (pstoken "match" *> pexpr <* pstoken "with")
    ((pstoken "|" <|> pwhitespace) *> parse_case)
    (many (pstoken "|" *> parse_case))
;;

let pEprint_int expr = lift (fun e -> Eprint_int e) (pstoken "print_int" *> pparens expr)

let pexpr =
  fix (fun expr ->
    let atom_expr =
      choice
        [ pEconst
        ; pEvar
        ; pparens expr
        ; pElist expr
        ; pEtuple expr
        ; pEfun expr
        ; pEoption expr
        ; pEmatch expr
        ; pEprint_int expr
        ]
    in
    let let_expr = plet expr in
    let ite_expr = pbranch (expr <|> atom_expr) <|> atom_expr in
    let app_expr = pEapp (ite_expr <|> atom_expr) <|> ite_expr in
    let un_expr =
      choice
        [ un_chain app_expr negation
        ; un_chain app_expr neg_sign
        ; un_chain app_expr pos_sign
        ]
    in
    let factor_expr = chain un_expr (mult <|> div) in
    let sum_expr = chain factor_expr (add <|> sub) in
    let rel_expr = chain sum_expr relation in
    let log_expr = chain rel_expr logic in
    choice [ let_expr; log_expr ])
;;

let pstructure =
  let pseval = pexpr >>| fun e -> SEval e in
  let psvalue =
    pstoken "let"
    *> lift3
         (fun r id id_list -> SValue (r, id, id_list))
         (pstoken "rec" *> (pws1 *> return Recursive) <|> return Non_recursive)
         (pvalue_binding pexpr)
         (many (pstoken "and" *> pvalue_binding pexpr))
  in
  choice [ pseval; psvalue ]
;;

let structure : structure t =
  sep_by (pstoken ";;") pstructure <* (pstoken ";;" <|> pwhitespace)
;;

let parse_expr str = parse_string ~consume:All structure str
