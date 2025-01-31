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
  | "function"
  | "type" -> true
  | _ -> false
;;

(*---------------------Control characters---------------------*)

let pwhitespace = take_while Char.is_whitespace
let pws1 = take_while1 Char.is_whitespace
let pstoken s = pwhitespace *> string s
let ptoken s = pwhitespace *> s
let pparens p = pstoken "(" *> p <* pstoken ")"
let psqparens p = pstoken "[" *> p <* pstoken "]"

(*------------------Prefix operators-----------------*)

let ppref_op =
  let pref_op =
    ptoken
      (let* first_char =
         take_while1 (function
           | '|'
           | '~'
           | '?'
           | '<'
           | '>'
           | '!'
           | '&'
           | '*'
           | '/'
           | '='
           | '+'
           | '-'
           | '@'
           | '^' -> true
           | _ -> false)
       in
       let* rest =
         take_while (function
           | '.'
           | ':'
           | '|'
           | '~'
           | '?'
           | '<'
           | '>'
           | '!'
           | '&'
           | '*'
           | '/'
           | '='
           | '+'
           | '-'
           | '@'
           | '^' -> true
           | _ -> false)
       in
       match first_char, rest with
       | "|", "" -> fail "Prefix operator cannot be called | "
       | "~", "" -> fail "Prefix operator cannot be called ~ "
       | "?", "" -> fail "Prefix operator cannot be called ? "
       | _ -> return (Id (first_char ^ rest)))
  in
  pparens pref_op
;;

let pEinf_op pexpr =
  ppref_op
  >>= fun inf_op ->
  lift2
    (fun left right -> Efun_application (Efun_application (Evar inf_op, left), right))
    (pws1 *> pexpr)
    (pwhitespace *> pexpr)
;;

(* let pEinf_op =
   pwhitespace *> pinf_op >>= fun inf_op -> return (fun e1 e2 -> Efun_application (Efun_application (Evar inf_op, e1), e2))
   ;; *)

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
    ; pstoken "unit" *> return (TPrim "unit")
    ]
;;

let plist_type ptype_opt = ptype_opt >>= fun t -> pstoken "list" *> return (TList t)

let ptuple_type ptype_opt =
  let star = pstoken "*" in
  lift3
    (fun t1 t2 rest -> TTuple (t1, t2, rest))
    ptype_opt
    (star *> ptype_opt)
    (many (star *> ptype_opt))
;;

let rec pfun_type ptype_opt =
  ptype_opt
  >>= fun left ->
  pstoken "->" *> pfun_type ptype_opt
  >>= (fun right -> return (TArrow (left, right)))
  <|> return left
;;

let poption_type ptype_opt = ptype_opt >>= fun t -> pstoken "option" *> return (TOption t)
(* let precord_type = varname >>= fun t -> return (TRecord t) *)

let ptype_helper =
  fix (fun typ ->
    (* let atom = patomic_type <|> pparens typ <|> precord_type in *)
    let atom = patomic_type <|> pparens typ in
    let list = plist_type atom <|> atom in
    let option = poption_type list <|> list in
    let tuple = ptuple_type option <|> option in
    let func = pfun_type tuple <|> tuple in
    func)
;;

let ptype =
  let t = ptype_helper in
  pstoken ":" *> t
;;

let pident = lift (fun t -> Id t) varname <|> ppref_op
let pat_var = pident >>| fun x -> PVar x
let pat_const = const >>| fun x -> PConst x
let pat_any = pstoken "_" *> return PAny

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

let rec pat_cons pat =
  let cons =
    pat
    >>= fun head ->
    pstoken "::" *> pat_cons pat
    >>= (fun tail -> return (PCons (head, tail)))
    <|> return head
  in
  pparens cons <|> cons
;;

let pat_option pat =
  lift
    (fun e -> POption e)
    (pstoken "Some" *> pat >>| (fun e -> Some e) <|> (pstoken "None" >>| fun _ -> None))
;;

let pat_ty pat =
  let ty_pat = lift2 (fun pat ty -> PConstraint (pat, ty)) pat ptype in
  ty_pat <|> pparens ty_pat
;;

let ppattern =
  fix (fun pat ->
    let patom =
      pat_const <|> pat_var <|> pat_any <|> pparens pat <|> pparens (pat_ty pat)
    in
    let poption = pat_option patom <|> patom in
    let pptuple = pat_tuple poption <|> poption in
    let pplist = pat_list pptuple <|> pptuple in
    let pcons = pat_cons pplist <|> pplist in
    let pty = pat_ty pcons <|> pcons in
    pty)
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
let cons = pbinop Cons "::"

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

let rec chainr e op =
  let* left = e in
  (let* f = op in
   let* right = chainr e op in
   return (f left right))
  <|> return left
;;

let un_chain e op =
  fix (fun self -> op >>= (fun unop -> self >>= fun e -> return (unop e)) <|> e)
;;

let rec pbody pexpr =
  ppattern
  >>= fun p ->
  many ppattern
  >>= fun patterns ->
  pbody pexpr <|> (pstoken "=" *> pexpr >>| fun e -> Efun (p, patterns, e))
;;

let pvalue_binding pexpr =
  lift2
    (fun ty_pattern expr -> Evalue_binding (ty_pattern, expr))
    ppattern
    (pstoken "=" *> pexpr <|> pbody pexpr)
;;

let plet pexpr =
  pstoken "let"
  *> lift4
       (fun rec_flag value_bindings and_bindings body ->
         Elet (rec_flag, value_bindings, and_bindings, body))
       (pstoken "rec" *> (pws1 *> return Recursive) <|> return Non_recursive)
       (pvalue_binding pexpr)
       (many (pstoken "and" *> pvalue_binding pexpr))
       (pstoken "in" *> pexpr)
;;

let pEfun pexpr =
  (* if there's only one argument, ascription without parentheses is possible *)
  let single_arg =
    lift2
      (fun arg body -> Efun (arg, [], body))
      (pstoken "fun" *> pws1 *> ppattern)
      (pstoken "->" *> pexpr)
  in
  let mult_args =
    lift3
      (fun arg args body -> Efun (arg, args, body))
      (pstoken "fun" *> pws1 *> ppattern)
      (many ppattern)
      (pstoken "->" *> pexpr)
  in
  single_arg <|> mult_args
;;

let pElist pexpr =
  let semicols = pstoken ";" in
  psqparens (sep_by semicols pexpr <* (semicols <|> pwhitespace) >>| fun x -> Elist x)
;;

let pEtuple pexpr =
  let commas = pstoken "," in
  let tuple =
    lift3
      (fun e1 e2 rest -> Etuple (e1, e2, rest))
      (pexpr <* commas)
      pexpr
      (many (commas *> pexpr))
    <* pwhitespace
  in
  pparens tuple <|> tuple
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
  let match_cases =
    lift3
      (fun e case case_l -> Ematch (e, case, case_l))
      (pstoken "match" *> pexpr <* pstoken "with")
      ((pstoken "|" <|> pwhitespace) *> parse_case)
      (many (pstoken "|" *> parse_case))
  in
  let function_cases =
    lift2
      (fun case case_l -> Efunction (case, case_l))
      (pstoken "function" *> pstoken "|" *> parse_case
       <|> pstoken "function" *> pwhitespace *> parse_case)
      (many (pstoken "|" *> parse_case))
  in
  function_cases <|> match_cases
;;

let pEconstraint pexpr = lift2 (fun expr t -> Econstraint (expr, t)) pexpr ptype

(*------------------Records-----------------*)

(* let pbraces p = pstoken "{" *> p <* pstoken "}"
let plabel_name = lift (fun t -> Label t) varname

let pErecord pexpr =
  let precord_field =
    lift2
      (fun label_name expr -> Erecord_field (label_name, expr))
      plabel_name
      (pstoken "=" *> pexpr)
  in
  pbraces
    (lift2
       (fun record_field record_fields -> Erecord (record_field, record_fields))
       precord_field
       (many (pstoken ";" *> precord_field))
     <* (pstoken ";" <|> pwhitespace))
;;

let pfield pexpr = pparens (pEconstraint pexpr) <|> pexpr 

 let pEfield_access pexpr =
  let base_expr = pfield pexpr in
  let rec parse_fields acc =
    pstoken "." *> plabel_name
    >>= fun field_name ->
    let new_expr = Efield_access (acc, field_name) in
    parse_fields new_expr <|> return new_expr
  in
  base_expr >>= fun base -> parse_fields base
;; *)

let pexpr =
  fix (fun expr ->
    let atom_expr =
      choice
        [ pEconst
        ; pEvar
        ; pparens expr
        ; pElist expr
        ; pEfun expr
        ; pEoption expr
        ; pEmatch expr (* ; pErecord expr *)
        ; pparens (pEconstraint expr)
        ]
    in
    let let_expr = plet expr in
    let ite_expr = pbranch (expr <|> atom_expr) <|> atom_expr in
    let inf_op = pEinf_op (ite_expr <|> atom_expr) <|> ite_expr in
    let app_expr = pEapp (inf_op <|> atom_expr) <|> inf_op in
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
    let tuple_expr = pEtuple log_expr <|> log_expr in
    (* let field_expr = pEfield_access tuple_expr <|> tuple_expr in
       let cons_expr = chainr field_expr cons in *)
    let cons_expr = chainr tuple_expr cons in
    choice [ let_expr; cons_expr ])
;;

(* let pfield_decl =
   lift2 (fun label_name t -> Sfield_decl (label_name, t)) plabel_name ptype
   ;; *)

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
  (* let pstype =
    lift3
      (fun name field fields -> SType (name, field, fields))
      (pstoken "type" *> varname)
      (pstoken "=" *> pstoken "{" *> pfield_decl)
      (many (pstoken ";" *> pfield_decl) <* (pstoken ";" <|> pwhitespace) <* pstoken "}")
  in *)
  choice [ pseval; psvalue ]
;;

let structure : structure t =
  let semicolons = many (pstoken ";;") in
  sep_by semicolons pstructure <* semicolons <* pwhitespace
;;

let parse_expr str = parse_string ~consume:All structure str
