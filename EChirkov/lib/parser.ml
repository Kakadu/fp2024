(** Copyright 2024-2025, Dmitri Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom
open Base

(* ========== errors ========== *)

type error =
  | UnexpectedToken of string
  | Expected of string
  | ReservedKeyword of string
  | WildcardUsed

let pp_error = function
  | UnexpectedToken tok -> "Unexpected token: " ^ tok
  | Expected msg -> "Expected: " ^ msg
  | ReservedKeyword name -> "Reserved keyword cannot be used: " ^ name
  | WildcardUsed -> "Wildcard '_' cannot be used as a variable name."
;;

(* ========== basic ========== *)

let is_ws = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false
;;

let ws = take_while is_ws

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_keyword = function
  | "let" | "in" | "if" | "then" | "else" | "fun" | "rec" | "true" | "false" | "and" ->
    true
  | _ -> false
;;

let is_id c = Char.is_alphanum c || Char.equal c '_' || Char.equal c '\''
let token str = ws *> string str
let parens s = token "(" *> s <* token ")"
let p_digits = take_while1 is_digit

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init <|> fail (Expected "operator" |> pp_error)
;;

let p_rec_flag =
  choice [ token "rec" *> take_while1 is_ws *> return Recursive; return Nonrecursive ]
;;

(* ========== consts ========== *)

(* let p_string =
   token "\"" *> take_till (Char.equal '\"') <* token "\"" >>| fun s -> CString s
   ;; *)

let p_integer =
  let* num = ws *> p_digits in
  let* next = peek_char in
  match next with
  | Some ('a' .. 'z' | '_') -> fail "unexpected character after number"
  | _ -> return (CInt (Int.of_string num))
;;

let p_boolean =
  let t = token "true" *> return (CBool true) in
  let f = token "false" *> return (CBool false) in
  choice [ t; f ]
;;

let p_unit = token "()" *> return CUnit

let p_const =
  choice
    ~failure_msg:(Expected "a constant (integer, string, boolean, unit)" |> pp_error)
    [ p_integer; (*p_string;*) p_boolean; p_unit ]
;;

let p_variable =
  let* _ = ws in
  let* first_char = peek_char_fail in
  match first_char with
  | 'a' .. 'z' | '_' ->
    let* name = take_while is_id in
    (match name with
     | "_" -> fail (pp_error WildcardUsed)
     | name when is_keyword name -> fail (pp_error (ReservedKeyword name))
     | name -> return name)
  | _ -> fail (pp_error (UnexpectedToken "Expected an identifier"))
;;

(* ========== type anonotaions ========== *)

let rec p_tlist t =
  let* t = t in
  let* _ = token "list" in
  p_tlist (return (ty_list t)) <|> return (ty_list t)
;;

let p_toption t =
  let* t = t in
  let* _ = token "option" in
  return (ty_option t)
;;

let p_ttuple t =
  let* f = t in
  let* s = token "*" *> t in
  let+ rest = many (token "*" *> t) in
  ty_tuple (f, s, rest)
;;

let p_type =
  fix
  @@ fun t ->
  let types =
    choice
      [ token "int" *> return ty_int
      ; token "bool" *> return ty_bool
      ; token "unit" *> return ty_unit
      ]
    <|> parens t
  in
  let t_list = p_tlist types <|> types in
  let t_opt = p_toption t_list <|> t_list in
  let t_tup = p_ttuple t_opt <|> t_opt in
  t_tup
;;

let p_etype e =
  let* e = token "(" *> e in
  let+ ty = token ":" *> p_type <* token ")" in
  EType (e, ty)
;;

(* ========== patterns ========== *)

let p_any = token "_" *> return PAny

let p_ptuple e =
  let tuple =
    lift3
      (fun e1 e2 rest -> PTuple (e1, e2, rest))
      (e <* token ",")
      e
      (many (token "," *> e))
    <* ws
  in
  parens tuple <|> tuple
;;

let p_plist e =
  token "[" *> sep_by (token ";" *> ws) e <* token "]" >>| fun es -> PList es
;;

let p_poption e =
  choice
    [ token "None" *> return (POption None)
    ; (token "Some" *> choice [ parens e; e ] >>| fun e -> POption (Some e))
    ]
;;

let p_pattern =
  fix
  @@ fun p ->
  let term =
    choice
      [ (p_variable >>| fun v -> PVar v)
      ; (p_unit >>| fun _ -> PConst CUnit)
      ; p_poption p (* ; p_ptype p *)
      ; p_plist p
      ; p_any
      ]
  in
  let tuples = p_ptuple term <|> term in
  tuples
;;

(* ========== exprs ========== *)

let p_list e = token "[" *> sep_by (token ";" *> ws) e <* token "]" >>| fun es -> EList es

let p_branch e =
  lift3
    (fun ei et ee -> EIf (ei, et, ee))
    (token "if" *> e)
    (token "then" *> e)
    (option None (token "else" *> e >>| fun ee -> Some ee))
;;

let p_binop tkn binop = token tkn *> return (fun el er -> EBinary (binop, el, er)) <* ws

let p_unop e =
  lift2
    (fun unop e -> EUnary (unop, e))
    (choice [ token "-" *> return Neg <* ws; token "+" *> return Pos <* ws ])
    e
;;

let p_tuple e =
  let tuple =
    lift3
      (fun e1 e2 rest -> ETuple (e1, e2, rest))
      (e <* token ",")
      e
      (many (token "," *> e))
    <* ws
  in
  parens tuple <|> tuple
;;

let p_option e =
  choice
    [ token "None" *> return (EOption None)
    ; (token "Some" *> choice [ parens e; e ] >>| fun e -> EOption (Some e))
    ]
;;

let p_fun e =
  let* ps = token "fun" *> many1 p_pattern in
  let+ e = token "->" *> e in
  List.fold_right ps ~init:e ~f:(fun p e -> EFun (p, e))
;;

let p_binding expr =
  let* p = p_pattern in
  let* ps = many p_pattern <* token "=" <* ws in
  let+ e = p_etype expr <|> expr in
  p, List.fold_right ps ~init:e ~f:(fun p e -> EFun (p, e))
;;

let p_let expr =
  lift4
    (fun rf b bl e -> ELet (rf, b, bl, e))
    (token "let" *> p_rec_flag)
    (p_binding expr)
    (many (token "and" *> p_binding expr))
    (token "in" *> expr)
;;

let p_expression =
  fix
  @@ fun e ->
  let term = parens e in
  let term = p_const >>| (fun e -> EConst e) <|> term in
  let term = p_etype e <|> term in
  let term = p_variable >>| (fun v -> EVar v) <|> term in
  let term = p_list e <|> term in
  let apply = chainl1 term (return (fun e1 e2 -> EApply (e1, e2))) in
  let opt = p_option apply <|> apply in
  let branch = p_branch e <|> opt in
  let unary_op = branch <|> p_unop branch in
  let multiplydivide_op = chainl1 unary_op (p_binop "*" Mul <|> p_binop "/" Div) in
  let plusminus_op = chainl1 multiplydivide_op (p_binop "+" Add <|> p_binop "-" Sub) in
  let compare_op =
    chainl1
      plusminus_op
      (choice
         [ p_binop "=" Eq
         ; p_binop "<>" NEq
         ; p_binop "<=" Lte
         ; p_binop "<" Lt
         ; p_binop ">=" Gte
         ; p_binop ">" Gt
         ])
  in
  let bool_op = chainl1 compare_op (p_binop "&&" And <|> p_binop "||" Or) in
  let tuples = p_tuple bool_op <|> bool_op in
  choice [ tuples; p_let e; p_fun e ]
;;

(* ========== top level ========== *)

let p_structure_item =
  lift3
    (fun rf b bl -> SValue (rf, b, bl))
    (token "let" *> p_rec_flag)
    (p_binding p_expression)
    (many (token "and" *> p_binding p_expression))
;;

let p_program = many p_structure_item <* ws

(* parse *)

let parse s = parse_string ~consume:All p_program s
