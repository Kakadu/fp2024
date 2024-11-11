(** Copyright 2024-2025, Sultanov Muhammet and Kudrya Alexander *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Base
open Ast

let is_whitespace = Char.is_whitespace
let is_digit = Char.is_digit

let is_uppercase = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_lowercase = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_letter c = is_uppercase c || is_lowercase c
let is_ident c = is_letter c || Char.equal '_' c

let is_keyword = function
  | "rec"
  | "end"
  | "then"
  | "fun"
  | "function"
  | "method"
  | "match"
  | "if"
  | "else"
  | "not"
  | "with"
  | "object"
  | "let"
  | "val"
  | "true"
  | "false"
  | "in" -> true
  | _ -> false
;;

(*=====================Control characters=====================*)

let ignore_spaces = take_while is_whitespace
let token1 p = ignore_spaces *> p
let token p = ignore_spaces *> string p
let left_paren = token "("
let right_paren = token ")"
let parse_in_parens p = left_paren *> p <* right_paren
let parse_in_brackets p = token "[" *> p <* token "]"
let double_semicolon = token ";;"

(*=====================Fold infix operators=====================*)

let parse_left_assoc expr oper =
  let rec continue acc =
    lift2 (fun func arg -> func acc arg) oper expr >>= continue <|> return acc
  in
  expr >>= continue
;;

let rec parse_right_assoc expr oper =
  expr
  >>= fun sub_expr ->
  oper >>= (fun func -> parse_right_assoc expr oper >>| func sub_expr) <|> return sub_expr
;;

(*=====================Constants=====================*)

let const_integer =
  token1 @@ take_while1 is_digit
  >>= fun s ->
  let num = Stdlib.int_of_string_opt s in
  match num with
  | Some n -> return @@ Int n
  | None -> fail "Invalid integer"
;;

let const_bool =
  token1 @@ take_while1 is_letter
  >>= function
  | "true" -> return @@ Bool true
  | "false" -> return @@ Bool false
  | _ -> fail "Invalid boolean"
;;

let const_float =
  token1 @@ take_while1 is_digit
  >>= fun s ->
  let num = Stdlib.float_of_string_opt s in
  match num with
  | Some n -> return @@ Float n
  | None -> fail "Invalid float"
;;

let const_nil = token "[]" *> return Nil
let const = choice [ const_integer; const_float; const_bool; const_nil ]

(*=====================Identifiers=====================*)

let check_ident i =
  match i with
  | i when is_keyword i -> fail "keyword"
  | "_" -> fail "unexpected wildcard"
  | _ -> return i
;;

let ident =
  token1 peek_char
  >>= (function
         | Some x when Char.equal x '_' || is_lowercase x -> return x
         | _ -> fail "Invalid identifier")
  >>= fun _ -> take_while is_ident >>= check_ident
;;

(*=====================Patterns=====================*)

let pat_const = const >>| fun p -> Pat_constant p
let pat_cons = token "::" *> return (fun a b -> Pat_cons (a, b))
let pat_any = token "_" *> ignore_spaces *> return Pat_any
let pat_val = ident >>| fun c -> Pat_var c
let tuple ident f = lift2 (fun h tl -> f @@ (h :: tl)) ident (many1 (token "," *> ident))
let pat_tuple pat = parse_in_parens (tuple pat (fun ps -> Pat_tuple ps))

let pattern =
  fix (fun pattern ->
    let term =
      choice [ parse_in_parens pattern; pat_const; pat_any; pat_val; pat_tuple pattern ]
    in
    let cons = parse_in_parens (parse_right_assoc term pat_cons) in
    cons <|> term)
;;

(*=====================Expressions=====================*)

let exp_const = const >>| fun c -> Exp_constant c
let exp_val = ident >>| fun c -> Exp_ident c
let exp_cons = token "::" *> return (fun a b -> Exp_list (a, b))

let exp_list expr =
  let rec creaet_list = function
    | [] -> Exp_constant Nil
    | h :: tl -> Exp_list (h, creaet_list tl)
  in
  let basic_list = parse_in_brackets @@ sep_by (token ";") expr >>| creaet_list in
  let cons_list = parse_right_assoc (expr <|> basic_list) exp_cons in
  basic_list <|> cons_list
;;

let exp_tuple expr = tuple expr (fun es -> Exp_tuple es)
let exp_apply expr = parse_left_assoc expr (return (fun f a -> Exp_apply (f, [ a ])))

let exp_ifthenelse expr =
  lift3
    (fun b t e -> Exp_ifthenelse (b, t, e))
    (token "if" *> expr)
    (token "then" *> expr)
    (option None (token "else" *> expr >>| Option.some))
;;

let exp_fun pexpr =
  token "fun" *> many1 pattern
  >>= fun args ->
  token "->" *> pexpr
  >>= fun e ->
  match List.rev args with
  | h :: tl ->
    return
      (List.fold_left
         ~init:(Exp_function ([ h ], e))
         ~f:(fun acc x -> Exp_function ([ x ], acc))
         tl)
  | _ -> fail "Invalid function"
;;

(* let val_binding pat exp = { pat; exp } *)
let efun i e = Exp_function ([ i ], e)
let exp_pattern_matching pexpr = lift2 (fun k v -> k, v) (pattern <* token "->") pexpr

let exp_match pexpr =
  token "match"
  *> lift2
       (fun v ptrns -> Exp_match (v, ptrns))
       (pexpr <* token "with")
       (exp_pattern_matching pexpr
        <|> token "|" *> exp_pattern_matching pexpr
        >>= fun p -> many (token "|" *> exp_pattern_matching pexpr) >>| fun ps -> p :: ps
       )
;;

let exp_decl pexpr =
  let exp =
    ignore_spaces *> many pattern
    >>= fun args ->
    token "=" *> pexpr
    >>= fun e ->
    match List.rev args with
    | h :: tl -> return (List.fold_left ~init:(efun h e) ~f:(fun acc x -> efun x acc) tl)
    | _ -> return e
  in
  token "let"
  *> lift3
       (fun d_rec d_pat d_exp -> { d_rec; d_pat; d_exp })
       (token "rec" *> return Rec <|> return Nonrec)
       (token1 pattern)
       exp
;;

let elet d e = Exp_let (d, e)
let exp_let pexpr = lift2 elet (exp_decl pexpr) (token "in" *> pexpr)

let exp_sinvk pexpr =
  let iter = token "#" *> ident in
  let rec helper acc =
    iter >>= fun sub -> helper (Exp_send (acc, sub)) <|> return (Exp_send (acc, sub))
  in
  let acc = lift2 (fun s m -> Exp_send (s, m)) pexpr (token "#" *> ident) in
  acc >>= helper <|> acc
;;

let exp_override pexpr =
  token "{<"
  *> lift
       (fun es -> Exp_override es)
       (sep_by (token ";") (ident >>= fun id -> token "=" *> pexpr >>| fun e -> id, e))
  <* token ">}"
;;

let exp_object pexpr =
  let ov =
    lift2 (fun a b -> Obj_val (a, b)) (token "val" *> ident) (token "=" *> pexpr)
  in
  let helper =
    ignore_spaces *> many pattern
    >>= fun args ->
    token "=" *> pexpr
    >>| fun e ->
    match List.rev args with
    | h :: tl ->
      List.fold_left
        ~init:(Exp_function ([ h ], e))
        ~f:(fun acc x -> Exp_function ([ x ], acc))
        tl
    | _ -> e
  in
  let om =
    lift3
      (fun a b c -> Obj_method (a, b, c))
      (token "method" *> token "private" *> return Private
       <|> token "method" *> return Public)
      ident
      helper
  in
  let self_pat = token "self" *> return (Pat_var "self") in
  token "object"
  *> lift2
       (fun self fields -> Exp_object { self; fields })
       (option Pat_any (parse_in_parens (pat_any <|> self_pat)))
       (many (ov <|> om) <* token "end")
;;

(*=====================Binary/Unary operators=====================*)

let bin_op chain1 e ops = chain1 e (ops >>| fun o l r -> Exp_binary (l, o, r))
let left_bin_op = bin_op parse_left_assoc
let right_bin_op = bin_op parse_right_assoc
let op l = choice (List.map ~f:(fun (o, n) -> token o *> return n) l)
let mult_div = op [ "*", Mult; "/", Div ]
let add_sub = op [ "+", Add; "-", Subt ]

let compare =
  op
    [ "<=", LessEqual
    ; "<", LessThan
    ; ">=", GreaterEqual
    ; ">", GreaterThan
    ; "=", Equal
    ; "<>", NotEqual
    ]
;;

let and_op = op [ "&&", And ]
let or_op = op [ "||", Or ]
let neg = choice [ token "not" *> return Not; token "-" *> return Neg ]

let expr =
  fix (fun pexpr ->
    let sube = choice [ parse_in_parens pexpr; exp_const; exp_val ] in
    let send = exp_sinvk sube in
    let term = exp_apply (send <|> sube) in
    let term =
      left_bin_op
        (term <|> (neg >>= fun op -> term >>| fun exp -> Exp_unary (op, exp)))
        mult_div
    in
    let term = left_bin_op term add_sub in
    let term = exp_list term <|> term in
    let term = left_bin_op term compare in
    let term = right_bin_op term and_op in
    let term = right_bin_op term or_op in
    let term = exp_tuple term <|> term in
    choice
      [ exp_ifthenelse pexpr
      ; exp_let pexpr
      ; exp_match pexpr
      ; exp_fun pexpr
      ; exp_object pexpr
      ; exp_override pexpr
      ; term
      ])
;;

let del = (double_semicolon <|> ignore_spaces) *> ignore_spaces

let str_item =
  expr
  >>| (fun e -> Str_eval e)
  <* double_semicolon
  <|> (token1 (exp_decl expr) >>| fun v -> Str_value v)
;;

let program = del *> many1 (str_item <* del)

(* Define a standard error message for parsing issues *)
let syntax_error_msg = "Syntax error"

(* Parser function that returns an error message wrapped in a Result *)
let parse s =
  match parse_string ~consume:All program s with
  | Ok v -> Ok v
  | Error _ -> Error syntax_error_msg
;;

(* Parser function that handles prefixes, also returning a standard error message *)
let parse_prefix s =
  match parse_string ~consume:Prefix program s with
  | Ok v -> Ok v
  | Error _ -> Error syntax_error_msg
;;
