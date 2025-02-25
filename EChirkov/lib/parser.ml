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
let p_sign = option "+" (token "-" <|> token "+")
let parens s = token "(" *> s <* token ")"

let newline =
  skip_while (function
    | ' ' | '\t' -> true
    | _ -> false)
  *> char '\n'
;;

let newlines = skip_many1 newline
let p_digits = take_while1 is_digit

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init <|> fail (Expected "operator" |> pp_error)
;;

let p_rec_flag =
  choice [ take_while1 is_ws *> token "rec" *> return Recursive; return Nonrecursive ]
;;

(* patterns *)

let p_pattern = fix @@ fun p -> return PAny

(* ========== consts ========== *)

let p_string =
  token "\"" *> take_till (Char.equal '\"') <* token "\"" >>| fun s -> CString s
;;

let p_integer = lift2 (fun s n -> CInt (Int.of_string (s ^ n))) p_sign p_digits

let p_boolean =
  let t = token "true" *> return (CBool true) in
  let f = token "false" *> return (CBool false) in
  choice [ t; f ]
;;

let p_unit = token "()" *> return CUnit

let p_const =
  choice
    ~failure_msg:(Expected "a constant (integer, string, boolean, unit)" |> pp_error)
    [ p_integer; p_string; p_boolean; p_unit ]
;;

let p_variable =
  let* _ = ws in
  let* first_char = peek_char_fail in
  match first_char with
  | 'a' .. 'z' | '_' ->
    let* name = take_while is_id in
    (match name with
     | "_" -> fail (WildcardUsed |> pp_error)
     | name when is_keyword name -> fail (ReservedKeyword name |> pp_error)
     | name -> return name)
  | _ -> fail (UnexpectedToken "Expected an identifier" |> pp_error)
;;

(* ========== exprs ========== *)

let p_list e =
  token "[" *> sep_by (token ";") e <* token "]" >>| fun e e_rest -> EList (e, e_rest)
;;

let p_expression =
  fix
  @@ fun e ->
  let term =
    choice
      [ parens e
      ; (p_variable >>| fun v -> EVar v)
      ; (p_const >>| fun e -> EConst e)
      ; p_list e
      ]
  in
  term
;;

(* let p_e_apply = chainl1 term (return (fun e1 e2 -> EApply (e1, e2)))
   in p_e_apply *)

(* ========== top level ========== *)

let p_binding = lift2 (fun p e -> p, e) p_pattern p_expression

(* let p_structure_eval = p_expression >>| fun e -> return (SEval e) *)
(*
   let p_structure_value =
   lift3
   (fun rf b bl -> return (SValue (rf, b, bl)))
   (token "let" *> p_rec_flag)
   p_binding
   (many (token "and" *> p_binding))
   ;;

   let p_structure_item =
   p_structure_value (* <|> p_structure_eval *)
   ;; *)

let p_program = many (p_expression <* newlines <* ws <|> (p_expression <* ws))

(* actuall parser function *)

let parse s = parse_string ~consume:All p_program s
let test_parse (s : string) (r : program) : bool = parse s = Result.Ok r
let%test "integer" = test_parse "7" [ EConst (CInt 7) ]
