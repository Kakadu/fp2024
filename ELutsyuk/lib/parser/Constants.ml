(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Forest.Ast
open Base
open PrsAuxilary

(* https://ocaml.org/manual/5.3/lex.html#sss:character-literals *)

let prs_escape_sequence =
  let p_ascii =
    choice
      [ (let* num_opt = take_while1 Char.is_digit >>| Caml.int_of_string_opt in
         match num_opt with
         | Some char_code when char_code >= 0 && char_code <= 255 ->
           return @@ Char.of_int_exn char_code
         | _ -> fail "Invalid decimal escape sequence")
      ; (char 'x'
         *>
         let* scanned =
           take_while (function
             | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true
             | _ -> false)
         in
         let num_opt = Caml.int_of_string_opt ("0x" ^ scanned) in
         match num_opt with
         | Some char_code when char_code >= 0 && char_code <= 255 ->
           return @@ Char.of_int_exn char_code
         | _ -> fail "Invalid hex escape sequence")
      ; (char 'o'
         *>
         let* scanned = take 3 in
         let num_opt = Caml.int_of_string_opt ("0o" ^ scanned) in
         match num_opt with
         | Some char_code when char_code >= 0 && char_code <= 255 ->
           return @@ Char.of_int_exn char_code
         | _ -> fail "Invalid octal escape sequence")
      ]
  in
  char '\\'
  *> choice
       [ char '\\' *> return '\\'
       ; char '\"' *> return '\"'
       ; char '\'' *> return '\''
       ; char 'n' *> return '\n'
       ; char 't' *> return '\t'
       ; char 'b' *> return '\b'
       ; char 'r' *> return '\r'
       ; string "space" *> return ' '
       ; p_ascii
       ]
;;

let prs_str =
  let p_regular_str =
    let is_content ch = Char.( <> ) ch '"' && Char.( <> ) ch '\\' in
    let p_content = choice [ prs_escape_sequence; satisfy is_content ] in
    let+ content_char_list = string "\"" *> many p_content <* string "\"" in
    String.of_char_list content_char_list
  in
  let p_quoted_str =
    string "{|" *> take_while (fun ch -> Char.( <> ) ch '|') <* string "|}"
  in
  let+ parsed = p_regular_str <|> p_quoted_str in
  Str parsed
;;

let prs_int =
  (* trim
  @@ *)
  let+ parsed = take_while1 Char.is_digit >>| Int.of_string in
  Int parsed
;;

let prs_bool =
  trim
  @@
  let+ parsed = choice [ token "true" *> return true; token "false" *> return false ] in
  Bool parsed
;;

let prs_unit =
  trim
  @@
  let+ _ = token "()" in
  Unit
;;

let prs_const = choice [ prs_int; prs_str; prs_bool; prs_unit ]
