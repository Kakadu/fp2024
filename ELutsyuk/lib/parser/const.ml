(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Ast
open Base
open Auxilary

let prs_int =
  let+ parsed = take_while1 Char.is_digit >>| Int.of_string in
  Int parsed
;;

(* https://ocaml.org/manual/5.3/lex.html#sss:character-literals *)

let prs_escape_sequence =
  let p_ascii =
    choice
      [ (let* num_opt = take_while Char.is_digit >>| Caml.int_of_string_opt in
         match num_opt with
         | Some char_code when char_code >= 0 && char_code <= 255 ->
           return @@ Char.of_int_exn char_code
         | _ -> fail "Invalid decimal escape sequence")
      ; (char 'x'
         *>
         let* scaned =
           take_while (function
             | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true
             | _ -> false)
         in
         let num_opt = Caml.int_of_string_opt ("0x" ^ scaned) in
         match num_opt with
         | Some char_code when char_code >= 0 && char_code <= 255 ->
           return @@ Char.of_int_exn char_code
         | _ -> fail "Invalid hex escape sequence")
      ; (char 'o'
         *>
         let* scaned = take 3 in
         let num_opt = Caml.int_of_string_opt ("0o" ^ scaned) in
         match num_opt with
         | Some char_code when char_code >= 0 && char_code <= 255 ->
           return @@ Char.of_int_exn char_code
         | _ -> fail "Invalid octal escape sequence")
      ]
  in
  char '\\'
  *> choice
       [ char '\\' *> return "\\"
       ; char '"' *> return "\""
       ; char '\'' *> return "\'"
       ; char 'n' *> return "\n"
       ; char 't' *> return "\t"
       ; char 'b' *> return "\b"
       ; char 'r' *> return "\r"
       ; string "space" *> return " "
       ; (let+ symbol = p_ascii >>| Char.escaped in
          symbol)
       ]
;;

let prs_str =
  let p_regular_str =
    let is_content ch = Char.( <> ) ch '"' && Char.( <> ) ch '\\' in
    let p_content =
      fix (fun p_content ->
        choice
          [ prs_escape_sequence
          ; satisfy is_content >>| Char.escaped
          ; p_content
          ; end_of_input *> fail "No closing quote"
          ])
    in
    let+ str_content_list = token "\"" *> many p_content <* token "\"" in
    String.concat str_content_list
  in
  let p_quotes_str = token "{|" *> take_while (fun _ -> true) <* token "|}" in
  let+ parsed = p_regular_str <|> p_quotes_str in
  Str parsed
;;

let prs_bool =
  let+ parsed = choice [ token "true" *> return true; token "false" *> return false ] in
  Bool parsed
;;

let prs_unit =
  let+ _ = token "()" in
  Unit
;;

let prs_const = choice [ prs_int; prs_str; prs_bool; prs_unit ]
