open Ast
open Angstrom

let pconstint = 
  (** add sign *)
  let%map n = take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string in
 Const_integer n

let pconstchar = 
  let%map _ = char '\'' *> skip_while (function ' ' -> true | _ -> false) in
  let%map c = satisfy (function c -> c >= ' ' && c <= '~') <* char '\'' in
  Const_char c

let pconststring =
  let%map _ = char '\"' in 
  let%map s = take_while (function c -> c <> '\"') in 
  Const_string s




let pass_ws = take_while Char.is_whitespace

(** Parser that matches string literals an 's' skipping all whitespaces before *)
let token s = pass_ws *> string s

(* let pstructure_item *)

let psemicolon = many check_chunk ";;"
let pstructure = parse_semicolon *> many (pstr_item <* parse_semicolon)
let parse str = parse_string ~consume:All structure str

let parse_fact str = 
  match parse str with
  | Ok str -> str
  | Error msg -> failwith msg