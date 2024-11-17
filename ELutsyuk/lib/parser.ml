(* Copyright 2024, Victoria Lutsyuk *)

(* SPDX-License-Identifier: MIT *)

open Angstrom

(*
   from homk: >>| : 'a t -> ('a -> 'b) -> 'b
   let* is a bind
*)

let parse_vica =
  let* v = char 'V' in
  let* i = char 'i' in
  let* c = char 'c' in
  let* a = char 'a' in
  return [ v; i; c; a ]
;;

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_keyword = function
  | "Some"
  | "None"
  | "rec"
  | "let"
  | "fun"
  | "if"
  | "then"
  | "else"
  | "while"
  | "match"
  | "in" -> true
  | _ -> false
;;

let parse_spaces = skip_while is_space
let parse_token t = parse_spaces *> t

let parse_id =
  let parse_first =
    satisfy (fun ch -> is_letter ch || Char.equal '_' ch) >>| fun ch -> Char.escaped ch
  in
  let parse_rest =
    take_while (fun ch -> is_letter ch || is_digit ch || Char.equal '_' ch)
  in
  (* мб функцию применения вынести? *)
  parse_token @@ lift2 (fun x y -> x ^ y) parse_first parse_rest
  >>= fun id ->
  if is_keyword id then fail "Identifier must not match the keyword." else return id
;;
