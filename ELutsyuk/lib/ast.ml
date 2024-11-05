(* не пойму, зачем в каждом файле писать, что под MIT... *)
(* CFG grammar for OCaml https://askra.de/software/ocaml-doc/4.02/full-grammar.html#sec91 *)

type id = Id of string (* for expressions' identification *)
[@@deriving show { with_path = false }]

type const =
  | Int of int (* ex: 1, 2, 3 *)
  | Str of string (* ex: "hello", "miniML" *)
  | Bool of bool (* true, false *)
[@@deriving show { with_path = false }]

type binary_op =
  | Eq (* = *)
  | Less (* < *)
  | More (* > *)
  | And (* && *)
  | Or (* || *)
  | Plus (* + *)
  | Min (* - *)
  | Mult (* * *)
  | Div (* / *)
[@@deriving show { with_path = false }]

type unary_op =
  | Not (* ! *)
  | UnPlus (* + *)
  | UnMin (* - *)
[@@deriving show { with_path = false }]
