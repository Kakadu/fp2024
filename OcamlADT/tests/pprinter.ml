open Ocamladt_lib.Parser
open Ocamladt_lib.Ast

let pp printer parser str =
  match Angstrom.parse_string ~consume:Angstrom.Consume.All parser str with
  | Ok res -> printer Format.std_formatter res
  | Error _ -> print_endline "Syntax error"