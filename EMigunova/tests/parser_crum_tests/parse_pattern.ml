open Angstrom
open EMigunova_lib.Ast

let test =
  parse_string
    ~consume:Prefix
    Parse.parse_pattern
    "(([d;Some Some (((df)))]::d,Some t::io))"
;;
