open Angstrom

let pp printer parser str =
  match parse_string ~consume:Angstrom.Consume.All parser str with
  | Ok res -> printer Format.std_formatter res
  | Error _ -> print_endline "Syntax error"
;;
