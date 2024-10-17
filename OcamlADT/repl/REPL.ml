open Stdio

type config = {
  mutable dump_parsetree : bool;
  mutable source_file : string option;
}

let read_source source_opt =
  match source_opt with
  | Some filename -> In_channel.with_file filename ~f:In_channel.input_all
  | None -> In_channel.input_all stdin

let parse_code code =
  if String.length code > 0 then
    Ok ("Parsed AST for: " ^ code)
  else
    Error "Empty input, nothing to parse"

let process_input cfg =
  let code = read_source cfg.source_file in
  match parse_code code with
  | Ok ast ->
      if cfg.dump_parsetree then
        printf "AST: %s\n" ast
      else
        printf "Code executed: %s\n" code
  | Error err -> eprintf "Error during parsing: %s\n" err

let () =
  let config = { dump_parsetree = false; source_file = None } in
  let () = let open Arg in
    parse
      [ ( "-dparsetree"
        , Unit (fun () -> config.dump_parsetree <- true)
        , "Parse tree dump" )
      ; ( "-srcfile"
        , String (fun filename -> config.source_file <- Some filename)
        , "Read code from source file" ) ]

      (fun _ ->
        Format.eprintf "Zero arguments are not supported\n";
        exit 1)
      "REPL for Ocaml+ADT"
  in
  process_input config
;;
