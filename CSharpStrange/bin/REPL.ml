open C_sharp_strange_lib.Ast
open C_sharp_strange_lib.Parser
open Printf
open Stdio

type opts =
  { mutable dump_parse_tree : bool
  ; mutable file_path : string option
  }

let () =
  let opts = { dump_parse_tree = false; file_path = None } in
  let _ =
    Arg.parse
      [ "-parseast", Arg.Unit (fun () -> opts.dump_parse_tree <- true), "\n"
      ; "-filepath", Arg.String (fun file_path -> opts.file_path <- Some file_path), "\n"
      ]
      (fun _ ->
         Stdlib.Format.eprintf "Something got wrong\n";
         Stdlib.exit 1)
      "\n"
  in
  let path =
    match opts.file_path with
    | None -> String.trim @@ In_channel.input_all stdin
    | Some path -> String.trim @@ In_channel.read_all path
  in
  match apply_parser parse_prog path with
  | Ok ast -> if opts.dump_parse_tree then print_endline (show_program ast)
  | Error msg -> failwith (sprintf "Failed to parse file: %s" msg)
;;
