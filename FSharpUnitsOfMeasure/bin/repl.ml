(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Parse.Structure
open Checks
open Interp.Interpret
open Interp.Misc
open Format

type config = {mutable file_path : string option}

let pprog = Angstrom.parse_string ~consume:Angstrom.Consume.All pprog

let help_msg =
  "\n\
   F# with units of measure interpreter\n\n\
   Read and interpret file: dune exec ./bin/repl.exe --file <filepath>\n\
   REPL: dune exec ./bin/repl.exe\n\n\

   REPL commands:\n\
   help         Display usage message\n\
   quit         Quit the REPL mode\n"
;;

let greetings_msg =
  "───────────────────────────────┬──────────────────────────────────────────────────────────────┬───────────────────────────────\n\
  \                               │ Welcome to F# with units of measure interpreter \
   version 1.0! │                                \n\
  \                               \
   └──────────────────────────────────────────────────────────────┘                                "
;;

let hori_line =
  "───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────"
;;

let pp_env env =
  Base.Map.iteri
    ~f:(fun ~key ~data ->
      match Base.Map.find env key with
      | Some _ ->
        if not (is_builtin_fun key)
        then
          if is_builtin_op key
          then print_endline (Format.asprintf "val ( %s ) : %s = %a" key "<type>" pp_value data)
          else print_endline (Format.asprintf "val %s : %s = %a" key "<type>" pp_value data)
      | None -> ())
    env;
  printf "\n"
;;

let run_single options =
  (* let run text env = *)
  let run text =
    match pprog text with
    | Error _ -> print_endline (Format.asprintf "Syntax error");
    print_endline hori_line
    (* env *)
    | Ok ast ->
      (* Infer *)
      match eval ast with
      | Ok (env, out_lst) ->
        List.iter (fun v -> match v with
        | Ok v' -> print_endline (Format.asprintf "- : %s = %a" "<type>" pp_value v')
        | _ -> ()) out_lst;
        pp_env env;
        print_endline hori_line;
      | Error e -> print_endline (Format.asprintf "Interpreter error: %a" pp_error e);
      print_endline hori_line
    in
    let open In_channel in
    match options.file_path with
    | Some file_name ->
      let text = with_open_bin file_name input_all |> String.trim in
      let _ = run text in ()
    | None ->
      let rec input_lines lines =
        match input_line stdin with
        | Some line ->
          if String.ends_with ~suffix: ";;" line
          then (
          let _ = run (lines  ^ "\n" ^ line) in
          input_lines "")
          else input_lines (lines  ^ "\n" ^ line)
        | None -> ()
      in
      let _ = input_lines "" in ()
;;

let () =
  print_endline greetings_msg;
  let options = {file_path = None} in
  let () =
    let open Arg in
    parse
      [( "--file"
        , String (fun filename -> options.file_path <- Some filename)
        , "Read code from the file and interpret" )
      ] (fun _ -> exit 1) "REPL"
  in
  run_single options
;;
