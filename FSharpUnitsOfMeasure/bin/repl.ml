(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open Parse.Structure
open Checks
open Interp.Interpret
open Interp.Misc
open Format
open Inference
open Inference.Scheme
open Inference.Infer

type config =
  { mutable file_path : string option
  ; mutable do_not_type : bool
  ; mutable greet_user : bool
  }

let pprog = Angstrom.parse_string ~consume:Angstrom.Consume.All pprog

let greetings_msg =
  {|
───────────────────────────────┬───────────────────────────────────────────┬───────────────────────────────
                               │ Welcome to MiniF# interpreter version 1.0!│
                               └───────────────────────────────────────────┘
|}
;;

let hori_line =
  "───────────────────────────────────────────────────────────────────────────────────────────────────────────"
;;

let pp_val_env env =
  Base.Map.iteri
    ~f:(fun ~key ~data ->
      match Base.Map.find env key with
      | Some _ ->
        if not (is_builtin_fun key)
        then
          if is_builtin_op key
          then
            print_endline
              (Format.asprintf "val ( %s ) : %s = %a" key "<type>" pp_value data)
          else
            print_endline (Format.asprintf "val %s : %s = %a" key "<type>" pp_value data)
      | None -> ())
    env;
  printf "\n"
;;

let pp_envs val_env type_env =
  Base.Map.iteri
    ~f:(fun ~key ~data ->
      match Base.Map.find val_env key with
      | Some _ ->
        (match Base.Map.find type_env key with
         | None -> Format.printf "-: %a\n" pp_value data
         | Some (Scheme (_, ttype)) ->
           let strtype = State.pp_core_type ttype in
           if not (is_builtin_fun key)
           then
             if is_builtin_op key
             then
               print_endline
                 (Format.asprintf "val ( %s ) : %s = %a" key strtype pp_value data)
             else
               print_endline
                 (Format.asprintf "val %s : %s = %a" key strtype pp_value data))
      | None -> ())
    val_env;
  printf "\n"
;;

let run_single options =
  let run text =
    match pprog text with
    | Error e -> print_endline (Format.asprintf "Syntax error: %s" e)
    | Ok ast ->
      if not options.do_not_type
      then (
        let type_env = infer builtin_env ast in
        match type_env with
        | Error e -> printf "%s" (State.pp_error e)
        | Ok (type_env, _) ->
          (match eval ast with
           | Ok (env, out_lst) ->
             List.iter
               (function
                 | Ok v' -> print_endline (Format.asprintf "- = %a" pp_value v')
                 | _ -> ())
               out_lst;
             pp_envs env type_env
           | Error e ->
             Format.printf "Interpreter error: ";
             Interp.Misc.pp_error std_formatter e))
      else (
        match eval ast with
        | Ok (env, out_lst) ->
          List.iter
            (function
              | Ok v' -> print_endline (Format.asprintf "- = %a" pp_value v')
              | _ -> ())
            out_lst;
          pp_val_env env
        | Error e ->
          Format.printf "Interpreter error: ";
          Interp.Misc.pp_error std_formatter e);
      if options.greet_user then print_endline hori_line else print_endline ""
  in
  let open In_channel in
  match options.file_path with
  | Some file_name ->
    let text = with_open_bin file_name input_all |> String.trim in
    let _ = run text in
    ()
  | None ->
    let rec input_lines lines =
      match input_line stdin with
      | Some line ->
        if String.ends_with ~suffix:";;" line
        then (
          let _ = run (lines ^ "\n" ^ line) in
          input_lines "")
        else input_lines (lines ^ "\n" ^ line)
      | None -> ()
    in
    let _ = input_lines "" in
    ()
;;

let () =
  let options = { file_path = None; do_not_type = false; greet_user = true } in
  let () =
    let open Arg in
    parse
      [ ( "--file"
        , String (fun filename -> options.file_path <- Some filename)
        , "Read code from the file and interpret" )
      ; ( "--do-not-type"
        , Unit (fun () -> options.do_not_type <- true)
        , "Turn off inference" )
      ; ( "--no-hi"
        , Unit (fun () -> options.greet_user <- false)
        , "Turn off greetings message" )
      ]
      (fun _ -> exit 1)
      "MiniF# interpreter version 1.0"
  in
  if options.greet_user then print_endline greetings_msg;
  run_single options
;;
