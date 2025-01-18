open Ast
open Parse
open Typecheck
open Eval

type options =
  { mutable show_ast : bool
  ; mutable run_typecheck : bool
  ; mutable file_string : string option
  }

let usage_msg =
  "Go subset interpreter\n\n\
   Usage: go-interpreter.exe <options> <filepath>\n\n\
   If filepath isn't specified, REPL will start running and the program will be read \
   from stdin\n\
   In REPL mode type:\n\n\
   \t\"guit\" - to quit REPL mode\n\
   \t\"help\" - to display this message\n\n\
   Options are:\n\n\
   \t--ast  Dump abstract syntax tree of a program\n\
   \t--typecheck  Typecheck the program and print result"
;;

let parse_word word =
  let open Angstrom in
  let ws = skip_while Base.Char.is_whitespace in
  ws *> string word *> ws
;;

let rec read_repl_input inp_chan =
  match In_channel.input_line inp_chan with
  | None -> Some (Ok [])
  | Some input ->
    (match parse (parse_word "help") input with
     | Ok () ->
       print_string usage_msg;
       flush stdout;
       read_repl_input inp_chan
     | Error _ ->
       (match parse (parse_word "quit") input with
        | Ok () -> None
        | Error _ ->
          (match parse parse_file input with
           | Error _ -> Some (Error ())
           | Ok [] -> read_repl_input inp_chan
           | Ok ast -> Some (Ok ast))))
;;

let run_repl options =
  let inp_chan = stdin in
  let rec helper read_repl_input =
    match read_repl_input inp_chan with
    | None -> ()
    | Some (Error ()) ->
      print_endline "Syntax error";
      helper read_repl_input
    | Some (Ok ast) ->
      if options.show_ast
      then (
        print_endline "AST dump:";
        pp_file Format.std_formatter ast;
        print_newline ());
      let typecheck_result =
        match type_check ast with
        | Result.Ok _ -> "correct"
        | Result.Error (Runtime_error _) -> "wtf runtime error while typecheck"
        | Result.Error (Type_check_error err) -> Errors.pp_typecheck_error err
      in
      if options.run_typecheck then Printf.printf "Typecheck result: %s" typecheck_result;
      (match typecheck_result with
       | "correct" ->
         (match eval ast with
          | Error (Runtime_error err) -> print_endline (Errors.pp_runtime_error err)
          | Ok _ | Error (Type_check_error _) -> flush stdout)
       | _ -> Printf.printf "Typecheck error: %s\n" typecheck_result);
      print_newline ();
      helper read_repl_input
  in
  helper read_repl_input
;;

let run_file options string =
  match parse parse_file string with
  | Error _ -> print_endline "Syntax error"
  | Ok ast ->
    if options.show_ast then pp_file Format.std_formatter ast;
    let typecheck_result =
      match type_check ast with
      | Result.Ok _ -> "correct"
      | Result.Error (Runtime_error _) -> "wtf runtime error while typecheck"
      | Result.Error (Type_check_error err) -> Errors.pp_typecheck_error err
    in
    if options.run_typecheck then Printf.printf "Typecheck result: %s\n" typecheck_result;
    (match typecheck_result with
     | "correct" ->
       (match eval ast with
        | Error (Runtime_error err) -> print_endline (Errors.pp_runtime_error err)
        | Ok _ | Error (Type_check_error _) -> ())
     | _ -> Printf.printf "Typecheck error: %s\n" typecheck_result)
;;

let () =
  let options = { show_ast = false; run_typecheck = false; file_string = None } in
  let arg_list =
    [ "--ast", Arg.Unit (fun () -> options.show_ast <- true), ""
    ; "--typecheck", Arg.Unit (fun () -> options.run_typecheck <- true), ""
    ]
  in
  let read_file path =
    if Sys.file_exists path
    then (
      let ch = open_in_bin path in
      let string = really_input_string ch (in_channel_length ch) in
      close_in ch;
      options.file_string <- Some string)
    else (
      Printf.eprintf "File %s not found\n" path;
      Stdlib.exit 255)
  in
  Arg.parse arg_list read_file usage_msg;
  match options.file_string with
  | Some string -> run_file options string
  | None -> run_repl options
;;
