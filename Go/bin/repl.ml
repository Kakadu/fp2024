open Parse
open Ast

type input =
  | Input of string
  | EOF

type options = { mutable show_ast : bool }

type 'a result =
  | Result of 'a
  | End
  | Empty

let input_upto_sep inp_chan =
  let take_line () = In_channel.input_line inp_chan in
  let read_code =
    let line = take_line () in
    match line with
    | None -> EOF
    | Some line -> Input line
  in
  read_code
;;

let print_result str =
  match parse parse_file str with
  | Ok res -> pp_file Format.std_formatter res
  | Error err -> print_endline err
;;

let run_task inp_chan =
  match input_upto_sep inp_chan with
  | EOF -> End
  | Input input -> if input = "" then Empty else Result (print_result input)
;;

let run_repl show_ast =
  let inp_chan = stdin in
  let rec helper run =
    match run inp_chan with
    | Result () ->
      if show_ast then Printf.printf "\n";
      flush stdout;
      helper run
    | Empty ->
      flush stdout;
      helper run
    | End -> ()
  in
  helper run_task
;;

let () =
  let options = { show_ast = false } in
  let arg_list =
    [ ( "--ast-tree"
      , Arg.Unit (fun _ -> options.show_ast <- true)
      , "View AST representation of code" )
    ]
  in
  let doc = "Read-Eval-Print-Loop for Go" in
  let invalid_arg _ =
    Printf.eprintf "Some of this arguments aren't supported\n";
    Stdlib.exit 255
  in
  let () = Arg.parse arg_list invalid_arg doc in
  run_repl options.show_ast
;;
