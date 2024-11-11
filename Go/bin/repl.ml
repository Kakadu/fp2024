open Base
open Stdlib
open Stdlib.Arg

type input =
  | Input of string
  | EOF

type options = { mutable show_ast : bool }

type 'a result =
  | Result of 'a
  | End
  | Empty
  | Error

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

let run_task inp_chan =
  match input_upto_sep inp_chan with
  | EOF -> End
  | Input input -> if input = "" then Empty else Result (Parse.pp_repl input)
;;

let run_repl show_ast =
  let inp_chan = stdin in
  let rec run_repl_helper run =
    match run inp_chan with
    | Result () ->
      if show_ast then Printf.printf "\n";
      flush stdout;
      run_repl_helper run
    | Empty ->
      flush stdout;
      run_repl_helper run
    | Error -> Printf.eprintf "Runtime error\n"
    | End -> ()
  in
  run_repl_helper run_task
;;

let () =
  let options = { show_ast = false } in
  let arg_list =
    [ ( "--ast-tree"
      , Unit (fun _ -> options.show_ast <- true)
      , "View ASR representation of code" )
    ]
  in
  let doc = "Read-Eval-Print-Loop for Go" in
  let invalid_arg _ =
    Printf.eprintf "Some of this arguments aren't supported\n";
    Stdlib.exit 255
  in
  let () = parse arg_list invalid_arg doc in
  run_repl options.show_ast
;;
