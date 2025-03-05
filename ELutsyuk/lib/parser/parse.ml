(** Copyright 2024, Victoria Lutsyuk *)

(** SPDX-License-Identifier: MIT *)

open Angstrom
open Ast
open PrsAuxilary
open Expr

let prs_program =
  let p_struct_binding =
    let* _ = token "let" in
    let* rec_state = choice [ token "rec" *> return Rec; return NonRec ] in
    let* binding = prs_let_binding prs_expr in
    let+ bindings_list = many (token "and" *> prs_let_binding prs_expr) in
    Binding (rec_state, binding, bindings_list)
  in
  let p_struct_eval =
    let+ eval = prs_expr in
    EvalExpr eval
  in
  trim
  @@ many
       (p_struct_binding
        <|> p_struct_eval
        <* token ";;"
        <* skip_ws
        <|> (p_struct_binding <|> p_struct_eval <* skip_ws))
  <* end_of_input
;;

let parse str = parse_string ~consume:All prs_program str

let parse_program input =
  match parse input with
  | Ok program -> Stdlib.Format.printf "%s\n" (show_program program)
  | Error err -> Stdlib.Format.printf "%s\n" err
;;
(* let parse_to_string input =
   match parse input with
   | Ok program -> show_program program
   | Error err -> err
   ;; *)
