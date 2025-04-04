(** Copyright 2024-2025, Ekaterina Tenyaeva *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ETenyaeva_lib.Ast
open ETenyaeva_lib.Parser
open ETenyaeva_lib.Inferencer

(* let () =
  let fact : structure =
    [ Binding
        { is_rec = Rec
        ; pat = PatVar "fact"
        ; expr =
            ExpFun
              ( PatVar "n"
              , ExpIfThenElse
                  ( ExpBinOper (LessEquals, ExpVar "n", ExpConst (Int 1))
                  , ExpConst (Int 1)
                  , Some (ExpBinOper
                      (Mult, ExpVar "n", ExpApp (ExpVar "fact", ExpBinOper (Sub, ExpVar "n", ExpConst (Int 1))))
                  ) ))
        }
    ]
  in
  print_endline (show_structure fact)
;; *)

let () =
  match parse {|
  [1; 2; 3; 4];; (1, true, (4, 6), [true; false]);; Some (2, 5);;
  |} with
  | Ok ast -> 
    (* Format.printf "%s\n" (show_structure ast) *)
    (match run_inferencer empty_env ast with
     | Ok (_, out_list) ->
       List.iter
         (function
           | Some id, type' -> Format.printf "val %s : %a\n" id pp_type type'
           | None, type' -> Format.printf "- : %a\n" pp_type type')
         out_list
     | Error e -> Format.printf "Inferencer error: %a\n" pp_error e)
  | Error _ -> Format.printf "Parser error\n"
;;