open Base
open Ast

let pprint_constant = function 
  | Const_integer n -> Int.to_string n
  | Const_char c -> Printf.sprintf "'%c'" c
  | Const_string s -> Printf.sprintf "\"%s\"" s

let rec pprint_pattern = function 
  | Pat_any -> "_"
  | Pat_var id -> id
  | Pat_constant c -> pprint_constant c
  | Pat_tuple (p1, p2, pl) ->
    "(" ^ pprint_pattern p1 ^ ", " ^ pprint_pattern p2 ^ ", " ^
    (List.fold pl ~init:"" ~f: (fun acc p -> acc ^ ", " ^ pprint_pattern p)) ^ ")"
  | Pat_construct (id, None) -> id
  | Pat_construct (id, Some p) -> id ^ "(" ^ pprint_pattern p ^ ")"

let pprint_rec = function 
  | Nonrecursive -> ""
  | Recursive -> "rec "

let rec pprint_expression = function
  | Exp_ident id -> id
  | Exp_constant ct -> pprint_constant ct
  | Exp_tuple (ex1, ex2, exl) ->
    "(" ^ pprint_expression ex1 ^ ", " ^ pprint_expression ex2 ^ ", " ^
    (List.fold exl ~init:"" ~f: (fun acc ex -> acc ^ ", " ^ pprint_expression ex)) ^ ")"
  | Exp_function (cs1, csl) ->
    "function " ^ pprint_case cs1 ^ 
    (List.fold csl ~init:"" ~f:(fun acc c -> acc ^ " | " ^ pprint_case c))
  | Exp_fun (pt, ptl, exp) ->
    "fun " ^ pprint_pattern pt ^
    (List.fold ptl ~init:"" ~f:(fun acc p -> acc ^ " " ^ pprint_pattern p)) ^
      " -> " ^ pprint_expression exp
  | Exp_apply (ex, exl) ->
    pprint_expression ex ^ " " ^
    (String.concat ~sep:" " (List.map exl ~f: pprint_expression))
  | Exp_match (ex, cs, csl) ->
    "match " ^ pprint_expression ex ^ " with " ^ pprint_case cs ^ 
    (List.fold csl ~init:"" ~f:(fun acc c -> acc ^ " | " ^ pprint_case c)) ^ ")"
  | Exp_if (ex1, ex2, None) ->
    "if " ^ pprint_expression ex1 ^ " then " ^ pprint_expression ex2
  | Exp_if (ex1, ex2, Some ex3) -> 
    "if " ^ pprint_expression ex1 ^ " then " ^ pprint_expression ex2 ^ " else " ^ pprint_expression ex3
  | Exp_let (rec_fl, vbind, ex) ->
    "let " ^ pprint_rec rec_fl ^
    (String.concat ~sep:" and " (List.map vbind ~f: pprint_value_binding)) ^
    " in " ^ pprint_expression ex
  | Exp_construct (id, None) -> id
  | Exp_construct (id, Some exp) -> id ^ " " ^ pprint_expression exp (* to fix, in terms of tuple?*)

and pprint_value_binding { pat; expr } = 
  pprint_pattern pat ^ " = " ^ pprint_expression expr

and pprint_case { left; right } = 
  pprint_pattern left ^ " = " ^ pprint_expression right


(* to add let pprint_type_expr = function 
  | Type_arrow 
  |
*)

let pprint_structure_item = function 
  | Str_eval exp -> pprint_expression exp 
  | Str_value (rec_flag, vbindl) -> 
    "let " ^ pprint_rec rec_flag ^ 
    (String.concat ~sep:" and " (List.map vbindl ~f: pprint_value_binding))


let pretty_print_program prog =
  String.concat ~sep:"\n" (List.map prog ~f:pprint_structure_item)