open Base
open Ocamladt_lib.Ast


let pprint_constant fmt = function 
  | Const_integer n -> Format.fprintf fmt "%d" n
  | Const_char c -> Format.fprintf fmt "%c" c
  | Const_string s -> Format.fprintf fmt "\"%s\"" s

let rec pprint_pattern fmt = function 
  | Pat_any -> Format.fprintf fmt "_"
  | Pat_var id -> Format.fprintf fmt "%s" id
  | Pat_constant c -> pprint_constant fmt c
  | Pat_tuple (p1, p2, pl) ->
    Format.fprintf fmt "(%a, %a%s)" 
      pprint_pattern p1
      pprint_pattern p2
      (String.concat ~sep:", " (List.map pl ~f:(fun p -> Format.asprintf "%a" pprint_pattern p)))
  | Pat_construct (id, None) -> Format.fprintf fmt "%s" id
  | Pat_construct (id, Some p) -> Format.fprintf fmt "%s(%a)" id pprint_pattern p

let pprint_rec fmt = function 
  | Nonrecursive -> Format.fprintf fmt ""
  | Recursive -> Format.fprintf fmt "rec "

let rec pprint_expression fmt = function
  | Exp_ident id -> Format.fprintf fmt "%s" id
  | Exp_constant ct -> pprint_constant fmt ct
  | Exp_tuple (ex1, ex2, exl) ->
    Format.fprintf fmt "(%a, %a%s)" 
      pprint_expression ex1
      pprint_expression ex2
      (String.concat ~sep:", " (List.map exl ~f:(fun ex -> Format.asprintf "%a" pprint_expression ex)))
  | Exp_function (cs1, csl) ->
    Format.fprintf fmt "function %a%s"
      pprint_case cs1
      (String.concat ~sep:" | " (List.map csl ~f:(fun c -> Format.asprintf "%a" pprint_case c)))
  | Exp_fun (pt, ptl, exp) ->
    Format.fprintf fmt "fun %a%s -> %a" 
      pprint_pattern pt
      (String.concat ~sep:"" (List.map ptl ~f:(fun p -> Format.asprintf " %a" pprint_pattern p)))
      pprint_expression exp
  | Exp_apply (ex, exl) ->
    Format.fprintf fmt "%a %s" 
      pprint_expression ex
      (String.concat ~sep:" " (List.map exl ~f:(fun ex -> Format.asprintf "%a" pprint_expression ex)))
  | Exp_match (ex, cs, csl) ->
    Format.fprintf fmt "match %a with %a%s" 
      pprint_expression ex
      pprint_case cs
      (String.concat ~sep:" | " (List.map csl ~f:(fun c -> Format.asprintf "%a" pprint_case c)))
  | Exp_if (ex1, ex2, None) ->
    Format.fprintf fmt "if %a then %a" pprint_expression ex1 pprint_expression ex2
  | Exp_if (ex1, ex2, Some ex3) -> 
    Format.fprintf fmt "if %a then %a else %a" pprint_expression ex1 pprint_expression ex2 pprint_expression ex3
  | Exp_let (rec_fl, vbind, ex) ->
    Format.fprintf fmt "let %a%s in %a"
      pprint_rec rec_fl
      (String.concat ~sep:" and " (List.map vbind ~f:(fun vb -> Format.asprintf "%a" pprint_value_binding vb)))
      pprint_expression ex
  | Exp_construct (id, None) -> Format.fprintf fmt "%s" id
  | Exp_construct (id, Some exp) -> Format.fprintf fmt "%s %a" id pprint_expression exp

and pprint_value_binding fmt { pat; expr } = 
  Format.fprintf fmt "%a = %a" pprint_pattern pat pprint_expression expr

and pprint_case fmt { left; right } = 
  Format.fprintf fmt "%a = %a" pprint_pattern left pprint_expression right

let pprint_structure_item fmt = function 
  | Str_eval exp -> 
    Format.fprintf fmt "%a ;; " pprint_expression exp
  | Str_value (rec_flag, vbindl) -> 
    Format.fprintf fmt "let %a%s in %a ;; " 
      pprint_rec rec_flag
      (String.concat ~sep:" and " (List.map vbindl ~f:(fun vb -> Format.asprintf "%a" pprint_value_binding vb)))
      pprint_expression (List.hd_exn vbindl).expr (* Handling the first value binding expression *)

let pprint_program fmt =
  List.iter ~f:(pprint_structure_item fmt)


