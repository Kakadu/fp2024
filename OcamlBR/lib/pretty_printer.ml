(*open Ast

(* Функция для красивого отображения выражений (expr) *)
let rec pretty_print_expr = function
  | Econst (Int i) -> string_of_int i
  | Econst (String s) -> "\"" ^ s ^ "\""
  | Econst (Bool b) -> string_of_bool b
  | Econst Unit -> "unit"
  | Evar id -> id
  | Ebin_op (op, e1, e2) -> 
      "(" ^ (pretty_print_expr e1) ^ " " ^ (pretty_print_bin_op op) ^ " " ^ (pretty_print_expr e2) ^ ")"
  | Eun_op (op, e) -> 
      (pretty_print_un_op op) ^ " " ^ (pretty_print_expr e)
  | Elet (flag, id, e1, e2) -> 
      let rec_flag = match flag with
        | Recursive -> "let rec " 
        | Non_recursive -> "let "
      in
      rec_flag ^ id ^ " = " ^ (pretty_print_expr e1) ^ " in " ^ (pretty_print_expr e2)
  | Efun (patterns, body) -> 
      "fun " ^ (String.concat " " (List.map pretty_print_pattern patterns)) ^ " -> " ^ (pretty_print_expr body)
  | Efun_application (e1, e2) -> 
      (pretty_print_expr e1) ^ " " ^ (pretty_print_expr e2)
  | Eif_then_else (e1, e2, e3) -> 
      "if " ^ (pretty_print_expr e1) ^ " then " ^ (pretty_print_expr e2) ^ 
      (match e3 with Some e -> " else " ^ (pretty_print_expr e) | None -> "")
  | Etuple es -> 
      "(" ^ String.concat ", " (List.map pretty_print_expr es) ^ ")"
  | Elist es -> 
      "[" ^ String.concat "; " (List.map pretty_print_expr es) ^ "]"

(* Функция для красивого отображения бинарных операций *)
and pretty_print_bin_op = function
  | Add -> "+"
  | Mult -> "*"
  | Sub -> "-"
  | Div -> "/"
  | Gt -> ">"
  | Lt -> "<"
  | Eq -> "="
  | Neq -> "!="
  | Gte -> ">="
  | Lte -> "<="
  | And -> "&&"
  | Or -> "||"

(* Функция для красивого отображения унарных операций *)
and pretty_print_un_op = function
  | Negative -> "-"
  | Positive -> "+"
  | Not -> "not"

(* Функция для красивого отображения паттернов *)
and pretty_print_pattern = function
  | PVar id -> id
  | PConst c -> pretty_print_const c
  | PAny -> "_"

(* Функция для красивого отображения констант *)
and pretty_print_const = function
  | Int i -> string_of_int i
  | String s -> "\"" ^ s ^ "\""
  | Bool b -> string_of_bool b
  | Unit -> "unit"
;;

(* Функция для красивого отображения структуры *)
let rec pretty_print_structure structure =
  String.concat "\n" (List.map pretty_print_structure_item structure)


(* Функция для красивого отображения элементов структуры *)
and pretty_print_structure_item = function
  | SEval expr -> pretty_print_expr expr
  | SValue (flag, id, expr, body) -> 
      (* Сначала печатаем let rec или let *)
      let rec_flag = match flag with
        | Recursive -> "let rec " 
        | Non_recursive -> "let "
      in
      (* Печатаем саму функцию *)
      rec_flag ^ id ^ " " ^ (pretty_print_expr expr) ^ " in " ^ (pretty_print_expr body)
;;
*)