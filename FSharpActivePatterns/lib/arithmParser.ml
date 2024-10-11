open Angstrom
open Ast

let ws = skip_while (function ' ' -> true | _ -> false)
let parens p = ws *> char '(' *> ws *> p <* ws <* char ')' <* ws
let add = ws *> char '+' *> ws *> return Binary_add
let sub = ws *> char '-' *> ws *> return Binary_subtract
let mul = ws *> char '*' *> ws *> return Binary_multiply
let div = ws *> char '/' *> ws *> return Binary_divide
let integer =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| (fun s -> Const (Int_lt (int_of_string s)))

let chainl1 e op =
  let rec go acc =
    (lift2 (fun f x -> Bin_expr (f, acc, x)) op e >>= go) <|> return acc in
  e >>= fun init -> go init <* ws

let expr : expr t =
  fix (fun expr ->
    let factor = (ws *> parens expr) <|> (ws *> integer) in
    let term   = chainl1 factor (mul <|> div) in
    chainl1 term (add <|> sub))

let parse (str:string) : expr =
  match parse_string ~consume:All expr str with
  | Ok v      -> v
  | Error msg -> failwith msg

(*
(* saves info about positions explicitly during parsing *)
let parse_with_pos parser pos =
  parser >>= fun (result, new_pos) ->
  return (result, pos, new_pos)
;;

(* assumes that something in brackets can be parsed further *)
let parse_parens p pos =
  char '(' *> p (pos + 1) >>= fun (expr_inside, new_pos) ->
  char ')' *> return (expr_inside, new_pos + 1)
;;
let parse_add pos =
  char '+' *> return (fun (left, l_start, l_end) (right, r_start, r_end) -> 
    (Bin_expr (Binary_add, left, right), l_start, r_end))
;;
let parse_sub pos =
  char '-' *> return (fun (left, l_start, l_end) (right, r_start, r_end) -> 
    (Bin_expr (Binary_subtract, left, right), l_start, r_end))
;;
let parse_mul pos =
  char '*' *> return (fun (left, l_start, l_end) (right, r_start, r_end) -> 
    (Bin_expr (Binary_multiply, left, right), l_start, r_end))
;;
let parse_div pos =
  char '/' *> return (fun (left, l_start, l_end) (right, r_start, r_end) -> 
    (Bin_expr (Binary_divide, left, right), l_start, r_end))
;;

let find_const pos =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
  >>= fun num ->
  let num_length = String.length num in
  return (Const (Int_lt (int_of_string num)), pos + num_length)
;;

(* finds a chain of operands with the same priority and left associativity, e.g., a-b+c or a*b/c *)
let find_left_chain e func pos =
  let rec go acc pos =
    (*  *)
    lift2 (fun f x -> f acc x) (func pos) (e pos) >>= fun new_acc ->
    go new_acc (pos + 1) <|> return acc
  in
  e pos >>= fun init -> go init pos

(* parses an arithmetic expression with +-*/() completely *)
let rec find_arithm_expr pos : (expr * int) t =
  let parser = (parse_parens find_arithm_expr pos) <|> find_const pos in
  parser >>= fun (factor, new_pos) ->
  let term = find_left_chain (fun pos -> parse_mul pos <|> parse_div pos) new_pos factor in
  find_left_chain term (fun pos -> parse_add pos <|> parse_sub pos) new_pos
;;


let parse (str : string) : expr =
  (* apply parser to string *)
  match parse_string ~consume:All (find_arithm_expr 0) str with
  | Ok (v, start_pos) -> v
  | Error msg -> 
    failwith (Printf.sprintf "Error at position: %s" msg) *)

(*
open Angstrom
open Ast

(* saves info about positions explicitly during parsing *)
let parse_with_pos parser pos =
  parser >>= fun (result, new_pos) ->
  return (result, pos, new_pos)
;;

(* assumes that something in brackets can be parsed further *)
let parse_parens p pos =
  char '(' *> parse_with_pos p (pos + 1) <* char ')'
;;
let parse_add pos =
  char '+' *> return (fun (left, l_start, l_end) (right, r_start, r_end) -> 
    (Bin_expr (Binary_add, left, right), l_start, r_end))
;;
let parse_sub pos =
  char '-' *> return (fun (left, l_start, l_end) (right, r_start, r_end) -> 
    (Bin_expr (Binary_subtract, left, right), l_start, r_end))
;;
let parse_mul pos =
  char '*' *> return (fun (left, l_start, l_end) (right, r_start, r_end) -> 
    (Bin_expr (Binary_multiply, left, right), l_start, r_end))
;;
let parse_div pos =
  char '/' *> return (fun (left, l_start, l_end) (right, r_start, r_end) -> 
    (Bin_expr (Binary_divide, left, right), l_start, r_end))
;;

let find_const pos =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
  >>= fun num ->
  let length = String.length num in
  return (Const (Int_lt (int_of_string num)), pos, pos + length)
;;

(* finds chain of operands on same priority level and with left associativity, such as a-b+c or a*b/c *)
let find_left_chain e func pos = 
  let rec go acc pos = 
    lift2 (fun f x -> f acc x) (func pos) (e pos) >>= fun new_acc ->
    go new_acc (pos + 1) <|> return acc
  in
  e pos >>= fun init -> go init pos
;;

(* parses arithmetical expression with +-*/() completely *)
let rec find_arithm_expr pos : (expr * int * int) t =
  let rec expr_with_pos pos =
    let factor = parse_parens (expr_with_pos pos) pos <|> find_const pos in
    let term = find_left_chain factor (parse_mul pos <|> parse_div pos) pos in
    find_left_chain term (parse_add pos <|> parse_sub pos) pos
  in
  expr_with_pos pos
;;

let parse (str : string) : expr =
  match parse_string ~consume:All (find_arithm_expr 0) str with
  | Ok (v, start_pos, end_pos) -> (
    Printf.printf "start is %d" start_pos;
    Printf.printf "end is %d" end_pos;
    v
  )
  | Error msg -> 
    failwith (Printf.sprintf "Error at position: %s" msg) *)



(*

open Angstrom
open Ast

(* saves info about positions in string before and after parsing *)
let parse_with_pos parser =
  pos >>= fun start_pos ->
  parser >>= fun result ->
  pos >>= fun end_pos ->
  return (result, start_pos, end_pos)
;;

let parse_parens p = char '(' *> parse_with_pos p <* char ')' (* assumes that something in brackets can be parsed further *)
let parse_add = char '+' *> return (fun (left, l_start, l_end) (right, r_start, r_end) -> 
  (Bin_expr (Binary_add, left, right), l_start, r_end))
let parse_sub = char '-' *> return (fun (left, l_start, l_end) (right, r_start, r_end) -> 
  (Bin_expr (Binary_subtract, left, right), l_start, r_end))
let parse_mul = char '*' *> return (fun (left, l_start, l_end) (right, r_start, r_end) -> 
  (Bin_expr (Binary_multiply, left, right), l_start, r_end))
let parse_div = char '/' *> return (fun (left, l_start, l_end) (right, r_start, r_end) -> 
  (Bin_expr (Binary_divide, left, right), l_start, r_end))

let find_const =
  pos >>= fun start_pos ->
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
  >>= fun num ->
  pos >>= fun end_pos ->
  return (Const (Int_lt (int_of_string num)), start_pos, end_pos)
;;

(* finds chain of operands on same priority level and with left associativity, such as a-b+c or a*b/c *)
let find_left_chain e func = 
  let rec go acc = lift2 (fun f x -> f acc x) func e >>= go <|> return acc in
  e >>= fun init -> go init
;;

(* parses arithmetical expression with +-*/() completely *)
let rec find_arithm_expr : (expr * int * int) t =
  let rec expr_with_pos () =
    let factor = parse_parens (expr_with_pos ()) <|> find_const in
    let term = find_left_chain factor (parse_mul <|> parse_div) in
    find_left_chain term (parse_add <|> parse_sub)
  in
  expr_with_pos ()
;;


let parse (str : string) : expr =
  match parse_string ~consume:All find_arithm_expr str with
  | Ok (v, start_pos, end_pos) -> (
    Printf.printf "start is %d" start_pos;
    Printf.printf "end is %d" end_pos;
    v
  )
  | Error msg -> 
    failwith (Printf.sprintf "Error at position: %s" msg) *)
