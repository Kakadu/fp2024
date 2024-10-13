open Ast
open Angstrom
open Base

let is_whitespace = function
| ' ' | '\t' | '\n' | '\r' -> true
| _ -> false

let pass_ws = skip_while is_whitespace

(** Parser that matches string literals an 's' skipping all whitespaces before *)
let token s = pass_ws *> string s
let parens p = char '(' *> p <* char ')'
let psemicolon = many @@ token ";;" (* fix to take_while *)
let pletters = satisfy (function 'a'..'z' | 'A'..'Z' | '_' -> true | _ -> false)
let ptowhitespace = function 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true | _ -> false

let pident =
  let* first = pletters in
  let* rest = take_while ptowhitespace in
  return ((String.make 1 first) ^ rest)

(*                   Constant expressions                         *)
let pconstintexpr = 
  let* sign = choice [ token "+"; token "-"; token " "] in 
  let* n = take_while1 (function '0' .. '9' -> true | _ -> false) in
  return (Exp_constant(Const_integer (int_of_string (sign ^ n))))

(* let pconstchar = 
  let* _ = token "'" in 
  let* c = satisfy (fun code -> code >= Char.to_int ' ' && code <= Char.to_int '~') in 
  let* _ = token "'" in 
  return (Const_char (c))
;; *)

let pconststringexpr = 
  let* _ = token "\"" in
  let* str = take_while1 (function '"' -> false | _ -> true) in 
  return (Exp_constant(Const_string (str)))

(*                   Arithm + other expressions                         *)
let lchain p op =
  let rec loop acc =
    (let* f = op in
     let* y = p in
     loop (f acc y))
    <|> return acc
  in
  let* x = p in
  loop x
let pidentexpr = 
  let* ident = pident in 
  return (Exp_ident(ident))

let parithmexpr pexpr ~mul ~div ~add ~sub ~les ~leq ~gre ~grq ~eq ~neq ~ando ~oro =
  let pmul = lchain pexpr (token "*" *> return (fun exp1 exp2 -> mul exp1 exp2)) in
  let pdiv = lchain pmul (token "/" *> return (fun exp1 exp2 -> div exp1 exp2)) in
  let psum = lchain pdiv (token "+" *> return (fun exp1 exp2 -> add exp1 exp2)) in
  let pdif = lchain psum (token "-" *> return (fun exp1 exp2 -> sub exp1 exp2)) in
  let ples = lchain pdif (token "<" *> return (fun exp1 exp2 -> les exp1 exp2)) in
  let pleq = lchain ples (token "<=" *> return (fun exp1 exp2 -> leq exp1 exp2)) in
  let pgre = lchain pleq (token ">" *> return (fun exp1 exp2 -> gre exp1 exp2)) in
  let pgrq = lchain pgre (token ">=" *> return (fun exp1 exp2 -> grq exp1 exp2)) in
  let peq = lchain pgrq (token "=" *> return (fun exp1 exp2 -> eq exp1 exp2)) in
  let pneq = lchain peq (token "!=" *> return (fun exp1 exp2 -> neq exp1 exp2)) in
  let pand = lchain pneq (token "&&" *> return (fun exp1 exp2 -> ando exp1 exp2)) in
  let por = lchain pand (token "||" *> return (fun exp1 exp2 -> oro exp1 exp2)) in
  por

(* Define pexpr to handle different expression types *)
let pexpr = fix (fun pexpr ->
  parithmexpr pexpr
    ~mul:(fun exp1 exp2 -> Exp_apply (Exp_ident "*", Exp_tuple (exp1, exp2, [])))
    ~div:(fun exp1 exp2 -> Exp_apply (Exp_ident "/", Exp_tuple (exp1, exp2, [])))
    ~add:(fun exp1 exp2 -> Exp_apply (Exp_ident "+", Exp_tuple (exp1, exp2, [])))
    ~sub:(fun exp1 exp2 -> Exp_apply (Exp_ident "-", Exp_tuple (exp1, exp2, [])))
    ~les:(fun exp1 exp2 -> Exp_apply (Exp_ident "<", Exp_tuple (exp1, exp2, [])))
    ~leq:(fun exp1 exp2 -> Exp_apply (Exp_ident "<=", Exp_tuple (exp1, exp2, [])))
    ~gre:(fun exp1 exp2 -> Exp_apply (Exp_ident ">", Exp_tuple (exp1, exp2, [])))
    ~grq:(fun exp1 exp2 -> Exp_apply (Exp_ident ">=", Exp_tuple (exp1, exp2, [])))
    ~eq:(fun exp1 exp2 -> Exp_apply (Exp_ident "=", Exp_tuple (exp1, exp2, [])))
    ~neq:(fun exp1 exp2 -> Exp_apply (Exp_ident "!=", Exp_tuple (exp1, exp2, [])))
    ~ando:(fun exp1 exp2 -> Exp_apply (Exp_ident "&&", Exp_tuple (exp1, exp2, [])))
    ~oro:(fun exp1 exp2 -> Exp_apply (Exp_ident "||", Exp_tuple (exp1, exp2, [])))
  (* <|> ptupleexpr *)
  <|> pidentexpr
  (* <|> pletexpr
  <|> pifexpr *)
  <|> pconstintexpr
  <|> pconststringexpr
)

(*                   Patterns                         *)
let pany = token "_" *> return Pat_any
let pvar =
  let* ident = pident in
  return (Pat_var ident)
let ppattern =
  pany <|> pvar (* <|> pconstant <|> ptuple <|> pconstruct, will be added in future, not necessary for fact *)

(*                   Structure items                         *)

let pvalue_binding =
  let* pat = ppattern in
  let* _ = token "=" in
  let* expr = pexpr in
  let value_binding = { pat; expr } in
  return value_binding

let psvalue = 
  (* we cant use let+ bc previous results are necessary *)
  let* rec_fl = token "rec" *> return Recursive <|> return Nonrecursive in
  let* value_binding_list = many pvalue_binding in 
  return (Str_value (rec_fl, value_binding_list))

let pseval = 
  let* expr = pexpr in
  return (Str_eval (expr))

(** It applies Str_eval to output of expression parser *)
let pstr_item =
  pseval <|> psvalue (*<|> psadt (* god bless us *)*)

let pstructure =
  let psemicolon = token ";;" in
  many (pstr_item <* psemicolon)

let parse str = parse_string ~consume:All pstructure str

let parse_fact str = 
  match parse str with
  | Ok str -> str
  | Error msg -> failwith msg





(* let parse str = 
  match parse_string ~consume:All pstructure str with
  | Ok result -> result  (* Assuming result is of type expression list *)
  | Error msg -> failwith msg *)

(* Example test cases for the parser *)
(* let () =
  let test_cases = [
    ("1 + 2;;", [Exp_apply (Exp_ident "+", Exp_tuple (Exp_constant (Const_integer 1), Exp_constant (Const_integer 2), []))]);
    ("'hello';;", [Exp_constant (Const_string "hello")]);
    ("(1, 2, 3);;", [Exp_tuple (Exp_constant (Const_integer 1), Exp_constant (Const_integer 2), [Exp_constant (Const_integer 3)])]);
    ("4 * (2 + 3);;", [Exp_apply (Exp_ident "*", Exp_tuple (Exp_constant (Const_integer 4), Exp_apply (Exp_ident "+", Exp_tuple (Exp_constant (Const_integer 2), Exp_constant (Const_integer 3), [])), []))]);
    ("8 / 2;;", [Exp_apply (Exp_ident "/", Exp_tuple (Exp_constant (Const_integer 8), Exp_constant (Const_integer 2), []))]);
    ("3 - 4 + 5;;", [Exp_apply (Exp_ident "+", Exp_tuple (Exp_apply (Exp_ident "-", Exp_tuple (Exp_constant (Const_integer 3), Exp_constant (Const_integer 4), [])), Exp_constant (Const_integer 5), []))]);
  ] in
  List.iter (fun (input, expected) ->
    let result = parse input in  (* Get the result from the parse function *)
    if result = expected then
      Printf.printf "Passed: %s\n" input
    else
      Printf.printf "Failed: %s (expected: %s, got: %s)\n" 
                    input 
                    ?? expected) 
                    (?? result)
  ) test_cases *)