open Ast
open Angstrom
open Base

let is_whitespace = function
| ' ' | '\t' | '\n' | '\r' -> true
| _ -> false

let pass_ws = skip_while is_whitespace

(** Parser that matches string literals an 's' skipping all whitespaces before *)
let token s = pass_ws *> string s

(* Parentheses helper *)
let parens p = char '(' *> p <* char ')'

let pdsemicolon = many @@ token ";;"

let psemicolon = token ";" 

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

let lchain p op =
  let rec loop acc =
    (let* f = op in
     let* y = p in
     loop (f acc y))
    <|> return acc
  in
  let* x = p in
  loop x


(** Parser that takese pexpr (parser of expressions) and returns parsed arithm expression, will be a part of whole expression parser*)
let parithmexpr pexpr = 
  let pmul = lchain pexpr (token "*" *> return (fun exp1 exp2 -> Exp_apply (Exp_ident "*", Exp_tuple (exp1, exp2, [])))) <|> pexpr in
  let pdiv = lchain pexpr (token "/" *> return (fun exp1 exp2 -> Exp_apply (Exp_ident "/", Exp_tuple (exp1, exp2, [])))) <|> pexpr in
  let psum = lchain pexpr (token "+" *> return (fun exp1 exp2 -> Exp_apply (Exp_ident "+", Exp_tuple (exp1, exp2, [])))) <|> pexpr in
  let pdif = lchain pexpr (token "-" *> return (fun exp1 exp2 -> Exp_apply (Exp_ident "-", Exp_tuple (exp1, exp2, [])))) <|> pexpr in
  let peq = lchain pexpr (token "=" *> return (fun exp1 exp2 -> Exp_apply (Exp_ident "=", Exp_tuple (exp1, exp2, [])))) <|> pexpr in
  let ples = lchain pexpr (token "<" *> return (fun exp1 exp2 -> Exp_apply (Exp_ident "<", Exp_tuple (exp1, exp2, [])))) <|> pexpr in
  let pleq = lchain pexpr (token "<=" *> return (fun exp1 exp2 -> Exp_apply (Exp_ident "<=", Exp_tuple (exp1, exp2, [])))) <|> pexpr in
  let pgre = lchain pexpr (token ">" *> return (fun exp1 exp2 -> Exp_apply (Exp_ident ">", Exp_tuple (exp1, exp2, [])))) <|> pexpr in
  let pgrq = lchain pexpr (token ">=" *> return (fun exp1 exp2 -> Exp_apply (Exp_ident ">=", Exp_tuple (exp1, exp2, [])))) <|> pexpr in
  let pneq = lchain pexpr (token "<>" *> return (fun exp1 exp2 -> Exp_apply (Exp_ident "<>", Exp_tuple (exp1, exp2, [])))) <|> pexpr in
  let pand = lchain pexpr (token "&&" *> return (fun exp1 exp2 -> Exp_apply (Exp_ident "&&", Exp_tuple (exp1, exp2, [])))) <|> pexpr in
  let por = lchain pexpr (token "||" *> return (fun exp1 exp2 -> Exp_apply (Exp_ident "||", Exp_tuple (exp1, exp2, [])))) <|> pexpr in
  pexpr

let pexpr = 
  parithmexpr <|> ptupleexpr <|> pidentexpr <|> pletexpr <|> pifexpr <|> pconstintexpr <|> pconststringexpr

let pvalue_binding = 
  let* pat = ppattern in 
  let* _ = token "=" in
  let* expr = pexpr in
  return { pat; expr }

let prec_flag = 
  let* _ = token "rec" *> return Recursive <|> return Nonrecursive

let pletexpr =
  let* _ = token "let" in
  let* rec_flag = prec_flag in
  let* many1 @@ value_binding = pvalue_binding in
  let* exrpession = pexpr in
  return (rec_flag, value_binding, expression)

let ptupleexpr =
  let* _ = token "(" in
  let* expression1 = identifier in
  let* _ = token ";" in
  let* expression2 = identifier in
  let* _ = token ";" in
  let* expressiontl = sep_by (char ';') identifier in
  let* _ = token ")" in
  return (expression1, expression2, expressiontl)
 
let pifexpr =
  let* _ = token "if" in
  let* condition = pexpr in
  let* _ = token "then" in
  let* expression = pexpr in
  let* alternative = option None (
    let* _ = token "else" in
    let expression = pexpr in
    return (Some expression)
  ) in
  return (condition, expression, alternative)




(** It applies Str_eval to output of expression parser *)
let pstr_item =
  let pseval = 
    let* expr = pexpr in
    Str_eval (expr)
  in

  let psvalue = 
    (* we cant use let+ bc previous results are necessary *)
    let* rec_flag = prec_flag in
    let* value_binding = many pvalue_binding in 
    Str_value (rec, value_binding)
  in

  in pseval <|> psvalue (*<|> psadt (* god bless us *)*)

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