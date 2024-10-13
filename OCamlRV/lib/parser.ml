open Angstrom
open Ast
open Base

(* Factorial example *)
(* let rec fact n = if n <= 1 then 1 else n * fact (n - 1);; *)
(*key words: let| rec| if | then| else|*)


let is_letter c =
  match c with
  | 'a' .. 'z' -> true
  | 'A' .. 'Z' -> true
  | _ -> false

let is_digit c =
  match c with
  | '0' .. '9' -> true
  | _ -> false


(* let is_whitespace c = 
  Char.equal c ' ' || Char.equal c '\t' || Char.equal c '\n' || Char.equal c '\r' *)

let is_keyword = function
  | "let"
  | "rec"
  | "in"
  | "fun"
  | "match"
  | "with"
  | "if"
  | "then"
  | "else"
  | "true"
  | "false" -> true
  | _ -> false
;;

(* specials:
() ; ;;
*)
(**)

let ws = take_while Char.is_whitespace
(* let ws = take_while is_whitespace *) (*whitespaces*)
let token s = ws *> string s (* takes symbols from ws to ws and convert to string*)

(* parsing numbers*)
let pnumber =
  let* sign = choice [ token "-"; token "+"; token "" ] in
  let* digits = take_while1 is_digit in
  return (int_of_string (sign ^ digits))

let pliteral =
  choice [
    (let* n = pnumber in return (ExprLiteral (IntLiteral n)));
    (token "true" *> return (ExprLiteral (BoolLiteral true)));
    (token "false" *> return (ExprLiteral (BoolLiteral false)));
    (token "()" *> return (ExprLiteral UnitLiteral));
    (token "nil" *> return (ExprLiteral NilLiteral))
  ]
let id =
  let* first_char = satisfy is_letter in
  let* rest = take_while (fun c -> is_letter c || is_digit c || Char.equal c '_') in
  return (String.make 1 first_char ^ rest)

let pvariable = 
  id >>= fun v -> return (ExprVariable v)

(* bin ops*)
let pbinop =
  fix( fun pexpression ->
  let* left = pexpression in
  let* op = choice [
    token "+"; 
    token "-"; 
    token "*"; 
    token "/"; 
    token "<"; 
    token ">"; 
    token "="
  ] in
  let* right = pexpression in
  match op with
  | "+" -> return (ExprBinOperation (Add, left, right))
  | "-" -> return (ExprBinOperation (Sub, left, right))
  | "*" -> return (ExprBinOperation (Mul, left, right))
  | "/" -> return (ExprBinOperation (Div, left, right))
  | "<" -> return (ExprBinOperation (Lt, left, right))
  | ">" -> return (ExprBinOperation (Gt, left, right))
  | "=" -> return (ExprBinOperation (Eq, left, right))
  | _ -> fail "unsupported binary operator"
  )
(* unary ops*)
let punop =
  fix (fun pexpression ->
    let* op = choice [token "+"; token "-"; token "not"] in
    let* expr = pexpression in
    match op with
    | "+" -> return (ExprUnOperation (UnaryPlus, expr))
    | "-" -> return (ExprUnOperation (UnaryMinus, expr))
    | "not" -> return (ExprUnOperation (UnaryNeg, expr))
    | _ -> fail "unsupported unary operator"
  )


(* if-then-else *)
let pif =
  fix(
    fun pexpression ->
      let* _ = token "if" in
        let* cond = pexpression in
        let* _ = token "then" in
        let* then_expr = pexpression in
        let* _ = token "else" in
        let* else_expr = pexpression in
        return (ExprIf (cond, then_expr, Some else_expr))
  )


(* let let rec pars *)
let pletrec =
  fix (
    fun pexpression ->
      let* _ = token "let" in
      let* _ = token "rec" in
      let* id = id in
      let* _ = token "n" in
      let* _ = token "=" in
      let* expr = pexpression in
       let* _ = token ";" in
      return (ExprLet (Rec, [(PVar id, expr)], ExprVariable id))
  )


(* Парсер для выражений, объединяющий другие парсеры *)
let pexpression =
  choice [
    pif;            (*if-then-else *)
    pbinop;         (* бинарные операции *)
    punop;          (*унарные операции *)
    pliteral;       (* литералы *)
    pvariable;      (* переменные *)
    pletrec         (* let/let rec *)
  ]
  (*

  SLAVA ESLI TI REALNO REVIEW DELAESH YBERYYYYY EtU STROCHKYYYY!!!!!
  aboba

  *)

let parse s = parse_string ~consume:Prefix pexpression s

let test_parse str expected =
  match parse str with
  | Ok actual ->
    (* Format.printf "Hello\n" *)
    true
    (* let is_eq = List.equal equal_decl expected actual in
    if is_eq then () else Format.printf "Actual %a\n" pp_program actual;
    is_eq *)
  | Error err ->
    Format.printf "%s\n" err;
    false
;;


let%test "test" = test_parse "let f = 5" [ ]
