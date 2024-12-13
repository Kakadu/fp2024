(** Copyright 2024-2027, Ilia Suponev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser_utility
open Utils

(** Data record which contains [Miniml.Ast]
    view of [binary_operator]
    and it's view in string *)
type binary_operator_parse_data =
  { oper_view : string
  ; oper_ast : binary_operator
  }

(** Predicate to check string as keyword of Miniml *)
let is_keyword = function
  | "true" | "false" -> true
  | "if" | "then" | "else" -> true
  | "fun" -> true
  | "let" | "rec" | "and" | "in" -> true
  | "function" -> true
  | "match" | "with" | "when" -> true
  | _ -> false
;;

let ident_symbol = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' | '\'' | '0' .. '9' -> true
  | _ -> false
;;

let is_constructor id =
  match id.[0] with
  | 'A' .. 'Z' -> true
  | _ -> false
;;

(** Parse [Miniml.identifier] value. *)
let ident
  ?(on_keyword = fun _ -> pfail)
  ?(on_constructor = preturn)
  ?(on_simple = preturn)
  =
  let helper = many (dsatisfy ident_symbol Fun.id) in
  skip_ws *> dsatisfy ident_symbol Fun.id
  >>= fun s ->
  if is_digit s || s = '\''
  then pfail
  else
    helper
    >>| (fun l -> string_of_char_list (s :: l))
    >>= fun id ->
    if is_keyword id
    then on_keyword id
    else if is_constructor id
    then on_constructor id
    else on_simple id
;;

(** Parser of keyword *)
let keyword word =
  let helper c = not (ident_symbol c) in
  let check_end = asatisfy helper in
  skip_ws *> ssequence word *> check_end *> preturn ()
;;

(** Parser of some elements sequence:
    - [start]: first element of sequence
    - [element_parser]: parser of one element in sequence
    - [list_converter]: coverter of elements sequence to one new element
    - [separator]: char sequence which splitted sequence elements
    - [notfound]: parser which will exec if after separator not found next element *)
let element_sequence
  : 'a 'b. 'a -> 'a parser -> ('a list -> 'b) -> string -> 'a parser -> 'b parser
  =
  fun start element_parser list_converter sep pnotfound ->
  let next_element sep =
    skip_ws
    *> (if is_keyword sep then keyword sep else ssequence sep >>| fun _ -> ())
    *> (element_parser <|> pnotfound)
  in
  skip_ws
  *> (many (next_element sep) >>| fun l -> list_converter (List.append [ start ] l))
;;

let bracket_sequence subparser state =
  (skip_ws *> symbol '(' *> subparser
   <* (skip_ws *> symbol ')' <|> perror "Not found close bracket"))
    state
;;

(* let rec type_parser state : core_type parse_result = (skip_ws *> type_identifier) state *)
let type_ident_parser =
  ident
    ~on_keyword:(fun k ->
      perror (Format.sprintf "Not found type identifier, finded keyword '%s'" k))
    ~on_constructor:(fun c ->
      perror (Format.sprintf "Not found type identifier, finded constructor '%s'" c))
    ~on_simple:preturn
;;

let type_identifier state =
  let helper =
    type_ident_parser
    >>= fun s ->
    match s with
    | "_" -> preturn AnyType
    | _ -> preturn (TypeIdentifier s)
  in
  (skip_ws *> helper) state
;;

let rec core_type_parser need_arrow state =
  (skip_ws *> tuple_type
   >>= fun t1 -> if need_arrow then arrow_type t1 <|> preturn t1 else preturn t1)
    state

and arrow_type t1 state =
  (skip_ws *> handle_error (ssequence "->" *> core_type_parser false) (fun _ -> pfail)
   >>= fun t2 -> preturn (ArrowType (t1, t2)))
    state

and tuple_type state =
  let helper t1 =
    element_sequence
      t1
      type_constructor
      (function
        | t1 :: t2 :: tl -> TupleType (t1, t2, tl)
        | _ -> t1)
      "*"
      (perror "Not found expression after tuple separator: '*'")
  in
  (skip_ws *> type_constructor >>= helper) state

and type_constructor state =
  let rec builder t1 = function
    | [] -> t1
    | t2 :: tail -> builder (TypeConstructor (t2, t1)) tail
  in
  (skip_ws *> basic_type
   >>= fun t1 -> many type_ident_parser >>= fun tl -> preturn (builder t1 tl))
    state

and basic_type state =
  (skip_ws *> bracket_type <|> type_identifier <|> perror "Not found type") state

and bracket_type state = bracket_sequence (core_type_parser true) state

(** Parser of integer literals: [0 .. Int64.max_int].

    [!] This parser returns also [ParseSuccess] or [ParseFail] *)
let integer =
  let rec helper counter =
    digit
    <|> ssequence "_" *> preturn (-1)
    >>= (fun v -> if v < 0 then helper counter else helper (v + (counter * 10)))
    <|> (preturn counter >>| fun v -> IntLiteral v)
  in
  skip_ws *> digit >>= helper
;;

(** Parser of boolean literals: [true], [false].

    [!] This parser returns also [ParseSuccess] or [ParseFail] *)
let boolean =
  skip_ws *> (keyword "true" >>| fun _ -> BoolLiteral true)
  <|> (keyword "false" >>| fun _ -> BoolLiteral false)
;;

(** Parser of patterns: [<pvar>] | [<ptuple>] *)
let rec pattern_parser expect_type state =
  let helper = function
    | PConstrain _ as p -> preturn p
    | p -> if expect_type then ptype true p else preturn p
  in
  (skip_ws *> (pvariable <|> ptuple) >>= helper) state

(** Parser of variable pattern *)
and pvariable state =
  let helper =
    ident
      ~on_keyword:(fun id ->
        perror
          (Format.sprintf "Unexpected identifier of pattern: '%s'. It is keyword." id))
      ~on_constructor:(fun id ->
        perror (Format.sprintf "Unexpected constructor on pattern position: '%s'." id))
      ~on_simple:preturn
  in
  (skip_ws
   *> (helper
       >>| fun id ->
       match id with
       | "_" -> PAny
       | _ -> PVar id))
    state

(** Parser of tuple pattern and unit pattern *)
and ptuple state =
  let tuple_elements pfirst =
    skip_ws
    *> element_sequence
         pfirst
         (pattern_parser true)
         (function
           | p1 :: p2 :: pl -> PTuple (p1, p2, pl)
           | _ -> pfirst)
         ","
         (perror "Not found expression after tuple separator: ','")
  in
  let brackets_subexpr =
    skip_ws *> pattern_parser true
    >>= (fun pfirst -> tuple_elements pfirst >>= ptype true)
    <|> preturn PUnit
  in
  bracket_sequence brackets_subexpr state

and ptype : bool -> pattern -> pattern parser =
  fun need_arrow p ->
  let helper =
    skip_ws
    *> symbol ':'
    *> (core_type_parser need_arrow <|> perror "Not found type of pattern")
  in
  skip_ws *> (helper >>= (fun t -> preturn (PConstrain (p, t))) <|> preturn p)
;;

(** Parser of constants expression: [integer] and [boolean]

    [!] This parser returns also [ParseSuccess] or [ParseFail] *)
let const_expr = skip_ws *> (integer <|> boolean) >>| fun l -> Const l

(** Parser of variable expression *)
let variable =
  let helper =
    ident
      ~on_keyword:(fun _ -> pfail)
      ~on_constructor:(fun id ->
        perror (Format.sprintf "Invalid variable identifier: '%s'." id))
      ~on_simple:preturn
  in
  skip_ws *> helper >>| fun s -> Variable s
;;

let constructor_name =
  let helper =
    ident ~on_keyword:(fun _ -> pfail) ~on_constructor:preturn ~on_simple:(fun _ -> pfail)
  in
  skip_ws *> helper
;;

(** Parser of all expression which defines on [Miniml.Ast] module *)
let rec expr state = (skip_ws *> block_expr) state

(** Parser of expression block *)
and block_expr state =
  let helper ex =
    element_sequence
      ex
      tuple_expr
      (function
        | ex :: [] -> ex
        | _ as l -> ExpressionBlock l)
      ";"
      pfail
    >>= fun block ->
    assequence ";;" *> preturn block <|> ssequence ";" *> preturn block <|> preturn block
  in
  (skip_ws *> (tuple_expr >>= helper)) state

(** Parser of tuple expression *)
and tuple_expr state =
  let helper ex =
    element_sequence
      ex
      boolean_expr
      (function
        | ex1 :: ex2 :: l -> Tuple (ex1, ex2, l)
        | _ -> ex)
      ","
      (perror "Not found expression after tuple separator: ','")
  in
  (skip_ws *> (boolean_expr >>= helper)) state

(** Parser of applyable expressions *)
and applyable_expr state =
  (skip_ws *> bracket_expr <|> func_expr <|> lambda_expr <|> variable) state

(** Parser of basic expressions:
    [<unary>] | [<if-expr>] | [<define_expr>] | [<apply-expr>] | [<const>] *)
and basic_expr applyable state =
  (skip_ws *> unary_expr applyable
   <|> if_expr
   <|> define_expr
   <|> list_expr
   <|> match_expr
   <|> constructor
   <|> (if applyable then applyable_expr else apply_expr)
   <|> const_expr)
    state

(** Parser of case expressions: *)
and case_parser is_first =
  let case_expr p f =
    skip_ws *> (expr <|> perror "Not found expression after special sequence '->' in case")
    >>= fun ex -> preturn { pattern = p; filter = f; result = ex }
  in
  let on_not_found_lambda f base_state state =
    match f with
    | Some _ -> perror "Not found special sequence '->' in case" state
    | None ->
      (pattern_parser false
       >>= ptype false
       >>= fun p ->
       (skip_ws *> ssequence "->" <|> perror "Not found special sequence '->' in case")
       *> case_expr p None)
        base_state
  in
  let result_helper p f base_state state =
    (skip_ws *> ssequence "->" *> case_expr p f <|> on_not_found_lambda f base_state)
      state
  in
  let filter_helper =
    let filter_expr =
      skip_ws
      *> (expr
          <|> perror "Not found expression of when clause"
          >>= fun f -> preturn (Some f))
    in
    skip_ws *> (keyword "when" *> filter_expr) <|> preturn None
  in
  let main_helper state =
    (skip_ws *> (pattern_parser true <|> perror "Not found pattern of case expression")
     >>= fun p -> filter_helper >>= fun f -> result_helper p f state)
      state
  in
  skip_ws *> (symbol '|' *> main_helper) <|> if is_first then main_helper else pfail

(** Parser of cases list *)
and cases_list on_not_found_first state =
  (skip_ws *> (case_parser true <|> on_not_found_first)
   >>= fun c -> many (case_parser false) >>= fun cl -> preturn (c :: cl))
    state

(** Parser of function expression:
    - [function <case> ... <case> ]*)
and func_expr state =
  (skip_ws
   *> keyword "function"
   *> cases_list (perror "Not found first case of function expression")
   >>= fun cl -> preturn (Func cl))
    state

(** Parser of match expression:
    - [match <expr> with <case> ... <case> ]*)
and match_expr state =
  (skip_ws
   *> keyword "match"
   *> skip_ws
   *> (expr <|> perror "Not found expression after keyword 'match'")
   >>= fun ex ->
   skip_ws
   *> (keyword "with" <|> perror "Not found keyword 'with' of match expression")
   *> cases_list (perror "Not found first case of match expression")
   >>= fun cl -> preturn (Match (ex, cl)))
    state

and constructor state =
  (skip_ws *> constructor_name
   >>= fun name ->
   expr
   >>= (fun ex -> preturn (Construct (name, Some ex)))
   <|> preturn (Construct (name, None)))
    state

(** Parser of unary expression *)
and unary_expr applyable state =
  let helper =
    symbol '+'
    *> (basic_expr applyable
        >>| (fun e -> Unary (Positive, e))
        <|> perror "Not found sub-expression of positive unary")
    <|> symbol '-'
        *> (basic_expr applyable
            >>| (fun e -> Unary (Negate, e))
            <|> perror "Not found sub-expression of negate unary")
  in
  (skip_ws *> (helper <|> symbol '~' *> helper)) state

(** Parser of brackets expression:
    - unit: [()]
    - else: [(<expr>)] *)
and bracket_expr state =
  let helper = expr <|> preturn (Const UnitLiteral) in
  bracket_sequence helper state

(** Parser of list expression *)
and list_expr state =
  let helper ex = element_sequence ex tuple_expr (fun l -> ExpressionsList l) ";" pfail in
  (skip_ws *> symbol '[' *> (tuple_expr >>= helper <|> preturn (ExpressionsList []))
   <* (skip_ws *> symbol ']' <|> perror "Not found close bracket of list expression"))
    state

(** Abstract parser of binary operations
    - [subparser]: parser of subexpression
    - [operations]: list of one priorety level binary operations *)
and binary_expression subparser operations state =
  let helper oper =
    ssequence oper.oper_view
    >>= fun cl ->
    if oper.oper_view = "-"
    then reverse (ssequence ">") (preturn cl) pfail
    else preturn cl
  in
  let operation left oper =
    skip_ws
    *> helper oper
    *> (subparser
        >>| (fun right -> Binary (left, oper.oper_ast, right))
        <|> perror
              (Printf.sprintf
                 "Not found right operand of '%s' binary operator"
                 oper.oper_view))
  in
  let rec next ex =
    skip_ws *> (one_of (List.map (operation ex) operations) >>= next) <|> preturn ex
  in
  (skip_ws *> subparser >>= next) state

(** Parser of binary expressions such as [<expr> * <expr>] and [<expr> / <expr>] *)
and multiply_expr state =
  binary_expression
    (basic_expr false)
    [ { oper_view = "*"; oper_ast = Multiply }; { oper_view = "/"; oper_ast = Division } ]
    state

(** Parser of binary expressions such as [<expr> + <expr>] and [<expr> - <expr>] *)
and summary_expr state =
  binary_expression
    multiply_expr
    [ { oper_view = "+"; oper_ast = Add }; { oper_view = "-"; oper_ast = Subtract } ]
    state

(** Parser of binary expressions such as
    - [<expr> = <expr>]
    - [<expr> <> <expr>]
    - [<expr> > <expr>]
    - [<expr> < <expr>]
    - [<expr> >= <expr>]
    - [<expr> <= <expr>] *)
and compare_expr state =
  binary_expression
    summary_expr
    [ { oper_view = "="; oper_ast = Equals }
    ; { oper_view = "<>"; oper_ast = Unequals }
    ; { oper_view = ">="; oper_ast = Gte }
    ; { oper_view = "<="; oper_ast = Lte }
    ; { oper_view = ">"; oper_ast = Gt }
    ; { oper_view = "<"; oper_ast = Lt }
    ]
    state

(** Parser of binary expressions such as [<expr> && <expr>] and [<expr> || <expr>] *)
and boolean_expr state =
  binary_expression
    compare_expr
    [ { oper_view = "&&"; oper_ast = And }; { oper_view = "||"; oper_ast = Or } ]
    state

and if_expr state =
  let else_block ex then_ex =
    skip_ws *> (expr >>| fun else_ex -> If (ex, then_ex, Some else_ex))
    <|> perror "Expected expression of 'else' branch for if expression"
  in
  let then_block ex =
    skip_ws
    *> (expr
        >>= fun then_ex ->
        skip_ws *> (keyword "else" *> else_block ex then_ex)
        <|> preturn (If (ex, then_ex, None)))
    <|> perror "Expected expression of 'then' branch for if expression"
  in
  (skip_ws
   *> keyword "if"
   *> (expr
       >>= (fun ex ->
             skip_ws
             *> (keyword "then" *> then_block ex
                 <|> perror "Not found 'then' branch for if-expression"))
       <|> perror "Not found if expression after keyword 'if'"))
    state

(** Parser of apply expressions such as [<applyable_expr>  <expr list>]*)
and apply_expr state =
  let bin_op_checker =
    skip_ws
    *> one_of
         (List.map
            assequence
            [ "+"; "-"; "/"; "*"; "&&"; "||"; "<="; ">="; "<>"; "<"; ">"; "=" ])
  in
  let helper ex =
    many (skip_ws *> basic_expr true)
    >>= fun l -> preturn (if is_empty l then ex else Apply (ex, l))
  in
  (skip_ws *> applyable_expr >>= fun ex -> bin_op_checker *> preturn ex <|> helper ex)
    state

(** Parser of lambdas definitions *)
and lambda_expr state =
  let patterns = skip_ws *> lmany1 (pattern_parser true) in
  let subexp pl =
    expr >>| (fun ex -> Lambda (pl, ex)) <|> perror "Not found expression of lambda"
  in
  let helper l =
    repeat (pattern_parser true) (l - 1)
    >>= fun pl ->
    pattern_parser false
    >>= ptype false
    >>= fun p ->
    skip_ws
    *> (ssequence "->" <|> perror "Not found special sequence '->' of lambda definition")
    *> preturn (pl @ [ p ])
  in
  (skip_ws *> keyword "fun"
   >>= fun _ s ->
   (patterns
    >>= (fun (l, pl) ->
          skip_ws *> (ssequence "->" *> subexp pl) <|> fun _ -> (helper l >>= subexp) s)
    <|> perror "Not found patterns for lambda definition")
     s)
    state

(** Parser of value-bindings: [<pattern> = <expr>]*)
and value_binding_parser state =
  let subpatterns = skip_ws *> many (skip_ws *> pattern_parser true) in
  let helper =
    pattern_parser true
    >>= fun p ->
    subpatterns >>= fun pl -> ptype true p >>= fun typedp -> preturn (typedp, pl)
  in
  (skip_ws
   *> (helper
       >>= fun (p, pl) ->
       skip_ws
       *> ssequence "="
       *> skip_ws
       *> (expr >>| fun ex -> if is_empty pl then p, ex else p, Lambda (pl, ex))
       <|> perror "Not found expression of let-definition"
       <|> perror "Not found special sequence '=' of let-definition binding expresssion")
   <|> perror "Not found name-pattern of let-definition")
    state

and value_bindings_parser state =
  (skip_ws *> value_binding_parser
   >>= (fun vb ->
         element_sequence
           vb
           value_binding_parser
           Fun.id
           "and"
           (perror "Not found value binding"))
   <|> perror "Not found value binging")
    state

(** Parser of let-definitions *)
and define_expr state =
  let inexpr =
    skip_ws
    *> (keyword "in"
        *> (expr <|> perror "Not found expression after keyword 'in' of let-xpression"))
    <|> perror "Not found keyword for let-expression"
  in
  let recursive =
    skip_ws *> keyword "rec" *> value_bindings_parser
    >>= fun vbl -> inexpr >>| fun ex -> Define ((Recursive, vbl), ex)
  in
  let nonrecursive =
    skip_ws *> value_bindings_parser
    >>= fun vbl -> inexpr >>| fun ex -> Define ((Nonrecursive, vbl), ex)
  in
  (skip_ws *> keyword "let" *> (recursive <|> nonrecursive)) state
;;

(** Parser of definition item *)
let define_item =
  let inexpr =
    skip_ws
    *> keyword "in"
    *> (expr <|> perror "Not found expression after keyword 'in' of let-xpression")
  in
  let recursive =
    skip_ws *> keyword "rec" *> value_bindings_parser
    >>= fun vbl ->
    inexpr
    >>| (fun ex -> EvalItem (Define ((Recursive, vbl), ex)))
    <|> preturn (DefineItem (Recursive, vbl))
  in
  let nonrecursive =
    skip_ws *> value_bindings_parser
    >>= fun vbl ->
    inexpr
    >>| (fun ex -> EvalItem (Define ((Nonrecursive, vbl), ex)))
    <|> preturn (DefineItem (Nonrecursive, vbl))
  in
  skip_ws *> keyword "let" *> (recursive <|> nonrecursive)
;;

(** Parser of eval item *)
let eval_item = skip_ws *> expr >>| fun ex -> EvalItem ex

(** Parser of all stricture item *)
let struct_item_parser =
  skip_ws
  *> (define_item
      <|> eval_item
      >>= fun item ->
      skip_ws
      *> (ssequence ";;" *> preturn item
          <|> perror "Not found close semicolons ';;' of structure item"))
;;

(** Parser of program item *)
let program_parser = skip_ws *> many struct_item_parser
