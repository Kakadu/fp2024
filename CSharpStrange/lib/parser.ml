open Ast
open Angstrom
open Base

(* Chain functions *)
let chainl0 expr op = op >>= (fun op1 -> expr >>| fun exp -> op1 exp) <|> expr

let chainl1 expr op =
  let rec pars e1 = lift2 (fun op1 e2 -> op1 e1 e2) op expr >>= pars <|> return e1 in
  expr >>= fun init -> pars init
;;

let chainr1 expr op =
  fix (fun x ->
    lift2 (fun op1 e2 -> op1 e2) (lift2 (fun e1 op2 -> op2 e1) expr op) x <|> expr)
;;

(* Special functions *)
let reserved =
  [ "true"
  ; "false"
  ; "if"
  ; "else"
  ; "while"
  ; "public"
  ; "static"
  ; "const"
  ; "void"
  ; "string"
  ; "char"
  ; "int"
  ; "bool"
  ; "for"
  ; "null"
  ; "new"
  ; "return"
  ; "break"
  ; "continue"
  ; "class"
  ; "async"
  ; "await"
  ; "select"
  ; "from"
  ]
;;

let in_reserved t = List.mem reserved t ~equal:String.equal

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let is_token_sym = function
  | 'a' .. 'z' | '0' .. '9' | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

let skip_spaces = skip_while is_space
let parens p = skip_spaces *> char '(' *> p <* skip_spaces <* char ')'
let braces p = skip_spaces *> char '{' *> p <* skip_spaces <* char '}'
let brackets p = skip_spaces *> char '[' *> p <* skip_spaces <* char ']'
let skip_semicolons = fix (fun f -> skip_spaces *> char ';' *> f <|> return "")
let skip_semicolons1 = skip_spaces *> char ';' *> skip_semicolons

(* Values *)

let parse_int =
  take_while1 Char.is_digit
  >>= fun num -> return @@ ValInt (int_of_string num) <|> fail "Not an int"
;;

let parse_char =
  char '\'' *> any_char
  <* char '\''
  >>= (fun c -> return @@ ValChar c)
  <|> fail "Not a char"
;;

let parse_bool =
  choice
    [ string "true" *> return (ValBool true); string "false" *> return (ValBool false) ]
  <|> fail "Not a bool"
;;

let parse_val_string =
  char '\"'
  *> take_till (function
    | '\"' -> true
    | _ -> false)
  <* char '\"'
  >>= (fun s -> return @@ ValString s)
  <|> fail "Not a string"
;;

let parse_null = string "null" *> return ValNull <|> fail "Not a null"

(* TODO: parse_array *)
let parse_array = fail "Not implemented"
let val_to_expr p = skip_spaces *> p >>| fun x -> EValue x

let parse_value =
  choice
    [ val_to_expr parse_bool
    ; val_to_expr parse_char
    ; val_to_expr parse_int
    ; val_to_expr parse_null
    ; val_to_expr parse_val_string
    ]
;;

let parse_id =
  take_while is_token_sym
  >>= fun str ->
  match not (String.is_empty str || in_reserved str || Char.is_digit str.[0]) with
  | true -> return (Id str)
  | _ -> fail "Not an identifier"
;;

(* Modifiers *)

let parse_modifiers =
  many
    (choice
       [ string "public" *> skip_spaces *> return MPublic
       ; string "static" *> skip_spaces *> return MStatic
       ; string "const" *> skip_spaces *> return MConst
       ; string "async" *> skip_spaces *> return MAsync
       ])
;;

(* Type words *)
let parse_type_word =
  (* TODO REMOVE AST!!!! *)
  take_while is_token_sym
  >>= function
  | "int" -> return @@ TypeBase TypeInt
  | "char" -> return @@ TypeBase TypeChar
  | "bool" -> return @@ TypeBase TypeBool
  | "string" -> return @@ TypeString
  | _ -> fail "Wrong type word"
;;

(* Expressions *)

(* Variables && functions *)
let parse_var_type =
  parse_type_word >>= fun x -> return (TypeVar x) <|> fail "Incorrect type"
;;

let parse_var =
  let parse_decl_id typ_ =
    char ' ' *> skip_spaces *> parse_id >>| fun id -> Var (typ_, id)
  in
  skip_spaces *> parse_var_type >>= parse_decl_id
;;

let parse_id_expr = skip_spaces *> parse_id >>= fun x -> return @@ EId x
let parse_call_id = parse_id_expr (* TODO Program.x *)

let parse_args_list arg =
  let args = arg <* skip_spaces *> char ',' <|> arg in
  parens @@ many args
;;

let parse_call_args id arg =
  parse_args_list arg >>= fun args -> return @@ EFuncCall (id, args)
;;

let parse_call_expr arg = parse_call_id >>= fun id -> parse_call_args id arg

(* Operations *)
let parse_op op typ = skip_spaces *> string op *> return typ

(* Binary operations *)
let parse_bin_op op typ = parse_op op typ >>| fun t a b -> EBinOp (t, a, b)
let ( ^+^ ) = parse_bin_op "+" OpAdd
let ( ^-^ ) = parse_bin_op "-" OpSub
let ( ^*^ ) = parse_bin_op "*" OpMul
let ( ^/^ ) = parse_bin_op "/" OpDiv
let ( ^%^ ) = parse_bin_op "%" OpMod
let ( ^==^ ) = parse_bin_op "==" OpEqual
let ( ^!=^ ) = parse_bin_op "!=" OpNonEqual
let ( ^<^ ) = parse_bin_op "<" OpLess
let ( ^>^ ) = parse_bin_op ">" OpMore
let ( ^<=^ ) = parse_bin_op "<=" OpLessEqual
let ( ^>=^ ) = parse_bin_op ">=" OpMoreEqual
let ( ^&&^ ) = parse_bin_op "&&" OpAnd
let ( ^||^ ) = parse_bin_op "||" OpOr
let ( ^=^ ) = parse_bin_op "=" OpAssign

(* Unary operations *)
let parse_un_op op typ = parse_op op typ >>| fun t a -> EUnOp (t, a)

(*TODO: check for increment/decrement ??*)
let ( ^!^ ) = parse_un_op "!" OpNot
let parse_new = parse_un_op "new" OpNew

let parse_ops =
  fix (fun expr ->
    let lv1 = choice [ parens expr; parse_value; parse_call_expr expr; parse_id_expr ] in
    let lv2 = chainl0 lv1 (choice [ parse_new; ( ^!^ ) ]) in
    let lv3 = chainl1 lv2 (choice [ ( ^*^ ); ( ^/^ ); ( ^%^ ) ]) in
    let lv4 = chainl1 lv3 (choice [ ( ^+^ ); ( ^-^ ) ]) in
    let lv5 = chainl1 lv4 (choice [ ( ^<=^ ); ( ^>=^ ); ( ^<^ ); ( ^>^ ) ]) in
    let lv6 = chainl1 lv5 (choice [ ( ^==^ ); ( ^!=^ ) ]) in
    let lv7 = chainl1 lv6 (choice [ ( ^&&^ ) ]) in
    let lv8 = chainl1 lv7 (choice [ ( ^||^ ) ]) in
    chainr1 lv8 (choice [ ( ^=^ ) ]))
;;

let parse_assign = lift3 (fun id eq ex -> eq id ex) parse_id_expr ( ^=^ ) parse_ops

(* Statements + LINQ *)

let get_opt p = p >>| fun x -> Some x

let parse_decl =
  lift2
    (fun dcl e -> SDecl (dcl, e))
    parse_var
    (option None (skip_spaces *> char '=' *> parse_ops >>| fun e -> Some e))
;;

(* TODO: check other return "" *)
let expr_to_stmt expr = expr >>| fun x -> SExpr x
let parse_stmt_ops = expr_to_stmt @@ choice [ parse_assign; parse_call_expr parse_ops ]

(* TODO: Check block contains (esp. other ifs) *)
let parse_if_else f_if_body =
  let parse_if_cond = string "if" *> skip_spaces *> parens parse_ops in
  let parse_else_cond ifls body =
    skip_spaces
    *> (get_opt @@ (string "else" *> skip_spaces *> choice [ ifls; body ]) <|> return None)
  in
  fix (fun ifls ->
    let parse_body = f_if_body <|> (parse_stmt_ops <* skip_semicolons1) in
    let parse_else_body = parse_else_cond ifls parse_body in
    lift3
      (fun cond if_body else_body -> SIf (cond, if_body, else_body))
      parse_if_cond
      parse_body
      parse_else_body)
;;

(* TODO: Check block contains *)
let parse_for body =
  let expr_to_option_stmt expr = get_opt @@ expr_to_stmt expr in
  let p_body = body <|> (parse_stmt_ops <* skip_semicolons1) in
  let p_for_init =
    option None (get_opt parse_decl <|> expr_to_option_stmt parse_assign)
  in
  let p_for_expr = option None (get_opt parse_ops) in
  let p_for =
    lift2
      (fun (f_init_p, f_cond_p, f_iter_p) f_body ->
         SFor (f_init_p, f_cond_p, f_iter_p, f_body))
      (parens
       @@ lift3
            (fun init cond incr -> init, cond, incr)
            (p_for_init <* skip_spaces <* char ';')
            (p_for_expr <* skip_spaces <* char ';')
            p_for_expr)
      p_body
  in
  string "for" *> p_for
;;

let parse_while body =
  let p_body = body <|> skip_semicolons1 *> parse_stmt_ops in
  let p_cond = parens parse_ops in
  let p_while = string "while" *> skip_spaces *> p_cond in
  lift2 (fun cond body -> SWhile (cond, body)) p_while p_body
;;

let parse_return =
  lift2
    (fun _ expr -> SReturn expr)
    (string "return")
    (parse_ops >>= (fun ret -> return (Some ret)) <|> return None)
;;

let parse_break = skip_spaces *> string "break" *> return SBreak
let parse_continue = skip_spaces *> string "continue" *> return SContinue

(* {{}} TODO ??*)

let parse_block =
  fix (fun block ->
    let sc p = p <* skip_semicolons1 in
    let op_sc p = p <* skip_semicolons in
    let body_step =
      choice
        ?failure_msg:(Some "Error in some block sentence")
        [ sc parse_decl
        ; sc parse_break
        ; sc parse_continue
        ; sc parse_return
        ; sc parse_stmt_ops
        ; op_sc @@ parse_if_else block
        ; op_sc @@ parse_for block
        ; op_sc @@ parse_while block
        ]
    in
    braces (skip_semicolons *> many (skip_spaces *> body_step))
    >>= fun stmt_lst -> return @@ SBlock stmt_lst)
;;

(* Program class functions *)
(* TODO - tests!! *)

let parse_field_sign =
  let f_value = skip_spaces *> char '=' *> get_opt parse_ops in
  lift4
    (fun f_modif f_type f_id f_val -> f_modif, f_type, f_id, f_val)
    (skip_spaces *> parse_modifiers)
    (skip_spaces *> parse_var_type)
    (skip_spaces *> parse_id)
    (option None f_value)
  <* skip_semicolons1
;;

let parse_method_type =
  (* TODO Fix!! *)
  let parse_void =
    take_while is_token_sym
    >>= fun x ->
    if String.( = ) x "void" then return @@ TypeBase TypeVoid else fail "Not a type"
  in
  choice [ (parse_type_word >>= fun x -> return @@ x); parse_void ]
;;

let parse_method_sign =
  let parse_args =
    parens @@ many @@ (skip_spaces *> parse_var <* skip_spaces <* char ',')
    >>= fun exp -> return (Params exp)
  in
  lift4
    (fun m_modif m_type m_id m_params -> m_modif, m_type, m_id, m_params)
    (skip_spaces *> parse_modifiers)
    (skip_spaces *> parse_var_type)
    (skip_spaces *> parse_id)
    parse_args
;;

let parse_method_member =
  lift2
    (fun (mds, tp, id, ps) bd -> Method (mds, tp, id, ps, bd))
    parse_method_sign
    parse_block
;;

let parse_field_member =
  parse_field_sign >>| fun (mds, tp, id, ex) -> VarField (mds, tp, id, ex)
;;

let parse_class_members =
  let member = choice [ parse_method_member; parse_field_member ] in
  braces @@ many member
;;

let parse_one_class1 =
  let class_id = skip_spaces *> string "class" *> skip_spaces *> parse_id in
  lift3
    (fun cl_modif cl_id cl_membs -> Class (cl_modif, cl_id, cl_membs))
    (skip_spaces *> parse_modifiers)
    class_id
    parse_class_members
;;

let parse_one_class = return (Class ([], Id "Not implemented", []))
let parse_prog : program t = parse_one_class <* skip_spaces >>| fun prog -> Program prog

(* Main functions *)

let parse_option p str =
  match parse_string p ~consume:Angstrom.Consume.All str with
  | Ok x -> Some x
  | Error _ -> None
;;

let apply_parser parser = parse_string ~consume:Consume.All parser
