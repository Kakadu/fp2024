[@@@ocaml.text "/*"]

(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open QCheck
open FSharpActivePatterns.Ast
open FSharpActivePatterns.PrintAst
open FSharpActivePatterns.Parser

let int_e x = Const (Int_lt x)
let bool_e x = Const (Bool_lt x)
let unit_e = Const Unit_lt
let string_e x = Const (String_lt x)

let gen_const =
  QCheck.Gen.(
    frequency
      [ 1, map int_e nat; 1, map bool_e bool; 1, return unit_e; 0, map string_e string ])
;;

let variable_e x = Variable (Ident x)

let gen_variable_name =
  let open QCheck.Gen in
  let gen_char_of_range l r = map Char.chr (int_range (Char.code l) (Char.code r)) in
  let gen_first_char =
    oneof [ gen_char_of_range 'a' 'z'; gen_char_of_range 'A' 'Z'; return '_' ]
  in
  let gen_next_char = oneof [ gen_first_char; gen_char_of_range '0' '9' ] in
  let gen_varname =
    map2
      (fun first rest ->
        String.make 1 first ^ String.concat "" (List.map (String.make 1) rest))
      gen_first_char
      (list_size (1 -- 5) gen_next_char)
  in
  map variable_e gen_varname
;;

let tuple_e l = Tuple l
let un_e unop e = Unary_expr (unop, e)
let gen_unop = QCheck.Gen.(oneof @@ List.map return [ Unary_minus; Unary_not ])
let bin_e op e1 e2 = Bin_expr (op, e1, e2)

let gen_binop =
  QCheck.Gen.(
    oneof
    @@ List.map
         return
         [ Binary_equal
         ; Binary_unequal
         ; Binary_less
         ; Binary_less_or_equal
         ; Binary_greater
         ; Binary_greater_or_equal
         ; Binary_add
         ; Binary_subtract
         ; Binary_multiply
         ; Logical_or
         ; Logical_and
         ; Binary_divide
           (* ; Binary_or_bitwise
              ; Binary_xor_bitwise
              ; Binary_and_bitwise *)
         ])
;;

let gen_expr =
  QCheck.Gen.(
    sized
    @@ fix (fun self n ->
      match n with
      | 0 -> frequency [ 1, gen_const; 1, gen_variable_name ]
      | n ->
        frequency
          [ 0, map tuple_e (list_size (0 -- 15) (self (n / 2)))
          ; 0, map2 un_e gen_unop (self (n / 2))
          ; 1, map3 bin_e gen_binop (self (n / 2)) (self (n / 2))
          ]))
;;

let shrink_lt =
  let open QCheck.Iter in
  function
  | Int_lt x -> QCheck.Shrink.int x >|= int_e
  | Bool_lt _ -> empty
  | Unit_lt -> empty
  | String_lt x -> QCheck.Shrink.string x >|= string_e
;;

let rec shrink_expr =
  let open QCheck.Iter in
  function
  | Const lt -> shrink_lt lt
  | Variable _ -> empty
  | Tuple t -> QCheck.Shrink.list ~shrink:shrink_expr t >|= fun a' -> Tuple a'
  | Unary_expr (op, e) -> return e <+> shrink_expr e >|= fun a' -> un_e op a'
  | Bin_expr (op, e1, e2) ->
    of_list [ e1; e2 ]
    <+> (shrink_expr e1 >|= fun a' -> bin_e op a' e2)
    <+> (shrink_expr e2 >|= fun a' -> bin_e op e1 a')
  | _ -> empty
;;

(* TODO *)
let rec shrink_statement =
  let open QCheck.Iter in
  function
  | Let (rec_flag, ident, args, expr) ->
    shrink_expr expr >|= fun a' -> Let (rec_flag, ident, args, a')
  | _ -> empty
;;

let gen_construction = QCheck.Gen.map (fun e -> Expr e) gen_expr

let shrink_construction =
  let open QCheck.Iter in
  function
  | Expr e -> shrink_expr e >|= fun a' -> Expr a'
  | Statement s -> shrink_statement s >|= fun a' -> Statement a'
;;

let arbitrary_construction =
  QCheck.make
    gen_construction
    ~print:(Format.asprintf "%a" print_construction)
    ~shrink:shrink_construction
;;

let run n =
  QCheck_runner.run_tests
    [ QCheck.(
        Test.make arbitrary_construction ~count:n (fun c ->
          Some c = parse (Format.asprintf "%a\n" print_construction c)))
    ]
;;
