(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open KeywordChecker
open TypedTree
open TypesPp

type ident = Ident of string (** identifier *) [@@deriving show { with_path = false }]

let gen_varname =
  let open QCheck.Gen in
  let loop =
    let gen_char_of_range l r = map Char.chr (int_range (Char.code l) (Char.code r)) in
    let gen_first_char =
      frequency
        [ 26, gen_char_of_range 'a' 'z'; 26, gen_char_of_range 'A' 'Z'; 1, return '_' ]
    in
    let gen_next_char =
      frequency [ 26 + 26 + 1, gen_first_char; 10, gen_char_of_range '0' '9' ]
    in
    map2
      (fun first rest -> String.make 1 first ^ Base.String.of_char_list rest)
      gen_first_char
      (list_size (1 -- 3) gen_next_char)
  in
  loop >>= fun name -> if is_keyword name then loop else return name
;;

let gen_ident = QCheck.Gen.map (fun s -> Ident s) gen_varname
(* let gen_ident_small_list = QCheck.Gen.(list_size (0 -- 3) gen_ident) *)

let gen_escape_sequence =
  let open QCheck.Gen in
  oneofl [ "\\\""; "\\\\"; "\\n"; "\\t" ]
;;

let gen_string_of_regular_char =
  let open QCheck.Gen in
  let gen_int =
    frequency
      [ 33 - 32 + 1, int_range 32 33
      ; 91 - 35 + 1, int_range 35 91
      ; 126 - 93 + 1, int_range 93 126
      ]
  in
  map (fun c -> String.make 1 c) (map Char.chr gen_int)
;;

let gen_string =
  let open QCheck.Gen in
  let atom = frequency [ 1, gen_escape_sequence; 30, gen_string_of_regular_char ] in
  let+ atoms = list_size (0 -- 20) atom in
  String.concat "" atoms
;;

type literal =
  | Int_lt of (int[@gen QCheck.Gen.pint]) (** [0], [1], [30] *)
  | Bool_lt of bool (** [false], [true] *)
  | String_lt of (string[@gen gen_string]) (** ["Hello world"] *)
  | Unit_lt (** [Unit] *)
[@@deriving show { with_path = false }, qcheck]

type binary_operator =
  | Binary_equal (** [=] *)
  | Binary_unequal (** [<>] *)
  | Binary_less (** [<] *)
  | Binary_less_or_equal (** [<=] *)
  | Binary_greater (** [>] *)
  | Binary_greater_or_equal (** [>=] *)
  | Binary_add (** [+] *)
  | Binary_subtract (** [-] *)
  | Binary_multiply (** [*] *)
  | Logical_or (** [||] *)
  | Logical_and (** [&&] *)
  | Binary_divide (** [/] *)
  | Binary_or_bitwise (** [|||] *)
  | Binary_xor_bitwise (** [^^^] *)
  | Binary_and_bitwise (** [&&&] *)
  | Binary_cons (** [::] *)
[@@deriving show { with_path = false }, qcheck]

type unary_operator =
  | Unary_minus (** unary [-] *)
  | Unary_not (** unary [not] *)
[@@deriving show { with_path = false }, qcheck]

type pattern =
  | Wild (** [_] *)
  | PList of
      (pattern list[@gen QCheck.Gen.(list_size (0 -- 3) (gen_pattern_sized (n / 20)))])
  (**[ [], [1;2;3] ] *)
  | PCons of pattern * pattern (**[ hd :: tl ] *)
  | PTuple of
      pattern
      * pattern
      * (pattern list[@gen QCheck.Gen.(list_size (0 -- 2) (gen_pattern_sized (n / 20)))])
  (** | [(a, b)] -> *)
  | PConst of literal (** | [4] -> *)
  | PVar of ident (** pattern identifier *)
  | POption of pattern option
  (*| Variant of (ident list[@gen gen_ident_small_list]) (** | [Blue, Green, Yellow] -> *) *)
  | PConstraint of pattern * (typ[@gen gen_typ_primitive])
[@@deriving show { with_path = false }, qcheck]

type is_recursive =
  | Nonrec (** let factorial n = ... *)
  | Rec (** let rec factorial n = ... *)
[@@deriving show { with_path = false }, qcheck]

type case = (pattern[@gen gen_pattern_sized n]) * (expr[@gen gen_expr_sized n])
[@@deriving show { with_path = false }, qcheck]

and expr =
  | Const of literal (** [Int], [Bool], [String], [Unit], [Null] *)
  | Tuple of
      (expr[@gen gen_expr_sized (n / 4)])
      * (expr[@gen gen_expr_sized (n / 4)])
      * (expr list[@gen QCheck.Gen.(list_size (0 -- 2) (gen_expr_sized (n / 20)))])
  (** [(1, "Hello world", true)] *)
  | List of (expr list[@gen QCheck.Gen.(list_size (0 -- 3) (gen_expr_sized (n / 20)))])
  (** [], [1;2;3] *)
  | Variable of ident (** [x], [y] *)
  | Unary_expr of unary_operator * expr (** -x *)
  | Bin_expr of binary_operator * expr * expr (** [1 + 2], [3 ||| 12], hd :: tl *)
  | If_then_else of
      (expr[@gen gen_expr_sized (n / 4)])
      * (expr[@gen gen_expr_sized (n / 4)])
      * (expr option[@gen QCheck.Gen.option (gen_expr_sized (n / 4))])
  (** [if n % 2 = 0 then "Even" else "Odd"] *)
  | Lambda of
      (pattern[@gen gen_pattern_sized (n / 2)])
      * (pattern list[@gen QCheck.Gen.(list_size (0 -- 2) (gen_pattern_sized (n / 20)))])
      * expr (** fun x y -> x + y *)
  | Apply of (expr[@gen gen_expr_sized (n / 4)]) * (expr[@gen gen_expr_sized (n / 4)])
  (** [sum 1 ] *)
  | Function of
      (case[@gen gen_case_sized (n / 4)])
      * (case list[@gen QCheck.Gen.(list_size (0 -- 2) (gen_case_sized (n / 20)))])
  (** [function | p1 -> e1 | p2 -> e2 | ... |]*)
  | Match of
      (expr[@gen gen_expr_sized (n / 4)])
      * (case[@gen gen_case_sized (n / 4)])
      * (case list[@gen QCheck.Gen.(list_size (0 -- 2) (gen_case_sized (n / 20)))])
  (** [match x with | p1 -> e1 | p2 -> e2 | ...] *)
  | LetIn of
      is_recursive
      * let_bind
      * (let_bind list
        [@gen QCheck.Gen.(list_size (0 -- 2) (gen_let_bind_sized (n / 20)))])
      * expr (** [let rec f x = if (x <= 0) then x else g x and g x = f (x-2) in f 3] *)
  | Option of expr option (** [int option] *)
  | EConstraint of expr * (typ[@gen gen_typ_primitive])
[@@deriving show { with_path = false }, qcheck]

and let_bind =
  | Let_bind of
      (pattern[@gen gen_pattern_sized (n / 2)])
      * (pattern list[@gen QCheck.Gen.(list_size (0 -- 3) (gen_pattern_sized (n / 4)))])
      * expr (** [let sum n m = n + m] *)
[@@deriving show { with_path = false }, qcheck]

let gen_expr =
  QCheck.Gen.(
    let* n = small_nat in
    gen_expr_sized n)
;;

let gen_let_bind =
  QCheck.Gen.(
    let* n = small_nat in
    gen_let_bind_sized n)
;;

type statement =
  | Let of
      is_recursive
      * let_bind
      * (let_bind list[@gen QCheck.Gen.(list_size (0 -- 2) gen_let_bind)])
  (** [let name = expr] *)
(*| ActivePattern of (ident list[@gen gen_ident_small_list]) * expr
  (** [let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd] *)*)
[@@deriving show { with_path = false }, qcheck]

type construction =
  | Expr of expr (** expression *)
  | Statement of statement (** statement *)
[@@deriving show { with_path = false }, qcheck]
