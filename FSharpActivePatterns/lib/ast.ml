(** Copyright 2024-2025, Ksenia Kotelnikova <xeniia.ka@gmail.com>, Gleb Nasretdinov <gleb.nasretdinov@proton.me> *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open KeywordChecker

type ident = Ident of string * string option (** identifier *)
[@@deriving eq, show { with_path = false }]

let gen_varname =
  let open QCheck.Gen in
  let loop =
    let gen_char_of_range l r = map Char.chr (int_range (Char.code l) (Char.code r)) in
    let gen_first_char =
      oneof [ gen_char_of_range 'a' 'z'; gen_char_of_range 'A' 'Z'; return '_' ]
    in
    let gen_next_char = oneof [ gen_first_char; gen_char_of_range '0' '9' ] in
    map2
      (fun first rest ->
        String.make 1 first ^ String.concat "" (List.map (String.make 1) rest))
      gen_first_char
      (list_size (1 -- 5) gen_next_char)
  in
  loop >>= fun name -> if is_keyword name then loop else return name
;;

let gen_ident = QCheck.Gen.map (fun s -> Ident (s, None)) gen_varname
let gen_ident_small_list = QCheck.Gen.(list_size (0 -- 5) gen_ident)

let gen_char =
  let open QCheck.Gen in
  let rec loop () =
    let* char = char_range (Char.chr 32) (Char.chr 126) in
    match char with
    | '\\' -> loop ()
    | '"' -> loop ()
    | _ -> return char
  in
  loop ()
;;

type literal =
  | Int_lt of (int[@gen QCheck.Gen.pint]) (** [0], [1], [30] *)
  | Bool_lt of bool (** [false], [true] *)
  | String_lt of (string[@gen QCheck.Gen.string_of gen_char]) (** ["Hello world"] *)
  | Unit_lt (** [Unit] *)
[@@deriving eq, show { with_path = false }, qcheck]

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
[@@deriving eq, show { with_path = false }, qcheck]

type unary_operator =
  | Unary_minus (** unary [-] *)
  | Unary_not (** unary [not] *)
[@@deriving eq, show { with_path = false }, qcheck]

type 'a list_type =
  | Cons_list of 'a * 'a list_type
  | Empty_list
[@@deriving eq, show { with_path = false }, qcheck]

type pattern =
  | Wild (** [_] *)
  | PList of pattern list_type (**[ [], hd :: tl, [1;2;3] ]*)
  | PTuple of
      pattern
      * pattern
      * (pattern list[@gen QCheck.Gen.(list_size (0 -- 5) (gen_pattern_sized (n / 2)))])
  (** | [(a, b)] -> *)
  | PConst of literal (** | [4] -> *)
  | PVar of ident (** pattern identifier *)
    (*| Variant of (ident list[@gen gen_ident_small_list]) (** | [Blue, Green, Yellow] -> *) *)
[@@deriving eq, show { with_path = false }, qcheck]

type is_recursive =
  | Nonrec (** let factorial n = ... *)
  | Rec (** let rec factorial n = ... *)
[@@deriving eq, show { with_path = false }, qcheck]

type expr =
  | Const of literal (** [Int], [Bool], [String], [Unit], [Null] *)
  | Tuple of
      expr
      * expr
      * (expr list[@gen QCheck.Gen.(list_size (0 -- 5) (gen_expr_sized (n / 2)))])
  (** [(1, "Hello world", true)] *)
  | List of expr list_type (** [], hd :: tl, [1;2;3] *)
  | Variable of ident (** [x], [y] *)
  | Unary_expr of unary_operator * expr (** -x *)
  | Bin_expr of binary_operator * expr * expr (** [1 + 2], [3 ||| 12] *)
  | If_then_else of expr * expr * expr option (** [if n % 2 = 0 then "Even" else "Odd"] *)
  | Lambda of
      (pattern[@gen gen_pattern_sized (n / 2)])
      * (pattern list[@gen QCheck.Gen.(list_size (0 -- 5) (gen_pattern_sized (n / 2)))])
      * expr (** fun x y -> x + y *)
  | Apply of expr * expr (** [sum 1 ] *)
  | Match of
      expr
      * (pattern[@gen gen_pattern_sized (n / 2)])
      * expr
      * ((pattern * expr) list
        [@gen
          QCheck.Gen.(
            list_size (0 -- 5) (pair (gen_pattern_sized (n / 2)) (gen_expr_sized (n / 2))))])
  (** [match x with | x -> ... | y -> ...] *)
  | LetIn of
      is_recursive
      * let_bind
      * (let_bind list[@gen QCheck.Gen.(list_size (0 -- 5) (gen_let_bind_sized (n / 2)))])
      * expr (** [let rec f x = if (x <= 0) then x else g x and g x = f (x-2) in f 3] *)
  | Option of expr option (** [int option] *)
[@@deriving eq, show { with_path = false }, qcheck]

and let_bind =
  | Let_bind of ident * (ident list[@gen gen_ident_small_list]) * expr
  (** [and sum n m = n+m] *)
[@@deriving eq, show { with_path = false }, qcheck]

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
      * (let_bind list[@gen QCheck.Gen.(list_size (0 -- 5) gen_let_bind)])
  (** [let name = expr] *)
(*| ActivePattern of (ident list[@gen gen_ident_small_list]) * expr
  (** [let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd] *)*)
[@@deriving eq, show { with_path = false }, qcheck]

type construction =
  | Expr of expr (** expression *)
  | Statement of statement (** statement *)
[@@deriving eq, show { with_path = false }, qcheck]
