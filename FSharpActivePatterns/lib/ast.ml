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

type literal =
  | Int_lt of int (** [0], [1], [30] *)
  | Bool_lt of bool (** [false], [true] *)
  | String_lt of string (** ["Hello world"] *)
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

type pattern =
  | Wild (** [_] *)
  | PEmptyList (** [] *)
  | PCons of pattern * pattern (** | [hd :: tl] -> *)
  | PTuple of pattern * pattern * pattern list (** | [(a, b)] -> *)
  (* [@gen QCheck.Gen.(list_size (0 -- 15) gen_pattern_sized ) ] *)
  | PConst of literal (** | [4] -> *)
  | PVar of ident (** pattern identifier *)
  | Variant of ident list (** | [Blue, Green, Yellow] -> *)
    (* [@gen QCheck.Gen.(list_size (0 -- 15) gen_ident ) ] *)
[@@deriving eq, show { with_path = false }, qcheck]

type is_recursive =
  | Nonrec (** let factorial n = ... *)
  | Rec (** let rec factorial n = ... *)
[@@deriving eq, show { with_path = false }, qcheck]

type expr =
  | Const of literal (** [Int], [Bool], [String], [Unit], [Null] *)
  | Tuple of expr * expr * expr list (** [(1, "Hello world", true)] *)
  (* [@gen QCheck.Gen.(list_size (0 -- 15) gen_expr ) ] *)
  | Empty_list (** [] *)
  | Cons_list of expr * expr
  (** {[
        [ 1; 2; 3 ] [ 1; 2; 3 ]
      ]} *)
  | Variable of ident (** [x], [y] *)
  | Unary_expr of unary_operator * expr (** -x *)
  | Bin_expr of binary_operator * expr * expr (** [1 + 2], [3 ||| 12] *)
  | If_then_else of expr * expr * expr option (** [if n % 2 = 0 then "Even" else "Odd"] *)
  | Lambda of ident list * expr (** fun x y -> x + y *)
  | Function_call of expr * expr (** [sum 1 ] *)
  | Match of expr * pattern * expr * (pattern * expr) list
  (** [match x with | x -> ... | y -> ...] *)
  (* [@gen QCheck.Gen.(quad gen_expr_sized gen_pattern_sized gen_expr_sized (list_size (0 -- 15) ( pair gen_pattern_sized gen_expr_sized)))] *)
  | LetIn of is_recursive * let_bind * let_bind list * expr
  (** [let rec f x = if (x <= 0) then x else g x and g x = f (x-2) in f 3] *)
  (* [@gen QCheck.Gen.(quad gen_is_recursive gen_let_bind (list_size (0--15) gen_let_bind) gen_expr_sized)] *)
  | Option of expr option (** [int option] *)
[@@deriving eq, show { with_path = false }, qcheck]

and let_bind = Let_bind of ident * ident list * expr (** [and sum n m = n+m] *)[@@deriving eq, show {with_path = false}, qcheck]

type statement =
  | Let of is_recursive * let_bind * let_bind list (** [let name = expr] *)
  (* [@gen QCheck.Gen.(triple gen_is_recursive gen_let_bind (list_size (0 --15) gen_let_bind) ) ] *)
  | ActivePattern of ident list * expr
  (** [let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd] *)
[@@deriving eq, show { with_path = false }, qcheck]

type construction =
  | Expr of expr (** expression *) [@gen QCheck.Gen.sized gen_expr_sized]
  | Statement of statement (** statement *)
[@@deriving eq, show { with_path = false }, qcheck]
