(** Copyright 2024, Vlasenco Daniel and Kudrya Alexandr *)

(** SPDX-License-Identifier: MIT *)

open QCheck.Gen
open Checks
open Base

let gen_id_first_char = frequency [ 5, char_range 'a' 'z'; 1, return '_' ]
let gen_digit = char_range '0' '9'

let gen_id_char =
  frequency [ 5, gen_id_first_char; 5, char_range 'A' 'Z'; 5, gen_digit; 1, return '\'' ]
;;

let gen_ident =
  let gen_name =
    let* fst = gen_id_first_char >|= fun c -> Char.to_string c in
    let range = if String.( = ) fst "_" then 1 -- 4 else 0 -- 4 in
    let* rest = string_size ~gen:gen_id_char range in
    return (fst ^ rest)
  in
  let rec loop gen =
    gen >>= fun name -> if not (is_keyword name) then return name else loop gen
  in
  loop gen_name
;;

(* Small positive integers (1 < |a| < 100) *)
let gen_posint =
  let rec loop gen = gen >>= fun n -> if n > 1 then return n else loop gen in
  loop small_nat
;;

let gen_type_var =
  let gen_type_var =
    map2 (fun fst num -> Printf.sprintf "%c%d" fst num) (char_range 'a' 'z') gen_posint
  in
  gen_type_var >>= fun type_var -> return ("'" ^ type_var)
;;

type measure_num =
  | Mnum_int of int (** Integer number in unit of measure *)
  | Mnum_float of float (** Real number in unit of measure *)
[@@deriving qcheck, show { with_path = false }]

type measure =
  | Measure_ident of (string[@gen gen_ident]) (** Measure identificator: [m] *)
  | Measure_prod of measure * measure (** Measure product: [sec * h], [kg m] *)
  | Measure_div of measure * measure (** Measure division: [m / sec] *)
  | Measure_pow of measure * (int[@gen gen_posint])
  (** Measure to the positive integer power: [m ^ n].
      Invariant: [n > 1] *)
  | Measure_dimless (** Dimensionless values, written as [1], as in [<1>] or [<1 / m>] *)
[@@deriving qcheck, show { with_path = false }]

(** Unit of measure: [1<m>], [9.8<kg m / s>], [0.3<kg^3>] etc. *)
type unit_of_measure = Unit_of_measure of measure_num * measure
[@@deriving qcheck, show { with_path = false }]

type constant =
  | Const_int of int (** Integer constants: [1] *)
  | Const_float of float (** Float constants: [3.14], [1e+5], [5.9E-3f] *)
  | Const_bool of bool (** Boolean constants [true] and [false] *)
  | Const_char of (char[@gen printable]) (** Char constants: ['a'] *)
  | Const_string of (string[@gen small_string ~gen:printable])
  (** String constants: ["foo"] *)
  | Const_unit (** Unit [()] *)
  | Const_unit_of_measure of unit_of_measure
  (** Units of measure constants: [5.0<cm>], [3<kg>] *)
[@@deriving qcheck, show { with_path = false }]

type core_type =
  | Type_int
  | Type_float
  | Type_bool
  | Type_char
  | Type_string
  | Type_unit
  | Type_var of (string[@gen gen_type_var])
  | Type_option of core_type
  | Type_list of core_type
  | Type_func of core_type * core_type (** Function type: [T1 -> T2] *)
  | Type_tuple of
      core_type
      * core_type
      * (core_type list[@gen small_list (gen_core_type_sized (n / 30))])
  (** [Type_tuple(T1, T2, [T3, ..., Tn])] represents:
      - [(T1 * T2)] when core_type list is []
      - [(T1 * T2 * T3 * ... * Tn)] when core_type list is Cons (A, B) *)
[@@deriving qcheck, show { with_path = false }]

type pattern =
  | Pattern_ident_or_op of (string[@gen gen_ident])
  (** Identificator or operation patterns: [x], [(+)] *)
  | Pattern_const of constant
  (** Constant patterns: [1], ['a'], ["foo"], [3.14], [5.0<cm>] *)
  | Pattern_wild (** Wildcard patterns [ _ ] *)
  | Pattern_typed of pattern * core_type (** Typed pattern [x : int] *)
  | Pattern_tuple of
      pattern * pattern * (pattern list[@gen small_list (gen_pattern_sized (n / 30))])
  (** [Pattern_tuple(P1, P2, [P3, ..., Pn])] represents:
      - [(P1, P2)] when pattern list is []
      - [(P1, P2, P3, ..., Pn)] when pattern list is Cons (A, B) *)
  | Pattern_list of (pattern list[@gen small_list (gen_pattern_sized (n / 30))])
  (** List patterns: [P1, ..., Pn] *)
  | Pattern_or of pattern * pattern
  (** OR patterns represent multiple satisfying patterns in pattern matching: [P1 | P2] *)
  | Pattern_cons of pattern * pattern (** CONS patterns as in [ x :: xs ] *)
  | Pattern_option of pattern option (** [Some p] or [None] *)
[@@deriving qcheck, show { with_path = false }]

type rec_flag =
  | Nonrecursive (** For nonrecursive function declarations *)
  | Recursive (** For recursive function declarations *)
[@@deriving qcheck, show { with_path = false }]

type expression =
  | Expr_const of constant
  (** Constant expressions: [1], ['a'], ["foo"], [3.14], [true], [5.0<cm>] *)
  | Expr_ident_or_op of (string[@gen gen_ident])
  (** Identificator or operation expressions: [x], [+] *)
  | Expr_typed of expression * core_type (** Typed expression: [(x: int)] *)
  | Expr_tuple of
      expression
      * expression
      * (expression list[@gen list_size (0 -- 4) (gen_expression_sized (n / 30))])
  (** [Expr_tuple(E1, E2, [E3, ..., En])] represents:
      - [(E1, E2)] when pattern list is []
      - [(E1, E2, E3, ..., En)] when pattern list is Cons (A, B) *)
  | Expr_list of
      (expression list[@gen list_size (0 -- 4) (gen_expression_sized (n / 30))])
  (** List expressions: [E1; ...; En] *)
  | Expr_lam of (pattern[@gen gen_pattern_sized (n / 30)]) * expression
  (** Anonimous functions: [Expr_lam(P, E)] represents [fun P -> E] *)
  | Expr_let of
      rec_flag
      * (val_binding[@gen gen_val_binding_sized (n / 30)])
      * (val_binding list[@gen list_size (0 -- 4) (gen_val_binding_sized (n / 30))])
      * expression
  (** [Expr_let(rec_flag, Bind(P1, E1), [Bind(P2, E2); ...; Bind(Pn, En)], E)] represents:
      - [let P1 = E1 in E] when val_binding list is [] and rec_flag is Nonrecursive
      - [let rec P1 = E1 in E] when val_binding list is [] and rec_flag is Recursive
      - [let P1 = E1 and ... and Pn = En in E] when val_binding list is Cons (A, B) and rec_flag is Nonrecursive
      - [let rec P1 = E1 and ... and Pn = En in E] when val_binding list is Cons (A, B) and rec_flag is Recursive *)
  | Expr_ifthenelse of expression * expression * expression option
  (** [if E1 then E2 else E3] *)
  | Expr_apply of expression * expression (** Application [E1 E2] *)
  | Expr_match of
      expression
      * (rule[@gen gen_rule_sized (n / 30)])
      * (rule list[@gen list_size (0 -- 4) (gen_rule_sized (n / 30))])
  (** [Expr_match(E, Rule(P1, E1), [Rule(P2, E2); ...; Rule(Pn, En)])] represents:
      - [match E with P1 -> E1] if rule list is []
      - [match E with P1 -> E1 | P2 -> E2 | ... | Pn -> En] if rule list is Cons (A, B) *)
  | Expr_function of
      (rule[@gen gen_rule_sized (n / 30)])
      * (rule list[@gen list_size (0 -- 4) (gen_rule_sized (n / 30))])
  (** [Expr_function(Rule(P1, E1), [Rule(P2, E2); ...; Rule(Pn, En)])] represents:
      - [function P1 -> E1] if rule list is []
      - [function P1 -> E1 | P2 -> E2 | ... | Pn -> En] if rule list is Cons (A, B) *)
  | Expr_option of expression option (** [Some e] or [None] *)
[@@deriving qcheck, show { with_path = false }]

(** [Bind(P, E)] represents [let P = E], [let rec P = E] or [and P = E] *)
and val_binding = Bind of (pattern[@gen gen_pattern_sized (n / 30)]) * expression
[@@deriving qcheck, show { with_path = false }]

(** [Rule(P, E)] represents [P -> E] in pattern matching *)
and rule = Rule of (pattern[@gen gen_pattern_sized (n / 30)]) * expression
[@@deriving qcheck, show { with_path = false }]

let gen_expression =
  let* n = small_nat in
  gen_expression_sized n
;;

let gen_val_binding =
  let* n = small_nat in
  gen_val_binding_sized n
;;

let gen_rule =
  let* n = small_nat in
  gen_rule_sized n
;;

type type_def =
  | Measure_type_def of (string[@gen gen_ident]) * measure option
  (** Measure type definition:
      - [[<Measure>] type I] when measure is None
      - [[<Measure>] type I = M] when measure is Some M *)
[@@deriving qcheck, show { with_path = false }]

type structure_item =
  | Str_item_eval of expression
  (** Structure item which is single expression: [E] or [do E] *)
  | Str_item_def of
      rec_flag
      * (val_binding[@gen gen_val_binding])
      * (val_binding list[@gen list_size (0 -- 4) gen_val_binding])
  (** [Str_item_def(rec_flag, (P1, E1), [(P2, E2); ...; (Pn, En)])] represents:
      - [let P1 = E1] when val_binding list is [] and rec_flag is Nonrecursive
      - [let rec P1 = E1] when val_binding list is [] and rec_flag is Recursive
      - [let P1 = E1 and ... and Pn = En] when val_binding list is Cons (A, B) and rec_flag is Nonrecursive
      - [let rec P1 = E1 and ... and Pn = En] when val_binding list is Cons (A, B) and rec_flag is Recursive *)
  | Str_item_type_def of type_def (** Structure item which is type definition *)
[@@deriving qcheck, show { with_path = false }]

type program = (structure_item list[@gen list_size (0 -- 4) gen_structure_item])
[@@deriving qcheck, show { with_path = false }]
