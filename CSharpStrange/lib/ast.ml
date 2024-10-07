(** Copyright 2024, Dmitrii Kuznetsov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type val_type =
  | ValInt of int (** Int value *)
  | ValChar of char (** Char value *)
  | ValNull (** Null *)
  | ValBool of bool (** Bool value *)
[@@deriving eq, show { with_path = false }]

(** Identidicator *)
type ident = Id of string [@@deriving eq, show { with_path = false }]

(** Basic types *)
type base_type =
  | TypeInt (** Declaration of int *)
  | TypeChar (** Declaration of char *)
  | TypeVoid (** Declaration of void *)
  | TypeBool (** Declaration of bool *)
[@@deriving eq, show { with_path = false }]

(** Type delcaration *)
type _type =
  | TypeBase of base_type (** Declaration of basic type *)
  | TypeArray of base_type (** Declaration of array of basic type *)
[@@deriving eq, show { with_path = false }]

(** Modifiers *)
type modifier =
  | Public (** Public modifier *)
  | Static (** Static modifier, used for main() method only *)
  | Const (** Const modifier *)
  | Async (** Async modifier *)
[@@deriving eq, show { with_path = false }]

(** Binary operations *)
type bin_op =
  | Add (** Sum: a [+] b *)
  | Sub (** a [-] b *)
  | Mul (** a [*] b *)
  | Div (** a [/] b in integers *)
  | Mod (** a [%] b *)
  | Equal (** a [==] b *)
  | NonEqual (** a [!=] b *)
  | Less (** a [<] b *)
  | More (** a [>] b *)
  | LessEqual (** a [<=] b *)
  | MoreEqual (** a [>=] b *)
  | And (** a [&&] b *)
  | Or (** a [||] b *)
[@@deriving eq, show { with_path = false }]

(** Unary operations *)
type un_op =
  | Inc (** [++] a or [++] a *)
  | Dec (** [--] a or [--] a *)
  | Not (** [!] a *)
[@@deriving eq, show { with_path = false }]

(** Language expressions *)
type expr =
  | Value of val_type (** Some value *)
  | BinOp of bin_op * expr * expr (** Binary operation *)
  | UnOp of un_op * expr (** Unary operation *)
  | Assign of expr * expr (** a [=] b *)
  | New of expr (** [new] a *)
  | ConstExpr of val_type (** Const expression *)
  | IdExpr of ident (** Identificator expression *)
  | ArrayAccess of expr * expr (** Array access: a = arr[i] *)
  | FuncCall of ident * expr list (** Call of function: name(arguments) *)
  | Lambda of expr * stmt (** Lambda expressions *)
  | Await of expr (** Await expression *)
  | LinqQuery of linq_query (** from identifier in expr select expr *)
[@@deriving eq, show { with_path = false }]

(** Language statements *)
and stmt =
  | For of stmt option * expr option * expr option * stmt
  (** For cycle: [for] (int i = 0, j = 3; i < 4; i++, j--) {} *)
  | If of expr * stmt * stmt option
  (** If condition: [if] (a) [then] { b } ([else] { c } ) *)
  | While of expr * stmt (** While cycle: [while] (a) { } *)
  | Return of expr (** Return: return (a) *)
  | StmtsBlock of stmt list (** Block of statements: { a }; could be empty: {} *)
  | Break (** Cycle break *)
  | Continue (** Cycle continue *)
  | Expr of expr (** Another expression *)
  | VarDeclare of _type * ident * expr option (** Var declaration *)
[@@deriving eq, show { with_path = false }]

(** From clauses *)
and from_clause = FromClause of string * ident
[@@deriving eq, show { with_path = false }]

(** Select clause *)
and select_clause = SelectClause of expr [@@deriving eq, show { with_path = false }]

(** LINQ query *)
and linq_query =
  | Query of from_clause * select_clause (** from identifier in identifier select expr *)
[@@deriving eq, show { with_path = false }]

(** C Sharp class fields *)
type field =
  | VarField of modifier list * _type * ident * expr option (** Class field *)
  | Method of modifier list * _type * ident * (_type * string) list * stmt
  (** Class method *)
[@@deriving eq, show { with_path = false }]

(** C Sharp class *)
type c_sharp_class =
  | Class of modifier list * ident * field list (** Basic class (Program) name *)
[@@deriving eq, show { with_path = false }]

(** Program AST *)
type program = Program of c_sharp_class [@@deriving eq, show { with_path = false }]
