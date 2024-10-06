type literal =
  | Int of int
  | String of string
  | Boolean of bool
[@@deriving show { with_path = false }]

type id = string [@@deriving show { with_path = false }]

type op_bin =
  | Plus
  | Minus
  | Mul
  | Div
  | Equal (** = **)
  | Greater
  | GreaterEqual
  | Less
  | LessEqual
  | And
  | Or
[@@deriving show { with_path = false }]

type op_un =
  | Not
  | UnMinus
[@@deriving show { with_path = false }]

type op_tern = IfThenElse [@@deriving show { with_path = false }]

type rec_flag =
  | Recursive
  | NotRecursive
[@@deriving show { with_path = false }]

type expr =
  | Const of literal
  | Variable of id
  | Op_un of op_un * expr
  | Op_bin of op_bin * expr * expr
  | Op_tern of op_tern * expr * expr * expr
  | Function of id * expr list
  | Binding of id * rec_flag * id list * expr
[@@deriving show { with_path = false }]
