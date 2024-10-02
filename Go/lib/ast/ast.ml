(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

(** Data types *)
type type' =
  | Type_int (** Integer type: [int] *)
  | Type_string (** String type: [string] *)
  | Type_bool (** Boolean type: [bool] *)
  | Type_array of type' * size (** Array types such as [[6]int], [[0]string] *)
  | Type_func of type' list option * type' list option
  (** Function types such as [func()], [func(string) (bool, int)] *)
  | Type_chan of type' (** Channel type [chan int] *)
[@@deriving show { with_path = false }]

and size = int [@@deriving show { with_path = false }]

(** Constants, a.k.a. literals *)
type const =
  | Const_int of int (** Integer constants such as [0], [123] *)
  | Const_string of string (** Constant strings such as ["my_string"] *)
  | Const_bool of bool (** Boolean constants: [true] and [false] *)
[@@deriving show { with_path = false }]

(** identificator for a variable or a function *)
type ident = string [@@deriving show { with_path = false }]

(** Binary operators *)
type bin_oper =
  | Bin_sum (** Binary sum: [+] *)
  | Bin_multiply (** Binary multiplication: [*] *)
  | Bin_subtract (** Binary subtraction: [-] *)
  | Bin_divide (** Binary divison: [/] *)
  | Bin_modulus (** Binary division by modulus: [%] *)
  | Bin_equal (** Binary check for equality: [==] *)
  | Bin_not_equal (** Binary check for inequlity: [!=] *)
  | Bin_greater (** Binary "greater than": [>] *)
  | Bin_greater_equal (** Binary "greater than or equal": [>=] *)
  | Bin_less (** Binary "less than": [<] *)
  | Bin_less_equal (** Binary "less than or equal": [<=] *)
  | Bin_and (** Binary "and": [&&] *)
  | Bin_or (** Binary "or": [||] *)
[@@deriving show { with_path = false }]

(** Unary operators *)
type unary_oper =
  | Unary_not (** Unary negation: [!] *)
  | Unary_plus (** Unary plus: [+] *)
  | Unary_minus (** Unary minus: [-]*)
[@@deriving show { with_path = false }]

(** Expressions that can be assigned to a variable or put in "if" statement *)
type expr =
  | Expr_nil (** A value of an unitialized channel or function: [nil] *)
  | Expr_const of const (** Constants such as [5], ["hi"], [false] *)
  | Expr_array of type' * expr list option (** Arrays such as [[3]int{3, get_four()}] *)
  | Expr_ident of ident (** An identificator for a variable such as [x] *)
  | Expr_index of expr * expr
  (** An access to an array element by its index such as: [my_array[i]], [get_array(1)[0]]*)
  | Expr_bin_oper of bin_oper * expr * expr
  (** Binary operations such as [a + b], [x || y] *)
  | Expr_un_oper of unary_oper * expr (** Unary operations such as [!z], [-f] *)
  | Expr_anon_func of anon_func (** See anon_func type *)
  | Expr_call of func_call (** See func_call type *)
[@@deriving show { with_path = false }]

(** An anonymous functions such as:
    [func() {}],
    [func(a, b int) (sum int) { sum = a + b; return }]
    [func(s1 string, s2 string) [2]string { return [2]string{s1,s2} }] *)
and anon_func =
  { args : (ident * type' option) list option (** arguments *)
  ; return_types : (ident option * type' option) list option
  (** return types, optional var names *)
  ; body : stmt option (** function body *)
  }
[@@deriving show { with_path = false }]

(** function calls such as:
    [my_func(arg1, arg2)],
    [c()()()],
    [func() { println("hello") }()] *)
and func_call = expr * expr list option [@@deriving show { with_path = false }]

(** Statement, a syntactic unit of imperative programming *)
and stmt =
  | Stmt_var_decl of var_decl (** See var_decl type *)
  | Stmt_assign of ident list * expr list
  (** Assignment to a variable such as [a = 3], [a, b = 4, 5] *)
  | Stmt_incr of ident (** An increment of a variable: [a++] *)
  | Stmt_decr of ident (** A decrement of a variable: [a--] *)
  | Stmt_if of stmt option * expr * stmt option * stmt option
  (** An if statement such as:
      [if a := 5; a >= 4 {
          do()
      } else {
          do_else()
      }] *)
  | Stmt_for of stmt option * expr option * stmt option * stmt option
  (** A for statement such as:
      [for i := 0; i < n; i++ {
          do()
      }] *)
  | Stmt_range of ident * ident option * expr * stmt option
  (** For with range statement such as:
      [for i, elem := range array {
          check(elem)
      }] *)
  | Stmt_break (** Break statement: [break] *)
  | Stmt_continue (** Continue statement: [continue] *)
  | Stmt_return of expr option
  (** Return statement such as [return some_expr] or [return] *)
  | Stmt_block of stmt list (** Block of statements in curly braces *)
  | Stmt_chan_send of ident * expr (** Channel send operation [c <- true] *)
  | Stmt_chan_recieve of ident * expr (** Channel recieve operation [z := <-c] *)
  | Stmt_call of func_call (** See func_call type *)
  | Stmt_defer of func_call (** See func_call type *)
  | Stmt_go of func_call (** See func_call type *)
[@@deriving show { with_path = false }]

(** Variable declarations such as:
    [var my_int1, my_int2 int],
    [var my_func = func() {}],
    [var a, b = 1 + 2, "3"]
    [var my_array = [3]int{1, 2, 3}],
    [flag := true] - the last works only inside of a function body *)
and var_decl =
  { var_name : ident list (** variables names *)
  ; var_type : type' option (** variables data type, optional *)
  ; init : expr list option (** variables initializers, optional *)
  }
[@@deriving show { with_path = false }]

(** Function declarations such as:
    [func sum_and_diff(a, b int) (sum, diff int) {
      sum = a + b
      diff = a - b
      return
    }] *)
type func_decl = ident * anon_func [@@deriving show { with_path = false }]

(** Top-level declarations *)
type top_decl =
  | Decl_var of var_decl
  | Decl_func of func_decl
[@@deriving show { with_path = false }]

(** The whole interpreted file, the root of the abstract syntax tree *)
type file = top_decl list [@@deriving show { with_path = false }]
