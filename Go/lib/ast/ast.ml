(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

(** Channel direction *)
type chan_dir =
  | Chan_bidirectional (** Bidirectional channel type such [chan] *)
  | Chan_receive (** Receive-only channel type [<-chan] *)
  | Chan_send (** Send-only channel type [chan<-] *)
[@@deriving show { with_path = false }]

(** Data types *)
type type' =
  | Type_int (** Integer type: [int] *)
  | Type_string (** String type: [string] *)
  | Type_bool (** Boolean type: [bool] *)
  | Type_array of int * type' (** Array types such as [[6]int], [[0]string] *)
  | Type_func of type' list * type' list
  (** Function types such as [func()], [func(string) (bool, int)].
      Empty lists mean that there is no arguments or return values *)
  | Type_chan of chan_dir * type'
  (** Channel type such as:
      [chan int], [<-chan string], [chan<- bool] *)
[@@deriving show { with_path = false }]

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

type return_values =
  | Only_types of type' * type' list (** i.e.  [(int, bool, string)], [int]*)
  | Ident_and_types of (ident * type') * (ident * type') list
  (** i.e.  [(a int, b string)], [(a , b int, c string)].
      The second example will be processed at parsing as [(a int, b int, c string)] *)
[@@deriving show { with_path = false }]

(** Expressions that can be assigned to a variable or put in "if" statement *)
type expr =
  | Expr_const of const (** Constants such as [5], ["hi"], [func()] *)
  | Expr_ident of ident (** An identificator for a variable such as [x] *)
  | Expr_index of expr * expr
  (** An access to an array element by its index such as: [my_array[i]], [get_array(1)[0]]*)
  | Expr_bin_oper of bin_oper * expr * expr
  (** Binary operations such as [a + b], [x || y] *)
  | Expr_un_oper of unary_oper * expr (** Unary operations such as [!z], [-f] *)
  | Expr_chan_receive of chan_receive (** See chan_receive type *)
  | Expr_call of func_call (** See func_call type *)
[@@deriving show { with_path = false }]

(** Constants, a.k.a. literals *)
and const =
  | Const_int of int (** Integer constants such as [0], [123] *)
  | Const_string of string (** Constant strings such as ["my_string"] *)
  | Const_array of int * type' * expr list
  (** Const arrays such as [[3]int{3, get()}].
      Empty list means that there is no initializers. *)
  | Const_func of anon_func (** See anon_func type *)
[@@deriving show { with_path = false }]

(** An anonymous functions such as:
    [func() {}],
    [func(a, b int) (sum int) { sum = a + b; return }]
    [func(s1 string, s2 string) [2]string { return [2]string{s1,s2} }] *)
and anon_func =
  { args : (ident * type') list
  (** Function arguments constructions such as:
      [func(a int, b string) ...],
      [func(a, b int, c string) ...].
      Empty list means that function doesn't take any arguments.
      The second example will be processed at parsing
      as [func(a int, b int, c string) ...] *)
  ; returns : return_values option
  (** None if function doesn't return anything. See return_values type *)
  ; body : block (** function body *)
  }
[@@deriving show { with_path = false }]

(** function calls such as:
    [my_func(arg1, arg2)],
    [c()()()],
    [func() { println("hello") }()].
    Empty list means that function doesn't take any arguments *)
and func_call = expr * expr list [@@deriving show { with_path = false }]

(** Channel receive such as: [<-c], [<-<-get_chan()] *)
and chan_receive = expr [@@deriving show { with_path = false }]

(** Channel send such as [c <- true] *)
and chan_send = ident * expr [@@deriving show { with_path = false }]

(** Lvalue in assignments *)
and lvalue =
  | Lvalue_ident of ident (** Lvalue of ident such as [my_var] *)
  | Lvalue_array_index of lvalue * expr
  (** Lvalue of array and index such as:
      [array[get_index()]], [array[i][j][k]] *)
[@@deriving show { with_path = false }]

(** Variable assignments *)
and assign =
  | Assign_mult_expr of (lvalue * expr) * (lvalue * expr) list
  (** Assignment to a variable with equal number of identifiers and initializers
      such as [a = 3], [a, b[0] = 4, 5]. *)
  | Assign_one_expr of lvalue * lvalue * lvalue list * func_call
  (** Assignment to a variable with multiple lvalues and
      one initializer that is a function call such as
      [a, b, c[i] = get_three()] *)
[@@deriving show { with_path = false }]

(** Variable declarations with [var] keyword *)
and long_var_decl =
  | Long_decl_no_init of type' * ident * ident list
  (** Declarations without initialization such as [var my_int1, my_int2 int] *)
  | Long_decl_mult_init of type' option * (ident * expr) * (ident * expr) list
  (** Declarations with initializer for each identifier such as:
      [var my_func func() = func() {}],
      [var a, b int = 1, 2],
      [var a, b = 1 + 2, "3"] *)
  | Long_decl_one_init of type' option * ident * ident * ident list * func_call
  (** Declarations with one initializer that is a function call
      for multiple identifiers such as [var a, b, c = get_three()] *)
[@@deriving show { with_path = false }]

(** Short variable declarations withous [var] keyword
    such as [flag, count := true, 0], [a, b := get_two()]. *)
and short_var_decl =
  | Short_decl_mult_init of (ident * expr) * (ident * expr) list
  (** Declarations with initializer for each identifier such as [flag, count := true, 0] *)
  | Short_decl_one_init of ident * ident * ident list * func_call
  (** Declarations with one initializer that is a function call
      for multiple identifiers such as [a, b := get_two()] *)
[@@deriving show { with_path = false }]

(** Statements that can be used in if init and for init and post *)
and if_for_init =
  | Init_assign of assign (** [a = 0] *)
  | Init_decl of short_var_decl (** [a := 0] *)
  | Init_incr of ident (** [a++] *)
  | Init_decr of ident (** [a--] *)
  | Init_call of func_call (** [a()] *)
  | Init_send of chan_send (** [c <- 1] *)
  | Init_receive of chan_receive (** [<-c] *)
[@@deriving show { with_path = false }]

(** An if statement such as:
    [if a := 5; a >= 4 {
          do()
      } else {
          do_else()
      }] *)
and if' =
  { init : if_for_init option
  ; cond : expr
  ; if_body : block
  ; else_body : else_body option (* block or if statement or None *)
  }
[@@deriving show { with_path = false }]

(** Variants of else body in if statement *)
and else_body =
  | Else_block of block (** Else body of statement block such as [else {}] *)
  | Else_if of if' (** Else body of another if statement such as [else if true {}] *)
[@@deriving show { with_path = false }]

(** Statement, a syntactic unit of imperative programming *)
and stmt =
  | Stmt_long_var_decl of long_var_decl (** See long_var_decl type *)
  | Stmt_short_var_decl of short_var_decl (** See short_var_decl type *)
  | Stmt_assign of assign (** See assign type *)
  | Stmt_incr of ident (** An increment of a variable: [a++] *)
  | Stmt_decr of ident (** A decrement of a variable: [a--] *)
  | Stmt_break (** Break statement: [break] *)
  | Stmt_continue (** Continue statement: [continue] *)
  | Stmt_return of expr list
  (** Return statement such as
      [return], [return some_expr], [return expr1, expr2] *)
  | Stmt_block of block (** See block type *)
  | Stmt_chan_send of chan_send (** Channel send statement such as [c <- true] *)
  | Stmt_chan_receive of chan_receive (** See chan_receive type *)
  | Stmt_call of func_call (** See func_call type *)
  | Stmt_defer of func_call (** Defer statement such as [defer clean()] *)
  | Stmt_go of func_call (** Go statement such as [go call()] *)
  | Stmt_if of if' (** If statement, see if' type *)
  | Stmt_for of
      { init : if_for_init option
      ; cond : expr option
      ; post : if_for_init option
      ; body : block
      } (** A for statement such as [for i := 0; i < n; i++ { do() }] *)
[@@deriving show { with_path = false }]

(** Block of statements in curly braces *)
and block = stmt list [@@deriving show { with_path = false }]

(** Function declarations such as:
    [func sum_and_diff(a, b int) (sum, diff int) {
      sum = a + b
      diff = a - b
      return
    }] *)
type func_decl = ident * anon_func [@@deriving show { with_path = false }]

(** Top-level declarations *)
type top_decl =
  | Decl_var of long_var_decl
  (** Top level variable declaration such as: [var a int], [var a, b = 1, "hi"] *)
  | Decl_func of func_decl
  (** Top level function declaration such as: [func f() {}], [func f(a, b int) string {}] *)
[@@deriving show { with_path = false }]

(** The whole interpreted file, the root of the abstract syntax tree *)
type file = top_decl list [@@deriving show { with_path = false }]
