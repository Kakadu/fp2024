(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast

module Ident : sig
  type t = ident

  val compare : t -> t -> int
end

module MapIdent : Map.S with type key = Ident.t

(** Value for [nil] identifier and unitialized functions and chanels *)
type nil = Nil

(** Value for chanels *)
type chan_value =
  | Chan_initialized of int
  (** Initialized chanel, identified by id (basically a link to a chanel) *)
  | Chan_uninitialized of nil

(** Values that can be stored in a variables *)
type value =
  | Value_int of int (** [3], [-100] *)
  | Value_string of string (** ["my_string"] *)
  | Value_bool of bool (** [true], [false] *)
  | Value_array of int * value list
  (** Array of values, invariant: number of values matches the size *)
  | Value_func of func_value
  | Value_chan of chan_value
  | Value_tuple of value list
  | Value_nil of nil
  (** Untyped [<nil>] value that is stored in [nil] predeclared identifier *)

and builtin =
  | Print
  | Println
  | Make
  | Close
  | Recover
  | Len
  | Panic

and func_type =
  | Closure of value MapIdent.t
  | FuncLit
  | Default

and func_value =
  | Func_initialized of func_type * anon_func
  | Func_uninitialized of nil
  | Func_builtin of builtin

type is_for_env =
  | For
  | Default

(** Local environment, [{}] block of statements with local variables *)
type local_env =
  { exec_block : block
  ; var_map : value MapIdent.t
  ; env_type : is_for_env
  }

and closure_frame =
  | Simple
  | Closure of local_env * local_env list

type stack_frame =
  { local_envs : local_env * local_env list
  (** Storage for local variables, new [{}] block creates new environment *)
  ; deferred_funcs : stack_frame list
  ; closure_envs : closure_frame
  ; returns : value option
  }

type waiting_state =
  | Ready
  (** State of the goroutine that doesn't try to receive from or send to a chanel, but another goroutine is running *)
  | Sending of { chan_id : int }
  (** State of goroutine that is trying to send value to a chanel *)
  | Receiving of { chan_id : int }
  (** State of goroutine that is trying to receive from a chanel *)

type goroutine =
  { stack : stack_frame * stack_frame list
  (** Stack of separate goroutine's local func calls. Is a tuple because there is always a root func *)
  ; go_id : int
  }

module WaitingGoroutine : sig
  type t =
    { state : waiting_state
    ; goroutine : goroutine
    }

  val compare : t -> t -> int
end

module GoSet : Set.S with type elt = WaitingGoroutine.t

module Chan : sig
  type t =
    { chan_id : int
    ; value : value option
    }

  val compare : t -> t -> int
end

module ChanSet : Set.S with type elt = Chan.t

type chanel_using_state =
  { receiving_goroutine : goroutine
  ; value : value
  }

type eval_state =
  { global_env : value MapIdent.t
  (** Stores values for predeclared identifiers and global variables and functions *)
  ; running : goroutine option
  (** Goroutine that is currently running, stored separately for time efficiency *)
  ; waiting : GoSet.t
  (** All waiting goroutines (ready to run or trying to interact with a chanel) *)
  ; chanels : ChanSet.t * int (** Set of opened chanels and id for next chanel *)
  ; is_using_chanel : chanel_using_state option
  (** The state indicates that value was sent through chanel, but not received yet *)
  }

(** Monad for evaluating the program state *)
module Monad : sig
  (** ['a t] is an interpreter that stores current state
      and the result of evaluation - ['a] (['a] or runtime error) *)
  type 'a t

  val return : 'a -> 'a t
  val fail : Errors.error -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( *> ) : 'a t -> 'b t -> 'b t
  val iter : ('a -> unit t) -> 'a list -> unit t
  val iter2 : ('a -> 'b -> unit t) -> 'a list -> 'b list -> unit t
  val map : ('a -> 'b t) -> 'a list -> 'b list t
  val run : 'a t -> eval_state -> eval_state * ('a, Errors.error) Result.t

  (* Global environment *)
  val read_global : value MapIdent.t t
  val save_global_id : ident -> value -> unit t

  (* Goroutines *)
  val read_waiting : GoSet.t t
  val create_goroutine : stack_frame -> unit t
  val run_goroutine : goroutine -> unit t

  (** Stops currently running goroutine with a given state *)
  val stop_running_goroutine : waiting_state -> unit t

  val delete_running_goroutine : unit t
  val read_running_id : int t
  val ready_waiting : goroutine -> unit t

  (* Global environment *)
  val read_returns : value option t
  val write_returns : value option -> unit t

  (* Chanels *)
  val find_chanel_fail : chan_value -> int t

  (** Pushes value [v] to the chanel. Returns [Some v] if psuhed sucessfully, [None] if there is already a value *)
  val push_chan_value : int -> value -> value option t

  val pop_chan_value : int -> value t

  (** Creates new chanel and returns it's id *)
  val create_chanel : int t

  (** Closes chanel. Fails if it is closed or uninited *)
  val close_chanel : chan_value -> unit t

  (* Sending state *)

  (** Enters chanel using state, accepts receiving goroutine and sent value. Fails if already in chanel using state *)
  val start_using_chanel : goroutine -> value -> unit t

  (** Exits chanel using state, returns receiving goroutine and sent value. Fails if not in chanel using state *)
  val use_chanel : (goroutine * value) t

  (** Returns [true] if in chanel using state, [false] otherwise *)
  val is_using_chanel : bool t

  (* Call stack *)
  val add_stack_frame : stack_frame -> unit t
  val delete_stack_frame : unit t

  (* Local environments *)
  val save_local_id : ident -> value -> unit t
  val add_env : block -> is_for_env -> unit t
  val read_local_envs : (local_env * local_env list) t
  val write_local_envs : local_env * local_env list -> unit t
  val delete_env : unit t
  val read_env_type : is_for_env t

  (* Deferred functions *)
  val add_deferred : stack_frame -> unit t
  val delete_deferred : unit t

  (* Reading ident value *)
  val read_ident : ident -> value t
  val update_ident : string -> value -> unit t

  (* statements execution *)
  val pop_next_statement : stmt option t
end
