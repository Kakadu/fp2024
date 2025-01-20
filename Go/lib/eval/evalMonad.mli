(** Copyright 2024-2025, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast

module Ident : sig
  type t = ident

  val compare : t -> t -> int
end

module MapIdent : Map.S with type key = Ident.t

(** Value for [nil] identifier and unitialized functions and chanels *)
type nil = Nil

type builtin =
  | Print
  | Println
  | Make
  | Close
  | Recover
  | Len
  | Panic

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

type defered_frame = value * value list

type stack_frame =
  { local_envs : local_env * local_env list
  (** Storage for local variables, new [{}] block creates new environment *)
  ; deferred_funcs : defered_frame list
  ; returns : value option
  ; panics : value list option
  }

type goroutine =
  { stack : stack_frame * stack_frame list
  (** Stack of separate goroutine's local func calls. Is a tuple because there is always a root func *)
  ; go_id : int
  }

module Goroutine : sig
  type t = goroutine

  val compare : t -> t -> int
end

module SendingGoroutines : sig
  type t =
    { send_queue : (goroutine * value) list
    ; chan_id : int
    }

  val compare : t -> t -> int
end

module ReceivingGoroutines : sig
  type t =
    { receive_queue : goroutine list
    ; chan_id : int
    }

  val compare : t -> t -> int
end

module Chan : sig
  type t =
    { chan_id : int
    ; value : value option
    }

  val compare : t -> t -> int
end

module ReadySet : Set.S with type elt = Goroutine.t
module SendingSet : Set.S with type elt = SendingGoroutines.t
module ReceivingSet : Set.S with type elt = ReceivingGoroutines.t
module ChanSet : Set.S with type elt = Chan.t

type inited_by =
  | Sender
  | Receiver

type chanel_using_state =
  { sending_goroutine : goroutine
  ; receiving_goroutine : goroutine
  ; value : value
  ; inited_by : inited_by
  }

(** The whole executing program state *)
type eval_state =
  { global_env : value MapIdent.t
  (** Stores values for predeclared identifiers and global variables and functions *)
  ; running : goroutine option
  (** Goroutine that is currently running, stored separately for time efficiency *)
  ; ready : ReadySet.t (** Set of all ready to run goroutines *)
  ; sending : SendingSet.t (** Set of opened chanels' send queues *)
  ; receiving : ReceivingSet.t (** Set of opened chanels' receive queues *)
  ; chanels : ChanSet.t * int (** Set of opened chanels and id for next chanel *)
  ; is_using_chanel : chanel_using_state option
  (** The state indicates that value was sent through chanel, but not received yet *)
  ; next_go_id : int (** An id that will be given to the next created goroutine *)
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

  (** Takes ident and value and saves the pair to global varibles map *)
  val save_global_id : ident -> value -> unit t

  (** Takes ident and returns value assigned to it.
      Fails if there is no such ident visible from current stack frame *)
  val read_ident : ident -> value t

  (** Takes ident and value and assigns the value to the ident.
      Fails if there is no such ident visible from current stack frame *)
  val update_ident : ident -> value -> unit t

  (** Returns set of all ready to run goroutines *)
  val read_ready : ReadySet.t t

  (** Takes a goroutine and adds it to the set of ready goroutines *)
  val add_ready : goroutine -> unit t

  (** [push_to_send_queue id go v] tries to push goroutine [go] that is trying to send value
      [v] to chanel with id [id] to its send queue. Fails if chanel is unitialized or closed *)
  val push_to_send_queue : int -> goroutine -> value -> unit t

  (** [pop_from_send_queue id] tries to pop sending goroutine with value it is sending
      from chanel's with given [id] send queue. Fails if chanel is unitialized or closed *)
  val pop_from_send_queue : int -> (goroutine * value) t

  (** [is_send_queue_not_empty id] returns [Some ()] if chanel's with given [id] send queue
      is not empty, or [None] otherwise. Fails if chanel is unitialized or closed *)
  val is_send_queue_not_empty : int -> unit option t

  (** [push_to_receive_queue id go] tries to push goroutine [go] to
      chanel's with id [id] receive queue. Fails if chanel is unitialized or closed *)
  val push_to_receive_queue : int -> goroutine -> unit t

  (** [pop_from_receive_queue id] tries to pop receiving goroutine from chanel's
      with given [id] receive queue. Fails if chanel is unitialized or closed *)
  val pop_from_receive_queue : int -> goroutine t

  (** [is_receive_queue_not_empty id] returns [Some ()] if chanel's with given [id] receive queue
      is not empty, or [None] otherwise. Fails if chanel is unitialized or closed *)
  val is_receive_queue_not_empty : int -> unit option t

  (** Takes stack frame and creates new goroutine with it.
      Adds its to waiting goroutines set with [Ready] waiting state *)
  val create_goroutine : stack_frame -> unit t

  (** Takes goroutine and assignes it [running] field.
      Doesn't execute the goroutine. Fails if another goroutine is already running *)
  val run_goroutine : goroutine -> unit t

  (** Runs random ready to run goroutine. Returns [Some ()] if there were some ready
      goroutines, [None] otherwise. Fails if another goroutine is already running *)
  val run_ready_goroutine : unit option t

  (** Deletes currently running goroutine and assigns [running] to [None].
      Should be called when goroutine finished executing *)
  val delete_running_goroutine : unit t

  (** Returns currently running goroutine's, fails if there is no running goroutine *)
  val read_running_fail : goroutine t

  (** Returns [Some ()] if there is ready to run goroutine, [None otherwise] *)
  val check_ready_goroutine : unit option t

  (* Global environment *)
  val read_returns : value option t
  val write_returns : value option -> unit t

  (** Takes chanel value and returns [chan_id] of a chanel it points to.
      Fails if there is no such chanel or it is uninitialized *)
  val find_chanel_fail : chan_value -> int t

  (** Tries to push value [v] to the chanel. Returns [None] if pushed sucessfully, [Some ()] if there is already a value *)
  val push_chan_value : int -> value -> unit option t

  (** Takes chanel id and pops value from the chanel with given id.
      Fails if there is no such chanel or no value in the chanel *)
  val pop_chan_value : int -> value t

  (** Creates new chanel and returns it's id *)
  val create_chanel : int t

  (** Closes chanel. Fails if it is already closed or uninitialized *)
  val close_chanel : chan_value -> unit t

  (** Enters chanel using state, accepts sending and receiving goroutines and sent value.
      Fails if already in chanel using state *)
  val start_using_chanel : chanel_using_state -> unit t

  (** Exits chanel using state, returns receiving goroutine and sent value. Fails if not in chanel using state *)
  val use_chanel : chanel_using_state t

  (** Returns state of using a chanel (sender, receiver and value) if in chanel using state, [None] otherwise *)
  val is_using_chanel : chanel_using_state option t

  (** Takes stack frame and pushes it to currently running goroutine's call stack *)
  val add_stack_frame : stack_frame -> unit t

  (** Pops stack frame from currently running goroutine's call stack *)
  val delete_stack_frame : unit t

  (** Takes ident and value and saves the pair to local varibles map *)
  val save_local_id : ident -> value -> unit t

  (** Takes block of statements and is_for_env flag, adds new local environment
      to currently running goroutine's last stack frame *)
  val add_env : block -> is_for_env -> unit t

  (** Returns stack of local environments of currently running goroutine's last stack frame *)
  val read_local_envs : (local_env * local_env list) t

  val write_local_envs : local_env * local_env list -> unit t

  (** Deletes last local environment of currently running goroutine's last stack frame *)
  val delete_env : unit t

  (** Return whether current local environment is a for loop or not *)
  val read_env_type : is_for_env t

  (** Takes stack frame and adds it to currently running goroutine's
      current stack frame's deferred functions stack *)
  val add_deferred : defered_frame -> unit t

  val read_deferred : defered_frame list t
  val write_panics : value list option -> unit t
  val read_panics : value list option t

  (** Returns next statement from currently running goroutine's
      current stack frame's execution block. [None] if the block is empty *)
  val pop_next_statement : stmt option t
end
