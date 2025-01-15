(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

type ('st, 'a) t

val return : 'a -> ('st, 'a) t
val fail : Errors.error -> ('st, 'b) t
val ( >>= ) : ('st, 'a) t -> ('a -> ('st, 'b) t) -> ('st, 'b) t
val ( let* ) : ('st, 'a) t -> ('a -> ('st, 'b) t) -> ('st, 'b) t
val ( *> ) : ('st, 'a) t -> ('st, 'b) t -> ('st, 'b) t
val ( >>| ) : ('st, 'a) t -> ('a -> 'b) -> ('st, 'b) t
val iter : ('a -> ('st, unit) t) -> 'a list -> ('st, unit) t
val iter2 : ('a -> 'b -> ('st, unit) t) -> 'a list -> 'b list -> ('st, unit) t
val map : ('a -> ('st, 'b) t) -> 'a list -> ('st, 'b list) t
val fold_left : ('a -> 'b -> ('st, 'a) t) -> 'a -> 'b list -> ('st, 'a) t
val read : ('st, 'st) t
val write : 'st -> ('st, unit) t
val run : ('st, 'a) t -> 'st -> 'st * ('a, Errors.error) Result.t
