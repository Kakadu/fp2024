(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

  $ ../bin/main.exe -infer -file manytests/typed/001fac.ml
  int -> int
  int

  $ ../bin/main.exe -infer -file manytests/typed/002fac.ml
  int -> (int -> f) -> f
  int

  $ ../bin/main.exe -infer -file manytests/typed/003fib.ml
  int -> int
  int -> int -> int -> int
  int

  $ ../bin/main.exe -infer -file manytests/typed/004manyargs.ml
  int
  i -> j -> k -> l -> m -> n -> o -> p -> q -> r -> int
  c -> d -> e -> int
  a -> a

  $ ../bin/main.exe -infer -file manytests/typed/005fix.ml
  g -> h -> int
  ((c -> d) -> c -> d) -> c -> d
  int

  $ ../bin/main.exe -infer -file manytests/typed/006partial.ml
  d -> int
  int

  $ ../bin/main.exe -infer -file manytests/typed/006partial2.ml
  a -> b -> c -> int
  int

  $ ../bin/main.exe -infer -file manytests/typed/006partial3.ml
  a -> c -> e -> unit
  int

  $ ../bin/main.exe -infer -file manytests/typed/007order.ml
  unit -> unit -> a -> unit -> b -> c -> unit -> d -> e -> int
  unit

  $ ../bin/main.exe -infer -file manytests/typed/008ascription.ml
  a -> b -> c -> int
  int

  $ ../bin/main.exe -infer -file manytests/typed/009let_poly.ml
  (int * bool)

  $ ../bin/main.exe -infer -file manytests/do_not_type/001.ml
  Infer error. Unbound variable 'fac'.

  $ ../bin/main.exe -infer -file manytests/do_not_type/002if.ml
  Infer error. Failed to unify types: int and bool.

  $ ../bin/main.exe -infer -file manytests/do_not_type/003occurs.ml
  Infer error. Occurs check failed. Type variable '2 occurs inside c -> g.

  $ ../bin/main.exe -infer -file manytests/do_not_type/004let_poly.ml
  Infer error. Failed to unify types: bool and int.

