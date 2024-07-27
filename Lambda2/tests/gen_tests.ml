[@@@ocaml.text "/*"]

(** Copyright 2021-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Lambda_lib
open Ast
open Lambda
open Utils

let zero = abs "g" @@ abs "y" @@ Var "y"
let one = abs "f" @@ abs "x" @@ app f (Var "x")
let two = abs "f" @@ abs "x" @@ app f (app f x)
let three = abs "f" @@ abs "x" @@ app f (app f (app f x))
let plus = abs "m" @@ abs "n" @@ abs "f" @@ abs "x" @@ app m @@ app f @@ app n @@ app f x
let mul = abs "x" @@ abs "y" @@ abs "z" @@ app x (app y z)
let true_ = abs "x" @@ abs "y" @@ Var "x"
let false_ = abs "x" @@ abs "y" @@ Var "y"
let isZero = abs "n" @@ app (app n (abs "x" false_)) true_

(* TODO: write the right if-then-else
   by adding thunk around then and else branches to delay the evaluation *)
let pred =
  let xxx = abs "g" @@ abs "h" @@ app h (app g f) in
  abs "n" @@ abs "f" @@ abs "x" @@ app (app (app n xxx) (abs "u" x)) (abs "u" (Var "u"))
;;

let pp = Pprintast.pp

let out_term file lam =
  Out_channel.with_open_text file (fun ch ->
    Format.fprintf (Format.formatter_of_out_channel ch) "%a%!" pp lam;
    flush ch)
;;

let () = out_term "lam_zero.txt" zero
let () = out_term "lam_one.txt" one
let () = out_term "lam_1+1.txt" (app plus @@ app one one)
let () = out_term "lam_2x1.txt" (app (app mul two) one)
let () = out_term "lam_3x2.txt" (app (app mul three) two)

(** Definition for normal order *)
module _ = struct
  let ite cond th el = app (app (app isZero cond) th) el

  let fact =
    abs "s"
    @@ abs "n"
    @@ ite (Var "n") one (app (app mul (app (var "s") (app pred (var "n")))) (var "n"))
  ;;

  let ygrek =
    let hack = abs "x" (app f (app x x)) in
    abs "f" (app hack hack)
  ;;

  let () = out_term "lam_fac3.txt" @@ app (app ygrek fact) three
end
