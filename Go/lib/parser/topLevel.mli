(** Copyright 2024, Karim Shakirov, Alexei Dmitrievtsev *)

(** SPDX-License-Identifier: MIT *)

open Ast
open Angstrom

(** [parse_file] parses the whole program such as
    [
func my_print(a int) {
	println(a)
}
func get() int {
    return 1
}
var a int
func main() {
	a = get()
	my_print(a)
}
] *)
val parse_file : file t
