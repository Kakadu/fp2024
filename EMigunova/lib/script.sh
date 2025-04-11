#!/bin/sh

ocamlfind ocamlc -c ast.ml
ocamlfind ocamlc -package angstrom  -c parse.mli
ocamlfind ocamlc -package angstrom  -c parse.ml
ocamlfind ocamlc -package base  -c inference.mli
ocamlfind ocamlc -package base  -c inference.ml
ocamlfind ocamlc -package base  -c interpreter.mli
ocamlfind ocamlc -package base  -c interpreter.ml


