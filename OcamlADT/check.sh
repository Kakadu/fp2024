#!/bin/bash

echo dune build
dune build

echo "opam exec -- dune build @fmt --profile=release"
opam exec -- dune build @fmt --profile=release

echo "opam exec -- zanuda -dir . -no-no_toplevel_eval"
opam exec -- zanuda -dir . -no-no_toplevel_eval

echo "opam exec -- dune runtest --instrument-with bisect_ppx --force"
opam exec -- dune runtest --instrument-with bisect_ppx --force
