.PHONY: all run fmt lint

all:
	dune build

run:
	./_build/default/bin/main.exe

fmt:
	dune build @fmt --auto-promote

lint:
	dune build @lint --force
