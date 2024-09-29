.PHONY: all run fmt lint zanuda

all:
	dune build -j4

run:
	./_build/default/bin/main.exe

fmt:
	dune build @fmt --auto-promote

lint:
	dune build @lint --force

zanuda:
	dune build @check @runtest -j4
	zanuda -dir .
