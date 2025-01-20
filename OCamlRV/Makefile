.PHONY: all run fmt lint zanuda

all:
	dune build -j8

fmt:
	dune build @fmt --auto-promote

lint:
	dune build @lint --force

zanuda:
	dune build @check @runtest -j4
	zanuda -dir .
