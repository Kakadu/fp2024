.PHONY: deps
DEPS = ocamlformat.0.26.2 ocaml-lsp-server.1.18.0 dune.3.16.0 odig
deps:
	opam install --yes $(DEPS)