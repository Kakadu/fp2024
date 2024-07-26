.PHONY: deps
DEPS = ocamlformat.0.26.2 ocaml-lsp-server.1.18.0 dune.3.16.0 odig
deps:
	opam install --yes $(DEPS)

NEW_NAME=Lambda2
copy_template:
	$(RM) -r $(NEW_NAME)
	cp Lambda $(NEW_NAME) -r
	$(RM) $(NEW_NAME)/DONT_REMOVE_THIS_DIRECTORY.md
	sed 's/(name Lambda)/(name $(NEW_NAME))/g' $(NEW_NAME)/dune-project -i
	sed 's/public_name Lambda/public_name $(NEW_NAME)/g' $(NEW_NAME)/lib/dune -i
	mv $(NEW_NAME)/Lambda.opam $(NEW_NAME)/$(NEW_NAME).opam
