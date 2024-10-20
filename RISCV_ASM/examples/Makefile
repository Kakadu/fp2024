all:
	@echo "Building all .s files in examples"
	@for dir in $(shell find . -mindepth 1 -maxdepth 1 -type d); do \
		folder_name=$$(basename $$dir); \
		cd $$dir && $(MAKE) --no-print-directory $$folder_name.s; \
	done

test: all
	@echo "Running dune runtest"
	@cd .. && dune runtest

clean:
	@echo "Cleaning all .s files in examples"
	@for dir in $(shell find . -mindepth 1 -maxdepth 1 -type d); do \
		folder_name=$$(basename $$dir); \
		rm -f $$dir/$$folder_name.s; \
	done
