.PHONY: all test fmt lint clean

all:
	dune build

test:
	dune test

clean:
	@$(RM) -r _build _coverage

fmt:
	dune build @fmt --auto-promote

lint:
	dune build @lint --force

release:
	dune build --profile=release
	dune runtest --profile=release

TEST_COV_D = /tmp/cov
COVERAGE_OPTS = --coverage-path $(TEST_COV_D) --expect lib/

.PHONY: test_coverage coverage
test_coverage: coverage
coverage:
	$(RM) -r $(TEST_COV_D)
	mkdir -p $(TEST_COV_D)
	BISECT_FILE=$(TEST_COV_D)/langauge dune runtest --no-print-directory \
          --instrument-with bisect_ppx --force
	bisect-ppx-report html $(COVERAGE_OPTS)
	bisect-ppx-report summary $(COVERAGE_OPTS)
	@echo "Use 'xdg-open _coverage/index.html' to see coverage report"
