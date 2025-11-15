.PHONY: all test exec fmt lint zanuda
all:
	dune build

test:
	dune test

exec:
	dune exec XML

fmt:
	dune build @fmt --auto-promote
	./runtime-ci-script/format.sh
lint:
	dune build @lint --force

zanuda:
	dune build @check @runtest -j4
	zanuda -dir .

TEST_COV_D = /tmp/cov
COVERAGE_OPTS = --coverage-path $(TEST_COV_D) --expect bin/

.PHONY: test_coverage coverage
test_coverage: coverage
coverage:
	$(RM) -r $(TEST_COV_D)
	mkdir -p $(TEST_COV_D)
	BISECT_FILE=$(TEST_COV_D)/language dune runtest --no-print-directory \
		--instrument-with bisect_ppx --force
	bisect-ppx-report html $(COVERAGE_OPTS)
	bisect-ppx-report summary $(COVERAGE_OPTS)
	@echo "Use 'xdg-open _coverage/index.html' to see coverage report"