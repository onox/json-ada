GNATPROVE = gnatprove --cwe --pedantic -k -j0 --output-header

.PHONY: build debug clean prove tests coverage

build:
	alr build

debug:
	alr build -XJSON_BUILD_MODE=debug

clean:
	-$(GNATPROVE) --clean -P json.gpr
	alr clean
	cd tests && alr clean
	rm -rf build tests/build tests/cov tests/TEST-*.xml

prove:
	$(GNATPROVE) --level=4 --prover=all --mode=check -P json.gpr

tests:
	cd tests && alr build -XJSON_BUILD_MODE=coverage
	cd tests && alr run -s

coverage: tests
	mkdir -p tests/cov
	lcov -q -c -d build/obj -d tests/build/obj -o tests/cov/unit.info
	lcov -q -r tests/cov/unit.info */adainclude/* -o tests/cov/unit.info
	lcov -q -r tests/cov/unit.info */tests/* -o tests/cov/unit.info
	genhtml -q --ignore-errors source -o tests/cov/html tests/cov/unit.info
	lcov -l tests/cov/unit.info
