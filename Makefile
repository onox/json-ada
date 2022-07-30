.PHONY: build clean prove tests coverage

build:
	cd json && alr build --validation

clean:
	-gnatprove --clean -P json/json.gpr
	cd json && alr clean
	cd tests && alr clean
	rm -rf json/build tests/build tests/build/cov tests/TEST-*.xml

prove:
	gnatprove -P json/json.gpr

tests:
	cd tests && alr build --development
	cd tests && alr run -s

coverage:
	mkdir -p tests/build/cov
	lcov -q -c -d json/build/obj -d tests/build/obj -o tests/build/cov/unit.info
	lcov -q -r tests/build/cov/unit.info */adainclude/* -o tests/build/cov/unit.info
	lcov -q -r tests/build/cov/unit.info */tests/* -o tests/build/cov/unit.info
	genhtml -q --ignore-errors source -o tests/build/cov/html tests/build/cov/unit.info
	lcov -l tests/build/cov/unit.info
