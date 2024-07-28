ALR_CLEAN = alr clean -- -p
ALR_BUILD = alr build --development --profiles="*=development"

.PHONY: build clean prove tests coverage

build:
	cd json && $(ALR_BUILD)
	cd tools && $(ALR_BUILD)

clean:
	-gnatprove --clean -P json/json.gpr
	cd json && $(ALR_CLEAN)
	cd tests && $(ALR_CLEAN)
	rm -rf json/build tests/build tests/TEST-*.xml

prove:
	gnatprove -P json/json.gpr

tests:
	cd tests && ADAFLAGS="--coverage -gnata" $(ALR_BUILD)
	cd tests && alr run -s

coverage:
	mkdir -p tests/build/cov
	gcovr --exclude test --html-nested tests/build/cov/coverage.html
