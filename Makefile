CFLAGS  ?= -O2 -march=native

GNATMAKE  = gprbuild -dm -p
GNATCLEAN = gprclean -q
GNATINSTALL = gprinstall
GNATPROVE = gnatprove --cwe --pedantic -k -j0 --output-header

PREFIX ?= /usr

includedir = $(PREFIX)/include
gprdir     = $(PREFIX)/share/gpr
libdir     = $(PREFIX)/lib
alidir     = $(libdir)

installcmd = $(GNATINSTALL) -p \
	--sources-subdir=$(includedir) \
	--project-subdir=$(gprdir) \
	--lib-subdir=$(libdir) \
	--ali-subdir=$(alidir) \
	--prefix=$(PREFIX)

.PHONY: build tests tools debug clean coverage prove install uninstall

build:
	$(GNATMAKE) -P tools/json_ada.gpr -cargs $(CFLAGS)

build_test:
	$(GNATMAKE) -P tests/unit/unit_tests.gpr -XMode=coverage -cargs -O0

tools:
	$(GNATMAKE) -P tools/tools.gpr -cargs $(CFLAGS)

debug:
	$(GNATMAKE) -P tools/json_ada.gpr -XMode=debug -cargs $(CFLAGS)

clean:
	-$(GNATPROVE) --clean -P tools/json_ada.gpr
	$(GNATCLEAN) -P tools/json_ada.gpr
	$(GNATCLEAN) -P tests/unit/unit_tests.gpr
	rm -rf bin build tests/unit/build test/cov TEST-*.xml

prove:
	$(GNATPROVE) --level=4 --prover=all --mode=check -P tools/json_ada.gpr

tests: build_test
	./tests/unit/test_bindings

coverage:
	mkdir -p tests/cov
	lcov -q -c -d tests/unit/build/obj -o tests/cov/unit.info
	lcov -q -r tests/cov/unit.info */adainclude/* -o tests/cov/unit.info
	lcov -q -r tests/cov/unit.info */tests/unit/* -o tests/cov/unit.info
	genhtml -q --ignore-errors source -o tests/cov/html tests/cov/unit.info
	lcov -l tests/cov/unit.info

install:
	$(installcmd) -f --install-name='json-ada' -P tools/json_ada.gpr

uninstall:
	$(installcmd) --uninstall --install-name='json-ada' -P tools/json_ada.gpr
