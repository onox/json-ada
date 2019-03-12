CFLAGS  ?= -O2 -march=native
LDFLAGS ?= -Wl,-z,relro -Wl,-z,now

GNATMAKE  = gprbuild -dm -p
GNATCLEAN = gprclean -q
GNATINSTALL = gprinstall

PREFIX ?= /usr

includedir = $(PREFIX)/include
gprdir     = $(PREFIX)/share/gpr
libdir     = $(PREFIX)/lib
alidir     = $(libdir)

.PHONY: build test debug clean coverage install

build:
	$(GNATMAKE) -P json_ada.gpr -cargs $(CFLAGS) -largs $(LDFLAGS)

build_test:
	$(GNATMAKE) -P test/unit/unit_tests.gpr -XMode=coverage -cargs -O0 -largs $(LDFLAGS)

debug:
	$(GNATMAKE) -P json_ada.gpr -XMode=debug -cargs $(CFLAGS) -largs $(LDFLAGS)

clean:
	$(GNATCLEAN) -P json_ada.gpr
	$(GNATCLEAN) -P test/unit/unit_tests.gpr
	rm -rf lib obj test/unit/obj test/cov

test: build_test
	./test/unit/test_bindings

coverage:
	mkdir -p test/cov
	lcov -q -c -d test/unit/obj -o test/cov/unit.info
	lcov -q -r test/cov/unit.info */adainclude/* -o test/cov/unit.info
	lcov -q -r test/cov/unit.info */test/unit/* -o test/cov/unit.info
	genhtml -q --ignore-errors source -o test/cov/html test/cov/unit.info
	lcov -l test/cov/unit.info

install: build
	$(GNATINSTALL) --relocate-build-tree -p -q -f --install-name='json-ada' \
		--sources-subdir=$(includedir) \
		--project-subdir=$(gprdir) \
		--lib-subdir=$(libdir) \
		--ali-subdir=$(alidir) \
		--prefix=$(PREFIX) -P json_ada.gpr
