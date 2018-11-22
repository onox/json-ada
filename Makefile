MODE ?= release

GNAT_FLAGS ?= -dm
CFLAGS  ?= -O2 -march=native
LDFLAGS ?= -Wl,-z,relro -Wl,-z,now

GNATMAKE  = gprbuild $(GNAT_FLAGS) -p -XCompiler_Flags="$(CFLAGS)" -XMode=$(MODE)
GNATCLEAN = gprclean -q
GNATINSTALL = gprinstall

PREFIX ?= /usr

includedir = $(PREFIX)/include
gprdir     = $(PREFIX)/share/gpr
libdir     = $(PREFIX)/lib
alidir     = $(libdir)

.PHONY: build test clean run_unit_tests install

build:
	$(GNATMAKE) -P json_ada.gpr -largs $(LDFLAGS)

test:
	$(GNATMAKE) -P test/unit/unit_tests.gpr -largs $(LDFLAGS)

clean:
	$(GNATCLEAN) -P json_ada.gpr
	$(GNATCLEAN) -P test/unit/unit_tests.gpr
	rmdir lib/json-ada lib obj test/unit/obj

run_unit_tests:
	./test/unit/test_bindings

install:
	$(GNATINSTALL) --relocate-build-tree -p -q -f --install-name='json-ada' \
		--sources-subdir=$(includedir) \
		--project-subdir=$(gprdir) \
		--lib-subdir=$(libdir) \
		--ali-subdir=$(alidir) \
		--prefix=$(PREFIX) -P json_ada.gpr
