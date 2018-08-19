MODE ?= release

GNAT_FLAGS ?= -dm
CFLAGS  ?= -O2 -march=native
LDFLAGS ?= -Wl,-z,relro -Wl,-z,now

GNATMAKE  = gprbuild $(GNAT_FLAGS) -p -XCompiler_Flags="$(CFLAGS)" -XMode=$(MODE)
GNATCLEAN = gprclean -q
GNATINSTALL = gprinstall

SRC_DIR = src
LIB_DIR = lib

PREFIX ?= /usr

includedir = $(PREFIX)/include
gprdir     = $(PREFIX)/share/gpr
libdir     = $(PREFIX)/lib
alidir     = $(libdir)

build_src:
	$(GNATMAKE) -P json_ada.gpr -largs $(LDFLAGS)

clean_src:
	$(GNATCLEAN) -P json_ada.gpr
	rmdir lib/json-ada lib obj

build_unit_tests:
	$(GNATMAKE) -P test/unit/unit_tests.gpr -largs $(LDFLAGS)

clean_unit_tests:
	$(GNATCLEAN) -P test/unit/unit_tests.gpr
	rmdir test/unit/obj

run_unit_tests:
	./test/unit/test_bindings

build: build_src

test: build_unit_tests

clean: clean_unit_tests clean_src

install:
	$(GNATINSTALL) --relocate-build-tree -p -q -f --install-name='json-ada' \
		--sources-subdir=$(includedir) \
		--project-subdir=$(gprdir) \
		--lib-subdir=$(libdir) \
		--ali-subdir=$(alidir) \
		--prefix=$(PREFIX) -P json_ada.gpr
