MODE ?= release

GNAT_FLAGS ?=
CFLAGS  ?= -O2 -march=native
LDFLAGS ?= -Wl,-z,relro -Wl,-z,now

GNATMAKE  = gprbuild $(GNAT_FLAGS) -p -XCompiler_Flags="$(CFLAGS)" -XMode=$(MODE)
GNATCLEAN = gprclean -q

INSTALL         = install
INSTALL_DATA    = $(INSTALL) --mode=644 --preserve-timestamps
INSTALL_ALI     = $(INSTALL) --mode=444

SRC_DIR = src
LIB_DIR = lib

MAJOR      = 1
MINOR      = 0
SO_FILE    = libjson-ada.so
SO_LIBRARY = $(SO_FILE).$(MAJOR).$(MINOR)

PREFIX ?= /usr

includedir = $(PREFIX)/include/json-ada
gprdir     = $(PREFIX)/lib/gnat
libdir     = $(PREFIX)/lib
alidir     = $(libdir)/json-ada

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

clean: clean_src clean_unit_tests

install:
	$(INSTALL) -d $(includedir)
	$(INSTALL) -d $(libdir)
	$(INSTALL) -d $(alidir)
	$(INSTALL) -d $(gprdir)

	$(INSTALL_DATA) $(SRC_DIR)/*.ad[bs] $(includedir)
	$(INSTALL_ALI) $(LIB_DIR)/json-ada/*.ali $(alidir)
	$(INSTALL_DATA) gnat/json_ada.gpr $(gprdir)

	$(INSTALL) $(LIB_DIR)/$(SO_LIBRARY) $(libdir)
	cd $(libdir) && ln -sf $(SO_LIBRARY) $(SO_FILE).$(MAJOR)
	cd $(libdir) && ln -sf $(SO_FILE).$(MAJOR) $(SO_FILE)
