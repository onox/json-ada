GNATMAKE=gprbuild -p

GNATCLEAN=gprclean -q

build_src:
	$(GNATMAKE) -P json_ada_lib.gpr -Xbuild=release

build_src_debug:
	$(GNATMAKE) -P json_ada_lib.gpr -Xbuild=debug

clean_src:
	$(GNATCLEAN) -P json_ada_lib.gpr
	rmdir lib obj

build_unit_tests:
	$(GNATMAKE) -P test/unit/unit_tests.gpr

clean_unit_tests:
	$(GNATCLEAN) -P test/unit/unit_tests.gpr
	rmdir test/unit/obj

run_unit_tests:
	./test/unit/test_bindings

build: build_src

test: build_unit_tests

clean: clean_src clean_unit_tests
