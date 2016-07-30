[![License](https://img.shields.io/:license-Apache_License_2.0-blue.svg)](https://github.com/onox/json-ada/blob/master/LICENSE.md)

json-ada
========

Ada 2012 bindings for JSON ([RFC 7159][url-rfc]).

Dependencies
------------

In order to build the bindings, you need to have:

 * An Ada 2012 compiler

 * [Ahven 2.x][url-ahven] if you want to build and run the unit tests

Compilation
-----------

A Makefile is provided to build the source code and tests. Use `make` to build
the source code:

    $ make

Use `make test` to build the unit tests.

Using json-ada in your project
------------------------------

Specify the dependency in your \*.gpr project file:

    with "json-ada";

Tests
-----

The project contains a set of unit tests. These can be run by build by uing `make`:

    $ make test

After having build the tests, run the unit tests:

    $ make run_unit_tests

License
-------

The Ada bindings and unit tests are licensed under the [Apache License 2.0][url-apache].

  [url-rfc]: https://tools.ietf.org/html/rfc7159
  [url-ahven]: http://ahven.stronglytyped.org
  [url-apache]: https://opensource.org/licenses/Apache-2.0
