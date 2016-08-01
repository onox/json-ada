[![License](https://img.shields.io/:license-Apache_License_2.0-blue.svg)](https://github.com/onox/json-ada/blob/master/LICENSE.md)

json-ada
========

An Ada 2012 library for parsing JSON ([RFC 7159][url-rfc]). The RFC does
not support comments, thus this library does not support it either. If
your JSON data contains comments, you should minify the data so that
comments are removed.

The library currently does not support Unicode yet.

Dependencies
------------

In order to build the library, you need to have:

 * An Ada 2012 compiler

 * [Ahven 2.x][url-ahven] if you want to build and run the unit tests

Using json-ada in your project
------------------------------

Specify the dependency in your \*.gpr project file:

    with "json_ada";

Compilation
-----------

A Makefile is provided to build the source code and tests. Use `make` to build
the source code:

    $ make

Tests
-----

The project contains a set of unit tests. Use `make test` to build the unit tests:

    $ make test

After having build the tests, run the unit tests:

    $ make run_unit_tests

Installation
------------

After having compiled the source code, the library can be installed by executing:

    $ make PREFIX=/usr install

Change `PREFIX` to the preferred destination folder.

License
-------

The Ada code and unit tests are licensed under the [Apache License 2.0][url-apache].

  [url-rfc]: https://tools.ietf.org/html/rfc7159
  [url-ahven]: http://ahven.stronglytyped.org
  [url-apache]: https://opensource.org/licenses/Apache-2.0
