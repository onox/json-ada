[![License](https://img.shields.io/:license-Apache_License_2.0-blue.svg)](https://github.com/onox/json-ada/blob/master/LICENSE.md)

json-ada
========

An Ada 2012 library for parsing JSON ([RFC 7159][url-rfc]). The RFC does
not support comments, thus this library does not support it either. If
your JSON data contains comments, you should minify the data so that
comments are removed.

The library supports Ada 2012's iterator and indexing syntax. Unicode
may not be supported yet.

Usage
-----

To parse a JSON text, first instantiate `JSON.Parsers`:

```ada
package Types is new JSON.Types (Long_Integer, Long_Float);
package Parsers is new JSON.Parsers (Types);
```

You can replace the actual generic parameters of `JSON.Types` with your
own types if you want.

Then create a stream and parse it:

```ada
Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
Value  : constant JSON_Value := Parsers.Parse (Stream);
```

The actual parameter of `Create_Stream` can be an access to a `String`, a
`Ada.Streams.Stream_IO.Stream_Access`, or an access to a
`Ada.Streams.Stream_Element_Array`.

`Value.Kind` can be one of:

 * `Null_Kind`
 * `Boolean_Kind`
 * `Integer_Kind`
 * `Float_Kind`
 * `String_Kind`
 * `Array_Kind`
 * `Object_Kind`

To check if `Value` is of a certain type (for example an array), you can write
`if Value.Kind = Array_Kind then`.

To print the image of `Value` (to serialize it), write `Value.Image`.

To get the value (`String`, `Unbounded_String`, generic `Integer_Type`
or `Float_Type`, or `Boolean`) of a `Value`, call `Value.Value`. An
`Invalid_Type_Error` exception will be raised if `Value` has a different
type than the type of the variable in which you try to store the result.

To get an element of a JSON array or object write `Value.Get (My_Positive_Index)`
or `Value.Get (My_String_Key)`, or use Ada 2012's indexing syntax
`Value (Index_Or_Key)`.

If `Value` is a JSON object, you can call `Get_Array_Or_Empty`,
`Get_Object_Or_Empty`, or `Get_Value_Or_Default`.

A JSON array or object provides the function `Length`. A JSON object provides
the function `Contains`.

To iterate over a JSON array or object, use the
Ada 2012 iterator syntax `for Element_Or_Key of Value`. The type is a
`JSON_Value` and represents an element of the array or the key of an object.

Dependencies
------------

In order to build the library, you need to have:

 * An Ada 2012 compiler

Optional dependencies:

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
