[![License](https://img.shields.io/github/license/onox/json-ada.svg?color=blue)](https://github.com/onox/json-ada/blob/master/LICENSE)
[![Build status](https://img.shields.io/shippable/5c87f0062c0a8108001e0ae3/master.svg)](https://app.shippable.com/github/onox/json-ada)
[![GitHub release](https://img.shields.io/github/release/onox/json-ada.svg)](https://github.com/onox/json-ada/releases/latest)
[![IRC](https://img.shields.io/badge/IRC-%23ada%20on%20freenode-orange.svg)](https://webchat.freenode.net/?channels=ada)

# json-ada

An Ada 2012 library for parsing JSON ([RFC 7159][url-rfc]). It supports
Ada 2012's iterator and indexing syntax. The RFC does not support
comments, thus this library does not support it either. If your JSON data
contains comments, you should minify the data so that comments are removed.

Unicode may not be supported yet.

## Usage

To parse a JSON text, first instantiate `JSON.Parsers`:

```ada
package Types is new JSON.Types (Long_Integer, Long_Float);
package Parsers is new JSON.Parsers (Types);
```

You can replace the actual generic parameters of `JSON.Types` with your
own types if you want. Specify `Maximum_Number_Length` (default is 30)
when instantiating `JSON.Types` to set the maximum length in characters
of numbers. The default maximum nesting depth can be specified with
`Default_Maximum_Depth` (default is 10).

If you want to check for duplicate keys when parsing, set
`Check_Duplicate_Keys` to `True` when instantiating `JSON.Parsers`. This
can make parsing large JSON texts slower. If set to `False` (the default),
then the `Get` operation will return the value of the first key that matches.

Then create a stream and parse it:

```ada
Stream    : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
Allocator : Types.Memory_Allocator (Maximum_Depth => 10);
Value     : constant JSON_Value := Parsers.Parse (Stream, Allocator);
```

The actual parameter of `Create_Stream` can be an access to a `String`
or an access to a `Ada.Streams.Stream_Element_Array`. Parameter `Maximum_Depth`
is optional and has by default the value of the generic parameter
`Default_Maximum_Depth` from the package `JSON.Types`.

After parsing, elements of arrays and objects are stored in the `Allocator`
object, while strings remain stored in the `Stream` object. These two
objects must not go out of scope before any `JSON_Value` goes out of scope.

#### Using a `JSON_Value` object

The **data type** of a `Value` can be retrieved with `Value.Kind`. The value
can be one of:

`Null_Kind`, `Boolean_Kind`, `Integer_Kind`, `Float_Kind`, `String_Kind`,
`Array_Kind`, or `Object_Kind`.

To check if `Value` is of a certain type (for example an array), you can write
`if Value.Kind = Array_Kind then`.

To get the **image** of `Value` (to serialize it), write `Value.Image`.

**Get** the value (`String`, generic `Integer_Type` or `Float_Type`, or
`Boolean`) of a `Value` by calling `Value.Value`. An `Invalid_Type_Error`
exception will be raised if `Value` has a different type than the type
of the variable in which you try to store the result.

To get an element of a JSON array or object write `Value.Get (Index_Or_Key)`
or use Ada 2012's indexing syntax `Value (Index_Or_Key)`.

If `Value` is a JSON object, you can call `Get_Array_Or_Empty`,
`Get_Object_Or_Empty`, or `Get` (with an additional parameter containing
the default value).

**Query** the length of a JSON array or object with the function `Length`.
A JSON object provides the function `Contains`.

**Iterate** over a JSON array or object by using the
Ada 2012 iterator syntax `for Element_Or_Key of Value`. The type is a
`JSON_Value` and represents an element of the array or the key of an object.

## Dependencies

In order to build the library, you need to have:

 * An Ada 2012 compiler

 * GPRBuild and `make`

Optional dependencies:

 * [Ahven 2.x][url-ahven] if you want to build and run the unit tests
 * `lcov` to generate a coverage report for unit tests

## Installing dependencies on Ubuntu 18.04 LTS

Install the dependencies using apt:

```sh
$ sudo apt install gnat-7 gprbuild libahven6-dev lcov
```

## Compilation

A Makefile is provided to build the source code and tests. Use `make` to build
the source code:

```
$ make
```

## Tests

The project contains a set of unit tests. Use `make tests` to build and
run the unit tests. A coverage report can be generated with `make coverage`:

```
$ make tests
$ make coverage
```

## Installation

After having compiled the source code, the library can be installed by executing:

```
$ make PREFIX=/usr install
```

Change `PREFIX` to the preferred destination folder, for example `~/.local`.

## Using json-ada in your project

Specify the dependency in your \*.gpr project file:

```ada
with "json_ada";
```

## Contributing

Read the [contributing guidelines][url-contributing] if you want to add
a bugfix or an improvement.

## License

The Ada code and unit tests are licensed under the [Apache License 2.0][url-apache].
The first line of each Ada file should contain an SPDX license identifier tag that
refers to this license:

    SPDX-License-Identifier: Apache-2.0

  [url-rfc]: https://tools.ietf.org/html/rfc7159
  [url-ahven]: http://ahven.stronglytyped.org
  [url-apache]: https://opensource.org/licenses/Apache-2.0
  [url-contributing]: /CONTRIBUTING.md
