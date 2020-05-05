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

```ada
with Ada.Command_Line;
with Ada.Text_IO;

with JSON.Parsers;
with JSON.Streams;
with JSON.Types;

procedure Example is
   package Types   is new JSON.Types (Long_Integer, Long_Float);
   package Parsers is new JSON.Parsers (Types);

   Text : constant JSON.Streams.Stream_Element_Array_Controlled :=
     JSON.Streams.Get_Stream_Element_Array (Ada.Command_Line.Argument (1));

   Parser : Parsers.Parser := Parsers.Create (JSON.Streams.Create_Stream (Text.Pointer));
   Value  : constant Types.JSON_Value := Parser.Parse;

   use Types;
begin
   --  The data type of a Value can be retrieved with `Value.Kind`. The value
   --  can be one of:
   --
   --  Null_Kind, Boolean_Kind, Integer_Kind, Float_Kind, String_Kind,
   --  Array_Kind, or Object_Kind.
   case Value.Kind is
      when Array_Kind | Object_Kind =>
         --  Query the length of a JSON array or object with the function `Length`
         Ada.Text_IO.Put_Line (Value.Length'Image);

         --  A JSON object provides the function `Contains`
         if Value.Kind = Object_Kind and then Value.Contains ("foo") then
            --  To get an element of a JSON array or object write `Value.Get (Index_Or_Key)`
            --  or use Ada 2012's indexing syntax `Value (Index_Or_Key)`.
            --
            --  If Value is a JSON object, you can call `Get_Array_Or_Empty`,
            --  `Get_Object_Or_Empty`, or `Get` (with an additional parameter containing
            --  the default value).
            Ada.Text_IO.Put_Line (Value ("foo").Kind'Image);
         end if;

         --  Iterate over a JSON array or object by using the
         --  Ada 2012 iterator syntax `for Element_Or_Key of Value`. The type is a
         --  JSON_Value and represents an element of the array or the key of an object.
         for Element of Value loop
            --  To get the image of JSON_Value (to serialize it), use function `Image`:
            Ada.Text_IO.Put_Line (Element.Image);
         end loop;
      when Float_Kind =>
         declare
            --  Get the value (String, generic Integer_Type or Float_Type, or Boolean)
            --  of a Value by calling `Value.Value`. An Invalid_Type_Error
            --  exception will be raised if Value has a different type than the type
            --  of the variable in which you try to store the result.
            Data : constant Long_Float := Value.Value;
         begin
            Ada.Text_IO.Put_Line (Data'Image);
         end;
      when others =>
         null;
   end case;
end Example;
```

Optional generic parameters of `JSON.Types`:

- `Maximum_Number_Length` (default is 30): Maximum length in characters of
  numbers

Optional generic parameters of `JSON.Parsers`:

- `Default_Maximum_Depth` (default is 10)

- `Check_Duplicate_Keys` (default is False): Check for duplicate keys when
  parsing. Parsing large JSON texts will be slower if enabled. If disabled
  then the `Get` operation will return the value of the first key that matches.

The actual parameter of `Create_Stream` can be an access to a `String`
or an access to a `Ada.Streams.Stream_Element_Array`.

The default maximum nesting depth can be overriden with
the optional second parameter `Maximum_Depth` of function `Create`.

After parsing, elements of arrays and objects are stored in the `Parser`
object, while strings remain stored in the `String` or `Stream_Element_Array`
object. These two objects must not go out of scope before any `JSON_Value`
goes out of scope.

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
