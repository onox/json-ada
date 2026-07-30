--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2016 onox <denkpadje@gmail.com>
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

with JSON.Streams;

generic
   type Integer_Type is range <>;
   type Float_Type is digits <>;

   Maximum_Number_Length : Positive := 30;
package JSON.Types with SPARK_Mode => On is
   --  JSON values and the operations to inspect and build them.
   --  @formal Integer_Type The integer type used for JSON integer values
   --  @formal Float_Type The floating-point type used for JSON float
   --    values
   --  @formal Maximum_Number_Length Maximum length, in characters, of the
   --    textual form of a number accepted while parsing

   pragma Preelaborate;

   Maximum_String_Length_Numbers : constant Positive := Maximum_Number_Length;
   --  Maximum length, in characters, of the textual form of a number

   type Value_Kind is
     (Array_Kind,
      Object_Kind,
      String_Kind,
      Integer_Kind,
      Float_Kind,
      Boolean_Kind,
      Null_Kind);
   --  The kind of data held by a JSON value
   --  @enum Array_Kind A JSON array
   --  @enum Object_Kind A JSON object
   --  @enum String_Kind A JSON string
   --  @enum Integer_Kind A JSON number with no fractional part or exponent
   --  @enum Float_Kind A JSON number with a fractional part or exponent
   --  @enum Boolean_Kind A JSON boolean (true or false)
   --  @enum Null_Kind The JSON null value

   subtype Composite_Kind is Value_Kind range Array_Kind .. Object_Kind;
   --  The kinds that hold elements or members: arrays and objects

   type JSON_Value is limited private;
   --  A node of a JSON document. Values are always handled through
   --  access values: owning JSON_Value_Access objects returned by the
   --  constructors below and by Parsers.Parse, and observers
   --  (access constant JSON_Value) returned by Get, First, and Next.

   type JSON_Value_Access is access JSON_Value;
   --  Owning access to a JSON value. The value owns its elements or
   --  members, so calling Free on the root of a document releases the
   --  whole document.

   -----------------------------------------------------------------------------
   --                                Observers                                --
   -----------------------------------------------------------------------------

   function Kind (Object : not null access constant JSON_Value) return Value_Kind;
   --  Return the kind of data held by the JSON value
   --  @param Object The JSON value to inspect
   --  @return The kind of the JSON value

   --  The Value functions require the JSON value to be of the right
   --  kind; this is a precondition instead of the Invalid_Type_Error
   --  exception raised by earlier versions of this library

   function Value (Object : not null access constant JSON_Value) return String
     with Pre => Kind (Object) = String_Kind;
   --  Return the string held by a JSON string value, with JSON escape
   --  sequences decoded; \uXXXX escape sequences (including UTF-16
   --  surrogate pairs) are decoded to UTF-8
   --  @param Object The JSON value, which must be of String_Kind
   --  @return The string value with escape sequences decoded

   function Value (Object : not null access constant JSON_Value) return Integer_Type
     with Pre => Kind (Object) = Integer_Kind;
   --  Return the integer held by a JSON integer value
   --  @param Object The JSON value, which must be of Integer_Kind
   --  @return The integer value

   function Value (Object : not null access constant JSON_Value) return Float_Type
     with Pre => Kind (Object) in Integer_Kind | Float_Kind;
   --  Return the floating-point value of a JSON number; an integer value
   --  is converted to a float
   --  @param Object The JSON value, which must be of Integer_Kind or
   --    Float_Kind
   --  @return The value as a floating-point number

   function Value (Object : not null access constant JSON_Value) return Boolean
     with Pre => Kind (Object) = Boolean_Kind;
   --  Return the boolean held by a JSON boolean value
   --  @param Object The JSON value, which must be of Boolean_Kind
   --  @return The boolean value

   function Length (Object : not null access constant JSON_Value) return Natural
     with Pre => Kind (Object) in Composite_Kind;
   --  Return the number of elements or members of a JSON array or object
   --  @param Object The JSON value, which must be an array or object
   --  @return The number of elements or members

   function Contains
     (Object : not null access constant JSON_Value;
      Key    : String) return Boolean
   with Pre => Kind (Object) = Object_Kind;
   --  Return True if the JSON object contains a member for the given key
   --
   --  This function has a time complexity of O(n).
   --  @param Object The JSON value, which must be of Object_Kind
   --  @param Key The member key to look for, in JSON escaped form
   --  @return True if the object has a member with the given key

   function Get
     (Object : not null access constant JSON_Value;
      Index  : Positive) return access constant JSON_Value
   with Pre => Kind (Object) = Array_Kind;
   --  Return the JSON value at the given index in the JSON array, or
   --  null if the array has no such index
   --  @param Object The JSON value, which must be of Array_Kind
   --  @param Index The one-based index of the element to return
   --  @return An observer of the element, or null if the index is out of
   --    range

   function Get
     (Object : not null access constant JSON_Value;
      Key    : String) return access constant JSON_Value
   with Pre => Kind (Object) = Object_Kind;
   --  Return the JSON value for the given key in the JSON object, or
   --  null if the object has no such key
   --
   --  This function has a time complexity of O(n).
   --  @param Object The JSON value, which must be of Object_Kind
   --  @param Key The member key to look up, in JSON escaped form
   --  @return An observer of the member value, or null if the object has
   --    no such key

   -----------------------------------------------------------------------------
   --                                Iterating                                --
   -----------------------------------------------------------------------------

   --  Iterate over the elements of an array or the members of an object
   --  with:
   --
   --     Element : access constant JSON_Value := First (Object);
   --     ...
   --     while Element /= null loop
   --        ...
   --        Element := Next (Element);
   --     end loop;
   --
   --  For members of an object, function Key returns the member key.

   function First (Object : not null access constant JSON_Value)
     return access constant JSON_Value
   with Pre => Kind (Object) in Composite_Kind;
   --  Return an observer of the first element or member of a JSON array or
   --  object, or null if it is empty
   --  @param Object The JSON value, which must be an array or object
   --  @return An observer of the first element or member, or null if empty

   function Next (Object : not null access constant JSON_Value)
     return access constant JSON_Value;
   --  Return an observer of the next element or member following Object in
   --  its array or object, or null if Object is the last one
   --  @param Object An element or member obtained from First or Next
   --  @return An observer of the following element or member, or null at
   --    the end

   function Key (Object : not null access constant JSON_Value) return String;
   --  Return the key of a member of a JSON object with JSON escape
   --  sequences decoded, or "" if the value is not a member of an object
   --  @param Object The JSON value to query
   --  @return The member key with escape sequences decoded, or "" if
   --    Object is not an object member

   function Has_Key (Object : not null access constant JSON_Value) return Boolean;
   --  Return True if the value is a member of a JSON object
   --  @param Object The JSON value to query
   --  @return True if Object is a member of a JSON object

   -----------------------------------------------------------------------------
   --                                  Image                                  --
   -----------------------------------------------------------------------------

   procedure Image
     (Object : not null access constant JSON_Value;
      Result : in out Streams.String_Buffer)
   with Always_Terminates,
        Exceptional_Cases => (Streams.Buffer_Overflow_Error => True);
   --  Append the JSON text of the value to Result
   --  @param Object The JSON value to serialize
   --  @param Result The buffer to which the JSON text is appended
   --  @exception Streams.Buffer_Overflow_Error Raised if the serialized
   --     text would exceed the buffer's capacity

   -----------------------------------------------------------------------------
   --                              Constructors                               --
   -----------------------------------------------------------------------------

   --  The objects returned by these functions own heap memory; either
   --  transfer ownership with Append, Insert, Prepend, or Prepend_Member,
   --  or call Free

   function Is_Standalone (Object : not null access constant JSON_Value) return Boolean;
   --  Return True if the value is not an element or member of an array
   --  or object. Values returned by the constructors below are
   --  standalone; the building procedures require and consume
   --  standalone values.
   --  @param Object The JSON value to query
   --  @return True if the value is not part of any array or object

   function Create_String (Value : String) return JSON_Value_Access
     with Post => Create_String'Result /= null
                    and then Kind (Create_String'Result) = String_Kind
                    and then Is_Standalone (Create_String'Result);
   --  Create a JSON string value from a string in JSON escaped form
   --  (the form in which strings appear in JSON text, without the
   --  surrounding '"' characters)
   --  @param Value The string, in JSON escaped form
   --  @return A new standalone JSON string value owned by the caller

   function Create_Integer (Value : Integer_Type) return JSON_Value_Access
     with Post => Create_Integer'Result /= null
                    and then Kind (Create_Integer'Result) = Integer_Kind
                    and then Is_Standalone (Create_Integer'Result);
   --  Create a JSON integer value
   --  @param Value The integer value
   --  @return A new standalone JSON integer value owned by the caller

   function Create_Float (Value : Float_Type) return JSON_Value_Access
     with Post => Create_Float'Result /= null
                    and then Kind (Create_Float'Result) = Float_Kind
                    and then Is_Standalone (Create_Float'Result);
   --  Create a JSON float value
   --  @param Value The floating-point value
   --  @return A new standalone JSON float value owned by the caller

   function Create_Boolean (Value : Boolean) return JSON_Value_Access
     with Post => Create_Boolean'Result /= null
                    and then Kind (Create_Boolean'Result) = Boolean_Kind
                    and then Is_Standalone (Create_Boolean'Result);
   --  Create a JSON boolean value
   --  @param Value The boolean value
   --  @return A new standalone JSON boolean value owned by the caller

   function Create_Null return JSON_Value_Access
     with Post => Create_Null'Result /= null
                    and then Kind (Create_Null'Result) = Null_Kind
                    and then Is_Standalone (Create_Null'Result);
   --  Create a JSON null value
   --  @return A new standalone JSON null value owned by the caller

   function Create_Array return JSON_Value_Access
     with Post => Create_Array'Result /= null
                    and then Kind (Create_Array'Result) = Array_Kind
                    and then Length (Create_Array'Result) = 0
                    and then Is_Standalone (Create_Array'Result);
   --  Create an empty JSON array; fill it with Append or Prepend
   --  @return A new standalone, empty JSON array owned by the caller

   function Create_Object return JSON_Value_Access
     with Post => Create_Object'Result /= null
                    and then Kind (Create_Object'Result) = Object_Kind
                    and then Length (Create_Object'Result) = 0
                    and then Is_Standalone (Create_Object'Result);
   --  Create an empty JSON object; fill it with Insert or Prepend_Member
   --  @return A new standalone, empty JSON object owned by the caller

   -----------------------------------------------------------------------------
   --                                Building                                 --
   -----------------------------------------------------------------------------

   procedure Append
     (Object : not null access JSON_Value;
      Value  : in out JSON_Value_Access)
   with Pre  => Kind (Object) = Array_Kind
                  and then Value /= null
                  and then Is_Standalone (Value)
                  and then Length (Object) < Natural'Last,
        Always_Terminates,
        Post => Value = null
                  and then Kind (Object) = Array_Kind
                  and then Length (Object) = Length (Object)'Old + 1
                  and then Is_Standalone (Object) = Is_Standalone (Object)'Old;
   --  Add a value to the end of a JSON array, taking ownership of Value
   --
   --  This procedure has a time complexity of O(n).
   --  @param Object The JSON array to append to
   --  @param Value The standalone value to add; set to null on return as
   --    ownership is transferred to Object

   procedure Insert
     (Object : not null access JSON_Value;
      Key    : String;
      Value  : in out JSON_Value_Access)
   with Pre  => Kind (Object) = Object_Kind
                  and then Value /= null
                  and then Is_Standalone (Value)
                  and then Length (Object) < Natural'Last,
        Always_Terminates,
        Post => Value = null
                  and then Kind (Object) = Object_Kind
                  and then Length (Object) = Length (Object)'Old + 1
                  and then Is_Standalone (Object) = Is_Standalone (Object)'Old;
   --  Add a member to the end of a JSON object, taking ownership of
   --  Value. Key must be in JSON escaped form. The presence of duplicate
   --  keys is not checked; use Contains before calling Insert if needed.
   --
   --  This procedure has a time complexity of O(n).
   --  @param Object The JSON object to add the member to
   --  @param Key The member key, in JSON escaped form
   --  @param Value The standalone value to add; set to null on return as
   --    ownership is transferred to Object

   procedure Prepend
     (Object : not null access JSON_Value;
      Value  : in out JSON_Value_Access)
   with Pre  => Kind (Object) = Array_Kind
                  and then Value /= null
                  and then Is_Standalone (Value)
                  and then Length (Object) < Natural'Last,
        Always_Terminates,
        Post => Value = null
                  and then Kind (Object) = Array_Kind
                  and then Length (Object) = Length (Object)'Old + 1
                  and then Is_Standalone (Object) = Is_Standalone (Object)'Old;
   --  Like Append, but adds the value to the front in O(1) time
   --  @param Object The JSON array to prepend to
   --  @param Value The standalone value to add; set to null on return as
   --    ownership is transferred to Object

   procedure Prepend_Member
     (Object : not null access JSON_Value;
      Key    : String;
      Value  : in out JSON_Value_Access)
   with Pre  => Kind (Object) = Object_Kind
                  and then Value /= null
                  and then Is_Standalone (Value)
                  and then Length (Object) < Natural'Last,
        Always_Terminates,
        Post => Value = null
                  and then Kind (Object) = Object_Kind
                  and then Length (Object) = Length (Object)'Old + 1
                  and then Is_Standalone (Object) = Is_Standalone (Object)'Old;
   --  Like Insert, but adds the member to the front in O(1) time
   --  @param Object The JSON object to prepend the member to
   --  @param Key The member key, in JSON escaped form
   --  @param Value The standalone value to add; set to null on return as
   --    ownership is transferred to Object

   procedure Reverse_Elements (Object : not null access JSON_Value)
     with Pre  => Kind (Object) in Composite_Kind,
          Always_Terminates,
          Post => Kind (Object) = Kind (Object)'Old
                    and then Length (Object) = Length (Object)'Old
                    and then Is_Standalone (Object) = Is_Standalone (Object)'Old;
   --  Reverse the order of the elements or members of a JSON array or
   --  object. Building a container with Prepend or Prepend_Member and
   --  then calling Reverse_Elements once is equivalent to building it
   --  with Append or Insert, in O(n) total time.
   --  @param Object The JSON array or object whose elements are reversed

   procedure Free (Object : in out JSON_Value_Access)
     with Always_Terminates,
          Post => Object = null,
          --  the freed access value is null, the old tree reaching only the
          --  deallocation: callers that drop it are not dropping a computed
          --  value
          Depends => (Object => null, null => Object);
   --  Release the value and everything it owns
   --  @param Object The value to release; set to null on return (a null
   --    value is accepted and is a no-op)

private

   use type Streams.Text_Access;

   type JSON_Value is limited record
      Kind          : Value_Kind          := Null_Kind;
      Next          : JSON_Value_Access   := null;
      Key           : Streams.Text_Access := null;
      Str           : Streams.Text_Access := null;
      Boolean_Value : Boolean             := False;
      Integer_Value : Integer_Type        := Integer_Type'First;
      Float_Value   : Float_Type          := Float_Type'First;
      First_Child   : JSON_Value_Access   := null;
      Length        : Natural             := 0;
   end record;
   --  Str is never null when Kind = String_Kind for values built with
   --  the constructors; the observers nevertheless treat a null Str as
   --  an empty string, because a type predicate stating the property
   --  cannot be re-established by GNATprove when a container is
   --  reconstructed at the end of a borrow.

end JSON.Types;
