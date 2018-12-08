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

private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Strings.Hash;

with Ada.Strings.Unbounded;
with Ada.Iterator_Interfaces;

generic
   type Integer_Type is range <>;
   type Float_Type is digits <>;
package JSON.Types is
   pragma Preelaborate;

   package SU renames Ada.Strings.Unbounded;

   type Value_Kind is
     (Array_Kind,
      Object_Kind,
      String_Kind,
      Integer_Kind,
      Float_Kind,
      Boolean_Kind,
      Null_Kind);

   type JSON_Value (Kind : Value_Kind) is tagged private
     with Constant_Indexing => Constant_Reference,
          Default_Iterator  => Iterate,
          Iterator_Element  => JSON_Value;

   function Image (Object : JSON_Value) return String;

   --  Value will raise an Invalid_Type_Error exception if
   --  the JSON value is of the wrong kind
   function Value (Object : JSON_Value) return String;
   function Value (Object : JSON_Value) return SU.Unbounded_String;
   function Value (Object : JSON_Value) return Integer_Type;
   function Value (Object : JSON_Value) return Float_Type;
   function Value (Object : JSON_Value) return Boolean;

   function Length (Object : JSON_Value) return Natural;
   --  For arrays and objects

   function Contains (Object : JSON_Value; Key : String) return Boolean;
   --  For objects

   function Get (Object : JSON_Value; Index : Positive) return JSON_Value;
   function Get (Object : JSON_Value; Key : String) return JSON_Value;

   procedure Append (Object : in out JSON_Value; Value : JSON_Value);
   --  For arrays

   procedure Insert
     (Object : in out JSON_Value;
      Key    : JSON_Value;
      Value  : JSON_Value);
   --  For objects

   Invalid_Type_Error : exception;

   -----------------------------------------------------------------------------
   --                                 Helpers                                 --
   -----------------------------------------------------------------------------

   function Get_Array_Or_Empty
     (Object : JSON_Value; Key : String) return JSON_Value
   with Inline;

   function Get_Object_Or_Empty
     (Object : JSON_Value; Key : String) return JSON_Value
   with Inline;

   function Get_Value_Or_Default
     (Object  : JSON_Value;
      Key     : String;
      Default : String) return JSON_Value
   with Inline;

   function Get_Value_Or_Default
     (Object  : JSON_Value;
      Key     : String;
      Default : Integer_Type) return JSON_Value
   with Inline;

   function Get_Value_Or_Default
     (Object  : JSON_Value;
      Key     : String;
      Default : Float_Type) return JSON_Value
   with Inline;

   function Get_Value_Or_Default
     (Object  : JSON_Value;
      Key     : String;
      Default : Boolean) return JSON_Value
   with Inline;

   -----------------------------------------------------------------------------
   --                              Constructors                               --
   -----------------------------------------------------------------------------

   function Create_String (Value : SU.Unbounded_String) return JSON_Value;

   function Create_Integer (Value : Integer_Type) return JSON_Value;

   function Create_Float (Value : Float_Type) return JSON_Value;

   function Create_Boolean (Value : Boolean) return JSON_Value;

   function Create_Null return JSON_Value;

   function Create_Array return JSON_Value;

   function Create_Object return JSON_Value;

   -----------------------------------------------------------------------------
   --                                Iterating                                --
   -----------------------------------------------------------------------------

   function Constant_Reference (Object : JSON_Value; Index : Positive)
     return JSON_Value;
   --  For Ada 2012 indexing syntax

   function Constant_Reference (Object : JSON_Value; Key : String)
     return JSON_Value;
   --  For Ada 2012 indexing syntax

   type Cursor (<>) is private;

   function Constant_Reference (Object : aliased JSON_Value; Position : Cursor)
     return JSON_Value;

   function Has_Element (Position : Cursor) return Boolean;

   package Value_Iterator_Interfaces is
     new Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Iterate (Object : JSON_Value)
     return Value_Iterator_Interfaces.Forward_Iterator'Class;

private

   type Vector_Type;
   type Map_Type;

   type Vector_Ptr is not null access Vector_Type;
   type Map_Ptr is not null access Map_Type;

   type JSON_Value (Kind : Value_Kind) is tagged record
      case Kind is
         when Array_Kind =>
            Vector : Vector_Ptr;
         when Object_Kind =>
            Map : Map_Ptr;
         when String_Kind =>
            String_Value : SU.Unbounded_String;
         when Integer_Kind =>
            Integer_Value : Integer_Type;
         when Float_Kind =>
            Float_Value : Float_Type;
         when Boolean_Kind =>
            Boolean_Value : Boolean;
         when others =>
            null;
      end case;
   end record;

   package JSON_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, JSON_Value);

   package JSON_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => JSON_Value,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   type Vector_Type is new JSON_Vectors.Vector with null record;
   type Map_Type    is new JSON_Maps.Map with null record;

   subtype Iterator_Kind is Value_Kind range Array_Kind .. Object_Kind;

   type Cursor (Kind : Iterator_Kind) is record
      case Kind is
         when Array_Kind =>
            Vector_Cursor : JSON_Vectors.Cursor;
         when Object_Kind =>
            Map_Cursor : JSON_Maps.Cursor;
      end case;
   end record;

   type Iterator (Kind : Iterator_Kind) is limited
     new Value_Iterator_Interfaces.Forward_Iterator with
   record
      case Kind is
         when Array_Kind =>
            Vector_Cursor : JSON_Vectors.Cursor;
         when Object_Kind =>
            Map_Cursor : JSON_Maps.Cursor;
      end case;
   end record;

   overriding function First (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor;

end JSON.Types;
