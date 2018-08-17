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

with Ada.Characters.Latin_1;

package body JSON.Types is

   function "+" (Text : String) return SU.Unbounded_String
     renames SU.To_Unbounded_String;

   function "+" (Text : SU.Unbounded_String) return String
     renames SU.To_String;

   function Value (Object : JSON_Value) return String is
   begin
      raise Invalid_Type_Error with "Value not a string";
      return "";
   end Value;

   function Value (Object : JSON_Value) return SU.Unbounded_String is
   begin
      raise Invalid_Type_Error with "Value not a string";
      return SU.To_Unbounded_String ("");
   end Value;

   function Value (Object : JSON_Value) return Integer_Type is
   begin
      raise Invalid_Type_Error with "Value not a integer";
      return 0;
   end Value;

   function Value (Object : JSON_Value) return Float_Type is
   begin
      raise Invalid_Type_Error with "Value not a float";
      return 0.0;
   end Value;

   function Value (Object : JSON_Value) return Boolean is
   begin
      raise Invalid_Type_Error with "Value not a boolean";
      return False;
   end Value;

   function Length (Object : JSON_Value) return Natural is
   begin
      raise Invalid_Type_Error with "Value not an object or array";
      return 0;
   end Length;

   function Contains (Object : JSON_Value; Key : String) return Boolean is
   begin
      raise Invalid_Type_Error with "Value not an object";
      return False;
   end Contains;

   function Get (Object : JSON_Value; Index : Positive) return JSON_Value'Class is
      Result : JSON_Null_Value;
   begin
      raise Invalid_Type_Error with "Value not an array";
      return Result;
   end Get;

   function Get (Object : JSON_Value; Key : String) return JSON_Value'Class is
      Result : JSON_Null_Value;
   begin
      raise Invalid_Type_Error with "Value not an object";
      return Result;
   end Get;

   function Create_String (Value : SU.Unbounded_String) return JSON_String_Value'Class is
   begin
      return JSON_String_Value'(String_Value => Value);
   end Create_String;

   function Create_Integer (Value : Integer_Type) return JSON_Integer_Value'Class is
   begin
      return JSON_Integer_Value'(Integer_Value => Value);
   end Create_Integer;

   function Create_Float (Value : Float_Type) return JSON_Float_Value'Class is
   begin
      return JSON_Float_Value'(Float_Value => Value);
   end Create_Float;

   function Create_Boolean (Value : Boolean) return JSON_Boolean_Value'Class is
   begin
      return JSON_Boolean_Value'(Boolean_Value => Value);
   end Create_Boolean;

   function Create_Null return JSON_Null_Value'Class is
      Value : JSON_Null_Value;
   begin
      return Value;
   end Create_Null;

   function Create_Array return JSON_Array_Value is
      Value : JSON_Array_Value;
   begin
      return Value;
   end Create_Array;

   function Create_Object return JSON_Object_Value is
      Value : JSON_Object_Value;
   begin
      return Value;
   end Create_Object;

   overriding
   function Value (Object : JSON_String_Value) return String
     is (+Object.String_Value);

   overriding
   function Value (Object : JSON_String_Value) return SU.Unbounded_String
     is (Object.String_Value);

   overriding
   function Value (Object : JSON_Boolean_Value) return Boolean
     is (Object.Boolean_Value);

   overriding
   function Value (Object : JSON_Integer_Value) return Integer_Type
     is (Object.Integer_Value);

   overriding
   function Value (Object : JSON_Integer_Value) return Float_Type
     is (Float_Type (Object.Integer_Value));

   overriding
   function Value (Object : JSON_Float_Value) return Float_Type
     is (Object.Float_Value);

   procedure Append (Object : in out JSON_Array_Value; Value : JSON_Value'Class) is
   begin
      Object.Vector.Append (Value);
   end Append;

   overriding
   function Length (Object : JSON_Array_Value) return Natural is
   begin
      return Natural (Object.Vector.Length);
   end Length;

   overriding
   function Get (Object : JSON_Array_Value; Index : Positive) return JSON_Value'Class is
     (Object.Vector.Element (Index));

   function Constant_Reference (Object : JSON_Array_Value; Position : JSON_Vectors.Cursor)
     return JSON_Vectors.Constant_Reference_Type
   is (Object.Vector.Constant_Reference (Position));

   function Iterate (Object : JSON_Array_Value)
     return JSON_Vectors.Vector_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return Object.Vector.Iterate;
   end Iterate;

   procedure Insert (Object : in out JSON_Object_Value;
                     Key    : JSON_String_Value'Class;
                     Value  : JSON_Value'Class) is
   begin
      Object.Map.Insert (Key.Value, Value);
   end Insert;

   overriding
   function Length (Object : JSON_Object_Value) return Natural is
   begin
      return Natural (Object.Map.Length);
   end Length;

   overriding
   function Contains (Object : JSON_Object_Value; Key : String) return Boolean is
   begin
      return Object.Map.Contains (Key);
   end Contains;

   overriding
   function Get (Object : JSON_Object_Value; Key : String) return JSON_Value'Class is
     (Object.Map.Element (Key));

   function Constant_Key (Object : JSON_Object_Value; Position : JSON_Maps.Cursor)
     return String is
   begin
      return JSON_Maps.Key (Position);
   end Constant_Key;

   function Iterate (Object : JSON_Object_Value)
     return JSON_Maps.Map_Iterator_Interfaces.Forward_Iterator'Class is
   begin
      return Object.Map.Iterate;
   end Iterate;

   function Get_Array_Or_Empty
     (Object : JSON_Value'Class; Key : String) return JSON_Array_Value
   is
      Empty_Value : JSON_Array_Value;
   begin
      if Object.Contains (Key) then
         return JSON_Array_Value (Object.Get (Key));
      else
         return Empty_Value;
      end if;
   end Get_Array_Or_Empty;

   function Get_Object_Or_Empty
     (Object : JSON_Value'Class; Key : String) return JSON_Object_Value
   is
      Empty_Value : JSON_Object_Value;
   begin
      if Object.Contains (Key) then
         return JSON_Object_Value (Object.Get (Key));
      else
         return Empty_Value;
      end if;
   end Get_Object_Or_Empty;

   function Get_Value_Or_Default
     (Object  : JSON_Value'Class;
      Key     : String;
      Default : String) return JSON_Value'Class is
   begin
      if Object.Contains (Key) then
         return Object.Get (Key);
      else
         return Create_String (+Default);
      end if;
   end Get_Value_Or_Default;

   function Get_Value_Or_Default
     (Object  : JSON_Value'Class;
      Key     : String;
      Default : Integer_Type) return JSON_Value'Class is
   begin
      if Object.Contains (Key) then
         return Object.Get (Key);
      else
         return Create_Integer (Default);
      end if;
   end Get_Value_Or_Default;

   function Get_Value_Or_Default
     (Object  : JSON_Value'Class;
      Key     : String;
      Default : Float_Type) return JSON_Value'Class is
   begin
      if Object.Contains (Key) then
         return Object.Get (Key);
      else
         return Create_Float (Default);
      end if;
   end Get_Value_Or_Default;

   function Get_Value_Or_Default
     (Object  : JSON_Value'Class;
      Key     : String;
      Default : Boolean) return JSON_Value'Class is
   begin
      if Object.Contains (Key) then
         return Object.Get (Key);
      else
         return Create_Boolean (Default);
      end if;
   end Get_Value_Or_Default;

   overriding
   function Image (Object : JSON_String_Value) return String is
      use Ada.Characters.Latin_1;
      Text : SU.Unbounded_String renames Object.String_Value;
      Esc  : SU.Unbounded_String;
      C    : Character;
   begin
      for Index in 1 .. SU.Length (Text) loop
         C := SU.Element (Text, Index);
         case C is
            when '"' | '\' | '/' =>
               SU.Append (Esc, '\' & C);
            when BS =>
               SU.Append (Esc, "\b");
            when FF =>
               SU.Append (Esc, "\f");
            when CR =>
               SU.Append (Esc, "\r");
            when LF =>
               SU.Append (Esc, "\n");
            when HT =>
               SU.Append (Esc, "\t");
            when others =>
               SU.Append (Esc, C);
         end case;
      end loop;
      return '"' & (+Esc) & '"';
   end Image;

   overriding
   function Image (Object : JSON_Integer_Value) return String is
      Result : constant String := Integer_Type'Image (Object.Integer_Value);
   begin
      if Object.Integer_Value < 0 then
         return Result;
      else
         return Result (2 .. Result'Last);
      end if;
   end Image;

   overriding
   function Image (Object : JSON_Float_Value) return String is
      Result : constant String := Float_Type'Image (Object.Float_Value);
   begin
      if Object.Float_Value < 0.0 then
         return Result;
      else
         return Result (2 .. Result'Last);
      end if;
   end Image;

   overriding
   function Image (Object : JSON_Boolean_Value) return String is
     (if Object.Boolean_Value then "true" else "false");

   overriding
   function Image (Object : JSON_Null_Value) return String is ("null");

   overriding
   function Image (Object : JSON_Array_Value) return String is
      Index  : Natural             := 0;
      Result : SU.Unbounded_String := +"[";
   begin
      for Element of Object loop
         Index := Index + 1;
         if Index > 1 then
            SU.Append (Result, ",");
         end if;
         SU.Append (Result, Element.Image);
      end loop;
      SU.Append (Result, "]");
      return +Result;
   end Image;

   overriding
   function Image (Object : JSON_Object_Value) return String is
      Index  : Natural             := 0;
      Result : SU.Unbounded_String := +"{";
   begin
      for Key of Object loop
         Index := Index + 1;
         if Index > 1 then
            SU.Append (Result, ',');
         end if;
         SU.Append (Result, '"' & Key & '"');
         SU.Append (Result, ':');
         SU.Append (Result, Object.Get (Key).Image);
      end loop;
      SU.Append (Result, '}');
      return +Result;
   end Image;

end JSON.Types;
