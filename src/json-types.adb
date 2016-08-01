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

package body JSON.Types is

   function Create_String (Value : SU.Unbounded_String) return JSON_String_Value'Class is
   begin
      return JSON_String_Value'(String_Value => Value);
   end Create_String;

   function Create_Integer (Value : Long_Integer) return JSON_Integer_Value'Class is
   begin
      return JSON_Integer_Value'(Integer_Value => Value);
   end Create_Integer;

   function Create_Float (Value : Long_Float) return JSON_Float_Value'Class is
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

   function Value (Object : JSON_String_Value) return String
     is (SU.To_String (Object.String_Value));

   function Value (Object : JSON_Boolean_Value) return Boolean
     is (Object.Boolean_Value);

   function Value (Object : JSON_Integer_Value) return Long_Integer
     is (Object.Integer_Value);

   function Value (Object : JSON_Float_Value) return Long_Float
     is (Object.Float_Value);

   procedure Append (Object : in out JSON_Array_Value; Value : JSON_Value'Class) is
   begin
      Object.Vector.Append (Value);
   end Append;

   function Length (Object : JSON_Array_Value) return Natural is
   begin
      return Natural (Object.Vector.Length);
   end Length;

   function Get (Object : JSON_Array_Value; Index : Positive) return JSON_String_Value'Class is
   begin
      return JSON_String_Value (Object.Vector.Element (Index));
   end Get;

   function Get (Object : JSON_Array_Value; Index : Positive) return JSON_Integer_Value'Class is
   begin
      return JSON_Integer_Value (Object.Vector.Element (Index));
   end Get;

   function Get (Object : JSON_Array_Value; Index : Positive) return JSON_Float_Value'Class is
   begin
      return JSON_Float_Value (Object.Vector.Element (Index));
   end Get;

   function Get (Object : JSON_Array_Value; Index : Positive) return JSON_Boolean_Value'Class is
   begin
      return JSON_Boolean_Value (Object.Vector.Element (Index));
   end Get;

   function Get (Object : JSON_Array_Value; Index : Positive) return JSON_Null_Value'Class is
   begin
      return JSON_Null_Value (Object.Vector.Element (Index));
   end Get;

   function Get (Object : JSON_Array_Value; Index : Positive) return JSON_Array_Value'Class is
   begin
      return JSON_Array_Value (Object.Vector.Element (Index));
   end Get;

   function Get (Object : JSON_Array_Value; Index : Positive) return JSON_Object_Value'Class is
   begin
      return JSON_Object_Value (Object.Vector.Element (Index));
   end Get;

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

   function Length (Object : JSON_Object_Value) return Natural is
   begin
      return Natural (Object.Map.Length);
   end Length;

   function Contains (Object : JSON_Object_Value; Key : String) return Boolean is
   begin
      return Object.Map.Contains (Key);
   end Contains;

   function Get (Object : JSON_Object_Value; Key : String) return JSON_String_Value'Class is
   begin
      return JSON_String_Value (Object.Map.Element (Key));
   end Get;

   function Get (Object : JSON_Object_Value; Key : String) return JSON_Integer_Value'Class is
   begin
      return JSON_Integer_Value (Object.Map.Element (Key));
   end Get;

   function Get (Object : JSON_Object_Value; Key : String) return JSON_Float_Value'Class is
   begin
      return JSON_Float_Value (Object.Map.Element (Key));
   end Get;

   function Get (Object : JSON_Object_Value; Key : String) return JSON_Boolean_Value'Class is
   begin
      return JSON_Boolean_Value (Object.Map.Element (Key));
   end Get;

   function Get (Object : JSON_Object_Value; Key : String) return JSON_Null_Value'Class is
   begin
      return JSON_Null_Value (Object.Map.Element (Key));
   end Get;

   function Get (Object : JSON_Object_Value; Key : String) return JSON_Array_Value'Class is
   begin
      return JSON_Array_Value (Object.Map.Element (Key));
   end Get;

   function Get (Object : JSON_Object_Value; Key : String) return JSON_Object_Value'Class is
   begin
      return JSON_Object_Value (Object.Map.Element (Key));
   end Get;

end JSON.Types;
