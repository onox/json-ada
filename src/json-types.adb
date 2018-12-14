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
with Ada.Characters.Handling;

package body JSON.Types is

   function "+" (Text : String) return SU.Unbounded_String
     renames SU.To_Unbounded_String;

   function "+" (Text : SU.Unbounded_String) return String
     renames SU.To_String;

   function Unescape (Text : String) return String is
      Value   : SU.Unbounded_String;
      Escaped : Boolean := False;

      use Ada.Characters.Latin_1;
   begin
      for C of Text loop
         if Escaped then
            case C is
               when '"' | '\' | '/' =>
                  SU.Append (Value, C);
               when 'b' =>
                  SU.Append (Value, BS);
               when 'f' =>
                  SU.Append (Value, FF);
               when 'n' =>
                  SU.Append (Value, LF);
               when 'r' =>
                  SU.Append (Value, CR);
               when 't' =>
                  SU.Append (Value, HT);
               when others =>
                  raise Program_Error;
            end case;
         elsif C = '"' then
            raise Program_Error;
         elsif C /= '\' then
            if Ada.Characters.Handling.Is_Control (C) then
               raise Program_Error;
            end if;
            SU.Append (Value, C);
         end if;
         Escaped := not Escaped and C = '\';
      end loop;
      return +Value;
   end Unescape;

   function Escape (Text : String) return String is
      Value : SU.Unbounded_String;

      use Ada.Characters.Latin_1;
   begin
      for C of Text loop
         case C is
            when '"' | '\' | '/' =>
               SU.Append (Value, '\' & C);
            when BS =>
               SU.Append (Value, "\b");
            when FF =>
               SU.Append (Value, "\f");
            when CR =>
               SU.Append (Value, "\r");
            when LF =>
               SU.Append (Value, "\n");
            when HT =>
               SU.Append (Value, "\t");
            when others =>
               SU.Append (Value, C);
         end case;
      end loop;
      return +Value;
   end Escape;

   -----------------------------------------------------------------------------
   --                              Constructors                               --
   -----------------------------------------------------------------------------

   function Create_String
     (Stream : Streams.Stream_Ptr;
      Offset, Length : Streams.AS.Stream_Element_Offset) return JSON_Value is
   begin
      return (Kind => String_Kind, Stream => Stream,
        String_Offset => Offset, String_Length => Length);
   end Create_String;

   function Create_String (Value : SU.Unbounded_String) return JSON_Value is
   begin
      return (Kind => Unbounded_String_Kind, String_Value => Value);
   end Create_String;

   function Create_Integer (Value : Integer_Type) return JSON_Value is
   begin
      return (Kind => Integer_Kind, Integer_Value => Value);
   end Create_Integer;

   function Create_Float (Value : Float_Type) return JSON_Value is
   begin
      return (Kind => Float_Kind, Float_Value => Value);
   end Create_Float;

   function Create_Boolean (Value : Boolean) return JSON_Value is
   begin
      return (Kind => Boolean_Kind, Boolean_Value => Value);
   end Create_Boolean;

   function Create_Null return JSON_Value is
   begin
      return (Kind => Null_Kind);
   end Create_Null;

   function Create_Array return JSON_Value is
   begin
      return (Kind => Array_Kind, Vector => new Vector_Type);
   end Create_Array;

   function Create_Object return JSON_Value is
   begin
      return (Kind => Object_Kind, Map => new Map_Type);
   end Create_Object;

   -----------------------------------------------------------------------------
   --                                  Value                                  --
   -----------------------------------------------------------------------------

   function Value (Object : JSON_Value) return String is
   begin
      if Object.Kind = String_Kind then
         return Unescape (Object.Stream.Get_String (Object.String_Offset, Object.String_Length));
      elsif Object.Kind = Unbounded_String_Kind then
         return Unescape (+Object.String_Value);
      else
         raise Invalid_Type_Error with "Value not a string";
      end if;
   end Value;

   function Value (Object : JSON_Value) return Boolean is
   begin
      if Object.Kind = Boolean_Kind then
         return Object.Boolean_Value;
      else
         raise Invalid_Type_Error with "Value not a boolean";
      end if;
   end Value;

   function Value (Object : JSON_Value) return Integer_Type is
   begin
      if Object.Kind = Integer_Kind then
         return Object.Integer_Value;
      else
         raise Invalid_Type_Error with "Value not a integer";
      end if;
   end Value;

   function Value (Object : JSON_Value) return Float_Type is
   begin
      if Object.Kind = Float_Kind then
         return Object.Float_Value;
      elsif Object.Kind = Integer_Kind then
         return Float_Type (Object.Integer_Value);
      else
         raise Invalid_Type_Error with "Value not a float";
      end if;
   end Value;

   -----------------------------------------------------------------------------

   function Length (Object : JSON_Value) return Natural is
   begin
      if Object.Kind = Array_Kind then
         return Natural (Object.Vector.Length);
      elsif Object.Kind = Object_Kind then
         return Natural (Object.Map.Length);
      else
         raise Invalid_Type_Error with "Value not an object or array";
      end if;
   end Length;

   function Contains (Object : JSON_Value; Key : String) return Boolean is
   begin
      if Object.Kind = Object_Kind then
         return Object.Map.Contains (Key);
      else
         raise Invalid_Type_Error with "Value not an object";
      end if;
   end Contains;

   function Get (Object : JSON_Value; Index : Positive) return JSON_Value is
   begin
      if Object.Kind = Array_Kind then
         return Object.Vector.Element (Index);
      else
         raise Invalid_Type_Error with "Value not an array";
      end if;
   end Get;

   function Get (Object : JSON_Value; Key : String) return JSON_Value is
   begin
      if Object.Kind = Object_Kind then
         return Object.Map.Element (Key);
      else
         raise Invalid_Type_Error with "Value not an object";
      end if;
   end Get;

   procedure Append (Object : in out JSON_Value; Value : JSON_Value) is
   begin
      if Object.Kind = Array_Kind then
         Object.Vector.Append (Value);
      else
         raise Invalid_Type_Error with "Value not an array";
      end if;
   end Append;

   procedure Insert
     (Object : in out JSON_Value;
      Key    : JSON_Value;
      Value  : JSON_Value) is
   begin
      if Object.Kind = Object_Kind then
         Object.Map.Insert (Key.Value, Value);
      else
         raise Invalid_Type_Error with "Value not an object";
      end if;
   end Insert;

   -----------------------------------------------------------------------------

   function Constant_Reference (Object : JSON_Value; Index : Positive)
     return JSON_Value renames Get;

   function Constant_Reference (Object : JSON_Value; Key : String)
     return JSON_Value renames Get;

   function Constant_Reference (Object : aliased JSON_Value; Position : Cursor)
     return JSON_Value is
   begin
      case Position.Kind is
         when Array_Kind =>
            return JSON_Vectors.Element (Position.Vector_Cursor);
         when Object_Kind =>
            return Create_String (SU.To_Unbounded_String
              (JSON_Maps.Key (Position.Map_Cursor)));
      end case;
   end Constant_Reference;

   function Has_Element (Position : Cursor) return Boolean is
   begin
      case Position.Kind is
         when Array_Kind =>
            return JSON_Vectors.Has_Element (Position.Vector_Cursor);
         when Object_Kind =>
            return JSON_Maps.Has_Element (Position.Map_Cursor);
      end case;
   end Has_Element;

   overriding
   function First (Object : Iterator) return Cursor is
   begin
      case Object.Kind is
         when Array_Kind =>
            return Cursor'(Kind => Array_Kind, Vector_Cursor => Object.Vector_Cursor);
         when Object_Kind =>
            return Cursor'(Kind => Object_Kind, Map_Cursor => Object.Map_Cursor);
      end case;
   end First;

   overriding
   function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor is
   begin
      case Object.Kind is
         when Array_Kind =>
            return Cursor'
              (Kind          => Array_Kind,
               Vector_Cursor => JSON_Vectors.Next (Position.Vector_Cursor));
         when Object_Kind =>
            return Cursor'
              (Kind       => Object_Kind,
               Map_Cursor => JSON_Maps.Next (Position.Map_Cursor));
      end case;
   end Next;

   function Iterate (Object : JSON_Value)
     return Value_Iterator_Interfaces.Forward_Iterator'Class is
   begin
      if Object.Kind = Array_Kind then
         return Iterator'
           (Kind          => Array_Kind,
            Vector_Cursor => Object.Vector.First);
      elsif Object.Kind = Object_Kind then
         return Iterator'
           (Kind       => Object_Kind,
            Map_Cursor => Object.Map.First);
      else
         raise Program_Error with "Can only iterate over an array or object";
      end if;
   end Iterate;

   -----------------------------------------------------------------------------
   --                                 Helpers                                 --
   -----------------------------------------------------------------------------

   function Get_Array_Or_Empty
     (Object : JSON_Value; Key : String) return JSON_Value is
   begin
      if Object.Contains (Key) then
         return Object.Get (Key);
      else
         return Create_Array;
      end if;
   end Get_Array_Or_Empty;

   function Get_Object_Or_Empty
     (Object : JSON_Value; Key : String) return JSON_Value is
   begin
      if Object.Contains (Key) then
         return Object.Get (Key);
      else
         return Create_Object;
      end if;
   end Get_Object_Or_Empty;

   function Get
     (Object  : JSON_Value;
      Key     : String;
      Default : String) return JSON_Value is
   begin
      if Object.Contains (Key) then
         return Object.Get (Key);
      else
         return Create_String (+Default);
      end if;
   end Get;

   function Get
     (Object  : JSON_Value;
      Key     : String;
      Default : Integer_Type) return JSON_Value is
   begin
      if Object.Contains (Key) then
         return Object.Get (Key);
      else
         return Create_Integer (Default);
      end if;
   end Get;

   function Get
     (Object  : JSON_Value;
      Key     : String;
      Default : Float_Type) return JSON_Value is
   begin
      if Object.Contains (Key) then
         return Object.Get (Key);
      else
         return Create_Float (Default);
      end if;
   end Get;

   function Get
     (Object  : JSON_Value;
      Key     : String;
      Default : Boolean) return JSON_Value is
   begin
      if Object.Contains (Key) then
         return Object.Get (Key);
      else
         return Create_Boolean (Default);
      end if;
   end Get;

   -----------------------------------------------------------------------------
   --                                  Image                                   -
   -----------------------------------------------------------------------------

   function Image_String (Object : JSON_Value) return String is
      Text : String renames Object.Stream.Get_String
        (Object.String_Offset, Object.String_Length);
   begin
      --  A string backed by a stream is always escaped. The tokenizer
      --  will verify that the string does not contain unexpected characters
      return '"' & Text & '"';
   end Image_String;

   function Image_Unbounded_String (Object : JSON_Value) return String is
     ('"' & Escape (+Object.String_Value) & '"');

   function Image_Integer (Object : JSON_Value) return String is
      Result : constant String := Integer_Type'Image (Object.Integer_Value);
   begin
      if Object.Integer_Value < 0 then
         return Result;
      else
         return Result (2 .. Result'Last);
      end if;
   end Image_Integer;

   function Image_Float (Object : JSON_Value) return String is
      Result : constant String := Float_Type'Image (Object.Float_Value);
   begin
      if Object.Float_Value < 0.0 then
         return Result;
      else
         return Result (2 .. Result'Last);
      end if;
   end Image_Float;

   function Image_Boolean (Object : JSON_Value) return String is
     (if Object.Boolean_Value then "true" else "false");

   function Image_Null (Object : JSON_Value) return String is ("null");

   function Image_Array (Object : JSON_Value) return String is
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
   end Image_Array;

   function Image_Object (Object : JSON_Value) return String is
      Index  : Natural             := 0;
      Result : SU.Unbounded_String := +"{";
   begin
      for Key of Object loop
         Index := Index + 1;
         if Index > 1 then
            SU.Append (Result, ',');
         end if;
         SU.Append (Result, '"' & Key.Value & '"');
         SU.Append (Result, ':');
         SU.Append (Result, Object.Get (Key.Value).Image);
      end loop;
      SU.Append (Result, '}');
      return +Result;
   end Image_Object;

   function Image (Object : JSON_Value) return String is
   begin
      case Object.Kind is
         when Array_Kind =>
            return Image_Array (Object);
         when Object_Kind =>
            return Image_Object (Object);
         when String_Kind =>
            return Image_String (Object);
         when Unbounded_String_Kind =>
            return Image_Unbounded_String (Object);
         when Integer_Kind =>
            return Image_Integer (Object);
         when Float_Kind =>
            return Image_Float (Object);
         when Boolean_Kind =>
            return Image_Boolean (Object);
         when Null_Kind =>
            return Image_Null (Object);
      end case;
   end Image;

end JSON.Types;
