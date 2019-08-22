--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2018 RREE <rolf.ebert.gcc@gmx.de>
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

with Ahven; use Ahven;

with JSON.Parsers;
with JSON.Streams;
with JSON.Types;

package body Test_Images is

   package Types is new JSON.Types (Long_Integer, Long_Float);
   package Parsers is new JSON.Parsers (Types);

   overriding
   procedure Initialize (T : in out Test) is
   begin
      T.Set_Name ("Images");

      T.Add_Test_Routine (Test_True_Text'Access, "Image 'true'");
      T.Add_Test_Routine (Test_False_Text'Access, "Image 'false'");
      T.Add_Test_Routine (Test_Null_Text'Access, "Image 'null'");
      T.Add_Test_Routine (Test_Escaped_Text'Access, "Image '""BS CR LF \ / HT""'");

      T.Add_Test_Routine (Test_Empty_String_Text'Access, "Image '""""'");
      T.Add_Test_Routine (Test_Non_Empty_String_Text'Access, "Image '""test""'");
      T.Add_Test_Routine (Test_Number_String_Text'Access, "Image '""12.34""'");

      T.Add_Test_Routine (Test_Integer_Number_Text'Access, "Image '42'");

      T.Add_Test_Routine (Test_Empty_Array_Text'Access, "Image '[]'");
      T.Add_Test_Routine (Test_One_Element_Array_Text'Access, "Image '[""test""]'");
      T.Add_Test_Routine (Test_Multiple_Elements_Array_Text'Access, "Image '[3.14, true]'");

      T.Add_Test_Routine (Test_Empty_Object_Text'Access, "Image '{}'");
      T.Add_Test_Routine (Test_One_Member_Object_Text'Access, "Image '{""foo"":""bar""}'");
      T.Add_Test_Routine (Test_Multiple_Members_Object_Text'Access, "Image '{""foo"":1,""bar"":2}'");

      T.Add_Test_Routine (Test_Array_Object_Array'Access, "Image '[{""foo"":[true, 42]}]'");
      T.Add_Test_Routine (Test_Object_Array_Object'Access, "Image '{""foo"":[null, {""bar"": 42}]}'");
   end Initialize;

   use Types;

   procedure Test_True_Text is
      Text : aliased String := "true";

      Stream    : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Allocator : Types.Memory_Allocator;
      Value : constant JSON_Value := Parsers.Parse (Stream, Allocator);
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_True_Text;

   procedure Test_False_Text is
      Text : aliased String := "false";

      Stream    : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Allocator : Types.Memory_Allocator;
      Value : constant JSON_Value := Parsers.Parse (Stream, Allocator);
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_False_Text;

   procedure Test_Null_Text is
      Text : aliased String := "null";

      Stream    : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Allocator : Types.Memory_Allocator;
      Value : constant JSON_Value := Parsers.Parse (Stream, Allocator);
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_Null_Text;

   procedure Test_Escaped_Text is
      Text : aliased String := """BS:\b LF:\n CR:\r \\ \/ HT:\t""";

      Stream    : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Allocator : Types.Memory_Allocator;
      Value : constant JSON_Value := Parsers.Parse (Stream, Allocator);
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_Escaped_Text;

   procedure Test_Empty_String_Text is
      Text : aliased String := """""";

      Stream    : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Allocator : Types.Memory_Allocator;
      Value : constant JSON_Value := Parsers.Parse (Stream, Allocator);
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_Empty_String_Text;

   procedure Test_Non_Empty_String_Text is
      Text : aliased String := """test""";

      Stream    : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Allocator : Types.Memory_Allocator;
      Value : constant JSON_Value := Parsers.Parse (Stream, Allocator);
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_Non_Empty_String_Text;

   procedure Test_Number_String_Text is
      Text : aliased String := """12.34""";

      Stream    : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Allocator : Types.Memory_Allocator;
      Value : constant JSON_Value := Parsers.Parse (Stream, Allocator);
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_Number_String_Text;

   procedure Test_Integer_Number_Text is
      Text : aliased String := "42";

      Stream    : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Allocator : Types.Memory_Allocator;
      Value : constant JSON_Value := Parsers.Parse (Stream, Allocator);
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_Integer_Number_Text;

   procedure Test_Empty_Array_Text is
      Text : aliased String := "[]";

      Stream    : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Allocator : Types.Memory_Allocator;
      Value : constant JSON_Value := Parsers.Parse (Stream, Allocator);
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_Empty_Array_Text;

   procedure Test_One_Element_Array_Text is
      Text : aliased String := "[""test""]";

      Stream    : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Allocator : Types.Memory_Allocator;
      Value : constant JSON_Value := Parsers.Parse (Stream, Allocator);
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_One_Element_Array_Text;

   procedure Test_Multiple_Elements_Array_Text is
      Text : aliased String := "[42,true]";

      Stream    : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Allocator : Types.Memory_Allocator;
      Value : constant JSON_Value := Parsers.Parse (Stream, Allocator);
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_Multiple_Elements_Array_Text;

   procedure Test_Empty_Object_Text is
      Text : aliased String := "{}";

      Stream    : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Allocator : Types.Memory_Allocator;
      Value : constant JSON_Value := Parsers.Parse (Stream, Allocator);
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_Empty_Object_Text;

   procedure Test_One_Member_Object_Text is
      Text : aliased String := "{""foo"":""bar""}";

      Stream    : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Allocator : Types.Memory_Allocator;
      Value : constant JSON_Value := Parsers.Parse (Stream, Allocator);
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_One_Member_Object_Text;

   procedure Test_Multiple_Members_Object_Text is
      Text  : aliased String := "{""foo"":1,""bar"":2}";
      Text2 : constant String := "{""bar"":2,""foo"":1}";

      Stream    : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Allocator : Types.Memory_Allocator;
      Value : constant JSON_Value := Parsers.Parse (Stream, Allocator);
      Image : constant String := Value.Image;
   begin
      Assert ((Text = Image) or else (Text2 = Image), "Image '" & Image & "' is not '" & Text & "'");
   end Test_Multiple_Members_Object_Text;

   procedure Test_Array_Object_Array is
      Text : aliased String := "[{""foo"":[true,42]}]";

      Stream    : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Allocator : Types.Memory_Allocator;
      Value : constant JSON_Value := Parsers.Parse (Stream, Allocator);
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_Array_Object_Array;

   procedure Test_Object_Array_Object is
      Text : aliased String := "{""foo"":[null,{""bar"":42}]}";

      Stream    : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Allocator : Types.Memory_Allocator;
      Value : constant JSON_Value := Parsers.Parse (Stream, Allocator);
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_Object_Array_Object;

end Test_Images;
