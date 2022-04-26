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

with AUnit.Assertions;
with AUnit.Test_Caller;

with JSON.Parsers;
with JSON.Types;

package body Test_Images is

   package Types is new JSON.Types (Long_Integer, Long_Float);
   package Parsers is new JSON.Parsers (Types);

   use AUnit.Assertions;

   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(Images) ";
   begin
      Test_Suite.Add_Test (Caller.Create (Name & "Image 'true'", Test_True_Text'Access));
      Test_Suite.Add_Test (Caller.Create (Name & "Image 'false'", Test_False_Text'Access));
      Test_Suite.Add_Test (Caller.Create (Name & "Image 'null'", Test_Null_Text'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Image '""BS CR LF \ / HT""'", Test_Escaped_Text'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Image '""""'", Test_Empty_String_Text'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Image '""test""'", Test_Non_Empty_String_Text'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Image '""12.34""'", Test_Number_String_Text'Access));

      Test_Suite.Add_Test (Caller.Create (Name & "Image '42'", Test_Integer_Number_Text'Access));

      Test_Suite.Add_Test (Caller.Create (Name & "Image '[]'", Test_Empty_Array_Text'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Image '[""test""]'", Test_One_Element_Array_Text'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Image '[3.14, true]'", Test_Multiple_Elements_Array_Text'Access));

      Test_Suite.Add_Test (Caller.Create (Name & "Image '{}'", Test_Empty_Object_Text'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Image '{""foo"":""bar""}'", Test_One_Member_Object_Text'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Image '{""foo"":1,""bar"":2}'", Test_Multiple_Members_Object_Text'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Image '[{""foo"":[true, 42]}]'", Test_Array_Object_Array'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Image '{""foo"":[null, {""bar"": 42}]}'", Test_Object_Array_Object'Access));

      return Test_Suite'Access;
   end Suite;

   use Types;

   procedure Test_True_Text (Object : in out Test) is
      Text : constant String := "true";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value : constant JSON_Value := Parser.Parse;
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_True_Text;

   procedure Test_False_Text (Object : in out Test) is
      Text : constant String := "false";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value : constant JSON_Value := Parser.Parse;
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_False_Text;

   procedure Test_Null_Text (Object : in out Test) is
      Text : constant String := "null";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value : constant JSON_Value := Parser.Parse;
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_Null_Text;

   procedure Test_Escaped_Text (Object : in out Test) is
      Text : constant String := """BS:\b LF:\n CR:\r \\ \/ HT:\t""";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value : constant JSON_Value := Parser.Parse;
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_Escaped_Text;

   procedure Test_Empty_String_Text (Object : in out Test) is
      Text : constant String := """""";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value : constant JSON_Value := Parser.Parse;
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_Empty_String_Text;

   procedure Test_Non_Empty_String_Text (Object : in out Test) is
      Text : constant String := """test""";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value : constant JSON_Value := Parser.Parse;
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_Non_Empty_String_Text;

   procedure Test_Number_String_Text (Object : in out Test) is
      Text : constant String := """12.34""";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value : constant JSON_Value := Parser.Parse;
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_Number_String_Text;

   procedure Test_Integer_Number_Text (Object : in out Test) is
      Text : constant String := "42";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value : constant JSON_Value := Parser.Parse;
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_Integer_Number_Text;

   procedure Test_Empty_Array_Text (Object : in out Test) is
      Text : constant String := "[]";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value : constant JSON_Value := Parser.Parse;
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_Empty_Array_Text;

   procedure Test_One_Element_Array_Text (Object : in out Test) is
      Text : constant String := "[""test""]";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value : constant JSON_Value := Parser.Parse;
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_One_Element_Array_Text;

   procedure Test_Multiple_Elements_Array_Text (Object : in out Test) is
      Text : constant String := "[42,true]";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value : constant JSON_Value := Parser.Parse;
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_Multiple_Elements_Array_Text;

   procedure Test_Empty_Object_Text (Object : in out Test) is
      Text : constant String := "{}";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value : constant JSON_Value := Parser.Parse;
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_Empty_Object_Text;

   procedure Test_One_Member_Object_Text (Object : in out Test) is
      Text : constant String := "{""foo"":""bar""}";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value : constant JSON_Value := Parser.Parse;
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_One_Member_Object_Text;

   procedure Test_Multiple_Members_Object_Text (Object : in out Test) is
      Text  : constant String := "{""foo"":1,""bar"":2}";
      Text2 : constant String := "{""bar"":2,""foo"":1}";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value : constant JSON_Value := Parser.Parse;
      Image : constant String := Value.Image;
   begin
      Assert
        ((Text = Image) or else (Text2 = Image), "Image '" & Image & "' is not '" & Text & "'");
   end Test_Multiple_Members_Object_Text;

   procedure Test_Array_Object_Array (Object : in out Test) is
      Text : constant String := "[{""foo"":[true,42]}]";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value : constant JSON_Value := Parser.Parse;
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_Array_Object_Array;

   procedure Test_Object_Array_Object (Object : in out Test) is
      Text : constant String := "{""foo"":[null,{""bar"":42}]}";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value : constant JSON_Value := Parser.Parse;
      Image : constant String := Value.Image;
   begin
      Assert (Text = Image, "Image not '" & Text & "'");
   end Test_Object_Array_Object;

end Test_Images;
