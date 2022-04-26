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

with AUnit.Assertions;
with AUnit.Test_Caller;

with JSON.Parsers;
with JSON.Types;

package body Test_Parsers is

   package Types is new JSON.Types (Long_Integer, Long_Float);
   package Parsers is new JSON.Parsers (Types, Check_Duplicate_Keys => True);

   use AUnit.Assertions;

   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(Parsers) ";
   begin
      Test_Suite.Add_Test (Caller.Create (Name & "Parse text 'true'", Test_True_Text'Access));
      Test_Suite.Add_Test (Caller.Create (Name & "Parse text 'false'", Test_False_Text'Access));
      Test_Suite.Add_Test (Caller.Create (Name & "Parse text 'null'", Test_Null_Text'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Parse text '""""'", Test_Empty_String_Text'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Parse text '""test""'", Test_Non_Empty_String_Text'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Parse text '""12.34""'", Test_Number_String_Text'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Parse text '42'", Test_Integer_Number_Text'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Parse text '42' as float", Test_Integer_Number_To_Float_Text'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Parse text '3.14'", Test_Float_Number_Text'Access));

      Test_Suite.Add_Test (Caller.Create (Name & "Parse text '[]'", Test_Empty_Array_Text'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Parse text '[""test""]'", Test_One_Element_Array_Text'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Parse text '[3.14, true]'", Test_Multiple_Elements_Array_Text'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Iterate over '[false, ""test"", 0.271e1]'", Test_Array_Iterable'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Iterate over '{""foo"":[1, ""2""],""bar"":[0.271e1]}'",
         Test_Multiple_Array_Iterable'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Parse text '{}'", Test_Empty_Object_Text'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Parse text '{""foo"":""bar""}'", Test_One_Member_Object_Text'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Parse text '{""foo"":1,""bar"":2}'", Test_Multiple_Members_Object_Text'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Iterate over '{""foo"":1,""bar"":2}'", Test_Object_Iterable'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Parse text '[{""foo"":[true, 42]}]'", Test_Array_Object_Array'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Parse text '{""foo"":[null, {""bar"": 42}]}'", Test_Object_Array_Object'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test getting array from text '{}'", Test_Object_No_Array'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Test getting object from text '{}'", Test_Object_No_Object'Access));

      --  Exceptions
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '[3.14""test""]'", Test_Array_No_Value_Separator_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '[true'", Test_Array_No_End_Array_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '[1]2'", Test_No_EOF_After_Array_Exception'Access));

      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text ''", Test_Empty_Text_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '{""foo"":1""bar"":2}'",
         Test_Object_No_Value_Separator_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '{""foo"",true}'", Test_Object_No_Name_Separator_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '{42:true}'", Test_Object_Key_No_String_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '{""foo"":true,}'", Test_Object_No_Second_Member_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '{""foo"":1,""foo"":2}'",
         Test_Object_Duplicate_Keys_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '{""foo"":}'", Test_Object_No_Value_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '{""foo"":true'", Test_Object_No_End_Object_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '{""foo"":true}[true]'", Test_No_EOF_After_Object_Exception'Access));

      return Test_Suite'Access;
   end Suite;

   use Types;

   procedure Fail (Message : String) is
   begin
      Assert (False, Message);
   end Fail;

   procedure Test_True_Text (Object : in out Test) is
      Text : constant String := "true";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      Assert (Value.Kind = Boolean_Kind, "Not a boolean");
      Assert (Value.Value, "Expected boolean value to be True");
   end Test_True_Text;

   procedure Test_False_Text (Object : in out Test) is
      Text : constant String := "false";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      Assert (Value.Kind = Boolean_Kind, "Not a boolean");
      Assert (not Value.Value, "Expected boolean value to be False");
   end Test_False_Text;

   procedure Test_Null_Text (Object : in out Test) is
      Text : constant String := "null";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      Assert (Value.Kind = Null_Kind, "Not a null");
   end Test_Null_Text;

   procedure Test_Empty_String_Text (Object : in out Test) is
      Text : constant String := """""";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      Assert (Value.Kind = String_Kind, "Not a string");
      Assert (Value.Value = "", "String value not empty");
   end Test_Empty_String_Text;

   procedure Test_Non_Empty_String_Text (Object : in out Test) is
      Text : constant String := """test""";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      Assert (Value.Kind = String_Kind, "Not a string");
      Assert (Value.Value = "test", "String value not equal to 'test'");
   end Test_Non_Empty_String_Text;

   procedure Test_Number_String_Text (Object : in out Test) is
      Text : constant String := """12.34""";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      Assert (Value.Kind = String_Kind, "Not a string");
      Assert (Value.Value = "12.34", "String value not equal to 12.34''");
   end Test_Number_String_Text;

   procedure Test_Integer_Number_Text (Object : in out Test) is
      Text : constant String := "42";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      Assert (Value.Kind = Integer_Kind, "Not an integer");
      Assert (Value.Value = 42, "Integer value not equal to 42");
   end Test_Integer_Number_Text;

   procedure Test_Integer_Number_To_Float_Text (Object : in out Test) is
      Text : constant String := "42";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      Assert (Value.Kind = Integer_Kind, "Not an integer");
      Assert (Value.Value = 42.0, "Integer value not equal to 42.0");
   end Test_Integer_Number_To_Float_Text;

   procedure Test_Float_Number_Text (Object : in out Test) is
      Text : constant String := "3.14";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      Assert (Value.Kind = Float_Kind, "Not a float");
      Assert (Value.Value = 3.14, "Float value not equal to 3.14");
   end Test_Float_Number_Text;

   procedure Test_Empty_Array_Text (Object : in out Test) is
      Text : constant String := "[]";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      Assert (Value.Kind = Array_Kind, "Not an array");
      Assert (Value.Length = 0, "Expected array to be empty");
   end Test_Empty_Array_Text;

   procedure Test_One_Element_Array_Text (Object : in out Test) is
      Text : constant String := "[""test""]";
      String_Value_Message : constant String := "Expected string at index 1 to be equal to 'test'";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      Assert (Value.Kind = Array_Kind, "Not an array");
      Assert (Value.Length = 1, "Expected length of array to be 1, got " & Value.Length'Image);

      begin
         Assert (Value.Get (1).Value = "test", String_Value_Message);
      exception
         when Constraint_Error =>
            Fail ("Could not get string value at index 1");
      end;
   end Test_One_Element_Array_Text;

   procedure Test_Multiple_Elements_Array_Text (Object : in out Test) is
      Text : constant String := "[3.14, true]";
      Float_Value_Message   : constant String := "Expected float at index 1 to be equal to 3.14";
      Boolean_Value_Message : constant String := "Expected boolean at index 2 to be True";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      Assert (Value.Kind = Array_Kind, "Not an array");
      Assert (Value.Length = 2, "Expected length of array to be 2");

      begin
         Assert (Value.Get (1).Value = 3.14, Float_Value_Message);
      exception
         when Constraint_Error =>
            Fail ("Could not get float value at index 1");
      end;
      begin
         Assert (Value.Get (2).Value, Boolean_Value_Message);
      exception
         when Constraint_Error =>
            Fail ("Could not get boolean value at index 2");
      end;
   end Test_Multiple_Elements_Array_Text;

   procedure Test_Array_Iterable (Object : in out Test) is
      Text : constant String := "[false, ""test"", 0.271e1]";
      Iterations_Message : constant String := "Unexpected number of iterations";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      Assert (Value.Kind = Array_Kind, "Not an array");
      Assert (Value.Length = 3, "Expected length of array to be 3");

      declare
         Iterations  : Natural := 0;
      begin
         for Element of Value loop
            Iterations := Iterations + 1;
            if Iterations = 1 then
               Assert (Element.Kind = Boolean_Kind, "Not a boolean");
               Assert (not Element.Value, "Expected boolean value to be False");
            elsif Iterations = 2 then
               Assert (Element.Kind = String_Kind, "Not a string");
               Assert (Element.Value = "test", "Expected string value to be 'test'");
            elsif Iterations = 3 then
               Assert (Element.Kind = Float_Kind, "Not a float");
               Assert (Element.Value = 2.71, "Expected float value to be 2.71");
            end if;
         end loop;
         Assert (Iterations = Value.Length, Iterations_Message);
      end;
   end Test_Array_Iterable;

   procedure Test_Multiple_Array_Iterable (Object : in out Test) is
      Text : constant String := "{""foo"":[1, ""2""],""bar"":[0.271e1]}";
      Iterations_Message : constant String := "Unexpected number of iterations";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      Assert (Value.Kind = Object_Kind, "Not an object");
      Assert (Value.Length = 2, "Expected length of object to be 2");

      declare
         Iterations  : Natural := 0;
      begin
         for Element of Value.Get ("foo") loop
            Iterations := Iterations + 1;
            if Iterations = 1 then
               Assert (Element.Kind = Integer_Kind, "Not an integer");
               Assert (Element.Value = 1, "Expected integer value to be 1");
            elsif Iterations = 2 then
               Assert (Element.Kind = String_Kind, "Not a string");
               Assert (Element.Value = "2", "Expected string value to be '2'");
            end if;
         end loop;
         Assert (Iterations = Value.Get ("foo").Length, Iterations_Message);
      end;

      declare
         Iterations  : Natural := 0;
      begin
         for Element of Value.Get ("bar") loop
            Iterations := Iterations + 1;
            if Iterations = 1 then
               Assert (Element.Kind = Float_Kind, "Not a float");
               Assert (Element.Value = 2.71, "Expected float value to be 2.71");
            end if;
         end loop;
         Assert (Iterations = Value.Get ("bar").Length, Iterations_Message);
      end;
   end Test_Multiple_Array_Iterable;

   procedure Test_Empty_Object_Text (Object : in out Test) is
      Text : constant String := "{}";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      Assert (Value.Kind = Object_Kind, "Not an object");
      Assert (Value.Length = 0, "Expected object to be empty");
   end Test_Empty_Object_Text;

   procedure Test_One_Member_Object_Text (Object : in out Test) is
      Text : constant String := "{""foo"":""bar""}";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      Assert (Value.Kind = Object_Kind, "Not an object");
      Assert (Value.Length = 1, "Expected length of object to be 1");

      Assert (Value.Get ("foo").Value = "bar", "Expected string value of 'foo' to be 'bar'");
   end Test_One_Member_Object_Text;

   procedure Test_Multiple_Members_Object_Text (Object : in out Test) is
      Text : constant String := "{""foo"":1,""bar"":2}";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      Assert (Value.Kind = Object_Kind, "Not an object");
      Assert (Value.Length = 2, "Expected length of object to be 2");

      Assert (Value.Get ("foo").Value = 1, "Expected integer value of 'foo' to be 1");
      Assert (Value.Get ("bar").Value = 2, "Expected integer value of 'bar' to be 2");
   end Test_Multiple_Members_Object_Text;

   procedure Test_Object_Iterable (Object : in out Test) is
      Text : constant String := "{""foo"":1,""bar"":2}";
      Iterations_Message : constant String := "Unexpected number of iterations";
      All_Keys_Message   : constant String := "Did not iterate over all expected keys";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      Assert (Value.Kind = Object_Kind, "Not an object");
      Assert (Value.Length = 2, "Expected length of object to be 2");

      declare
         Iterations  : Natural := 0;
         Retrieved_Foo : Boolean := False;
         Retrieved_Bar : Boolean := False;
      begin
         for Key of Value loop
            Iterations := Iterations + 1;
            if Iterations in 1 .. 2 then
               Assert (Key.Kind = String_Kind, "Not String");
               Assert (Key.Value in "foo" | "bar",
                 "Expected string value to be equal to 'foo' or 'bar'");

               Retrieved_Foo := Retrieved_Foo or Key.Value = "foo";
               Retrieved_Bar := Retrieved_Bar or Key.Value = "bar";
            end if;
         end loop;
         Assert (Iterations = Value.Length, Iterations_Message);
         Assert (Retrieved_Foo and Retrieved_Bar, All_Keys_Message);
      end;
   end Test_Object_Iterable;

   procedure Test_Array_Object_Array (Object : in out Test) is
      Text : constant String := "[{""foo"":[true, 42]}]";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      Assert (Value.Kind = Array_Kind, "Not an array");
      Assert (Value.Length = 1, "Expected length of array to be 1");

      begin
         declare
            Object : constant JSON_Value := Value.Get (1);
         begin
            Assert (Object.Length = 1, "Expected length of object to be 1");
            declare
               Array_Value : constant JSON_Value := Object.Get ("foo");
            begin
               Assert (Array_Value.Length = 2, "Expected length of array 'foo' to be 2");
               Assert (Array_Value.Get (2).Value = 42,
                 "Expected integer value at index 2 to be 42");
            end;
         exception
            when Constraint_Error =>
               Fail ("Value of 'foo' not an array");
         end;
      exception
         when Constraint_Error =>
            Fail ("First element in array not an object");
      end;
   end Test_Array_Object_Array;

   procedure Test_Object_Array_Object (Object : in out Test) is
      Text : constant String := "{""foo"":[null, {""bar"": 42}]}";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      Assert (Value.Kind = Object_Kind, "Not an object");
      Assert (Value.Length = 1, "Expected length of object to be 1");

      begin
         declare
            Array_Value : constant JSON_Value := Value.Get ("foo");
         begin
            Assert (Array_Value.Length = 2, "Expected length of array 'foo' to be 2");
            declare
               Object : constant JSON_Value := Array_Value.Get (2);
            begin
               Assert (Object.Length = 1, "Expected length of object to be 1");
               declare
                  Integer_Value : constant JSON_Value := Object.Get ("bar");
               begin
                  Assert (Integer_Value.Value = 42, "Expected integer value of 'bar' to be 42");
               end;
            exception
               when Constraint_Error =>
                  Fail ("Element 'bar' in object not an integer");
            end;
         exception
            when Constraint_Error =>
               Fail ("Value of index 2 not an object");
         end;
      exception
         when Constraint_Error =>
            Fail ("Element 'foo' in object not an array");
      end;
   end Test_Object_Array_Object;

   procedure Test_Object_No_Array (Object : in out Test) is
      Text : constant String := "{}";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      begin
         declare
            Object : JSON_Value := Value.Get ("foo");
            pragma Unreferenced (Object);
         begin
            Fail ("Expected Constraint_Error");
         end;
      exception
         when Constraint_Error =>
            null;
      end;

      begin
         declare
            Object : constant JSON_Value := Value.Get_Array_Or_Empty ("foo");
         begin
            Assert (Object.Length = 0, "Expected empty array");
         end;
      exception
         when Constraint_Error =>
            Fail ("Unexpected Constraint_Error");
      end;
   end Test_Object_No_Array;

   procedure Test_Object_No_Object (Object : in out Test) is
      Text : constant String := "{}";

      Parser : Parsers.Parser := Parsers.Create (Text);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      begin
         declare
            Object : JSON_Value := Value.Get ("foo");
            pragma Unreferenced (Object);
         begin
            Fail ("Expected Constraint_Error");
         end;
      exception
         when Constraint_Error =>
            null;
      end;

      begin
         declare
            Object : constant JSON_Value := Value.Get_Object_Or_Empty ("foo");
         begin
            Assert (Object.Length = 0, "Expected empty object");
         end;
      exception
         when Constraint_Error =>
            Fail ("Unexpected Constraint_Error");
      end;
   end Test_Object_No_Object;

   procedure Test_Empty_Text_Exception (Object : in out Test) is
      Text : constant String := "";

      Parser : Parsers.Parser := Parsers.Create (Text);
   begin
      declare
         Value  : JSON_Value := Parser.Parse;
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Parse_Error");
      end;
   exception
      when Parsers.Parse_Error =>
         null;
   end Test_Empty_Text_Exception;

   procedure Test_Array_No_Value_Separator_Exception (Object : in out Test) is
      Text : constant String := "[3.14""test""]";

      Parser : Parsers.Parser := Parsers.Create (Text);
   begin
      declare
         Value  : JSON_Value := Parser.Parse;
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Parse_Error");
      end;
   exception
      when Parsers.Parse_Error =>
         null;
   end Test_Array_No_Value_Separator_Exception;

   procedure Test_Array_No_End_Array_Exception (Object : in out Test) is
      Text : constant String := "[true";

      Parser : Parsers.Parser := Parsers.Create (Text);
   begin
      declare
         Value  : JSON_Value := Parser.Parse;
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Parse_Error");
      end;
   exception
      when Parsers.Parse_Error =>
         null;
   end Test_Array_No_End_Array_Exception;

   procedure Test_No_EOF_After_Array_Exception (Object : in out Test) is
      Text : constant String := "[1]2";

      Parser : Parsers.Parser := Parsers.Create (Text);
   begin
      declare
         Value  : JSON_Value := Parser.Parse;
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Parse_Error");
      end;
   exception
      when Parsers.Parse_Error =>
         null;
   end Test_No_EOF_After_Array_Exception;

   procedure Test_Object_No_Value_Separator_Exception (Object : in out Test) is
      Text : constant String := "{""foo"":1""bar"":2}";

      Parser : Parsers.Parser := Parsers.Create (Text);
   begin
      declare
         Value  : JSON_Value := Parser.Parse;
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Parse_Error");
      end;
   exception
      when Parsers.Parse_Error =>
         null;
   end Test_Object_No_Value_Separator_Exception;

   procedure Test_Object_No_Name_Separator_Exception (Object : in out Test) is
      Text : constant String := "{""foo"",true}";

      Parser : Parsers.Parser := Parsers.Create (Text);
   begin
      declare
         Value  : JSON_Value := Parser.Parse;
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Parse_Error");
      end;
   exception
      when Parsers.Parse_Error =>
         null;
   end Test_Object_No_Name_Separator_Exception;

   procedure Test_Object_Key_No_String_Exception (Object : in out Test) is
      Text : constant String := "{42:true}";

      Parser : Parsers.Parser := Parsers.Create (Text);
   begin
      declare
         Value  : JSON_Value := Parser.Parse;
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Parse_Error");
      end;
   exception
      when Parsers.Parse_Error =>
         null;
   end Test_Object_Key_No_String_Exception;

   procedure Test_Object_No_Second_Member_Exception (Object : in out Test) is
      Text : constant String := "{""foo"":true,}";

      Parser : Parsers.Parser := Parsers.Create (Text);
   begin
      declare
         Value  : JSON_Value := Parser.Parse;
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Parse_Error");
      end;
   exception
      when Parsers.Parse_Error =>
         null;
   end Test_Object_No_Second_Member_Exception;

   procedure Test_Object_Duplicate_Keys_Exception (Object : in out Test) is
      Text : constant String := "{""foo"":1,""foo"":2}";

      Parser : Parsers.Parser := Parsers.Create (Text);
   begin
      declare
         Value  : JSON_Value := Parser.Parse;
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Constraint_Error");
      end;
   exception
      when Constraint_Error =>
         null;
   end Test_Object_Duplicate_Keys_Exception;

   procedure Test_Object_No_Value_Exception (Object : in out Test) is
      Text : constant String := "{""foo"":}";

      Parser : Parsers.Parser := Parsers.Create (Text);
   begin
      declare
         Value  : JSON_Value := Parser.Parse;
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Parse_Error");
      end;
   exception
      when Parsers.Parse_Error =>
         null;
   end Test_Object_No_Value_Exception;

   procedure Test_Object_No_End_Object_Exception (Object : in out Test) is
      Text : constant String := "{""foo"":true";

      Parser : Parsers.Parser := Parsers.Create (Text);
   begin
      declare
         Value  : JSON_Value := Parser.Parse;
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Parse_Error");
      end;
   exception
      when Parsers.Parse_Error =>
         null;
   end Test_Object_No_End_Object_Exception;

   procedure Test_No_EOF_After_Object_Exception (Object : in out Test) is
      Text : constant String := "{""foo"":true}[true]";

      Parser : Parsers.Parser := Parsers.Create (Text);
   begin
      declare
         Value  : JSON_Value := Parser.Parse;
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Parse_Error");
      end;
   exception
      when Parsers.Parse_Error =>
         null;
   end Test_No_EOF_After_Object_Exception;

end Test_Parsers;
