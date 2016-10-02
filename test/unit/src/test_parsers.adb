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

with Ahven; use Ahven;

with JSON.Parsers;
with JSON.Streams;
with JSON.Types;

package body Test_Parsers is

   package Types is new JSON.Types (Long_Integer, Long_Float);
   package Parsers is new JSON.Parsers (Types);

   overriding
   procedure Initialize (T : in out Test) is
   begin
      T.Set_Name ("Parsers");

      T.Add_Test_Routine (Test_True_Text'Access, "Parse text 'true'");
      T.Add_Test_Routine (Test_False_Text'Access, "Parse text 'false'");
      T.Add_Test_Routine (Test_Null_Text'Access, "Parse text 'null'");

      T.Add_Test_Routine (Test_Empty_String_Text'Access, "Parse text '""""'");
      T.Add_Test_Routine (Test_Non_Empty_String_Text'Access, "Parse text '""test""'");
      T.Add_Test_Routine (Test_Number_String_Text'Access, "Parse text '""12.34""'");

      T.Add_Test_Routine (Test_Integer_Number_Text'Access, "Parse text '42'");
      T.Add_Test_Routine (Test_Float_Number_Text'Access, "Parse text '3.14'");

      T.Add_Test_Routine (Test_Empty_Array_Text'Access, "Parse text '[]'");
      T.Add_Test_Routine (Test_One_Element_Array_Text'Access, "Parse text '[""test""]'");
      T.Add_Test_Routine (Test_Multiple_Elements_Array_Text'Access, "Parse text '[3.14, true]'");
      T.Add_Test_Routine (Test_Array_Iterable'Access, "Iterate over '[false, ""test"", 0.271e1]'");

      T.Add_Test_Routine (Test_Empty_Object_Text'Access, "Parse text '{}'");
      T.Add_Test_Routine (Test_One_Member_Object_Text'Access, "Parse text '{""foo"":""bar""}'");
      T.Add_Test_Routine (Test_Multiple_Members_Object_Text'Access, "Parse text '{""foo"":1,""bar"":2}'");
      T.Add_Test_Routine (Test_Object_Iterable'Access, "Iterate over '{""foo"":1,""bar"":2}'");

      T.Add_Test_Routine (Test_Array_Object_Array'Access, "Parse text '[{""foo"":[true, 42]}]'");
      T.Add_Test_Routine (Test_Object_Array_Object'Access, "Parse text '{""foo"":[null, {""bar"": 42}]}'");

      T.Add_Test_Routine (Test_Object_No_Array'Access, "Test getting array from text '{}'");
      T.Add_Test_Routine (Test_Object_No_Object'Access, "Test getting object from text '{}'");

      --  Exceptions
      T.Add_Test_Routine (Test_Array_No_Value_Separator_Exception'Access, "Reject text '[3.14""test""]'");
      T.Add_Test_Routine (Test_Array_No_End_Array_Exception'Access, "Reject text '[true'");
      T.Add_Test_Routine (Test_No_EOF_After_Array_Exception'Access, "Reject text '[1]2'");

      T.Add_Test_Routine (Test_Empty_Text_Exception'Access, "Reject text ''");
      T.Add_Test_Routine (Test_Object_No_Value_Separator_Exception'Access, "Reject text '{""foo"":1""bar"":2}'");
      T.Add_Test_Routine (Test_Object_No_Name_Separator_Exception'Access, "Reject text '{""foo"",true}'");
      T.Add_Test_Routine (Test_Object_Key_No_String_Exception'Access, "Reject text '{42:true}'");
      T.Add_Test_Routine (Test_Object_No_Second_Member_Exception'Access, "Reject text '{""foo"":true,}'");
      T.Add_Test_Routine (Test_Object_Duplicate_Keys_Exception'Access, "Reject text '{""foo"":1,""foo"":2}'");
      T.Add_Test_Routine (Test_Object_No_Value_Exception'Access, "Reject text '{""foo"":}'");
      T.Add_Test_Routine (Test_Object_No_End_Object_Exception'Access, "Reject text '{""foo"":true'");
      T.Add_Test_Routine (Test_No_EOF_After_Object_Exception'Access, "Reject text '{""foo"":true}[true]'");
   end Initialize;

   use Types;

   procedure Test_True_Text is
      Text : aliased String := "true";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Value  : constant JSON_Value'Class := Parsers.Parse (Stream);
   begin
      Assert (Value in JSON_Boolean_Value, "Not JSON_Boolean_Value");
      Assert (JSON_Boolean_Value (Value).Value, "Expected boolean value to be True");
   end Test_True_Text;

   procedure Test_False_Text is
      Text : aliased String := "false";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Value  : constant JSON_Value'Class := Parsers.Parse (Stream);
   begin
      Assert (Value in JSON_Boolean_Value, "Not JSON_Boolean_Value");
      Assert (not JSON_Boolean_Value (Value).Value, "Expected boolean value to be False");
   end Test_False_Text;

   procedure Test_Null_Text is
      Text : aliased String := "null";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Value  : constant JSON_Value'Class := Parsers.Parse (Stream);
   begin
      Assert (Value in JSON_Null_Value, "Not JSON_Null_Value");
   end Test_Null_Text;

   procedure Test_Empty_String_Text is
      Text : aliased String := """""";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Value  : constant JSON_Value'Class := Parsers.Parse (Stream);
   begin
      Assert (Value in JSON_String_Value, "Not JSON_String_Value");
      Assert (JSON_String_Value (Value).Value = "", "String value not empty");
   end Test_Empty_String_Text;

   procedure Test_Non_Empty_String_Text is
      Text : aliased String := """test""";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Value  : constant JSON_Value'Class := Parsers.Parse (Stream);
   begin
      Assert (Value in JSON_String_Value, "Not JSON_String_Value");
      Assert (JSON_String_Value (Value).Value = "test", "String value not equal to 'test'");
   end Test_Non_Empty_String_Text;

   procedure Test_Number_String_Text is
      Text : aliased String := """12.34""";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Value  : constant JSON_Value'Class := Parsers.Parse (Stream);
   begin
      Assert (Value in JSON_String_Value, "Not JSON_String_Value");
      Assert (JSON_String_Value (Value).Value = "12.34", "String value not equal to 12.34''");
   end Test_Number_String_Text;

   procedure Test_Integer_Number_Text is
      Text : aliased String := "42";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Value  : constant JSON_Value'Class := Parsers.Parse (Stream);
   begin
      Assert (Value in JSON_Integer_Value, "Not JSON_Integer_Value");
      Assert (JSON_Integer_Value (Value).Value = 42, "Integer value not equal to 42");
   end Test_Integer_Number_Text;

   procedure Test_Float_Number_Text is
      Text : aliased String := "3.14";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Value  : constant JSON_Value'Class := Parsers.Parse (Stream);
   begin
      Assert (Value in JSON_Float_Value, "Not JSON_Float_Value");
      Assert (JSON_Float_Value (Value).Value = 3.14, "Float value not equal to 3.14");
   end Test_Float_Number_Text;

   procedure Test_Empty_Array_Text is
      Text : aliased String := "[]";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Value  : constant JSON_Value'Class := Parsers.Parse (Stream);
   begin
      Assert (Value in JSON_Array_Value, "Not JSON_Array_Value");
      Assert (Value.Length = 0, "Expected array to be empty");
   end Test_Empty_Array_Text;

   procedure Test_One_Element_Array_Text is
      Text : aliased String := "[""test""]";
      String_Value_Message : constant String := "Expected string at index 1 to be equal to 'test'";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Value  : constant JSON_Value'Class := Parsers.Parse (Stream);
   begin
      Assert (Value in JSON_Array_Value, "Not JSON_Array_Value");
      Assert (Value.Length = 1, "Expected length of array to be 1");

      begin
         Assert (JSON_Array_Value (Value).Get (1).Value = "test", String_Value_Message);
      exception
         when Constraint_Error =>
            Fail ("Could not get string value at index 1");
      end;
   end Test_One_Element_Array_Text;

   procedure Test_Multiple_Elements_Array_Text is
      Text : aliased String := "[3.14, true]";
      Float_Value_Message   : constant String := "Expected float at index 1 to be equal to 3.14";
      Boolean_Value_Message : constant String := "Expected boolean at index 2 to be True";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Value  : constant JSON_Value'Class := Parsers.Parse (Stream);
   begin
      Assert (Value in JSON_Array_Value, "Not JSON_Array_Value");
      Assert (Value.Length = 2, "Expected length of array to be 2");

      begin
         Assert (JSON_Array_Value (Value).Get (1).Value = 3.14, Float_Value_Message);
      exception
         when Constraint_Error =>
            Fail ("Could not get float value at index 1");
      end;
      begin
         Assert (JSON_Array_Value (Value).Get (2).Value, Boolean_Value_Message);
      exception
         when Constraint_Error =>
            Fail ("Could not get boolean value at index 2");
      end;
   end Test_Multiple_Elements_Array_Text;

   procedure Test_Array_Iterable is
      Text : aliased String := "[false, ""test"", 0.271e1]";
      Iterations_Message : constant String := "Unexpected number of iterations";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Value  : constant JSON_Value'Class := Parsers.Parse (Stream);
   begin
      Assert (Value in JSON_Array_Value, "Not JSON_Array_Value");
      Assert (Value.Length = 3, "Expected length of array to be 3");

      declare
         Array_Value : constant JSON_Array_Value'Class := JSON_Array_Value (Value);
         Iterations  : Natural := 0;
      begin
         for Element of Array_Value loop
            Iterations := Iterations + 1;
            if Iterations = 1 then
               Assert (Element in JSON_Boolean_Value, "Not JSON_Boolean_Value");
               Assert (not JSON_Boolean_Value (Element).Value, "Expected boolean value to be False");
            elsif Iterations = 2 then
               Assert (Element in JSON_String_Value, "Not JSON_String_Value");
               Assert (JSON_String_Value (Element).Value = "test", "Expected string value to be 'test'");
            elsif Iterations = 3 then
               Assert (Element in JSON_Float_Value, "Not JSON_Float_Value");
               Assert (JSON_Float_Value (Element).Value = 2.71, "Expected float value to be 2.71");
            end if;
         end loop;
         Assert (Iterations = Array_Value.Length, Iterations_Message);
      end;
   end Test_Array_Iterable;

   procedure Test_Empty_Object_Text is
      Text : aliased String := "{}";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Value  : constant JSON_Value'Class := Parsers.Parse (Stream);
   begin
      Assert (Value in JSON_Object_Value, "Not JSON_Object_Value");
      Assert (Value.Length = 0, "Expected object to be empty");
   end Test_Empty_Object_Text;

   procedure Test_One_Member_Object_Text is
      Text : aliased String := "{""foo"":""bar""}";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Value  : constant JSON_Value'Class := Parsers.Parse (Stream);
   begin
      Assert (Value in JSON_Object_Value, "Not JSON_Object_Value");
      Assert (Value.Length = 1, "Expected length of object to be 1");

      Assert (JSON_Object_Value (Value).Get ("foo").Value = "bar", "Expected string value of 'foo' to be 'bar'");
   end Test_One_Member_Object_Text;

   procedure Test_Multiple_Members_Object_Text is
      Text : aliased String := "{""foo"":1,""bar"":2}";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Value  : constant JSON_Value'Class := Parsers.Parse (Stream);
   begin
      Assert (Value in JSON_Object_Value, "Not JSON_Object_Value");
      Assert (Value.Length = 2, "Expected length of object to be 2");

      Assert (JSON_Object_Value (Value).Get ("foo").Value = 1, "Expected integer value of 'foo' to be 1");
      Assert (JSON_Object_Value (Value).Get ("bar").Value = 2, "Expected integer value of 'bar' to be 2");
   end Test_Multiple_Members_Object_Text;

   procedure Test_Object_Iterable is
      Text : aliased String := "{""foo"":1,""bar"":2}";
      Iterations_Message : constant String := "Unexpected number of iterations";
      All_Keys_Message   : constant String := "Did not iterate over all expected keys";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Value  : constant JSON_Value'Class := Parsers.Parse (Stream);
   begin
      Assert (Value in JSON_Object_Value, "Not JSON_Object_Value");
      Assert (Value.Length = 2, "Expected length of object to be 2");

      declare
         Object_Value : constant JSON_Object_Value'Class := JSON_Object_Value (Value);
         Iterations  : Natural := 0;
         Retrieved_Foo : Boolean := False;
         Retrieved_Bar : Boolean := False;
      begin
         for Key of Object_Value loop
            Iterations := Iterations + 1;
            if Iterations in 1 .. 2 then
               Assert (Key in String, "Not String");
               Assert (Key in "foo" | "bar", "Expected string value to be equal to 'foo' or 'bar'");

               Retrieved_Foo := Retrieved_Foo or Key = "foo";
               Retrieved_Bar := Retrieved_Bar or Key = "bar";
            end if;
         end loop;
         Assert (Iterations = Object_Value.Length, Iterations_Message);
         Assert (Retrieved_Foo and Retrieved_Bar, All_Keys_Message);
      end;
   end Test_Object_Iterable;

   procedure Test_Array_Object_Array is
      Text : aliased String := "[{""foo"":[true, 42]}]";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Value  : constant JSON_Value'Class := Parsers.Parse (Stream);
   begin
      Assert (Value in JSON_Array_Value, "Not JSON_Array_Value");
      Assert (Value.Length = 1, "Expected length of array to be 1");

      begin
         declare
            Object : constant JSON_Value'Class := JSON_Array_Value (Value).Get (1);
         begin
            Assert (Object.Length = 1, "Expected length of object to be 1");
            declare
               Array_Value : constant JSON_Value'Class := Object.Get ("foo");
            begin
               Assert (Array_Value.Length = 2, "Expected length of array 'foo' to be 2");
               Assert (Array_Value.Get (2).Value = 42, "Expected integer value at index 2 to be 42");
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

   procedure Test_Object_Array_Object is
      Text : aliased String := "{""foo"":[null, {""bar"": 42}]}";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Value  : constant JSON_Value'Class := Parsers.Parse (Stream);
   begin
      Assert (Value in JSON_Object_Value, "Not JSON_Object_Value");
      Assert (Value.Length = 1, "Expected length of object to be 1");

      begin
         declare
            Array_Value : constant JSON_Value'Class := JSON_Object_Value (Value).Get ("foo");
         begin
            Assert (Array_Value.Length = 2, "Expected length of array 'foo' to be 2");
            declare
               Object : constant JSON_Value'Class := Array_Value.Get (2);
            begin
               Assert (Object.Length = 1, "Expected length of object to be 1");
               declare
                  Integer_Value : constant JSON_Value'Class := Object.Get ("bar");
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

   procedure Test_Object_No_Array is
      Text : aliased String := "{}";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Value  : constant JSON_Value'Class := Parsers.Parse (Stream);
   begin
      begin
         declare
            Object : JSON_Array_Value := Value.Get_Array ("foo");
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
            Object : constant JSON_Array_Value := Value.Get_Array_Or_Empty ("foo");
         begin
            Assert (Object.Length = 0, "Expected empty JSON_Array_Value");
         end;
      exception
         when Constraint_Error =>
            Fail ("Unexpected Constraint_Error");
      end;
   end Test_Object_No_Array;

   procedure Test_Object_No_Object is
      Text : aliased String := "{}";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Value  : constant JSON_Value'Class := Parsers.Parse (Stream);
   begin
      begin
         declare
            Object : JSON_Object_Value := Value.Get_Object ("foo");
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
            Object : constant JSON_Object_Value := Value.Get_Object_Or_Empty ("foo");
         begin
            Assert (Object.Length = 0, "Expected empty JSON_Object_Value");
         end;
      exception
         when Constraint_Error =>
            Fail ("Unexpected Constraint_Error");
      end;
   end Test_Object_No_Object;

   procedure Test_Empty_Text_Exception is
      Text : aliased String := "";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
   begin
      declare
         Value  : JSON_Value'Class := Parsers.Parse (Stream);
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Parse_Error");
      end;
   exception
      when Parsers.Parse_Error =>
         null;
   end Test_Empty_Text_Exception;

   procedure Test_Array_No_Value_Separator_Exception is
      Text : aliased String := "[3.14""test""]";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
   begin
      declare
         Value  : JSON_Value'Class := Parsers.Parse (Stream);
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Parse_Error");
      end;
   exception
      when Parsers.Parse_Error =>
         null;
   end Test_Array_No_Value_Separator_Exception;

   procedure Test_Array_No_End_Array_Exception is
      Text : aliased String := "[true";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
   begin
      declare
         Value  : JSON_Value'Class := Parsers.Parse (Stream);
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Parse_Error");
      end;
   exception
      when Parsers.Parse_Error =>
         null;
   end Test_Array_No_End_Array_Exception;

   procedure Test_No_EOF_After_Array_Exception is
      Text : aliased String := "[1]2";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
   begin
      declare
         Value  : JSON_Value'Class := Parsers.Parse (Stream);
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Parse_Error");
      end;
   exception
      when Parsers.Parse_Error =>
         null;
   end Test_No_EOF_After_Array_Exception;

   procedure Test_Object_No_Value_Separator_Exception is
      Text : aliased String := "{""foo"":1""bar"":2}";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
   begin
      declare
         Value  : JSON_Value'Class := Parsers.Parse (Stream);
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Parse_Error");
      end;
   exception
      when Parsers.Parse_Error =>
         null;
   end Test_Object_No_Value_Separator_Exception;

   procedure Test_Object_No_Name_Separator_Exception is
      Text : aliased String := "{""foo"",true}";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
   begin
      declare
         Value  : JSON_Value'Class := Parsers.Parse (Stream);
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Parse_Error");
      end;
   exception
      when Parsers.Parse_Error =>
         null;
   end Test_Object_No_Name_Separator_Exception;

   procedure Test_Object_Key_No_String_Exception is
      Text : aliased String := "{42:true}";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
   begin
      declare
         Value  : JSON_Value'Class := Parsers.Parse (Stream);
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Parse_Error");
      end;
   exception
      when Parsers.Parse_Error =>
         null;
   end Test_Object_Key_No_String_Exception;

   procedure Test_Object_No_Second_Member_Exception is
      Text : aliased String := "{""foo"":true,}";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
   begin
      declare
         Value  : JSON_Value'Class := Parsers.Parse (Stream);
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Parse_Error");
      end;
   exception
      when Parsers.Parse_Error =>
         null;
   end Test_Object_No_Second_Member_Exception;

   procedure Test_Object_Duplicate_Keys_Exception is
      Text : aliased String := "{""foo"":1,""foo"":2}";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
   begin
      declare
         Value  : JSON_Value'Class := Parsers.Parse (Stream);
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Constraint_Error");
      end;
   exception
      when Constraint_Error =>
         null;
   end Test_Object_Duplicate_Keys_Exception;

   procedure Test_Object_No_Value_Exception is
      Text : aliased String := "{""foo"":}";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
   begin
      declare
         Value  : JSON_Value'Class := Parsers.Parse (Stream);
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Parse_Error");
      end;
   exception
      when Parsers.Parse_Error =>
         null;
   end Test_Object_No_Value_Exception;

   procedure Test_Object_No_End_Object_Exception is
      Text : aliased String := "{""foo"":true";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
   begin
      declare
         Value  : JSON_Value'Class := Parsers.Parse (Stream);
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Parse_Error");
      end;
   exception
      when Parsers.Parse_Error =>
         null;
   end Test_Object_No_End_Object_Exception;

   procedure Test_No_EOF_After_Object_Exception is
      Text : aliased String := "{""foo"":true}[true]";

      Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
   begin
      declare
         Value  : JSON_Value'Class := Parsers.Parse (Stream);
         pragma Unreferenced (Value);
      begin
         Fail ("Expected Parse_Error");
      end;
   exception
      when Parsers.Parse_Error =>
         null;
   end Test_No_EOF_After_Object_Exception;

end Test_Parsers;
