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

with Ada.Characters.Latin_1;

with AUnit.Assertions;
with AUnit.Test_Caller;

with JSON.Streams;
with JSON.Tokenizers;
with JSON.Types;

package body Test_Tokenizers is

   package Types is new JSON.Types (Long_Integer, Long_Float);
   package Tokenizers is new JSON.Tokenizers (Types);
   package Streams renames JSON.Streams;

   use type JSON.Streams.AS.Stream_Element_Offset;

   String_Offset_Message : constant String := "String value at wrong offset";
   String_Length_Message : constant String := "String value has incorrect length";

   use AUnit.Assertions;

   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(Tokenizers) ";
   begin
      Test_Suite.Add_Test (Caller.Create
        (Name & "Tokenize text 'null'", Test_Null_Token'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Tokenize text 'true'", Test_True_Token'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Tokenize text 'false'", Test_False_Token'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Tokenize text '""""'", Test_Empty_String_Token'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Tokenize text '""test""'", Test_Non_Empty_String_Token'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Tokenize text '""12.34""'", Test_Number_String_Token'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Tokenize text '""horizontal\ttab""'",
         Test_Escaped_Character_String_Token'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Tokenize text '""foo\""\\bar""'",
         Test_Escaped_Quotation_Solidus_String_Token'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Tokenize text '0'", Test_Zero_Number_Token'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Tokenize text '42'", Test_Integer_Number_Token'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Tokenize text '3.14'", Test_Float_Number_Token'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Tokenize text '-2.71'", Test_Negative_Float_Number_Token'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Tokenize text '4e2'", Test_Integer_Exponent_Number_Token'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Tokenize text '0.314e1'", Test_Float_Exponent_Number_Token'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Tokenize text '4e-1'", Test_Float_Negative_Exponent_Number_Token'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Tokenize text '[]'", Test_Empty_Array_Tokens'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Tokenize text '[null]'", Test_One_Element_Array_Tokens'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Tokenize text '[1,2]'", Test_Two_Elements_Array_Tokens'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Tokenize text '{}'", Test_Empty_Object_Tokens'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Tokenize text '{""foo"":""bar""}'", Test_One_Pair_Object_Tokens'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Tokenize text '{""foo"": true,""bar"":false}'",
         Test_Two_Pairs_Object_Tokens'Access));

      --  Exceptions
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '""no\nnewline""'",
         Test_Control_Character_String_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '""unexpected\xcharacter""'",
         Test_Unexpected_Escaped_Character_String_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '-'", Test_Minus_Number_EOF_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '-,'", Test_Minus_Number_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '3.'", Test_End_Dot_Number_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '1E'", Test_End_Exponent_Number_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '1.E'", Test_End_Dot_Exponent_Number_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '1E-'", Test_End_Exponent_Minus_Number_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '1E,'", Test_End_Exponent_One_Digit_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '1E-,'", Test_End_Exponent_Minus_One_Digit_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '+42'", Test_Prefixed_Plus_Number_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '-02'", Test_Leading_Zeroes_Integer_Number_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text '-003.14'", Test_Leading_Zeroes_Float_Number_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text 'tr'", Test_Incomplete_True_Text_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text 'f'", Test_Incomplete_False_Text_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text 'nul'", Test_Incomplete_Null_Text_Exception'Access));
      Test_Suite.Add_Test (Caller.Create
        (Name & "Reject text 'unexpected'", Test_Unknown_Keyword_Text_Exception'Access));

      return Test_Suite'Access;
   end Suite;

   use type Tokenizers.Token_Kind;

   procedure Assert_Kind (Left, Right : Tokenizers.Token_Kind; Message : String) is
   begin
      Assert (Left = Right, Message & " (Expected " & Right'Image & "; Got " & Left'Image & ")");
   end Assert_Kind;

   procedure Fail (Message : String) is
   begin
      Assert (False, Message);
   end Fail;

   procedure Expect_EOF (Stream : aliased in out Streams.Stream) is
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token, Expect_EOF => True);
   exception
      when Tokenizers.Tokenizer_Error =>
         Fail ("Expected EOF");
   end Expect_EOF;

   --  Keyword
   procedure Test_Null_Token (Object : in out Test) is
      Text : constant String := "null";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Null_Token, "Not Null_Token");
      Expect_EOF (Stream);
   end Test_Null_Token;

   procedure Test_True_Token (Object : in out Test) is
      Text : constant String := "true";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Boolean_Token, "Not Boolean_Token");
      Assert (Token.Boolean_Value, "Boolean value not True");
      Expect_EOF (Stream);
   end Test_True_Token;

   procedure Test_False_Token (Object : in out Test) is
      Text : constant String := "false";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Boolean_Token, "Not Boolean_Token");
      Assert (not Token.Boolean_Value, "Boolean value not False");
      Expect_EOF (Stream);
   end Test_False_Token;

   --  String
   procedure Test_Empty_String_Token (Object : in out Test) is
      Text : constant String := """""";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.String_Token, "Not String_Token");
      Assert (Token.String_Offset = 2, String_Offset_Message);
      Assert (Token.String_Length = 0, String_Length_Message);
      Expect_EOF (Stream);
   end Test_Empty_String_Token;

   procedure Test_Non_Empty_String_Token (Object : in out Test) is
      Text : constant String := """test""";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.String_Token, "Not String_Token");
      Assert (Token.String_Offset = 2, String_Offset_Message);
      Assert (Token.String_Length = 4, String_Length_Message);
      Expect_EOF (Stream);
   end Test_Non_Empty_String_Token;

   procedure Test_Number_String_Token (Object : in out Test) is
      Text : constant String := """12.34""";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.String_Token, "Not String_Token");
      Assert (Token.String_Offset = 2, String_Offset_Message);
      Assert (Token.String_Length = 5, String_Length_Message);
      Expect_EOF (Stream);
   end Test_Number_String_Token;

   procedure Test_Escaped_Character_String_Token (Object : in out Test) is
      Text : constant String := """horizontal\ttab""";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.String_Token, "Not String_Token");
      Assert (Token.String_Offset = 2, String_Offset_Message);
      Assert (Token.String_Length = 15, String_Length_Message);
      Expect_EOF (Stream);
   end Test_Escaped_Character_String_Token;

   procedure Test_Escaped_Quotation_Solidus_String_Token (Object : in out Test) is
      Text : constant String := """foo\""\\bar""";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.String_Token, "Not String_Token");
      Assert (Token.String_Offset = 2, String_Offset_Message);
      Assert (Token.String_Length = 10, String_Length_Message);
      Expect_EOF (Stream);
   end Test_Escaped_Quotation_Solidus_String_Token;

   --  Integer/Float number
   procedure Test_Zero_Number_Token (Object : in out Test) is
      Text : constant String := "0";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Integer_Token, "Not Integer_Token");
      Assert (Token.Integer_Value = 0, "Integer value not equal to 0");
      Expect_EOF (Stream);
   end Test_Zero_Number_Token;

   procedure Test_Integer_Number_Token (Object : in out Test) is
      Text : constant String := "42";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Integer_Token, "Not Integer_Token");
      Assert (Token.Integer_Value = 42, "Integer value not equal to 42");
      Expect_EOF (Stream);
   end Test_Integer_Number_Token;

   procedure Test_Float_Number_Token (Object : in out Test) is
      Text : constant String := "3.14";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Float_Token, "Not Float_Token");
      Assert (Token.Float_Value = 3.14, "Float value not equal to 3.14");
      Expect_EOF (Stream);
   end Test_Float_Number_Token;

   procedure Test_Negative_Float_Number_Token (Object : in out Test) is
      Text : constant String := "-2.71";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Float_Token, "Not Float_Token");
      Assert (Token.Float_Value = -2.71, "Float value not equal to -2.71");
      Expect_EOF (Stream);
   end Test_Negative_Float_Number_Token;

   procedure Test_Integer_Exponent_Number_Token (Object : in out Test) is
      Text : constant String := "4e2";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Integer_Token, "Not Integer_Token");
      Assert (Token.Integer_Value = 400, "Integer value not equal to 400");
      Expect_EOF (Stream);
   end Test_Integer_Exponent_Number_Token;

   procedure Test_Float_Exponent_Number_Token (Object : in out Test) is
      Text : constant String := "0.314e1";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Float_Token, "Not Float_Token");
      Assert (Token.Float_Value = 3.14, "Float value not equal to 3.14");
      Expect_EOF (Stream);
   end Test_Float_Exponent_Number_Token;

   procedure Test_Float_Negative_Exponent_Number_Token (Object : in out Test) is
      Text : constant String := "4e-1";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Float_Token, "Not Float_Token");
      Assert (Token.Float_Value = 0.4, "Float value not equal to 0.4");
      Expect_EOF (Stream);
   end Test_Float_Negative_Exponent_Number_Token;

   --  Array
   procedure Test_Empty_Array_Tokens (Object : in out Test) is
      Text : constant String := "[]";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Begin_Array_Token, "Not Begin_Array_Token");
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.End_Array_Token, "Not End_Array_Token");
      Expect_EOF (Stream);
   end Test_Empty_Array_Tokens;

   procedure Test_One_Element_Array_Tokens (Object : in out Test) is
      Text : constant String := "[null]";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Begin_Array_Token, "Not Begin_Array_Token");

      --  null
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Null_Token, "Not Null_Token");

      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.End_Array_Token, "Not End_Array_Token");
      Expect_EOF (Stream);
   end Test_One_Element_Array_Tokens;

   procedure Test_Two_Elements_Array_Tokens (Object : in out Test) is
      Text : constant String := "[1,2]";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Begin_Array_Token, "Not Begin_Array_Token");

      --  1
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Integer_Token, "Not Integer_Token");
      Assert (Token.Integer_Value = 1, "Integer value not equal to 1");

      --  ,
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Value_Separator_Token, "Not Value_Separator_Token");

      --  2
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Integer_Token, "Not Integer_Token");
      Assert (Token.Integer_Value = 2, "Integer value not equal to 2");

      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.End_Array_Token, "Not End_Array_Token");
      Expect_EOF (Stream);
   end Test_Two_Elements_Array_Tokens;

   --  Object
   procedure Test_Empty_Object_Tokens (Object : in out Test) is
      Text : constant String := "{}";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Begin_Object_Token, "Not Begin_Object_Token");
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.End_Object_Token, "Not End_Object_Token");
      Expect_EOF (Stream);
   end Test_Empty_Object_Tokens;

   procedure Test_One_Pair_Object_Tokens (Object : in out Test) is
      Text : constant String := "{""foo"":""bar""}";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Begin_Object_Token, "Not Begin_Object_Token");

      --  "foo"
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.String_Token, "Not String_Token");
      Assert (Token.String_Offset = 3, String_Offset_Message);
      Assert (Token.String_Length = 3, String_Length_Message);

      --  :
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Name_Separator_Token, "Not Name_Separator_Token");

      --  "bar"
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.String_Token, "Not String_Token");
      Assert (Token.String_Offset = 9, String_Offset_Message);
      Assert (Token.String_Length = 3, String_Length_Message);

      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.End_Object_Token, "Not End_Object_Token");
      Expect_EOF (Stream);
   end Test_One_Pair_Object_Tokens;

   procedure Test_Two_Pairs_Object_Tokens (Object : in out Test) is
      Text : constant String := "{""foo"":true,""bar"":false}";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Begin_Object_Token, "Not Begin_Object_Token");

      --  "foo"
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.String_Token, "Not String_Token");
      Assert (Token.String_Offset = 3, String_Offset_Message);
      Assert (Token.String_Length = 3, String_Length_Message);

      --  :
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Name_Separator_Token, "Not Name_Separator_Token");

      --  true
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Boolean_Token, "Not Boolean_Token");
      Assert (Token.Boolean_Value, "Boolean value not True");

      --  ,
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Value_Separator_Token, "Not Value_Separator_Token");

      --  "bar"
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.String_Token, "Not String_Token");
      Assert (Token.String_Offset = 14, String_Offset_Message);
      Assert (Token.String_Length = 3, String_Length_Message);

      --  :
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Name_Separator_Token, "Not Name_Separator_Token");

      --  false
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Boolean_Token, "Not Boolean_Token");
      Assert (not Token.Boolean_Value, "Boolean value not False");

      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.End_Object_Token, "Not End_Object_Token");
      Expect_EOF (Stream);
   end Test_Two_Pairs_Object_Tokens;

   --  Exceptions
   procedure Test_Control_Character_String_Exception (Object : in out Test) is
      LF : Character renames Ada.Characters.Latin_1.LF;
      Text : constant String := """no" & LF & "newline""";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_Control_Character_String_Exception;

   procedure Test_Unexpected_Escaped_Character_String_Exception (Object : in out Test) is
      Text : constant String := """unexpected\xcharacter""";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_Unexpected_Escaped_Character_String_Exception;

   procedure Test_Minus_Number_EOF_Exception (Object : in out Test) is
      Text : constant String := "-";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_Minus_Number_EOF_Exception;

   procedure Test_Minus_Number_Exception (Object : in out Test) is
      Text : constant String := "-,";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_Minus_Number_Exception;

   procedure Test_End_Dot_Number_Exception (Object : in out Test) is
      Text : constant String := "3.";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_End_Dot_Number_Exception;

   procedure Test_End_Exponent_Number_Exception (Object : in out Test) is
      Text : constant String := "1E";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_End_Exponent_Number_Exception;

   procedure Test_End_Dot_Exponent_Number_Exception (Object : in out Test) is
      Text : constant String := "1.E";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_End_Dot_Exponent_Number_Exception;

   procedure Test_End_Exponent_Minus_Number_Exception (Object : in out Test) is
      Text : constant String := "1E-";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_End_Exponent_Minus_Number_Exception;

   procedure Test_End_Exponent_One_Digit_Exception (Object : in out Test) is
      Text : constant String := "1E,";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_End_Exponent_One_Digit_Exception;

   procedure Test_End_Exponent_Minus_One_Digit_Exception (Object : in out Test) is
      Text : constant String := "1E-,";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_End_Exponent_Minus_One_Digit_Exception;

   procedure Test_Prefixed_Plus_Number_Exception (Object : in out Test) is
      Text : constant String := "+42";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_Prefixed_Plus_Number_Exception;

   procedure Test_Leading_Zeroes_Integer_Number_Exception (Object : in out Test) is
      Text : constant String := "-02";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_Leading_Zeroes_Integer_Number_Exception;

   procedure Test_Leading_Zeroes_Float_Number_Exception (Object : in out Test) is
      Text : constant String := "-003.14";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_Leading_Zeroes_Float_Number_Exception;

   procedure Test_Incomplete_True_Text_Exception (Object : in out Test) is
      Text : constant String := "tr";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_Incomplete_True_Text_Exception;

   procedure Test_Incomplete_False_Text_Exception (Object : in out Test) is
      Text : constant String := "f";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_Incomplete_False_Text_Exception;

   procedure Test_Incomplete_Null_Text_Exception (Object : in out Test) is
      Text : constant String := "nul";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_Incomplete_Null_Text_Exception;

   procedure Test_Unknown_Keyword_Text_Exception (Object : in out Test) is
      Text : constant String := "unexpected";
      Stream : aliased Streams.Stream := Streams.Create_Stream (Streams.From_Text (Text));
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_Unknown_Keyword_Text_Exception;

end Test_Tokenizers;
