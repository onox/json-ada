--
--  Copyright (c) 2016 onox <denkpadje@gmail.com>
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

with Ahven; use Ahven;

with JSON.Streams;
with JSON.Tokenizers;
with JSON.Types;

package body Test_Tokenizers is

   package Types is new JSON.Types (Long_Integer, Long_Float);
   package Tokenizers is new JSON.Tokenizers (Types);

   use type JSON.Streams.AS.Stream_Element_Offset;

   String_Offset_Message : constant String := "String value at wrong offset";
   String_Length_Message : constant String := "String value has incorrect length";

   overriding
   procedure Initialize (T : in out Test) is
   begin
      T.Set_Name ("Tokenizers");

      T.Add_Test_Routine (Test_Null_Token'Access, "Tokenize text 'null'");
      T.Add_Test_Routine (Test_True_Token'Access, "Tokenize text 'true'");
      T.Add_Test_Routine (Test_False_Token'Access, "Tokenize text 'false'");

      T.Add_Test_Routine (Test_Empty_String_Token'Access, "Tokenize text '""""'");
      T.Add_Test_Routine (Test_Non_Empty_String_Token'Access, "Tokenize text '""test""'");
      T.Add_Test_Routine (Test_Number_String_Token'Access, "Tokenize text '""12.34""'");
      T.Add_Test_Routine (Test_Escaped_Character_String_Token'Access, "Tokenize text '""horizontal\ttab""'");
      T.Add_Test_Routine (Test_Escaped_Quotation_Solidus_String_Token'Access, "Tokenize text '""foo\""\\bar""'");

      T.Add_Test_Routine (Test_Zero_Number_Token'Access, "Tokenize text '0'");
      T.Add_Test_Routine (Test_Integer_Number_Token'Access, "Tokenize text '42'");
      T.Add_Test_Routine (Test_Float_Number_Token'Access, "Tokenize text '3.14'");
      T.Add_Test_Routine (Test_Negative_Float_Number_Token'Access, "Tokenize text '-2.71'");
      T.Add_Test_Routine (Test_Integer_Exponent_Number_Token'Access, "Tokenize text '4e2'");
      T.Add_Test_Routine (Test_Float_Exponent_Number_Token'Access, "Tokenize text '0.314e1'");
      T.Add_Test_Routine (Test_Float_Negative_Exponent_Number_Token'Access,  "Tokenize text '4e-1'");

      T.Add_Test_Routine (Test_Empty_Array_Tokens'Access, "Tokenize text '[]'");
      T.Add_Test_Routine (Test_One_Element_Array_Tokens'Access, "Tokenize text '[null]'");
      T.Add_Test_Routine (Test_Two_Elements_Array_Tokens'Access, "Tokenize text '[1,2]'");

      T.Add_Test_Routine (Test_Empty_Object_Tokens'Access, "Tokenize text '{}'");
      T.Add_Test_Routine (Test_One_Pair_Object_Tokens'Access, "Tokenize text '{""foo"":""bar""}'");
      T.Add_Test_Routine (Test_Two_Pairs_Object_Tokens'Access, "Tokenize text '{""foo"": true,""bar"":false}'");

      --  Exceptions
      T.Add_Test_Routine (Test_Control_Character_String_Exception'Access, "Reject text '""no\nnewline""'");
      T.Add_Test_Routine (Test_Unexpected_Escaped_Character_String_Exception'Access, "Reject text '""unexpected\xcharacter""'");
      T.Add_Test_Routine (Test_Minus_Number_EOF_Exception'Access, "Reject text '-'");
      T.Add_Test_Routine (Test_Minus_Number_Exception'Access, "Reject text '-,'");
      T.Add_Test_Routine (Test_End_Dot_Number_Exception'Access, "Reject text '3.'");
      T.Add_Test_Routine (Test_End_Exponent_Number_Exception'Access, "Reject text '1E'");
      T.Add_Test_Routine (Test_End_Dot_Exponent_Number_Exception'Access, "Reject text '1.E'");
      T.Add_Test_Routine (Test_End_Exponent_Minus_Number_Exception'Access, "Reject text '1E-'");
      T.Add_Test_Routine (Test_Prefixed_Plus_Number_Exception'Access, "Reject text '+42'");
      T.Add_Test_Routine (Test_Leading_Zeroes_Integer_Number_Exception'Access, "Reject text '-02'");
      T.Add_Test_Routine (Test_Leading_Zeroes_Float_Number_Exception'Access, "Reject text '-003.14'");
      T.Add_Test_Routine (Test_Incomplete_True_Text_Exception'Access, "Reject text 'tr'");
      T.Add_Test_Routine (Test_Incomplete_False_Text_Exception'Access, "Reject text 'f'");
      T.Add_Test_Routine (Test_Incomplete_Null_Text_Exception'Access, "Reject text 'nul'");
      T.Add_Test_Routine (Test_Unknown_Keyword_Text_Exception'Access, "Reject text 'unexpected'");

   end Initialize;

   use type Tokenizers.Token_Kind;
   use type Types.SU.Unbounded_String;

   procedure Assert_Kind is new Assert_Equal
     (Tokenizers.Token_Kind, Tokenizers.Token_Kind'Image);

   procedure Expect_EOF (Stream : aliased in out JSON.Streams.Stream'Class) is
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token, Expect_EOF => True);
   exception
      when Tokenizers.Tokenizer_Error =>
         Fail ("Expected EOF");
   end Expect_EOF;

   --  Keyword
   procedure Test_Null_Token is
      Text : aliased String := "null";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Null_Token, "Not Null_Token");
      Expect_EOF (Stream);
   end Test_Null_Token;

   procedure Test_True_Token is
      Text : aliased String := "true";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Boolean_Token, "Not Boolean_Token");
      Assert (Token.Boolean_Value, "Boolean value not True");
      Expect_EOF (Stream);
   end Test_True_Token;

   procedure Test_False_Token is
      Text : aliased String := "false";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Boolean_Token, "Not Boolean_Token");
      Assert (not Token.Boolean_Value, "Boolean value not False");
      Expect_EOF (Stream);
   end Test_False_Token;

   --  String
   procedure Test_Empty_String_Token is
      Text : aliased String := """""";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.String_Token, "Not String_Token");
      Assert (Token.String_Offset = 2, String_Offset_Message);
      Assert (Token.String_Length = 0, String_Length_Message);
      Expect_EOF (Stream);
   end Test_Empty_String_Token;

   procedure Test_Non_Empty_String_Token is
      Text : aliased String := """test""";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.String_Token, "Not String_Token");
      Assert (Token.String_Offset = 2, String_Offset_Message);
      Assert (Token.String_Length = 4, String_Length_Message);
      Expect_EOF (Stream);
   end Test_Non_Empty_String_Token;

   procedure Test_Number_String_Token is
      Text : aliased String := """12.34""";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.String_Token, "Not String_Token");
      Assert (Token.String_Offset = 2, String_Offset_Message);
      Assert (Token.String_Length = 5, String_Length_Message);
      Expect_EOF (Stream);
   end Test_Number_String_Token;

   procedure Test_Escaped_Character_String_Token is
      Text : aliased String := """horizontal\ttab""";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.String_Token, "Not String_Token");
      Assert (Token.String_Offset = 2, String_Offset_Message);
      Assert (Token.String_Length = 15, String_Length_Message);
      Expect_EOF (Stream);
   end Test_Escaped_Character_String_Token;

   procedure Test_Escaped_Quotation_Solidus_String_Token is
      Text : aliased String := """foo\""\\bar""";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.String_Token, "Not String_Token");
      Assert (Token.String_Offset = 2, String_Offset_Message);
      Assert (Token.String_Length = 10, String_Length_Message);
      Expect_EOF (Stream);
   end Test_Escaped_Quotation_Solidus_String_Token;

   --  Integer/Float number
   procedure Test_Zero_Number_Token is
      Text : aliased String := "0";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Integer_Token, "Not Integer_Token");
      Assert (Token.Integer_Value = 0, "Integer value not equal to 0");
      Expect_EOF (Stream);
   end Test_Zero_Number_Token;

   procedure Test_Integer_Number_Token is
      Text : aliased String := "42";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Integer_Token, "Not Integer_Token");
      Assert (Token.Integer_Value = 42, "Integer value not equal to 42");
      Expect_EOF (Stream);
   end Test_Integer_Number_Token;

   procedure Test_Float_Number_Token is
      Text : aliased String := "3.14";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Float_Token, "Not Float_Token");
      Assert (Token.Float_Value = 3.14, "Float value not equal to 3.14");
      Expect_EOF (Stream);
   end Test_Float_Number_Token;

   procedure Test_Negative_Float_Number_Token is
      Text : aliased String := "-2.71";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Float_Token, "Not Float_Token");
      Assert (Token.Float_Value = -2.71, "Float value not equal to -2.71");
      Expect_EOF (Stream);
   end Test_Negative_Float_Number_Token;

   procedure Test_Integer_Exponent_Number_Token is
      Text : aliased String := "4e2";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Integer_Token, "Not Integer_Token");
      Assert (Token.Integer_Value = 400, "Integer value not equal to 400");
      Expect_EOF (Stream);
   end Test_Integer_Exponent_Number_Token;

   procedure Test_Float_Exponent_Number_Token is
      Text : aliased String := "0.314e1";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Float_Token, "Not Float_Token");
      Assert (Token.Float_Value = 3.14, "Float value not equal to 3.14");
      Expect_EOF (Stream);
   end Test_Float_Exponent_Number_Token;

   procedure Test_Float_Negative_Exponent_Number_Token is
      Text : aliased String := "4e-1";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Float_Token, "Not Float_Token");
      Assert (Token.Float_Value = 0.4, "Float value not equal to 0.4");
      Expect_EOF (Stream);
   end Test_Float_Negative_Exponent_Number_Token;

   --  Array
   procedure Test_Empty_Array_Tokens is
      Text : aliased String := "[]";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Begin_Array_Token, "Not Begin_Array_Token");
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.End_Array_Token, "Not End_Array_Token");
      Expect_EOF (Stream);
   end Test_Empty_Array_Tokens;

   procedure Test_One_Element_Array_Tokens is
      Text : aliased String := "[null]";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
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

   procedure Test_Two_Elements_Array_Tokens is
      Text : aliased String := "[1,2]";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
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
   procedure Test_Empty_Object_Tokens is
      Text : aliased String := "{}";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.Begin_Object_Token, "Not Begin_Object_Token");
      Tokenizers.Read_Token (Stream, Token);
      Assert_Kind (Token.Kind, Tokenizers.End_Object_Token, "Not End_Object_Token");
      Expect_EOF (Stream);
   end Test_Empty_Object_Tokens;

   procedure Test_One_Pair_Object_Tokens is
      Text : aliased String := "{""foo"":""bar""}";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
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

   procedure Test_Two_Pairs_Object_Tokens is
      Text : aliased String := "{""foo"":true,""bar"":false}";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
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
   procedure Test_Control_Character_String_Exception is
      LF : Character renames Ada.Characters.Latin_1.LF;
      Text : aliased String := """no" & LF & "newline""";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_Control_Character_String_Exception;

   procedure Test_Unexpected_Escaped_Character_String_Exception is
      Text : aliased String := """unexpected\xcharacter""";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_Unexpected_Escaped_Character_String_Exception;

   procedure Test_Minus_Number_EOF_Exception is
      Text : aliased String := "-";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_Minus_Number_EOF_Exception;

   procedure Test_Minus_Number_Exception is
      Text : aliased String := "-,";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_Minus_Number_Exception;

   procedure Test_End_Dot_Number_Exception is
      Text : aliased String := "3.";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_End_Dot_Number_Exception;

   procedure Test_End_Exponent_Number_Exception is
      Text : aliased String := "1E";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_End_Exponent_Number_Exception;

   procedure Test_End_Dot_Exponent_Number_Exception is
      Text : aliased String := "1.E";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_End_Dot_Exponent_Number_Exception;

   procedure Test_End_Exponent_Minus_Number_Exception is
      Text : aliased String := "1E-";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_End_Exponent_Minus_Number_Exception;

   procedure Test_Prefixed_Plus_Number_Exception is
      Text : aliased String := "+42";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_Prefixed_Plus_Number_Exception;

   procedure Test_Leading_Zeroes_Integer_Number_Exception is
      Text : aliased String := "-02";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_Leading_Zeroes_Integer_Number_Exception;

   procedure Test_Leading_Zeroes_Float_Number_Exception is
      Text : aliased String := "-003.14";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_Leading_Zeroes_Float_Number_Exception;

   procedure Test_Incomplete_True_Text_Exception is
      Text : aliased String := "tr";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_Incomplete_True_Text_Exception;

   procedure Test_Incomplete_False_Text_Exception is
      Text : aliased String := "f";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_Incomplete_False_Text_Exception;

   procedure Test_Incomplete_Null_Text_Exception is
      Text : aliased String := "nul";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_Incomplete_Null_Text_Exception;

   procedure Test_Unknown_Keyword_Text_Exception is
      Text : aliased String := "unexpected";
      Stream : aliased JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      Fail ("Expected Tokenizer_Error");
   exception
      when Tokenizers.Tokenizer_Error =>
         null;
   end Test_Unknown_Keyword_Text_Exception;

end Test_Tokenizers;
