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

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.IO_Exceptions;

package body JSON.Tokenizers is

   procedure Read_String (Stream     : in out Streams.Stream'Class;
                          Next_Token : out Token) is
      C : Character;
      Value   : SU.Unbounded_String;
      Escaped : Boolean := False;

      use Ada.Characters.Latin_1;
   begin
      loop
         C := Stream.Read_Character;

         --  An unescaped '"' character denotes the end of the string
         exit when not Escaped and C = '"';

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
               when 'u' =>
                  --  TODO Support escaped unicode
                  raise Program_Error with "Escaped unicode not supported yet";
               when others =>
                  raise Tokenizer_Error with "Unexpected escaped character in string";
            end case;
         elsif C /= '\' then
            --  Check C is not a control character
            if Ada.Characters.Handling.Is_Control (C) then
               raise Tokenizer_Error with "Unexpected control character in string";
            end if;
            SU.Append (Value, C);
         end if;
         Escaped := not Escaped and C = '\';
      end loop;
      Next_Token := Token'(Kind => String_Token, String_Value => Value);
   end Read_String;

   procedure Test_Leading_Zeroes (First : Character; Value : SU.Unbounded_String) is
      Leading_Zero_Message : constant String := "Leading zeroes in number are not allowed";
      Minus_Digit_Message  : constant String := "Expected at least one digit after - sign";
   begin
      if First = '-' then
         if SU.Length (Value) >= 3 and then SU.Slice (Value, 1, 2) = "-0" then
            raise Tokenizer_Error with Leading_Zero_Message;
         elsif SU.Length (Value) = 1 then
            raise Tokenizer_Error with Minus_Digit_Message;
         end if;
      elsif First = '0' and SU.Length (Value) >= 2 then
         raise Tokenizer_Error with Leading_Zero_Message;
      end if;
   end Test_Leading_Zeroes;

   procedure Read_Number (Stream     : in out Streams.Stream'Class;
                          First      : Character;
                          Next_Token : out Token) is
      C : Character;
      Value : SU.Unbounded_String;
      Is_Float, Checked_Leading_Zeroes : Boolean := False;
      Error_Dot_Message : constant String := "Number must contain at least one digit after decimal point";
      Error_Exp_Message : constant String := "Expected optional +/- sign after e/E and then at least one digit";
   begin
      if First = '+' then
         raise Tokenizer_Error with "Prefixing number with '+' character is not allowed";
      end if;
      SU.Append (Value, First);

      --  Accept sequence of digits, including leading zeroes
      loop
         C := Stream.Read_Character;
         exit when C not in '0' .. '9';
         SU.Append (Value, C);
      end loop;

      --  Test whether value contains leading zeroes
      Test_Leading_Zeroes (First, Value);
      Checked_Leading_Zeroes := True;

      --  Tokenize fraction part
      if C = '.' then
         Is_Float := True;

         --  Append the dot
         SU.Append (Value, C);

         -- Require at least one digit after decimal point
         begin
            C := Stream.Read_Character;
            if C not in '0' .. '9' then
               raise Tokenizer_Error with Error_Dot_Message;
            end if;
            SU.Append (Value, C);
         exception
            when Ada.IO_Exceptions.End_Error =>
               raise Tokenizer_Error with Error_Dot_Message;
         end;

         --  Accept sequence of digits
         loop
            C := Stream.Read_Character;
            exit when C not in '0' .. '9';
            SU.Append (Value, C);
         end loop;
      end if;

      --  Tokenize exponent part
      if C in 'e' | 'E' then
         --  Append the 'e' or 'E' character
         SU.Append (Value, C);

         begin
            C := Stream.Read_Character;
            --  Append optional '+' or '-' character
            if C in '+' | '-' then
               --  If exponent is negative, number will be a float
               if C = '-' then
                  Is_Float := True;
               end if;

               SU.Append (Value, C);

               --  Require at least one digit after +/- sign
               C := Stream.Read_Character;
               if C not in '0' .. '9' then
                  raise Tokenizer_Error with "Expected at least one digit after +/- sign in number";
               end if;
               SU.Append (Value, C);
            elsif C in '0' .. '9' then
               SU.Append (Value, C);
            else
               raise Tokenizer_Error with Error_Exp_Message;
            end if;
         exception
            when Ada.IO_Exceptions.End_Error =>
               raise Tokenizer_Error with Error_Exp_Message;
         end;

         --  Accept sequence of digits
         loop
            C := Stream.Read_Character;
            exit when C not in '0' .. '9';
            SU.Append (Value, C);
         end loop;
      end if;

      if Is_Float then
         Next_Token := Token'(Kind => Float_Token,
           Float_Value => Types.Float_Type'Value (SU.To_String (Value)));
      else
         Next_Token := Token'(Kind => Integer_Token,
           Integer_Value => Types.Integer_Type'Value (SU.To_String (Value)));
      end if;

      Stream.Write_Character (C);
   exception
      --  End_Error is raised if the number if followed by an EOF
      when Ada.IO_Exceptions.End_Error =>
         --  Test whether value contains leading zeroes
         if not Checked_Leading_Zeroes then
            Test_Leading_Zeroes (First, Value);
         end if;

         if Is_Float then
            Next_Token := Token'(Kind => Float_Token,
              Float_Value => Types.Float_Type'Value (SU.To_String (Value)));
         else
            Next_Token := Token'(Kind => Integer_Token,
              Integer_Value => Types.Integer_Type'Value (SU.To_String (Value)));
         end if;
   end Read_Number;

   procedure Read_Literal_True (Stream     : in out Streams.Stream'Class;
                                Next_Token : out Token) is
      C1, C2, C3 : Character;
      Error_Message : constant String := "Expected literal 'true'";
   begin
      C1 := Stream.Read_Character;
      C2 := Stream.Read_Character;
      C3 := Stream.Read_Character;

      if C1 = 'r' and C2 = 'u' and C3 = 'e' then
         Next_Token := Token'(Kind => Boolean_Token, Boolean_Value => True);
      else
         raise Tokenizer_Error with Error_Message;
      end if;
   exception
      when Ada.IO_Exceptions.End_Error =>
         raise Tokenizer_Error with Error_Message;
   end Read_Literal_True;

   procedure Read_Literal_False (Stream     : in out Streams.Stream'Class;
                                 Next_Token : out Token) is
      C1, C2, C3, C4 : Character;
      Error_Message : constant String := "Expected literal 'false'";
   begin
      C1 := Stream.Read_Character;
      C2 := Stream.Read_Character;
      C3 := Stream.Read_Character;
      C4 := Stream.Read_Character;

      if C1 = 'a' and C2 = 'l' and C3 = 's' and C4 = 'e' then
         Next_Token := Token'(Kind => Boolean_Token, Boolean_Value => False);
      else
         raise Tokenizer_Error with Error_Message;
      end if;
   exception
      when Ada.IO_Exceptions.End_Error =>
         raise Tokenizer_Error with Error_Message;
   end Read_Literal_False;

   procedure Read_Literal_Null (Stream     : in out Streams.Stream'Class;
                                Next_Token : out Token) is
      C1, C2, C3 : Character;
      Error_Message : constant String := "Expected literal 'null'";
   begin
      C1 := Stream.Read_Character;
      C2 := Stream.Read_Character;
      C3 := Stream.Read_Character;

      if C1 = 'u' and C2 = 'l' and C3 = 'l' then
         Next_Token := Token'(Kind => Null_Token);
      else
         raise Tokenizer_Error with Error_Message;
      end if;
   exception
      when Ada.IO_Exceptions.End_Error =>
         raise Tokenizer_Error with Error_Message;
   end Read_Literal_Null;

   procedure Read_Token (Stream     : in out Streams.Stream'Class;
                         Next_Token : out Token;
                         Expect_EOF : Boolean := False) is
      C : Character;

      use Ada.Characters.Latin_1;
   begin
      loop
         --  Read the first next character and decide which token it could be part of
         C := Stream.Read_Character;

         --  Skip whitespace
         exit when C not in Space | HT | LF | CR;
      end loop;

      if Expect_EOF then
         raise Tokenizer_Error with "Expected to read EOF";
      end if;

      case C is
         when '[' =>
            Next_Token := Token'(Kind => Begin_Array_Token);
         when '{' =>
            Next_Token := Token'(Kind => Begin_Object_Token);
         when ']' =>
            Next_Token := Token'(Kind => End_Array_Token);
         when '}' =>
            Next_Token := Token'(Kind => End_Object_Token);
         when ':' =>
            Next_Token := Token'(Kind => Name_Separator_Token);
         when ',' =>
            Next_Token := Token'(Kind => Value_Separator_Token);
         when '"' =>
            Read_String (Stream, Next_Token);
         when '0' .. '9' | '+' | '-' =>
            Read_Number (Stream, C, Next_Token);
         when 't' =>
            Read_Literal_True (Stream, Next_Token);
         when 'f' =>
            Read_Literal_False (Stream, Next_Token);
         when 'n' =>
            Read_Literal_Null (Stream, Next_Token);
         when others =>
            raise Tokenizer_Error with "Unexpected character";
      end case;
   exception
      when Ada.IO_Exceptions.End_Error =>
         if Expect_EOF then
            Next_Token := Token'(Kind => EOF_Token);
         else
            raise Tokenizer_Error with "Unexpectedly read EOF";
         end if;
   end Read_Token;

end JSON.Tokenizers;
