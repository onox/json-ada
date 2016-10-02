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

with Ada.Exceptions;

with JSON.Tokenizers;

package body JSON.Parsers is

   package Tokenizers is new JSON.Tokenizers (Types);

   use type Tokenizers.Token_Kind;

   function Parse_Token (Stream : in out Streams.Stream'Class;
                         Token  : Tokenizers.Token)
     return Types.JSON_Value'Class;

   function Parse_Array (Stream : in out Streams.Stream'Class)
     return Types.JSON_Array_Value'Class is
      Token : Tokenizers.Token;

      JSON_Array : Types.JSON_Array_Value'Class := Types.Create_Array;
      Repeat : Boolean := False;
   begin
      loop
         Tokenizers.Read_Token (Stream, Token);

         --  Either expect ']' character or (if not the first element)
         --  a value separator (',' character)
         if Token.Kind = Tokenizers.End_Array_Token then
            exit;
         elsif Repeat and Token.Kind /= Tokenizers.Value_Separator_Token then
            raise Parse_Error with "Expected value separator (',' character)";
         elsif Repeat then
            --  Value separator has been read, now read the next value
            Tokenizers.Read_Token (Stream, Token);
         end if;

         --  Parse value and append it to the array
         JSON_Array.Append (Parse_Token (Stream, Token));

         Repeat := True;
      end loop;

      return JSON_Array;
   end Parse_Array;

   function Parse_Object (Stream : in out Streams.Stream'Class)
     return Types.JSON_Object_Value'Class is
      Token : Tokenizers.Token;

      JSON_Object : Types.JSON_Object_Value'Class := Types.Create_Object;
      Repeat : Boolean := False;
   begin
      loop
         Tokenizers.Read_Token (Stream, Token);

         --  Either expect '}' character or (if not the first member)
         --  a value separator (',' character)
         if Token.Kind = Tokenizers.End_Object_Token then
            exit;
         elsif Repeat and Token.Kind /= Tokenizers.Value_Separator_Token then
            raise Parse_Error with "Expected value separator (',' character)";
         elsif Repeat then
            --  Value separator has been read, now read the next value
            Tokenizers.Read_Token (Stream, Token);
         end if;

         --  Parse member key
         if Token.Kind /= Tokenizers.String_Token then
            raise Parse_Error with "Expected key to be a string";
         end if;

         declare
            Key : constant Types.JSON_String_Value'Class := Types.Create_String (Token.String_Value);
         begin
            --  Expect name separator (':' character) between key and value
            Tokenizers.Read_Token (Stream, Token);
            if Token.Kind /= Tokenizers.Name_Separator_Token then
               raise Parse_Error with "Expected name separator (':' character)";
            end if;

            --  Parse member value and insert key-value pair in the object
            Tokenizers.Read_Token (Stream, Token);
            JSON_Object.Insert (Key, Parse_Token (Stream, Token));
         end;

         Repeat := True;
      end loop;

      return JSON_Object;
   end Parse_Object;

   function Parse_Token (Stream : in out Streams.Stream'Class;
                         Token  : Tokenizers.Token)
     return Types.JSON_Value'Class is
   begin
      case Token.Kind is
         when Tokenizers.Begin_Array_Token =>
            return Parse_Array (Stream);
         when Tokenizers.Begin_Object_Token =>
            return Parse_Object (Stream);
         when Tokenizers.String_Token =>
            return Types.Create_String (Token.String_Value);
         when Tokenizers.Integer_Token =>
            return Types.Create_Integer (Token.Integer_Value);
         when Tokenizers.Float_Token =>
            return Types.Create_Float (Token.Float_Value);
         when Tokenizers.Boolean_Token =>
            return Types.Create_Boolean (Token.Boolean_Value);
         when Tokenizers.Null_Token =>
            return Types.Create_Null;
         when others =>
            raise Parse_Error with "Unexpected token " & Tokenizers.Token_Kind'Image (Token.Kind);
      end case;
   end Parse_Token;

   function Parse (Stream : in out Streams.Stream'Class)
     return Types.JSON_Value'Class is
      Token : Tokenizers.Token;
   begin
      Tokenizers.Read_Token (Stream, Token);
      return Value : constant Types.JSON_Value'Class := Parse_Token (Stream, Token) do
         Tokenizers.Read_Token (Stream, Token, Expect_EOF => True);
      end return;
   exception
      when E : Tokenizers.Tokenizer_Error =>
         raise Parse_Error with Ada.Exceptions.Exception_Message (E);
   end Parse;

end JSON.Parsers;
