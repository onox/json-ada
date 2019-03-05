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

with Ahven.Framework;

package Test_Tokenizers is

   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test);

private

   --  Keyword
   procedure Test_Null_Token;
   procedure Test_True_Token;
   procedure Test_False_Token;

   --  String
   procedure Test_Empty_String_Token;
   procedure Test_Non_Empty_String_Token;
   procedure Test_Number_String_Token;
   procedure Test_Escaped_Character_String_Token;
   procedure Test_Escaped_Quotation_Solidus_String_Token;

   --  Integer/float number
   procedure Test_Zero_Number_Token;
   procedure Test_Integer_Number_Token;
   procedure Test_Float_Number_Token;
   procedure Test_Negative_Float_Number_Token;
   procedure Test_Integer_Exponent_Number_Token;
   procedure Test_Float_Exponent_Number_Token;
   procedure Test_Float_Negative_Exponent_Number_Token;

   --  Array
   procedure Test_Empty_Array_Tokens;
   procedure Test_One_Element_Array_Tokens;
   procedure Test_Two_Elements_Array_Tokens;

   --  Object
   procedure Test_Empty_Object_Tokens;
   procedure Test_One_Pair_Object_Tokens;
   procedure Test_Two_Pairs_Object_Tokens;

   --  Exceptions
   procedure Test_Control_Character_String_Exception;
   procedure Test_Unexpected_Escaped_Character_String_Exception;
   procedure Test_Minus_Number_EOF_Exception;
   procedure Test_Minus_Number_Exception;
   procedure Test_End_Dot_Number_Exception;
   procedure Test_End_Exponent_Number_Exception;
   procedure Test_End_Dot_Exponent_Number_Exception;
   procedure Test_End_Exponent_Minus_Number_Exception;
   procedure Test_End_Exponent_One_Digit_Exception;
   procedure Test_End_Exponent_Minus_One_Digit_Exception;
   procedure Test_Prefixed_Plus_Number_Exception;
   procedure Test_Leading_Zeroes_Integer_Number_Exception;
   procedure Test_Leading_Zeroes_Float_Number_Exception;
   procedure Test_Incomplete_True_Text_Exception;
   procedure Test_Incomplete_False_Text_Exception;
   procedure Test_Incomplete_Null_Text_Exception;
   procedure Test_Unknown_Keyword_Text_Exception;

end Test_Tokenizers;
