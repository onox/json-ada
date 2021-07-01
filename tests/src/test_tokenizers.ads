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

with AUnit.Test_Suites;
with AUnit.Test_Fixtures;

package Test_Tokenizers is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   --  Keyword
   procedure Test_Null_Token (Object : in out Test);
   procedure Test_True_Token (Object : in out Test);
   procedure Test_False_Token (Object : in out Test);

   --  String
   procedure Test_Empty_String_Token (Object : in out Test);
   procedure Test_Non_Empty_String_Token (Object : in out Test);
   procedure Test_Number_String_Token (Object : in out Test);
   procedure Test_Escaped_Character_String_Token (Object : in out Test);
   procedure Test_Escaped_Quotation_Solidus_String_Token (Object : in out Test);

   --  Integer/float number
   procedure Test_Zero_Number_Token (Object : in out Test);
   procedure Test_Integer_Number_Token (Object : in out Test);
   procedure Test_Float_Number_Token (Object : in out Test);
   procedure Test_Negative_Float_Number_Token (Object : in out Test);
   procedure Test_Integer_Exponent_Number_Token (Object : in out Test);
   procedure Test_Float_Exponent_Number_Token (Object : in out Test);
   procedure Test_Float_Negative_Exponent_Number_Token (Object : in out Test);

   --  Array
   procedure Test_Empty_Array_Tokens (Object : in out Test);
   procedure Test_One_Element_Array_Tokens (Object : in out Test);
   procedure Test_Two_Elements_Array_Tokens (Object : in out Test);

   --  Object
   procedure Test_Empty_Object_Tokens (Object : in out Test);
   procedure Test_One_Pair_Object_Tokens (Object : in out Test);
   procedure Test_Two_Pairs_Object_Tokens (Object : in out Test);

   --  Exceptions
   procedure Test_Control_Character_String_Exception (Object : in out Test);
   procedure Test_Unexpected_Escaped_Character_String_Exception (Object : in out Test);
   procedure Test_Minus_Number_EOF_Exception (Object : in out Test);
   procedure Test_Minus_Number_Exception (Object : in out Test);
   procedure Test_End_Dot_Number_Exception (Object : in out Test);
   procedure Test_End_Exponent_Number_Exception (Object : in out Test);
   procedure Test_End_Dot_Exponent_Number_Exception (Object : in out Test);
   procedure Test_End_Exponent_Minus_Number_Exception (Object : in out Test);
   procedure Test_End_Exponent_One_Digit_Exception (Object : in out Test);
   procedure Test_End_Exponent_Minus_One_Digit_Exception (Object : in out Test);
   procedure Test_Prefixed_Plus_Number_Exception (Object : in out Test);
   procedure Test_Leading_Zeroes_Integer_Number_Exception (Object : in out Test);
   procedure Test_Leading_Zeroes_Float_Number_Exception (Object : in out Test);
   procedure Test_Incomplete_True_Text_Exception (Object : in out Test);
   procedure Test_Incomplete_False_Text_Exception (Object : in out Test);
   procedure Test_Incomplete_Null_Text_Exception (Object : in out Test);
   procedure Test_Unknown_Keyword_Text_Exception (Object : in out Test);

end Test_Tokenizers;
