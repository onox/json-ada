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

package Test_Parsers is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   --  Keyword
   procedure Test_True_Text (Object : in out Test);
   procedure Test_False_Text (Object : in out Test);
   procedure Test_Null_Text (Object : in out Test);

   --  String
   procedure Test_Empty_String_Text (Object : in out Test);
   procedure Test_Non_Empty_String_Text (Object : in out Test);
   procedure Test_Number_String_Text (Object : in out Test);

   --  Integer/float number
   procedure Test_Integer_Number_Text (Object : in out Test);
   procedure Test_Integer_Number_To_Float_Text (Object : in out Test);
   procedure Test_Float_Number_Text (Object : in out Test);

   --  Array
   procedure Test_Empty_Array_Text (Object : in out Test);
   procedure Test_One_Element_Array_Text (Object : in out Test);
   procedure Test_Multiple_Elements_Array_Text (Object : in out Test);
   procedure Test_Array_Iterable (Object : in out Test);
   procedure Test_Multiple_Array_Iterable (Object : in out Test);

   --  Object
   procedure Test_Empty_Object_Text (Object : in out Test);
   procedure Test_One_Member_Object_Text (Object : in out Test);
   procedure Test_Multiple_Members_Object_Text (Object : in out Test);
   procedure Test_Object_Iterable (Object : in out Test);

   procedure Test_Array_Object_Array (Object : in out Test);
   procedure Test_Object_Array_Object (Object : in out Test);

   procedure Test_Object_No_Array (Object : in out Test);
   procedure Test_Object_No_Object (Object : in out Test);

   --  Exceptions
   procedure Test_Empty_Text_Exception (Object : in out Test);

   procedure Test_Array_No_Value_Separator_Exception (Object : in out Test);
   procedure Test_Array_No_End_Array_Exception (Object : in out Test);
   procedure Test_No_EOF_After_Array_Exception (Object : in out Test);

   procedure Test_Object_No_Value_Separator_Exception (Object : in out Test);
   procedure Test_Object_No_Name_Separator_Exception (Object : in out Test);
   procedure Test_Object_Key_No_String_Exception (Object : in out Test);
   procedure Test_Object_No_Second_Member_Exception (Object : in out Test);
   procedure Test_Object_Duplicate_Keys_Exception (Object : in out Test);
   procedure Test_Object_No_Value_Exception (Object : in out Test);
   procedure Test_Object_No_End_Object_Exception (Object : in out Test);
   procedure Test_No_EOF_After_Object_Exception (Object : in out Test);

end Test_Parsers;
