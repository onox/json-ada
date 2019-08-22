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

with Ahven.Framework;

package Test_Parsers is

   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test);

private

   --  Keyword
   procedure Test_True_Text;
   procedure Test_False_Text;
   procedure Test_Null_Text;

   --  String
   procedure Test_Empty_String_Text;
   procedure Test_Non_Empty_String_Text;
   procedure Test_Number_String_Text;

   --  Integer/float number
   procedure Test_Integer_Number_Text;
   procedure Test_Integer_Number_To_Float_Text;
   procedure Test_Float_Number_Text;

   --  Array
   procedure Test_Empty_Array_Text;
   procedure Test_One_Element_Array_Text;
   procedure Test_Multiple_Elements_Array_Text;
   procedure Test_Array_Iterable;
   procedure Test_Multiple_Array_Iterable;

   --  Object
   procedure Test_Empty_Object_Text;
   procedure Test_One_Member_Object_Text;
   procedure Test_Multiple_Members_Object_Text;
   procedure Test_Object_Iterable;

   procedure Test_Array_Object_Array;
   procedure Test_Object_Array_Object;

   procedure Test_Object_No_Array;
   procedure Test_Object_No_Object;

   --  Exceptions
   procedure Test_Empty_Text_Exception;

   procedure Test_Array_No_Value_Separator_Exception;
   procedure Test_Array_No_End_Array_Exception;
   procedure Test_No_EOF_After_Array_Exception;

   procedure Test_Object_No_Value_Separator_Exception;
   procedure Test_Object_No_Name_Separator_Exception;
   procedure Test_Object_Key_No_String_Exception;
   procedure Test_Object_No_Second_Member_Exception;
   procedure Test_Object_Duplicate_Keys_Exception;
   procedure Test_Object_No_Value_Exception;
   procedure Test_Object_No_End_Object_Exception;
   procedure Test_No_EOF_After_Object_Exception;

end Test_Parsers;
