--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2018 RREE <rolf.ebert.gcc@gmx.de>
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

package Test_Images is

   type Test is new Ahven.Framework.Test_Case with null record;

   overriding
   procedure Initialize (T : in out Test);

private

   --  Keyword
   procedure Test_True_Text;
   procedure Test_False_Text;
   procedure Test_Null_Text;
   procedure Test_Escaped_Text;

   --  String
   procedure Test_Empty_String_Text;
   procedure Test_Non_Empty_String_Text;
   procedure Test_Number_String_Text;

   --  Integer number
   procedure Test_Integer_Number_Text;

   --  Array
   procedure Test_Empty_Array_Text;
   procedure Test_One_Element_Array_Text;
   procedure Test_Multiple_Elements_Array_Text;

   --  Object
   procedure Test_Empty_Object_Text;
   procedure Test_One_Member_Object_Text;
   procedure Test_Multiple_Members_Object_Text;

   procedure Test_Array_Object_Array;
   procedure Test_Object_Array_Object;

end Test_Images;
