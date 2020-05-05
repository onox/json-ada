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

with Ahven; use Ahven;

with JSON.Parsers;
with JSON.Streams;
with JSON.Types;

package body Test_Streams is

   package Types is new JSON.Types (Long_Integer, Long_Float);
   package Parsers is new JSON.Parsers (Types);

   overriding
   procedure Initialize (T : in out Test) is
   begin
      T.Set_Name ("Streams");

      T.Add_Test_Routine (Test_Stream_IO'Access, "Parse float_number.txt");
   end Initialize;

   use Types;

   procedure Test_Stream_IO is
      File_Name : constant String := "float_number.txt";

      Text : constant JSON.Streams.Stream_Element_Array_Controlled :=
        JSON.Streams.Get_Stream_Element_Array (File_Name);

      Parser : Parsers.Parser := Parsers.Create (JSON.Streams.Create_Stream (Text.Pointer));
      Value  : constant JSON_Value := Parser.Parse;
   begin
      Assert (Value.Kind = Float_Kind, "Not a float");
      Assert (Value.Value = 3.14, "Expected float value to be equal to 3.14");
   end Test_Stream_IO;

end Test_Streams;
