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

with AUnit.Assertions;
with AUnit.Test_Caller;

with JSON.Parsers;
with JSON.Types;

package body Test_Streams is

   package Types is new JSON.Types (Long_Integer, Long_Float);
   package Parsers is new JSON.Parsers (Types);

   use AUnit.Assertions;

   package Caller is new AUnit.Test_Caller (Test);

   Test_Suite : aliased AUnit.Test_Suites.Test_Suite;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Name : constant String := "(Streams) ";
   begin
      Test_Suite.Add_Test (Caller.Create
        (Name & "Parse float_number.txt", Test_Stream_IO'Access));

      return Test_Suite'Access;
   end Suite;

   use Types;

   procedure Test_Stream_IO (Object : in out Test) is
      File_Name : constant String := "float_number.txt";

      Parser : Parsers.Parser := Parsers.Create_From_File (File_Name);
      Value  : constant JSON_Value := Parser.Parse;
   begin
      Assert (Value.Kind = Float_Kind, "Not a float");
      Assert (Value.Value = 3.14, "Expected float value to be equal to 3.14");
   end Test_Stream_IO;

end Test_Streams;
