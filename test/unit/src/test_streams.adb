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

with Ada.Streams.Stream_IO;

with Ahven; use Ahven;

with JSON.Parsers;
with JSON.Streams;
with JSON.Types;

package body Test_Streams is

   overriding
   procedure Initialize (T : in out Test) is
   begin
      T.Set_Name ("Streams");

      T.Add_Test_Routine (Test_Stream_IO'Access, "Parse float_number.txt");
   end Initialize;

   use JSON.Types;

   procedure Test_Stream_IO is
      File   : Ada.Streams.Stream_IO.File_Type;
      File_Stream : Ada.Streams.Stream_IO.Stream_Access;
      File_Name : constant String := "float_number.txt";
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, File_Name);
      File_Stream := Ada.Streams.Stream_IO.Stream (File);

      declare
         Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (File_Stream);
         Value  : constant JSON_Value'Class := JSON.Parsers.Parse (Stream);
      begin
         Assert (Value in JSON_Float_Value, "Not JSON_Float_Value");
         Assert (JSON_Float_Value (Value).Value = 3.14, "Expected float value to be equal to 3.14");

         Ada.Streams.Stream_IO.Close (File);
      exception
         when others =>
            Ada.Streams.Stream_IO.Close (File);
            raise;
      end;
   end Test_Stream_IO;

end Test_Streams;
