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
with Ada.Unchecked_Deallocation;

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
   use Ada.Streams;

   type Stream_Element_Array_Access is access Stream_Element_Array;

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Stream_Element_Array, Name => Stream_Element_Array_Access);

   procedure Test_Stream_IO is
      File      : Stream_IO.File_Type;
      File_Name : constant String := "float_number.txt";

      Bytes : Stream_Element_Array_Access;
   begin
      Stream_IO.Open (File, Stream_IO.In_File, File_Name);

      declare
         File_Size : constant Stream_Element_Offset
           := Stream_Element_Offset (Stream_IO.Size (File));

         subtype Byte_Array is Stream_Element_Array (1 .. File_Size);
      begin
         Bytes := new Byte_Array;
         Byte_Array'Read (Stream_IO.Stream (File), Bytes.all);

         declare
            Stream : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Bytes);
            Value  : constant JSON_Value := Parsers.Parse (Stream);
         begin
            Assert (Value.Kind = Float_Kind, "Not a float");
            Assert (Value.Value = 3.14, "Expected float value to be equal to 3.14");
         end;
      end;

      Stream_IO.Close (File);
      Free (Bytes);
   exception
      when others =>
         Stream_IO.Close (File);
         Free (Bytes);
         raise;
   end Test_Stream_IO;

end Test_Streams;
