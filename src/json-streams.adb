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

with Ada.Characters.Latin_1;
with Ada.IO_Exceptions;

package body JSON.Streams is

   overriding
   procedure Read_Character (Object : in out Stream_Object; Item : out Character) is
   begin
      Character'Read (Object.Stream, Item);
   end Read_Character;

   overriding
   procedure Read_Character (Object : in out Stream_String; Item : out Character) is
   begin
      if Object.Index not in Object.Text'Range then
         raise Ada.IO_Exceptions.End_Error;
      end if;

      Item := Object.Text (Object.Index);
      Object.Index := Object.Index + 1;
   end Read_Character;

   function Has_Buffered_Character (Object : Stream) return Boolean
     is (Object.Next_Character /= Ada.Characters.Latin_1.NUL);

   function Read_Character (Object : in out Stream) return Character is
      C : Character;
   begin
      if Object.Next_Character = Ada.Characters.Latin_1.NUL then
         Stream'Class (Object).Read_Character (C);
      else
         C := Object.Next_Character;
         Object.Next_Character := Ada.Characters.Latin_1.NUL;
      end if;
      return C;
   end Read_Character;

   procedure Write_Character (Object : in out Stream; Next : Character) is
   begin
      Object.Next_Character := Next;
   end Write_Character;

   function Create_Stream (Stream_Access : Ada.Streams.Stream_IO.Stream_Access)
     return Stream'Class is
   begin
      return Stream_Object'(Stream => Stream_Access, Next_Character => Ada.Characters.Latin_1.NUL);
   end Create_Stream;

   function Create_Stream (Text : access String) return Stream'Class is
   begin
      return Stream_String'(Text => Text, Next_Character => Ada.Characters.Latin_1.NUL, Index => 1);
   end Create_Stream;

end JSON.Streams;
