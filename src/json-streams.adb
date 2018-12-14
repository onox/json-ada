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
with Ada.Unchecked_Conversion;

package body JSON.Streams is

   use type AS.Stream_Element_Offset;

   overriding
   procedure Read_Character (Object : in out Stream_String; Item : out Character) is
   begin
      if Integer (Object.Index) not in Object.Text'Range then
         raise Ada.IO_Exceptions.End_Error;
      end if;

      Item := Object.Text (Integer (Object.Index));
      Object.Index := Object.Index + 1;
   end Read_Character;

   overriding
   procedure Read_Character (Object : in out Stream_Bytes; Item : out Character) is
      function Convert is new Ada.Unchecked_Conversion
        (Source => AS.Stream_Element, Target => Character);
   begin
      if Object.Index not in Object.Bytes'Range then
         raise Ada.IO_Exceptions.End_Error;
      end if;

      Item := Convert (Object.Bytes (Object.Index));
      Object.Index := Object.Index + 1;
   end Read_Character;

   function Has_Buffered_Character (Object : Stream) return Boolean
     is (Object.Next_Character /= Ada.Characters.Latin_1.NUL);

   function Read_Character (Object : in out Stream) return Character is
      Index : AS.Stream_Element_Offset;
   begin
      return Object.Read_Character (Index);
   end Read_Character;

   function Read_Character
     (Object : in out Stream;
      Index  : out AS.Stream_Element_Offset) return Character
   is
      C : Character;
   begin
      Index := Object.Index;
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

   overriding
   function Get_String
     (Object : Stream_String;
      Offset, Length : AS.Stream_Element_Offset) return String is
   begin
      return Object.Text (Integer (Offset) .. Integer (Offset + Length - 1));
   end Get_String;

   overriding
   function Get_String
     (Object : Stream_Bytes;
      Offset, Length : AS.Stream_Element_Offset) return String
   is
      subtype Constrained_String is String (1 .. Integer (Length));

      function Convert is new Ada.Unchecked_Conversion
        (Source => AS.Stream_Element_Array, Target => Constrained_String);
   begin
      return Convert (Object.Bytes (Offset .. Offset + Length - 1));
   end Get_String;

   function Create_Stream (Text : not null access String) return Stream'Class is
   begin
      return Stream_String'(Text => Text,
                            Next_Character => Ada.Characters.Latin_1.NUL,
                            Index => AS.Stream_Element_Offset (Text'First));
   end Create_Stream;

   function Create_Stream
     (Bytes : not null access AS.Stream_Element_Array) return Stream'Class is
   begin
      return Stream_Bytes'(Bytes => Bytes,
                           Next_Character => Ada.Characters.Latin_1.NUL,
                           Index => Bytes'First);
   end Create_Stream;

end JSON.Streams;
