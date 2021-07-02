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

private with Ada.Finalization;

with Ada.Streams;

package JSON.Streams with SPARK_Mode => On is
   pragma Preelaborate;

   package AS renames Ada.Streams;

   type Stream (<>) is tagged private;

   type Stream_Ptr is not null access all Streams.Stream;

   function Has_Buffered_Character (Object : Stream) return Boolean;

   function Read_Character (Object : in out Stream) return Character
     with Post => not Object.Has_Buffered_Character;

   function Read_Character
     (Object : in out Stream;
      Index  : out AS.Stream_Element_Offset) return Character
   with Post => not Object.Has_Buffered_Character;
   --  Writes the offset of the read character to Index. This is needed
   --  for string tokens.

   procedure Write_Character (Object : in out Stream; Next : Character)
     with Pre => not Object.Has_Buffered_Character;

   function Get_String
     (Object : Stream;
      Offset, Length : AS.Stream_Element_Offset) return String;

   function Create_Stream (Text : not null access String) return Stream;

   function Create_Stream
     (Bytes : not null access AS.Stream_Element_Array) return Stream;

   -----------------------------------------------------------------------------

   type Stream_Element_Array_Controlled is tagged limited private;

   type Stream_Element_Array_Access is access all AS.Stream_Element_Array;

   function Pointer
     (Object : Stream_Element_Array_Controlled) return Stream_Element_Array_Access;

   function Get_Stream_Element_Array
     (File_Name : String) return Stream_Element_Array_Controlled;

private

   type Stream (Bytes : not null Stream_Element_Array_Access) is tagged record
      Next_Character : Character;
      Index : AS.Stream_Element_Offset;
   end record;

   -----------------------------------------------------------------------------

   type Stream_Element_Array_Controlled is limited
     new Ada.Finalization.Limited_Controlled with
   record
      Pointer : Stream_Element_Array_Access;
   end record;

   overriding
   procedure Finalize (Object : in out Stream_Element_Array_Controlled);

end JSON.Streams;
