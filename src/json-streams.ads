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

package JSON.Streams is
   pragma Preelaborate;

   package AS renames Ada.Streams;

   type Stream is abstract tagged private;

   procedure Read_Character (Object : in out Stream; Item : out Character) is abstract;

   function Has_Buffered_Character (Object : Stream) return Boolean
     with Inline;

   function Read_Character (Object : in out Stream) return Character
     with Post'Class => not Stream'Class (Object).Has_Buffered_Character;

   procedure Write_Character (Object : in out Stream; Next : Character)
     with Pre'Class => not Stream'Class (Object).Has_Buffered_Character;

   function Create_Stream
     (Stream_Access : AS.Stream_IO.Stream_Access) return Stream'Class;

   function Create_Stream (Text : access String) return Stream'Class;

   function Create_Stream
     (Bytes : access AS.Stream_Element_Array) return Stream'Class;

private

   type Stream is abstract tagged record
      Next_Character : Character;
   end record;

   type Stream_Object
     (Stream : AS.Stream_IO.Stream_Access) is new Stream with null record;

   overriding
   procedure Read_Character (Object : in out Stream_Object; Item : out Character);

   type Stream_String (Text : access String) is new Stream with record
      Index : Natural := 1;
   end record;

   overriding
   procedure Read_Character (Object : in out Stream_String; Item : out Character);

   type Stream_Bytes (Bytes : access AS.Stream_Element_Array) is new Stream with record
      Index : AS.Stream_Element_Offset := Bytes'First;
   end record;

   overriding
   procedure Read_Character (Object : in out Stream_Bytes; Item : out Character);

end JSON.Streams;
