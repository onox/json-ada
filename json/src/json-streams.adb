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

with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;

package body JSON.Streams with SPARK_Mode => On is

   procedure Free_Text is new Ada.Unchecked_Deallocation
     (Object => String, Name => Text_Access);

   -----------------------------------------------------------------------------
   --                              String buffer                              --
   -----------------------------------------------------------------------------

   procedure Append (Object : in out String_Buffer; Text : String) is
      New_Length : Natural;
   begin
      if Text'Length > Natural'Last - Object.Length then
         raise Buffer_Overflow_Error with "String buffer full";
      end if;
      New_Length := Object.Length + Text'Length;

      if Object.Text = null then
         Object.Text := new String'(1 .. Natural'Max (New_Length, 16) => ' ');
      elsif Object.Text'Last < New_Length then
         declare
            New_Last : constant Natural := Natural'Max
              (New_Length,
               (if Object.Text'Last <= Natural'Last / 2 then
                  2 * Object.Text'Last
                else
                  Natural'Last));
            New_Text : constant Text_Access := new String'(1 .. New_Last => ' ');
            Old_Text : Text_Access := Object.Text;
         begin
            New_Text (1 .. Object.Length) := Old_Text (1 .. Object.Length);
            Object.Text := New_Text;
            Free_Text (Old_Text);
         end;
      end if;

      if Text'Length > 0 then
         Object.Text (Object.Length + 1 .. New_Length) := Text;
      end if;
      Object.Length := New_Length;
   end Append;

   function To_String (Object : String_Buffer) return String is
   begin
      if Object.Text = null then
         return "";
      else
         declare
            Result : constant String (1 .. Object.Length)
              := Object.Text (1 .. Object.Length);
         begin
            return Result;
         end;
      end if;
   end To_String;

   procedure Destroy (Object : in out String_Buffer) is
   begin
      Object.Length := 0;
      Free_Text (Object.Text);
   end Destroy;

   -----------------------------------------------------------------------------
   --                                  Stream                                 --
   -----------------------------------------------------------------------------

   procedure Read_Character
     (Object : in out Stream;
      Result : out Character;
      EOF    : out Boolean) is
   begin
      if Object.Has_Next then
         Result := Object.Next_Character;
         Object.Has_Next := False;
         EOF := False;
      elsif Object.Text = null or else Object.Index > Object.Text'Last then
         Result := ' ';
         EOF := True;
      else
         Result := Object.Text (Object.Index);
         Object.Index := Object.Index + 1;
         EOF := False;
      end if;
   end Read_Character;

   procedure Read_Character
     (Object : in out Stream;
      Index  : out Positive;
      Result : out Character;
      EOF    : out Boolean) is
   begin
      if Object.Text = null or else Object.Index > Object.Text'Last then
         Index := 1;
         Result := ' ';
         EOF := True;
      else
         Index := Object.Index;
         Result := Object.Text (Object.Index);
         Object.Index := Object.Index + 1;
         EOF := False;
      end if;
   end Read_Character;

   procedure Write_Character (Object : in out Stream; Next : Character) is
   begin
      Object.Next_Character := Next;
      Object.Has_Next := True;
   end Write_Character;

   function Get_String
     (Object : Stream;
      Offset : Positive;
      Count  : Natural) return String is
   begin
      if Count = 0 then
         return "";
      else
         declare
            Result : constant String (1 .. Count)
              := Object.Text (Offset .. Offset + Count - 1);
         begin
            return Result;
         end;
      end if;
   end Get_String;

   procedure From_Text (Object : out Stream; Text : String) is
      subtype Constrained_String is String (1 .. Text'Length);
   begin
      Object.Index := 1;
      Object.Next_Character := ' ';
      Object.Has_Next := False;
      --  The conversion slides the bounds of Text to 1 .. Text'Length
      Object.Text := new Constrained_String'(Constrained_String (Text));
   end From_Text;

   procedure From_File (Object : out Stream; File_Name : String) is
      package IO renames Ada.Streams.Stream_IO;

      use type Ada.Streams.Stream_Element_Offset;
      use type IO.Count;

      --  Ada.Streams.Stream_IO is not annotated with SPARK contracts;
      --  its effects on the file system are outside the proof boundary
      pragma Warnings
        (GNATprove, Off, "no Global contract available*",
         Reason => "Stream_IO is not annotated with SPARK contracts");
      pragma Warnings
        (GNATprove, Off, "assuming * has no effect on global items",
         Reason => "Stream_IO is not annotated with SPARK contracts");
      pragma Warnings
        (GNATprove, Off, "no Always_Terminates aspect available*",
         Reason => "Stream_IO is not annotated with SPARK contracts");
      pragma Warnings
        (GNATprove, Off, "assuming * always terminates",
         Reason => "Stream_IO is not annotated with SPARK contracts");
      pragma Warnings
        (GNATprove, Off, "statement has no effect",
         Reason => "closing the file affects only the unmodeled file system");
      pragma Warnings
        (GNATprove, Off, "*is set by ""Close"" but not used after the call",
         Reason => "the file object is not used after it is closed");

      File : IO.File_Type;
   begin
      Object.Index := 1;
      Object.Next_Character := ' ';
      Object.Has_Next := False;
      Object.Text := null;

      IO.Open (File, IO.In_File, File_Name);

      if IO.Size (File) > IO.Count (Positive'Last - 1) then
         IO.Close (File);
         raise Ada.IO_Exceptions.Use_Error with "File too large";
      end if;

      declare
         Size : constant Ada.Streams.Stream_Element_Offset
           := Ada.Streams.Stream_Element_Offset (IO.Size (File));
         Text : constant Text_Access := new String'(1 .. Natural (Size) => ' ');
      begin
         if Size > 0 then
            declare
               type Bytes_Access is access Ada.Streams.Stream_Element_Array;

               procedure Free_Bytes is new Ada.Unchecked_Deallocation
                 (Object => Ada.Streams.Stream_Element_Array,
                  Name   => Bytes_Access);

               Bytes : Bytes_Access
                 := new Ada.Streams.Stream_Element_Array'(1 .. Size => 0);
               Last  : Ada.Streams.Stream_Element_Offset;
            begin
               IO.Read (File, Bytes.all, Last);
               for J in 1 .. Ada.Streams.Stream_Element_Offset'Min (Last, Size) loop
                  Text (Natural (J)) := Character'Val (Bytes (J));
               end loop;
               Free_Bytes (Bytes);
            end;
         end if;
         IO.Close (File);

         Object.Text := Text;
      end;
   end From_File;

   procedure Destroy (Object : in out Stream) is
   begin
      Object.Index := 1;
      Object.Has_Next := False;
      Object.Next_Character := ' ';
      --  inert once Has_Next is False, but leaves no component holding a
      --  value from before the call, as the Depends contract states
      Free_Text (Object.Text);
   end Destroy;

end JSON.Streams;
