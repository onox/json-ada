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

with Ada.IO_Exceptions;

package JSON.Streams with SPARK_Mode => On is
   pragma Preelaborate;

   type Text_Access is access String;
   --  Access type designating a heap-allocated String, used to hold the
   --  text owned by a String_Buffer or a Stream

   -----------------------------------------------------------------------------
   --                              String buffer                              --
   -----------------------------------------------------------------------------

   Buffer_Overflow_Error : exception;
   --  Raised when the text held by a String_Buffer would become longer
   --  than Natural'Last characters

   type String_Buffer is limited private;
   --  A growable string. An object of this type owns heap memory; call
   --  Destroy to release it.

   function Length (Object : String_Buffer) return Natural;
   --  Return the number of characters currently held by the buffer
   --
   --  @param Object The string buffer to query
   --  @return The current length of the buffered text

   function Has_Storage (Object : String_Buffer) return Boolean;
   --  Return whether the buffer currently owns heap memory
   --
   --  @param Object The string buffer to query
   --  @return True if the buffer owns heap memory

   procedure Append (Object : in out String_Buffer; Text : String)
     with Always_Terminates,
          Post => Length (Object) = Length (Object)'Old + Text'Length,
          Exceptional_Cases => (Buffer_Overflow_Error => True);
   --  Append the given text to the end of the buffer, growing it as needed
   --
   --  Buffer_Overflow_Error is raised iff the new length would exceed
   --  Natural'Last; the buffer is unchanged in that case
   --
   --  @param Object The string buffer to append to
   --  @param Text The characters to append
   --  @exception Buffer_Overflow_Error Raised when the resulting length
   --    would exceed Natural'Last

   function To_String (Object : String_Buffer) return String
     with Post => To_String'Result'Length = Length (Object);
   --  Return a String holding a copy of the buffered characters
   --
   --  @param Object The string buffer to read
   --  @return A String equal in length to Length (Object)

   procedure Destroy (Object : in out String_Buffer)
     with Always_Terminates,
          Post => Length (Object) = 0 and not Has_Storage (Object),
          --  the released buffer is a constant, its old contents reaching
          --  only the deallocation: callers that drop it are not dropping a
          --  computed value
          Depends => (Object => null, null => Object);
   --  Release the heap memory owned by the buffer, leaving it empty; the
   --  buffer then owns no heap memory
   --
   --  @param Object The string buffer to release

   -----------------------------------------------------------------------------
   --                                  Stream                                 --
   -----------------------------------------------------------------------------

   type Stream is limited private;
   --  A character stream over a text held in heap memory. An object of
   --  this type owns the text; call Destroy to release it. A stream
   --  without text behaves as a stream over an empty text.

   function Length (Object : Stream) return Natural
     with Post => Length'Result < Positive'Last;
   --  Length of the text of the stream
   --
   --  @param Object The stream to query
   --  @return The number of characters in the stream's text

   function Position (Object : Stream) return Positive
     with Post => Position'Result <= Length (Object) + 1;
   --  Index of the next character to be read from the text
   --
   --  @param Object The stream to query
   --  @return The one-based index of the next character to be read

   function Has_Buffered_Character (Object : Stream) return Boolean;
   --  Return whether a character has been pushed back into the stream by
   --  Write_Character and not yet consumed
   --
   --  @param Object The stream to query
   --  @return True if a buffered character is pending

   function Has_Storage (Object : Stream) return Boolean;
   --  Return whether the stream currently owns a heap-allocated text
   --
   --  @param Object The stream to query
   --  @return True if the stream owns a heap-allocated text

   function Valid_Span
     (Object : Stream;
      Offset : Positive;
      Count  : Natural) return Boolean
   is (Count = 0
         or else (Offset <= Length (Object)
                    and then Count <= Length (Object) - Offset + 1));
   --  True if the given span designates characters of the text
   --
   --  @param Object The stream whose text the span refers to
   --  @param Offset The one-based index of the first character of the span
   --  @param Count The number of characters in the span
   --  @return True if the span lies within the stream's text
   --    (an empty span is always valid)

   procedure Read_Character
     (Object : in out Stream;
      Result : out Character;
      EOF    : out Boolean)
   with Always_Terminates,
        Post =>
     Length (Object) = Length (Object)'Old
       and not Has_Buffered_Character (Object)
       and (if EOF then
              Position (Object) = Position (Object)'Old
                and Position (Object) = Length (Object) + 1
                and not Has_Buffered_Character (Object)'Old
            elsif Has_Buffered_Character (Object)'Old then
              Position (Object) = Position (Object)'Old
            else
              Position (Object)'Old <= Length (Object)
                and Position (Object) = Position (Object)'Old + 1);
   --  Return the buffered character if present, otherwise the character
   --  at the current position. EOF is True iff the text is exhausted.
   --
   --  @param Object The stream to read from
   --  @param Result The character that was read (meaningful only when EOF
   --    is False)
   --  @param EOF True iff the text is exhausted and no character was read

   procedure Read_Character
     (Object : in out Stream;
      Index  : out Positive;
      Result : out Character;
      EOF    : out Boolean)
   with Pre  => not Has_Buffered_Character (Object),
        Always_Terminates,
        Post =>
     Length (Object) = Length (Object)'Old
       and not Has_Buffered_Character (Object)
       and (if EOF then
              Position (Object) = Position (Object)'Old
                and Position (Object) = Length (Object) + 1
            else
              Index = Position (Object)'Old
                and Index <= Length (Object)
                and Position (Object) = Index + 1);
   --  Writes the index of the read character to Index. This is needed
   --  for string tokens.
   --
   --  @param Object The stream to read from
   --  @param Index The one-based index of the read character (meaningful
   --    only when EOF is False)
   --  @param Result The character that was read (meaningful only when EOF
   --    is False)
   --  @param EOF True iff the text is exhausted and no character was read

   procedure Write_Character (Object : in out Stream; Next : Character)
     with Pre  => not Has_Buffered_Character (Object),
          Always_Terminates,
          Post =>
     Has_Buffered_Character (Object)
       and Length (Object) = Length (Object)'Old
       and Position (Object) = Position (Object)'Old;
   --  Buffer one character; the next call to Read_Character returns it
   --
   --  @param Object The stream to push the character back into
   --  @param Next The character to buffer

   function Get_String
     (Object : Stream;
      Offset : Positive;
      Count  : Natural) return String
   with Pre  => Valid_Span (Object, Offset, Count),
        Post => Get_String'Result'Length = Count
                  and Get_String'Result'First = 1;
   --  Return a copy of the designated characters of the text
   --
   --  @param Object The stream whose text is copied
   --  @param Offset The one-based index of the first character to copy
   --  @param Count The number of characters to copy
   --  @return A String of length Count, indexed from 1, holding the copy

   procedure From_Text (Object : out Stream; Text : String)
     with Pre  => Text'Length < Positive'Last,
          Always_Terminates,
          Post =>
     Length (Object) = Text'Length
       and Position (Object) = 1
       and not Has_Buffered_Character (Object);
   --  Create a stream over a copy of the given text
   --
   --  If Object held a text, that text is not released; call Destroy
   --  first to avoid a memory leak.
   --
   --  @param Object The stream to initialize
   --  @param Text The text to copy into the stream

   procedure From_File (Object : out Stream; File_Name : String)
     with Always_Terminates,
          Post =>
       Position (Object) = 1 and not Has_Buffered_Character (Object),
          Exceptional_Cases =>
            (Ada.IO_Exceptions.Name_Error
               | Ada.IO_Exceptions.Use_Error => True);
   --  Create a stream over the contents of the given file
   --
   --  Propagates the Ada.IO_Exceptions exceptions of
   --  Ada.Streams.Stream_IO.Open, and Ada.IO_Exceptions.Use_Error if the
   --  file is larger than Positive'Last - 1 characters.
   --
   --  If Object held a text, that text is not released; call Destroy
   --  first to avoid a memory leak.
   --
   --  @param Object The stream to initialize
   --  @param File_Name The path of the file whose contents become the text

   procedure Destroy (Object : in out Stream)
     with Always_Terminates,
          Post =>
       Length (Object) = 0
         and not Has_Buffered_Character (Object)
         and not Has_Storage (Object),
          --  see the String_Buffer Destroy: the released stream is a constant
          Depends => (Object => null, null => Object);
   --  Release the text owned by the stream; the stream then owns no heap
   --  memory
   --
   --  @param Object The stream to release

private

   type Stream is limited record
      Text           : Text_Access := null;
      Index          : Positive    := 1;
      Next_Character : Character   := ' ';
      Has_Next       : Boolean     := False;
   end record
     with Dynamic_Predicate =>
       (if Stream.Text = null then
          Stream.Index = 1
        else
          Stream.Text'First = 1
            and then Stream.Text'Last < Positive'Last
            and then Stream.Index <= Stream.Text'Last + 1);

   function Length (Object : Stream) return Natural
     is (if Object.Text = null then 0 else Object.Text'Last);
   --  Length of the text of the stream; zero when no text is held
   --
   --  @param Object The stream to query
   --  @return The number of characters in the stream's text

   function Position (Object : Stream) return Positive is (Object.Index);
   --  Index of the next character to be read from the text
   --
   --  @param Object The stream to query
   --  @return The one-based index of the next character to be read

   function Has_Buffered_Character (Object : Stream) return Boolean
     is (Object.Has_Next);
   --  Return whether a character has been pushed back and not yet consumed
   --
   --  @param Object The stream to query
   --  @return True if a buffered character is pending

   function Has_Storage (Object : Stream) return Boolean
     is (Object.Text /= null);
   --  Return whether the stream currently owns a heap-allocated text
   --
   --  @param Object The stream to query
   --  @return True if the stream owns a heap-allocated text

   type String_Buffer is limited record
      Text   : Text_Access := null;
      Length : Natural     := 0;
   end record
     with Dynamic_Predicate =>
       (if String_Buffer.Text = null then
          String_Buffer.Length = 0
        else
          String_Buffer.Text'First = 1
            and then String_Buffer.Length <= String_Buffer.Text'Last);

   function Length (Object : String_Buffer) return Natural is (Object.Length);
   --  Return the number of characters currently held by the buffer
   --
   --  @param Object The string buffer to query
   --  @return The current length of the buffered text

   function Has_Storage (Object : String_Buffer) return Boolean
     is (Object.Text /= null);
   --  Return whether the buffer currently owns heap memory
   --
   --  @param Object The string buffer to query
   --  @return True if the buffer owns heap memory

end JSON.Streams;
