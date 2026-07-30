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

with JSON.Types;
with JSON.Streams;

generic
   with package Types is new JSON.Types (<>);

   Default_Maximum_Depth : Positive := 10;

   Check_Duplicate_Keys : Boolean := False;
package JSON.Parsers with SPARK_Mode => On is
   --  Parse JSON text into an owned tree of Types.JSON_Value nodes.
   --  @formal Types The instance of JSON.Types whose JSON_Value tree the
   --    parser builds
   --  @formal Default_Maximum_Depth Default limit on the nesting depth of
   --    arrays and objects, applied when Create or Create_From_File is
   --    called without an explicit Maximum_Depth
   --  @formal Check_Duplicate_Keys If enabled, Parse raises Parse_Error
   --    when an object contains duplicate keys; parsing is slower when
   --    enabled

   pragma Preelaborate;

   use type Types.JSON_Value_Access;

   type Parser is limited private;
   --  A parser owns a copy of the JSON text; call Destroy to release it.
   --  The documents returned by Parse are independent of the parser and
   --  are released separately with Types.Free.

   procedure Create
     (Object        : out Parser;
      Text          : String;
      Maximum_Depth : Positive := Default_Maximum_Depth)
   with Pre => Text'Length < Positive'Last,
        Always_Terminates;
   --  Initialize a parser with an in-memory JSON text, of which the parser
   --  stores a copy.
   --
   --  If Object held a text, that text is not released; call Destroy
   --  first to avoid a memory leak.
   --  @param Object The parser to initialize
   --  @param Text The JSON text to parse; the parser keeps a private copy
   --  @param Maximum_Depth Maximum nesting depth of arrays and objects

   procedure Create_From_File
     (Object        : out Parser;
      File_Name     : String;
      Maximum_Depth : Positive := Default_Maximum_Depth)
   with Always_Terminates,
        Exceptional_Cases =>
          (Ada.IO_Exceptions.Name_Error
             | Ada.IO_Exceptions.Use_Error => True);
   --  Initialize a parser with the contents of a file.
   --
   --  Propagates the exceptions of Streams.From_File.
   --
   --  If Object held a text, that text is not released; call Destroy
   --  first to avoid a memory leak.
   --  @param Object The parser to initialize
   --  @param File_Name Path of the file whose contents are parsed
   --  @param Maximum_Depth Maximum nesting depth of arrays and objects
   --  @exception Ada.IO_Exceptions.Name_Error Raised if the file does not
   --     exist
   --  @exception Ada.IO_Exceptions.Use_Error Raised if the file cannot be
   --     opened or read

   procedure Parse
     (Object   : in out Parser;
      Document : aliased out Types.JSON_Value_Access)
   with Always_Terminates,
        Post              =>
          Document /= null and then Types.Is_Standalone (Document),
        Exceptional_Cases => (Parse_Error => True);
   --  Parse the JSON text of the parser and return the root value of
   --  the document. The caller owns the document and must release it
   --  with Types.Free.
   --
   --  The returned document is standalone, so it can be passed directly
   --  to Types.Append, Insert, Prepend, or Prepend_Member to graft it
   --  into a larger tree. Those procedures also require the destination
   --  container to have room (Length < Natural'Last); Parse gives no such
   --  guarantee about the returned document itself, because a parsed
   --  array or object may hold up to Natural'Last elements. To add
   --  elements to a parsed container, check Types.Length against
   --  Natural'Last first.
   --  @param Object The parser holding the JSON text to parse
   --  @param Document The root of the parsed document, owned by the
   --    caller; standalone, so it may itself be added to another array
   --    or object
   --  @exception Parse_Error Raised if the JSON text is malformed

   function Has_Storage (Object : Parser) return Boolean;
   --  Return whether the parser currently owns heap memory (the copy of
   --  the JSON text it was created with)
   --  @param Object The parser to query
   --  @return True if the parser owns heap memory

   procedure Destroy (Object : in out Parser)
     with Always_Terminates,
          Post => not Has_Storage (Object),
          --  the destroyed parser is a constant, its old text reaching only
          --  the deallocation: callers that drop it are not dropping a
          --  computed value
          Depends => (Object => null, null => Object);
   --  Release the text owned by the parser; the parser then owns no heap
   --  memory
   --  @param Object The parser whose text is released

   Parse_Error : exception;
   --  Raised by Parse when the JSON text is malformed

private

   type Parser is limited record
      Stream        : Streams.Stream;
      Maximum_Depth : Positive := Default_Maximum_Depth;
   end record;

   function Has_Storage (Object : Parser) return Boolean
     is (Streams.Has_Storage (Object.Stream));
   --  Return whether the parser currently owns heap memory
   --  @param Object The parser to query
   --  @return True if the parser owns heap memory

end JSON.Parsers;
