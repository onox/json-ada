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

private with Ada.Containers.Indefinite_Holders;

with JSON.Types;
with JSON.Streams;

generic
   with package Types is new JSON.Types (<>);

   Default_Maximum_Depth : Positive := 10;

   Check_Duplicate_Keys  : Boolean  := False;
   --  If enabled, raise a Constraint_Error when an object contains
   --  duplicate keys. Parsing a JSON text will be slower if enabled.
package JSON.Parsers with SPARK_Mode => On is
   pragma Preelaborate;

   type Parser is tagged limited private;

   function Create
     (Stream        : Streams.Stream'Class;
      Maximum_Depth : Positive := Default_Maximum_Depth) return Parser;

   function Parse (Object : in out Parser) return Types.JSON_Value;

   Parse_Error : exception;

private

   package Stream_Holders is new Ada.Containers.Indefinite_Holders
     (Element_Type => Streams.Stream'Class, "=" => Streams."=");

   package Memory_Holders is new Ada.Containers.Indefinite_Holders
     (Element_Type => Types.Memory_Allocator, "=" => Types."=");

   type Parser is tagged limited record
      Stream    : Stream_Holders.Holder;
      Allocator : Memory_Holders.Holder;
   end record;

end JSON.Parsers;
