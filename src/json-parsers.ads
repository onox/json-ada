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

with JSON.Types;
with JSON.Streams;

generic
   with package Types is new JSON.Types (<>);

   Check_Duplicate_Keys  : Boolean  := False;
   --  If enabled, raise a Constraint_Error when an object contains
   --  duplicate keys. Parsing a JSON text will be slower if enabled.
package JSON.Parsers with SPARK_Mode => On is
   pragma Preelaborate;

   function Parse
     (Stream    : aliased in out Streams.Stream'Class;
      Allocator : aliased in out Types.Memory_Allocator) return Types.JSON_Value;

   Parse_Error : exception;

end JSON.Parsers;
