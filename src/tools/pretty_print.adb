--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2020 onox <denkpadje@gmail.com>
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

with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with JSON.Types;
with JSON.Parsers;
with JSON.Streams;

procedure Pretty_Print is
   package Types   is new JSON.Types (Long_Integer, Long_Float);
   package Parsers is new JSON.Parsers (Types);

   Text : aliased JSON.Streams.AS.Stream_Element_Array :=
     JSON.Streams.Get_Stream_Element_Array (Ada.Command_Line.Argument (1));

   Stream    : JSON.Streams.Stream'Class := JSON.Streams.Create_Stream (Text'Access);
   Allocator : Types.Memory_Allocator (Maximum_Depth => 10);
   Value     : constant Types.JSON_Value := Parsers.Parse (Stream, Allocator);

   type Indent_Type is range 2 .. 8;

   procedure Print
     (Value  : Types.JSON_Value;
      Indent : Indent_Type := 4;
      Level  : Positive    := 1)
   is
      use all type Types.Value_Kind;
      use Ada.Strings.Fixed;

      Index  : Positive := 1;
      Spaces : constant Natural := Natural (Indent);
   begin
      case Value.Kind is
         when Object_Kind =>
            if Value.Length > 0 then
               Ada.Text_IO.Put_Line ("{");

               for E of Value loop
                  if Index > 1 then
                     Ada.Text_IO.Put_Line (",");
                  end if;

                  --  Print key and element
                  Ada.Text_IO.Put (Spaces * Level * ' ' & E.Image & ": ");
                  Print (Value (E.Value), Indent, Level + 1);

                  Index := Index + 1;
               end loop;

               Ada.Text_IO.New_Line;
               Ada.Text_IO.Put (Spaces * (Level - 1) * ' ' & "}");
            else
               Ada.Text_IO.Put ("{}");
            end if;
         when Array_Kind =>
            if Value.Length > 0 then
               Ada.Text_IO.Put_Line ("[");

               for E of Value loop
                  if Index > 1 then
                     Ada.Text_IO.Put_Line (",");
                  end if;

                  --  Print element
                  Ada.Text_IO.Put (Spaces * Level * ' ');
                  Print (E, Indent, Level + 1);

                  Index := Index + 1;
               end loop;

               Ada.Text_IO.New_Line;
               Ada.Text_IO.Put (Spaces * (Level - 1) * ' ' & "]");
            else
               Ada.Text_IO.Put ("[]");
            end if;
         when others =>
            Ada.Text_IO.Put (Value.Image);
      end case;
   end Print;
begin
   Print (Value, Indent => 4);
end Pretty_Print;
