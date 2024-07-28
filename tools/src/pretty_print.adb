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

procedure Pretty_Print is
   package ACL renames Ada.Command_Line;
   package TIO renames Ada.Text_IO;

   package Types   is new JSON.Types (Long_Integer, Long_Float);
   package Parsers is new JSON.Parsers (Types);

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

   File_Only : constant Boolean := ACL.Argument_Count = 1;
   Is_Quiet  : constant Boolean := ACL.Argument_Count = 2 and then ACL.Argument (1) = "-q";
begin
   if not (File_Only or Is_Quiet) then
      TIO.Put_Line ("Usage: [-q] <path to .json file>");
      ACL.Set_Exit_Status (ACL.Failure);
      return;
   end if;

   declare
      Parser : Parsers.Parser := Parsers.Create_From_File (ACL.Argument (ACL.Argument_Count));
      Value  : constant Types.JSON_Value := Parser.Parse;
   begin
      if not Is_Quiet then
         Print (Value, Indent => 4);
      end if;
   end;
end Pretty_Print;
