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

with Ada.Characters.Latin_1;
--  Big_Integers is an Ada 2022 standard unit; silence GNAT 16's advisory
--  (-gnatwi) about referencing it from this library's earlier language mode.
pragma Warnings (Off, "* is an Ada 2022 unit");
with Ada.Numerics.Big_Numbers.Big_Integers;
pragma Warnings (On, "* is an Ada 2022 unit");
with Ada.Unchecked_Deallocation;

package body JSON.Types with SPARK_Mode => On is

   use Ada.Numerics.Big_Numbers.Big_Integers;

   procedure Free_Text is new Ada.Unchecked_Deallocation
     (Object => String, Name => Streams.Text_Access);

   procedure Dealloc is new Ada.Unchecked_Deallocation
     (Object => JSON_Value, Name => JSON_Value_Access);

   function Size (V : access constant JSON_Value) return Big_Natural is
     (if V = null then
        To_Big_Integer (0)
      else
        To_Big_Integer (1) + Size (V.First_Child) + Size (V.Next))
   with Ghost, Subprogram_Variant => (Structural => V);
   --  Number of nodes reachable from V, following both children and
   --  siblings: the measure showing that freeing and reversing terminate.
   --  The SPARK ownership model guarantees the tree is acyclic and
   --  finite, so the recursion is well defined; its own termination is
   --  established by the structural variant.

   function To_Text (Value : String) return not null Streams.Text_Access is
      subtype Constrained_String is String (1 .. Value'Length);
   begin
      --  The conversion slides the bounds of Value to 1 .. Value'Length
      return new Constrained_String'(Constrained_String (Value));
   end To_Text;

   function Unescape (Text : String) return String is
      use Ada.Characters.Latin_1;

      Result : String (1 .. Text'Length) := (others => ' ');
      Last   : Natural := 0;

      --  Number of characters of Text already consumed as part of a
      --  multi-character escape sequence and still to be stepped over
      Skip : Natural := 0;

      function To_Hex_Digit (Value : Character) return Natural is
        (case Value is
           when '0' .. '9' => Character'Pos (Value) - Character'Pos ('0'),
           when 'a' .. 'f' => Character'Pos (Value) - Character'Pos ('a') + 10,
           when 'A' .. 'F' => Character'Pos (Value) - Character'Pos ('A') + 10,
           when others     => 0)
      with Post => To_Hex_Digit'Result <= 15;

      function Is_Hex_Quad_At (Index : Positive) return Boolean is
        (Index in Text'Range
           and then Text'Last - Index >= 3
           and then (for all J in Index .. Index + 3 =>
                       Text (J) in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F'));
      --  True if Text has 4 hexadecimal digits starting at Index

      function Hex_Quad_At (Index : Positive) return Natural is
        (((To_Hex_Digit (Text (Index)) * 16
           + To_Hex_Digit (Text (Index + 1))) * 16
           + To_Hex_Digit (Text (Index + 2))) * 16
           + To_Hex_Digit (Text (Index + 3)))
      with Pre  => Is_Hex_Quad_At (Index),
           Post => Hex_Quad_At'Result <= 16#FFFF#;
      --  Value of the 4 hexadecimal digits starting at Index, as a
      --  UTF-16 code unit

      procedure Append_Code_Point (Code : Natural)
        with Pre  => Code <= 16#10FFFF#
                       and then Last <= Result'Length - 4,
             Post => Last in Last'Old + 1 .. Last'Old + 4
                       and then (if Code <= 16#FFFF# then
                                   Last <= Last'Old + 3)
      --  Append the UTF-8 encoding (1 to 4 bytes) of a code point
      is
      begin
         if Code <= 16#7F# then
            Result (Last + 1) := Character'Val (Code);
            Last := Last + 1;
         elsif Code <= 16#7FF# then
            Result (Last + 1) := Character'Val (16#C0# + Code / 64);
            Result (Last + 2) := Character'Val (16#80# + Code mod 64);
            Last := Last + 2;
         elsif Code <= 16#FFFF# then
            Result (Last + 1) := Character'Val (16#E0# + Code / 4096);
            Result (Last + 2) := Character'Val (16#80# + (Code / 64) mod 64);
            Result (Last + 3) := Character'Val (16#80# + Code mod 64);
            Last := Last + 3;
         else
            Result (Last + 1) := Character'Val (16#F0# + Code / 262144);
            Result (Last + 2) := Character'Val (16#80# + (Code / 4096) mod 64);
            Result (Last + 3) := Character'Val (16#80# + (Code / 64) mod 64);
            Result (Last + 4) := Character'Val (16#80# + Code mod 64);
            Last := Last + 4;
         end if;
      end Append_Code_Point;
   begin
      for J in Text'Range loop
         pragma Loop_Invariant (Last <= J - Text'First + Skip);
         pragma Loop_Invariant (Skip <= Text'Last - J + 1);

         if Skip > 0 then
            Skip := Skip - 1;
         else
            declare
               C : constant Character := Text (J);
            begin
               if C = '\' and then J < Text'Last then
                  declare
                     E : constant Character := Text (J + 1);
                  begin
                     if E = 'u'
                       and then Text'Last - J >= 5
                       and then Is_Hex_Quad_At (J + 2)
                     then
                        declare
                           Unit : constant Natural := Hex_Quad_At (J + 2);
                        begin
                           if Unit in 16#D800# .. 16#DBFF#
                             and then Text'Last - J >= 11
                             and then Text (J + 6) = '\'
                             and then Text (J + 7) = 'u'
                             and then Is_Hex_Quad_At (J + 8)
                             and then Hex_Quad_At (J + 8) in 16#DC00# .. 16#DFFF#
                           then
                              --  A UTF-16 surrogate pair encoding a
                              --  code point beyond the Basic
                              --  Multilingual Plane
                              Append_Code_Point
                                (16#10000#
                                   + (Unit - 16#D800#) * 16#400#
                                   + (Hex_Quad_At (J + 8) - 16#DC00#));
                              Skip := 11;
                           elsif Unit in 16#D800# .. 16#DFFF# then
                              --  A lone surrogate is not a valid code
                              --  point; the tokenizer rejects it, but
                              --  emit U+FFFD (replacement character)
                              --  to keep this function total
                              Append_Code_Point (16#FFFD#);
                              Skip := 5;
                           else
                              Append_Code_Point (Unit);
                              Skip := 5;
                           end if;
                        end;
                     else
                        --  A single-character escape sequence (or a
                        --  malformed \u escape, which the tokenizer
                        --  rejects; the character is copied unchanged
                        --  to keep this function total)
                        Last := Last + 1;
                        Result (Last) :=
                          (case E is
                             when 'b'    => BS,
                             when 'f'    => FF,
                             when 'n'    => LF,
                             when 'r'    => CR,
                             when 't'    => HT,
                             when others => E);
                        Skip := 1;
                     end if;
                  end;
               elsif C = '\' then
                  --  A trailing \ at the end of the text, which the
                  --  tokenizer rejects; skip it to keep this function
                  --  total
                  null;
               else
                  Last := Last + 1;
                  Result (Last) := C;
               end if;
            end;
         end if;
      end loop;
      return Result (1 .. Last);
   end Unescape;

   -----------------------------------------------------------------------------
   --                                Observers                                --
   -----------------------------------------------------------------------------

   function Kind (Object : not null access constant JSON_Value) return Value_Kind
     is (Object.Kind);

   function Value (Object : not null access constant JSON_Value) return String
     is (if Object.Str = null then "" else Unescape (Object.Str.all));

   function Value (Object : not null access constant JSON_Value) return Integer_Type
     is (Object.Integer_Value);

   function Value (Object : not null access constant JSON_Value) return Float_Type
     is (if Object.Kind = Integer_Kind then
           Float_Type (Object.Integer_Value)
         else
           Object.Float_Value);

   function Value (Object : not null access constant JSON_Value) return Boolean
     is (Object.Boolean_Value);

   function Length (Object : not null access constant JSON_Value) return Natural
     is (Object.Length);

   function Get
     (Object : not null access constant JSON_Value;
      Index  : Positive) return access constant JSON_Value
   is
      Node  : access constant JSON_Value := Object.First_Child;
      Count : Positive := 1;
   begin
      while Node /= null loop
         pragma Loop_Invariant (Count <= Index);
         pragma Loop_Variant (Structural => Node);

         if Count = Index then
            return Node;
         end if;
         Count := Count + 1;
         Node := Node.Next;
      end loop;
      return null;
   end Get;

   function Get
     (Object : not null access constant JSON_Value;
      Key    : String) return access constant JSON_Value
   is
      Node : access constant JSON_Value := Object.First_Child;
   begin
      while Node /= null loop
         pragma Loop_Variant (Structural => Node);

         if Node.Key /= null and then Node.Key.all = Key then
            return Node;
         end if;
         Node := Node.Next;
      end loop;
      return null;
   end Get;

   function Contains
     (Object : not null access constant JSON_Value;
      Key    : String) return Boolean
   is (Get (Object, Key) /= null);

   -----------------------------------------------------------------------------
   --                                Iterating                                --
   -----------------------------------------------------------------------------

   function First (Object : not null access constant JSON_Value)
     return access constant JSON_Value
   is (Object.First_Child);

   function Next (Object : not null access constant JSON_Value)
     return access constant JSON_Value
   is (Object.Next);

   function Key (Object : not null access constant JSON_Value) return String
     is (if Object.Key = null then "" else Unescape (Object.Key.all));

   function Has_Key (Object : not null access constant JSON_Value) return Boolean
     is (Object.Key /= null);

   -----------------------------------------------------------------------------
   --                                  Image                                  --
   -----------------------------------------------------------------------------

   procedure Append_Image
     (Object : not null access constant JSON_Value;
      Result : in out Streams.String_Buffer)
   with Always_Terminates,
        Subprogram_Variant => (Structural => Object),
        Exceptional_Cases  => (Streams.Buffer_Overflow_Error => True);

   procedure Append_Image
     (Object : not null access constant JSON_Value;
      Result : in out Streams.String_Buffer) is
   begin
      case Object.Kind is
         when Array_Kind | Object_Kind =>
            Streams.Append
              (Result, (if Object.Kind = Array_Kind then "[" else "{"));
            declare
               Node          : access constant JSON_Value := Object.First_Child;
               First_Element : Boolean := True;
            begin
               while Node /= null loop
                  pragma Loop_Variant (Structural => Node);

                  if not First_Element then
                     Streams.Append (Result, ",");
                  end if;
                  First_Element := False;

                  if Object.Kind = Object_Kind then
                     Streams.Append (Result, """");
                     if Node.Key /= null then
                        Streams.Append (Result, Node.Key.all);
                     end if;
                     Streams.Append (Result, """:");
                  end if;

                  Append_Image (Node, Result);
                  Node := Node.Next;
               end loop;
            end;
            Streams.Append
              (Result, (if Object.Kind = Array_Kind then "]" else "}"));
         when String_Kind =>
            Streams.Append (Result, """");
            if Object.Str /= null then
               Streams.Append (Result, Object.Str.all);
            end if;
            Streams.Append (Result, """");
         when Integer_Kind =>
            declare
               Image : constant String := Integer_Type'Image (Object.Integer_Value);
            begin
               if Object.Integer_Value < 0 then
                  Streams.Append (Result, Image);
               else
                  Streams.Append (Result, Image (2 .. Image'Last));
               end if;
            end;
         when Float_Kind =>
            declare
               Image : constant String := Float_Type'Image (Object.Float_Value);
            begin
               if Object.Float_Value < 0.0 then
                  Streams.Append (Result, Image);
               else
                  Streams.Append (Result, Image (2 .. Image'Last));
               end if;
            end;
         when Boolean_Kind =>
            Streams.Append
              (Result, (if Object.Boolean_Value then "true" else "false"));
         when Null_Kind =>
            Streams.Append (Result, "null");
      end case;
   end Append_Image;

   procedure Image
     (Object : not null access constant JSON_Value;
      Result : in out Streams.String_Buffer) is
   begin
      Append_Image (Object, Result);
   end Image;

   -----------------------------------------------------------------------------
   --                              Constructors                               --
   -----------------------------------------------------------------------------

   function Is_Standalone (Object : not null access constant JSON_Value) return Boolean
     is (Object.Next = null and Object.Key = null);

   function Create_String (Value : String) return JSON_Value_Access is
     (new JSON_Value'(Kind => String_Kind, Str => To_Text (Value), others => <>));

   function Create_Integer (Value : Integer_Type) return JSON_Value_Access is
     (new JSON_Value'(Kind => Integer_Kind, Integer_Value => Value, others => <>));

   function Create_Float (Value : Float_Type) return JSON_Value_Access is
     (new JSON_Value'(Kind => Float_Kind, Float_Value => Value, others => <>));

   function Create_Boolean (Value : Boolean) return JSON_Value_Access is
     (new JSON_Value'(Kind => Boolean_Kind, Boolean_Value => Value, others => <>));

   function Create_Null return JSON_Value_Access is
     (new JSON_Value'(Kind => Null_Kind, others => <>));

   function Create_Array return JSON_Value_Access is
     (new JSON_Value'(Kind => Array_Kind, others => <>));

   function Create_Object return JSON_Value_Access is
     (new JSON_Value'(Kind => Object_Kind, others => <>));

   -----------------------------------------------------------------------------
   --                                Building                                 --
   -----------------------------------------------------------------------------

   procedure Append (Object : not null access JSON_Value; Value : in out JSON_Value_Access) is
   begin
      if Object.First_Child = null then
         Object.First_Child := Value;
      else
         declare
            Node : access JSON_Value := Object.First_Child;
         begin
            while Node.Next /= null loop
               pragma Loop_Invariant (Node /= null);
               pragma Loop_Variant (Structural => Node);
               Node := Node.Next;
            end loop;
            Node.Next := Value;
         end;
      end if;
      Value := null;
      Object.Length := Object.Length + 1;
   end Append;

   procedure Insert
     (Object : not null access JSON_Value;
      Key    : String;
      Value  : in out JSON_Value_Access) is
   begin
      Value.Key := To_Text (Key);
      if Object.First_Child = null then
         Object.First_Child := Value;
      else
         declare
            Node : access JSON_Value := Object.First_Child;
         begin
            while Node.Next /= null loop
               pragma Loop_Invariant (Node /= null);
               pragma Loop_Variant (Structural => Node);
               Node := Node.Next;
            end loop;
            Node.Next := Value;
         end;
      end if;
      Value := null;
      Object.Length := Object.Length + 1;
   end Insert;

   procedure Prepend (Object : not null access JSON_Value; Value : in out JSON_Value_Access) is
      Tmp : JSON_Value_Access := Object.First_Child;
   begin
      Object.First_Child := null;
      Value.Next := Tmp;
      Object.First_Child := Value;
      Value := null;
      Object.Length := Object.Length + 1;
   end Prepend;

   procedure Prepend_Member
     (Object : not null access JSON_Value;
      Key    : String;
      Value  : in out JSON_Value_Access)
   is
      Tmp : JSON_Value_Access := Object.First_Child;
   begin
      Object.First_Child := null;
      Value.Key := To_Text (Key);
      Value.Next := Tmp;
      Object.First_Child := Value;
      Value := null;
      Object.Length := Object.Length + 1;
   end Prepend_Member;

   procedure Reverse_Elements (Object : not null access JSON_Value) is
      Previous : JSON_Value_Access := null;
      Current  : JSON_Value_Access := Object.First_Child;
   begin
      Object.First_Child := null;
      while Current /= null loop
         pragma Loop_Variant (Decreases => Size (Current));
         declare
            Tmp : JSON_Value_Access := Current.Next;
         begin
            Current.Next := Previous;
            Previous := Current;
            Current := Tmp;
         end;
      end loop;
      Object.First_Child := Previous;
   end Reverse_Elements;

   --  Free_Node carries the Subprogram_Variant that Free cannot: the
   --  variant would have to appear on Free's declaration in the spec,
   --  where the ghost Size measure is not visible

   procedure Free_Node (Object : in out JSON_Value_Access)
   with Post               => Object = null,
        Always_Terminates,
        Subprogram_Variant => (Decreases => Size (Object)),
        --  matches Free's contract, so that neither this recursion nor Free
        --  reports the null it writes as an unused assignment
        Depends            => (Object => null, null => Object);

   procedure Free_Node (Object : in out JSON_Value_Access) is
      Node : JSON_Value_Access := Object;
      --  the tree, moved out of the parameter so that Object is nulled on
      --  every path and not only on the ones that iterate
   begin
      Object := null;

      while Node /= null loop
         pragma Loop_Invariant (Size (Node) <= Size (Node)'Loop_Entry);
         pragma Loop_Variant (Decreases => Size (Node));

         Free_Node (Node.First_Child);
         Free_Text (Node.Key);
         Free_Text (Node.Str);

         declare
            Rest : constant JSON_Value_Access := Node.Next;
         begin
            Node.Next := null;
            Dealloc (Node);
            Node := Rest;
         end;
      end loop;
   end Free_Node;

   procedure Free (Object : in out JSON_Value_Access) is
   begin
      Free_Node (Object);
   end Free;

end JSON.Types;
