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

with JSON.Tokenizers;

package body JSON.Parsers with SPARK_Mode => On is

   pragma Warnings
     (GNATprove, Off, "*is set by ""Parse_Value"" but not used after the call",
      Reason => "only Status matters when the parsed value is not used");
   pragma Warnings
     (GNATprove, Off, "*is set by ""Scan_Token"" but not used after the call",
      Reason => "only the status of the final EOF check matters");

   package Tokenizers is new JSON.Tokenizers (Types);

   use type Tokenizers.Token_Kind;
   use type Tokenizers.Token_Status;
   use type Types.Value_Kind;

   type Parse_Status is
     (Ok,
      Tokenizer_Failed,
      Unexpected_Token,
      Expected_Value_Separator,
      Expected_Name_Separator,
      Expected_String_Key,
      Duplicate_Key,
      Depth_Exceeded,
      Too_Many_Elements);

   function Message
     (Status : Parse_Status;
      Detail : Tokenizers.Token_Status) return String
   with Pre => Status /= Ok;

   function Message
     (Status : Parse_Status;
      Detail : Tokenizers.Token_Status) return String
   is (case Status is
         when Tokenizer_Failed =>
           (if Detail /= Tokenizers.Success then
              Tokenizers.Error_Message (Detail)
            else
              "Tokenizer error"),
         when Unexpected_Token =>
           "Unexpected token",
         when Expected_Value_Separator =>
           "Expected value separator (',' character)",
         when Expected_Name_Separator =>
           "Expected name separator (':' character)",
         when Expected_String_Key =>
           "Expected key to be a string",
         when Duplicate_Key =>
           "JSON object contains duplicate key",
         when Depth_Exceeded =>
           "Maximum depth exceeded",
         when Too_Many_Elements =>
           "Too many elements or members",
         when Ok =>
           raise Program_Error);

   --  Parse_Value, Parse_Array, and Parse_Object report failures via
   --  Status instead of raising an exception, so that each level of the
   --  recursion can release the partial tree it owns; procedure Parse
   --  raises Parse_Error at the top

   procedure Parse_Value
     (Stream        : in out Streams.Stream;
      Current       : Tokenizers.Token;
      Depth         : Positive;
      Maximum_Depth : Positive;
      Result        : out Types.JSON_Value_Access;
      Status        : out Parse_Status;
      Detail        : out Tokenizers.Token_Status)
   with Pre  =>
     Depth <= Maximum_Depth
       and then (if Current.Kind = Tokenizers.String_Token then
                   Streams.Valid_Span
                     (Stream, Current.String_Offset, Current.String_Length)),
        Post =>
     Streams.Length (Stream) = Streams.Length (Stream)'Old
       and then Streams.Position (Stream) >= Streams.Position (Stream)'Old
       and then (if Streams.Has_Buffered_Character (Stream)
                     and then Streams.Position (Stream) = Streams.Position (Stream)'Old
                 then
                   Streams.Has_Buffered_Character (Stream)'Old)
       and then (if Status = Ok then
                   Result /= null and then Types.Is_Standalone (Result)
                 else
                   Result = null),
        Always_Terminates,
        Subprogram_Variant =>
          (Decreases => Maximum_Depth - Depth, Decreases => 0);

   procedure Parse_Array
     (Stream        : in out Streams.Stream;
      Depth         : Positive;
      Maximum_Depth : Positive;
      Result        : out Types.JSON_Value_Access;
      Status        : out Parse_Status;
      Detail        : out Tokenizers.Token_Status)
   with Pre  => Depth <= Maximum_Depth,
        Post =>
     Streams.Length (Stream) = Streams.Length (Stream)'Old
       and then Streams.Position (Stream) >= Streams.Position (Stream)'Old
       and then (if Streams.Has_Buffered_Character (Stream)
                     and then Streams.Position (Stream) = Streams.Position (Stream)'Old
                 then
                   Streams.Has_Buffered_Character (Stream)'Old)
       and then (if Status = Ok then
                   Result /= null and then Types.Is_Standalone (Result)
                 else
                   Result = null),
        Always_Terminates,
        Subprogram_Variant =>
          (Decreases => Maximum_Depth - Depth, Decreases => 1);

   procedure Parse_Object
     (Stream        : in out Streams.Stream;
      Depth         : Positive;
      Maximum_Depth : Positive;
      Result        : out Types.JSON_Value_Access;
      Status        : out Parse_Status;
      Detail        : out Tokenizers.Token_Status)
   with Pre  => Depth <= Maximum_Depth,
        Post =>
     Streams.Length (Stream) = Streams.Length (Stream)'Old
       and then Streams.Position (Stream) >= Streams.Position (Stream)'Old
       and then (if Streams.Has_Buffered_Character (Stream)
                     and then Streams.Position (Stream) = Streams.Position (Stream)'Old
                 then
                   Streams.Has_Buffered_Character (Stream)'Old)
       and then (if Status = Ok then
                   Result /= null and then Types.Is_Standalone (Result)
                 else
                   Result = null),
        Always_Terminates,
        Subprogram_Variant =>
          (Decreases => Maximum_Depth - Depth, Decreases => 1);

   procedure Parse_Value
     (Stream        : in out Streams.Stream;
      Current       : Tokenizers.Token;
      Depth         : Positive;
      Maximum_Depth : Positive;
      Result        : out Types.JSON_Value_Access;
      Status        : out Parse_Status;
      Detail        : out Tokenizers.Token_Status) is
   begin
      Result := null;
      Status := Ok;
      Detail := Tokenizers.Success;

      case Current.Kind is
         when Tokenizers.Begin_Array_Token =>
            if Depth >= Maximum_Depth then
               Status := Depth_Exceeded;
            else
               Parse_Array
                 (Stream, Depth + 1, Maximum_Depth, Result, Status, Detail);
            end if;
         when Tokenizers.Begin_Object_Token =>
            if Depth >= Maximum_Depth then
               Status := Depth_Exceeded;
            else
               Parse_Object
                 (Stream, Depth + 1, Maximum_Depth, Result, Status, Detail);
            end if;
         when Tokenizers.String_Token =>
            Result := Types.Create_String
              (Streams.Get_String
                 (Stream, Current.String_Offset, Current.String_Length));
         when Tokenizers.Integer_Token =>
            Result := Types.Create_Integer (Current.Integer_Value);
         when Tokenizers.Float_Token =>
            Result := Types.Create_Float (Current.Float_Value);
         when Tokenizers.Boolean_Token =>
            Result := Types.Create_Boolean (Current.Boolean_Value);
         when Tokenizers.Null_Token =>
            Result := Types.Create_Null;
         when others =>
            Status := Unexpected_Token;
      end case;
   end Parse_Value;

   procedure Parse_Array
     (Stream        : in out Streams.Stream;
      Depth         : Positive;
      Maximum_Depth : Positive;
      Result        : out Types.JSON_Value_Access;
      Status        : out Parse_Status;
      Detail        : out Tokenizers.Token_Status)
   is
      Container : Types.JSON_Value_Access := Types.Create_Array;
      Element   : Types.JSON_Value_Access := null;

      Token        : Tokenizers.Token;
      Token_Status : Tokenizers.Token_Status;

      Repeat : Boolean := False;
   begin
      Result := null;
      Status := Ok;
      Detail := Tokenizers.Success;

      loop
         pragma Loop_Invariant (Container /= null);
         pragma Loop_Invariant (Types.Kind (Container) = Types.Array_Kind);
         pragma Loop_Invariant (Types.Is_Standalone (Container));
         pragma Loop_Invariant (Element = null);
         pragma Loop_Invariant (Status = Ok);
         pragma Loop_Invariant
           (Streams.Length (Stream) = Streams.Length (Stream)'Loop_Entry);
         pragma Loop_Invariant
           (Streams.Position (Stream) >= Streams.Position (Stream)'Loop_Entry);
         pragma Loop_Invariant
           (if Streams.Has_Buffered_Character (Stream)
              and then Streams.Position (Stream) = Streams.Position (Stream)'Loop_Entry
            then Streams.Has_Buffered_Character (Stream)'Loop_Entry);
         pragma Loop_Variant
           (Decreases =>
              Streams.Length (Stream) - Streams.Position (Stream),
            Decreases =>
              (if Streams.Has_Buffered_Character (Stream) then 1 else 0));

         Tokenizers.Scan_Token (Stream, Token, Token_Status);
         if Token_Status /= Tokenizers.Success then
            Types.Free (Container);
            Status := Tokenizer_Failed;
            Detail := Token_Status;
            return;
         end if;

         --  Either expect ']' character or (if not the first element)
         --  a value separator (',' character)
         if Token.Kind = Tokenizers.End_Array_Token then
            exit;
         elsif Repeat then
            if Token.Kind /= Tokenizers.Value_Separator_Token then
               Types.Free (Container);
               Status := Expected_Value_Separator;
               return;
            end if;

            --  Value separator has been read, now read the next value
            Tokenizers.Scan_Token (Stream, Token, Token_Status);
            if Token_Status /= Tokenizers.Success then
               Types.Free (Container);
               Status := Tokenizer_Failed;
               Detail := Token_Status;
               return;
            end if;
         end if;

         --  Parse the value and prepend it to the array
         Parse_Value
           (Stream, Token, Depth, Maximum_Depth, Element, Status, Detail);
         if Status /= Ok then
            Types.Free (Container);
            return;
         end if;

         if Types.Length (Container) = Natural'Last then
            Types.Free (Container);
            Types.Free (Element);
            Status := Too_Many_Elements;
            return;
         end if;
         Types.Prepend (Container, Element);

         Repeat := True;
      end loop;

      Types.Reverse_Elements (Container);
      Result := Container;
   end Parse_Array;

   procedure Parse_Object
     (Stream        : in out Streams.Stream;
      Depth         : Positive;
      Maximum_Depth : Positive;
      Result        : out Types.JSON_Value_Access;
      Status        : out Parse_Status;
      Detail        : out Tokenizers.Token_Status)
   is
      Container : Types.JSON_Value_Access := Types.Create_Object;
      Element   : Types.JSON_Value_Access := null;

      Token        : Tokenizers.Token;
      Token_Status : Tokenizers.Token_Status;

      Repeat : Boolean := False;
   begin
      Result := null;
      Status := Ok;
      Detail := Tokenizers.Success;

      loop
         pragma Loop_Invariant (Container /= null);
         pragma Loop_Invariant (Types.Kind (Container) = Types.Object_Kind);
         pragma Loop_Invariant (Types.Is_Standalone (Container));
         pragma Loop_Invariant (Element = null);
         pragma Loop_Invariant (Status = Ok);
         pragma Loop_Invariant
           (Streams.Length (Stream) = Streams.Length (Stream)'Loop_Entry);
         pragma Loop_Invariant
           (Streams.Position (Stream) >= Streams.Position (Stream)'Loop_Entry);
         pragma Loop_Invariant
           (if Streams.Has_Buffered_Character (Stream)
              and then Streams.Position (Stream) = Streams.Position (Stream)'Loop_Entry
            then Streams.Has_Buffered_Character (Stream)'Loop_Entry);
         pragma Loop_Variant
           (Decreases =>
              Streams.Length (Stream) - Streams.Position (Stream),
            Decreases =>
              (if Streams.Has_Buffered_Character (Stream) then 1 else 0));

         Tokenizers.Scan_Token (Stream, Token, Token_Status);
         if Token_Status /= Tokenizers.Success then
            Types.Free (Container);
            Status := Tokenizer_Failed;
            Detail := Token_Status;
            return;
         end if;

         --  Either expect '}' character or (if not the first member)
         --  a value separator (',' character)
         if Token.Kind = Tokenizers.End_Object_Token then
            exit;
         elsif Repeat then
            if Token.Kind /= Tokenizers.Value_Separator_Token then
               Types.Free (Container);
               Status := Expected_Value_Separator;
               return;
            end if;

            --  Value separator has been read, now read the next member
            Tokenizers.Scan_Token (Stream, Token, Token_Status);
            if Token_Status /= Tokenizers.Success then
               Types.Free (Container);
               Status := Tokenizer_Failed;
               Detail := Token_Status;
               return;
            end if;
         end if;

         --  Parse the member key
         if Token.Kind /= Tokenizers.String_Token then
            Types.Free (Container);
            Status := Expected_String_Key;
            return;
         end if;

         declare
            Member_Key : constant String := Streams.Get_String
              (Stream, Token.String_Offset, Token.String_Length);
         begin
            if Check_Duplicate_Keys
              and then Types.Contains (Container, Member_Key)
            then
               Types.Free (Container);
               Status := Duplicate_Key;
               return;
            end if;

            --  Expect a name separator (':' character) between the key
            --  and the value
            Tokenizers.Scan_Token (Stream, Token, Token_Status);
            if Token_Status /= Tokenizers.Success then
               Types.Free (Container);
               Status := Tokenizer_Failed;
               Detail := Token_Status;
               return;
            end if;

            if Token.Kind /= Tokenizers.Name_Separator_Token then
               Types.Free (Container);
               Status := Expected_Name_Separator;
               return;
            end if;

            --  Parse the member value and prepend the member
            Tokenizers.Scan_Token (Stream, Token, Token_Status);
            if Token_Status /= Tokenizers.Success then
               Types.Free (Container);
               Status := Tokenizer_Failed;
               Detail := Token_Status;
               return;
            end if;

            Parse_Value
              (Stream, Token, Depth, Maximum_Depth, Element, Status, Detail);
            if Status /= Ok then
               Types.Free (Container);
               return;
            end if;

            if Types.Length (Container) = Natural'Last then
               Types.Free (Container);
               Types.Free (Element);
               Status := Too_Many_Elements;
               return;
            end if;
            Types.Prepend_Member (Container, Member_Key, Element);
         end;

         Repeat := True;
      end loop;

      Types.Reverse_Elements (Container);
      Result := Container;
   end Parse_Object;

   procedure Create
     (Object        : out Parser;
      Text          : String;
      Maximum_Depth : Positive := Default_Maximum_Depth)
   is
   begin
      Object.Maximum_Depth := Maximum_Depth;
      Streams.From_Text (Object.Stream, Text);
   end Create;

   procedure Create_From_File
     (Object        : out Parser;
      File_Name     : String;
      Maximum_Depth : Positive := Default_Maximum_Depth)
   is
   begin
      Object.Maximum_Depth := Maximum_Depth;
      Streams.From_File (Object.Stream, File_Name);
   end Create_From_File;

   procedure Parse
     (Object   : in out Parser;
      Document : aliased out Types.JSON_Value_Access)
   is
      Token        : Tokenizers.Token;
      Token_Status : Tokenizers.Token_Status;

      Root   : Types.JSON_Value_Access;
      Status : Parse_Status;
      Detail : Tokenizers.Token_Status;
   begin
      Document := null;

      Tokenizers.Scan_Token (Object.Stream, Token, Token_Status);
      if Token_Status /= Tokenizers.Success then
         raise Parse_Error with Tokenizers.Error_Message (Token_Status);
      end if;

      Parse_Value
        (Object.Stream, Token, 1, Object.Maximum_Depth, Root, Status, Detail);
      if Status /= Ok then
         raise Parse_Error with Message (Status, Detail);
      end if;

      Tokenizers.Scan_Token
        (Object.Stream, Token, Token_Status, Expect_EOF => True);
      if Token_Status /= Tokenizers.Success then
         Types.Free (Root);
         raise Parse_Error with Tokenizers.Error_Message (Token_Status);
      end if;

      Document := Root;
   end Parse;

   procedure Destroy (Object : in out Parser) is
   begin
      Streams.Destroy (Object.Stream);
      Object.Maximum_Depth := Default_Maximum_Depth;
      --  back to the record's default: Create sets the depth, so no component
      --  holds a value from before the call, as the Depends contract states
   end Destroy;

end JSON.Parsers;
