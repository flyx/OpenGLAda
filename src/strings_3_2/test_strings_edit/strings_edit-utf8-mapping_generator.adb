--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Mapping_Generator         Luebeck            --
--  Mapping generation                             Winter, 2008       --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--
--
--  This  procedure  adjusts  Strings_Edit.UTF8.Mapping.adb according to
--  the file unicodedata.txt. It is used as follows:
--
--      Strings_Edit-UTF8-Mapping_Generator <source-file> <data-file>
--
--   Here:
--
--      <source-file> is strings_edit.utf8.mapping.adb
--      <data-file>   is unicodedata.txt
--
--  The file UnicodeData.txt can be obtained from:
--
--     ftp://ftp.unicode.org/Public/UNIDATA/UnicodeData.txt
--
--  It is distributed by Unicode, Inc.
--
--     http://www.unicode.org
--
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Characters.Latin_1;   use Ada.Characters.Latin_1;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO;              use Ada.Text_IO;
with Strings_Edit.Quoted;      use Strings_Edit.Quoted;

with Strings_Edit.Integer_Edit;

procedure Strings_Edit.UTF8.Mapping_Generator is
   Item_Size : constant := 3;

   File    : File_Type;
   Line    : String (1..2048);
   Line_No : Natural := 0;
   Length  : Integer;
   Pointer : Integer;

   type Text_Buffer_State is (Prologue, Has_Index, Has_Data, Epilogue);
   type Text_Buffer_Index is range 1..4096;
   type Text_Buffer is array (Text_Buffer_Index) of Unbounded_String;

   Buffer   : Text_Buffer;
   Index_At : Text_Buffer_Index;
   Data_At  : Text_Buffer_Index;
   Free     : Text_Buffer_Index := Text_Buffer_Index'First;

   type Code_Position is range Code_Point'First..Code_Point'Last;
   package Code_Point_Edit is
      new Strings_Edit.Integer_Edit (Code_Position);
   use Code_Point_Edit;

   Size : Code_Position := 0;
   type Item;
   type Item_Ptr is access Item;
   type Triplet is record
      Code, Upper, Lower : Code_Position;
   end record;
   type Triplet_Array is array (Positive range 1..Item_Size) of Triplet;
   type Item is record
      Size : Positive := 1;
      List : Triplet_Array;
      Next : Item_Ptr;
   end record;
   Head : Item_Ptr;
   Tail : Item_Ptr;

   procedure Add_Line is
   begin
      if Free = Text_Buffer_Index'Last then
         Raise_Exception (Data_Error'Identity, "File is too long");
      end if;
      Buffer (Free) := To_Unbounded_String (Line (1..Length));
      Free := Free + 1;
   end Add_Line;

   procedure Read_Line is
   begin
      Get_Line (File, Line, Length);
      Line_No := Line_No + 1;
      if Length = Line'Last then
         Raise_Exception
         (  Data_Error'Identity,
            "Too long line" & Integer'Image (Line_No)
         );
      end if;
      while (  Length > Line'First
            and then
               (  Line (Length) = LF
               or else
                  Line (Length) = CR
            )  )
      loop
         Length := Length - 1;
      end loop;
      Line (Length + 1) := Character'Val (0);
      Pointer := Line'First;
   end Read_Line;

   procedure Get (Point : out Code_Position) is
   begin
      Get (Line, Pointer, SpaceAndTab);
      Get (Line, Pointer, Point, 16);
   exception
      when End_Error | Data_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            (  "Hexadecimal code point number is expected at"
            &  Integer'Image (Pointer)
         )  );
      when Constraint_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            (  "Hexadecimal code point number at"
            &  Integer'Image (Pointer)
            &  " is out of range"
         )  );
   end Get;

   procedure Get_Semicolon is
   begin
      Get (Line, Pointer, SpaceAndTab);
      if Pointer <= Length then
         if Line (Pointer) = ';' then
            Pointer := Pointer + 1;
         else
            Raise_Exception
            (  Data_Error'Identity,
               (  "Semicolon is expected at"
               &  Integer'Image (Pointer)
            )  );
         end if;
      end if;
   end Get_Semicolon;

begin
   if Argument_Count /= 2 then
      Put_Line
      (  Standard_Error,
         (  "Use: strings_edit-utf8-mapping_generator "
         &  "../strings_edit-utf8-mappings.adb unicodedata.txt"
      )  );
      Set_Exit_Status (Failure);
      return;
   end if;
   declare
      Output : constant String := Argument (1);
      Input  : constant String := Argument (2);
   begin
      begin
         Open (File, In_File, Output);
      exception
         when Name_Error =>
            Put_Line ("Cannot open " & Quote (Output));
            Set_Exit_Status (Failure);
            return;
         when Error : others =>
            Put_Line
            (  "Cannot open "
            &  Quote (Output)
            &  ": "
            &  Exception_Information (Error)
            );
            Set_Exit_Status (Failure);
            return;
      end;
      declare
         State : Text_Buffer_State := Prologue;
      begin
         --
         -- Reading the Strings_Edit.UTF8.Mappings file
         --
         Line_No := 0;
         loop
            Read_Line;
            case State is
               when Prologue =>
                  Get (Line, Pointer, SpaceAndTab);
                  if Is_Prefix
                     (  "type Categorization_Index is",
                        Line,
                        Pointer
                     )
                  then
                     State    := Has_Index;
                     Index_At := Free;
                  else
                     Add_Line;
                  end if;
               when Has_Index =>
                  Get (Line, Pointer, SpaceAndTab);
                  if Is_Prefix
                     (  "Mapping : constant Categorization_Array :=",
                        Line,
                        Pointer
                     )
                  then
                     State   := Has_Data;
                     Data_At := Free;
                  else
                     Add_Line;
                  end if;
               when Has_Data =>
                  Get (Line, Pointer, SpaceAndTab);
                  if Is_Prefix (");", Line, Pointer) then
                     State := Epilogue;
                  end if;
               when Epilogue =>
                  Add_Line;
            end case;
         end loop;
      exception
         when End_Error =>
            Close (File);
            if State /= Epilogue then
               Put_Line
               (  "File "
               &  Quote (Output)
               &  " is not recognized as "
               &  "Strings_Edit.UTF8.Mapping source"
               );
               return;
            end if;
         when Error : Data_Error =>
            Put_Line
            (  "Malformed "
            &  Quote (Output)
            &  " (Line"
            &  Integer'Image (Line_No)
            &  ") "
            &  Exception_Message (Error)
            );
            Set_Exit_Status (Failure);
            return;
         when Error : others =>
            Put_Line
            (  "Internal error while reading "
            &  Quote (Output)
            &  " (Line"
            &  Integer'Image (Line_No)
            &  ") "
            &  Exception_Information (Error)
            );
            Set_Exit_Status (Failure);
            return;
      end;
      --
      -- Dealing with UnicodeData.txt
      --
      begin
         Open (File, In_File, Input);
      exception
         when Name_Error =>
            Put_Line ("Cannot open " & Quote (Input));
            Set_Exit_Status (Failure);
            return;
         when Error : others =>
            Put_Line
            (  "Cannot open "
            &  Quote (Input)
            &  ": "
            &  Exception_Information (Error)
            );
            Set_Exit_Status (Failure);
            return;
      end;
      declare
         This      : Triplet;
         Has_Upper : Boolean;
         Has_Lower : Boolean;
         Has_Case  : Boolean;
      begin
         --
         -- Reading the UnicodeData file
         --
         Line_No := 0;
         loop
            Read_Line;
            Get (Line, Pointer, SpaceAndTab);
            if Pointer <= Length then
               Get (This.Code);
               for Semicolon in 1..2 loop
                  Get (Line, Pointer);
                  while Pointer < Length and then Line (Pointer) /= ';'
                  loop
                     Pointer := Pointer + 1;
                  end loop;
                  Get_Semicolon;
               end loop;
               Get (Line, Pointer, SpaceAndTab);
               if Pointer + 1 > Length then
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "General category specification is expected at"
                     &  Integer'Image (Pointer)
                  )  );
               end if;
               Pointer := Pointer + 2;
               case To_Lower (Line (Pointer - 2)) is
                  when 'l' =>
                     case To_Lower (Line (Pointer - 1)) is
                        when 'u' | 'l' => Has_Case := True;
                        when others    => Has_Case := False;
                     end case;
                  when others =>
                     Has_Case := False;
               end case;
               Get_Semicolon;
               for Semicolon in 4..12 loop
                  Get (Line, Pointer);
                  while Pointer < Length and then Line (Pointer) /= ';'
                  loop
                     Pointer := Pointer + 1;
                  end loop;
                  Get_Semicolon;
               end loop;
               Get (Line, Pointer, SpaceAndTab);
               if Pointer > Length or else Line (Pointer) = ';' then
                  Has_Upper  := False;
                  This.Upper := This.Code;
                  Pointer    := Pointer + 1;
               else
                  Get (This.Upper);
                  Get_Semicolon;
                  Has_Upper := True;
               end if;
               if Pointer > Length or else Line (Pointer) = ';' then
                  Has_Lower  := False;
                  This.Lower := This.Code;
               else
                  Get (This.Lower);
                  Get_Semicolon;
                  Has_Lower := True;
               end if;
               if Has_Case or Has_Upper or Has_Lower then
                  if Tail = null then
                     Head := new Item;
                     Tail := Head;
                     Tail.List (1) := This;
                  elsif Tail.Size < Item_Size then
                     Tail.Size := Tail.Size + 1;
                     Tail.List (Tail.Size) := This;
                  else
                     Tail.Next := new Item;
                     Tail := Tail.Next;
                     Tail.List (1) := This;
                  end if;
                  Size := Size + 1;
               end if;
            end if;
         end loop;
      exception
         when End_Error =>
            Close (File);
         when Error : Data_Error =>
            Put_Line
            (  "Malformed "
            &  Quote (Input)
            &  " (Line"
            &  Integer'Image (Line_No)
            &  ") "
            &  Exception_Message (Error)
            );
            Set_Exit_Status (Failure);
            return;
         when Error : others =>
            Put_Line
            (  "Internal error while reading "
            &  Quote (Input)
            &  " (Line"
            &  Integer'Image (Line_No)
            &  ") "
            &  Integer'Image (Line_No)
            &  Exception_Information (Error)
            );
            Set_Exit_Status (Failure);
            return;
      end;
      --
      -- Writing the output
      --
      begin
         Open (File, Out_File, Output);
      exception
         when Name_Error =>
            Put_Line ("Cannot open " & Quote (Output) & " for write");
            Set_Exit_Status (Failure);
            return;
         when Error : others =>
            Put_Line
            (  "Cannot open "
            &  Quote (Output)
            &  ": "
            &  Exception_Information (Error)
            );
            Set_Exit_Status (Failure);
            return;
      end;
      begin
         for Line in 1..Index_At - 1 loop
            Put_Line (File, To_String (Buffer (Line)));
         end loop;
         Put_Line
         (  File,
            (  "   type Categorization_Index is range "
            &  "1.."
            &  Image (Size)
            &  ";"
         )  );
         for Line in Index_At..Data_At - 1 loop
            Put_Line (File, To_String (Buffer (Line)));
         end loop;
         Put_Line
         (  File,
            "   Mapping : constant Categorization_Array :="
         );
         Put_Line (File, "   (");
         Tail := Head;
         while Tail /= null loop
            Put (File, "      ");
            for Index in 1..Tail.Size loop
               Put (File, "(16#");
               Put (File, Image (Tail.List (Index).Code,  16));
               Put (File, "#,16#");
               Put (File, Image (Tail.List (Index).Upper, 16));
               Put (File, "#,16#");
               Put (File, Image (Tail.List (Index).Lower, 16));
               if Size > 1 then
                  Put (File, "#),");
                  Size := Size - 1;
               else
                  Put (File, "#)");
               end if;
            end loop;
            Tail := Tail.Next;
            New_Line (File);
         end loop;
         Put_Line (File, "   );");
         for Line in Data_At..Free - 1 loop
            Put_Line (File, To_String (Buffer (Line)));
         end loop;
         Close (File);
      exception
         when Error : others =>
            Put_Line
            (  "Cannot write "
            &  Quote (Output)
            &  ": "
            &  Exception_Information (Error)
            );
            Set_Exit_Status (Failure);
            return;
      end;
   end;
   Set_Exit_Status (Success);
exception
   when Error : others =>
      Put ("Error :");
      Put_Line (Exception_Information (Error));
end Strings_Edit.UTF8.Mapping_Generator;
