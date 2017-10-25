--                                                                    --
--  package Strings_Edit            Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2000       --
--                                                                    --
--                                Last revision :  10:24 26 Dec 2009  --
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

with Ada.IO_Exceptions;  use Ada.IO_Exceptions;

package body Strings_Edit is

   function GetDigit (Symbol : Character) return Natural is
   begin
      case Symbol is
         when '0' => return 0;
         when '1' => return 1;
         when '2' => return 2;
         when '3' => return 3;
         when '4' => return 4;
         when '5' => return 5;
         when '6' => return 6;
         when '7' => return 7;
         when '8' => return 8;
         when '9' => return 9;
         when 'A' | 'a' => return 10;
         when 'B' | 'b' => return 11;
         when 'C' | 'c' => return 12;
         when 'D' | 'd' => return 13;
         when 'E' | 'e' => return 14;
         when 'F' | 'f' => return 15;
         when others => return 16;
      end case;
   end GetDigit;

   function Is_Prefix (Prefix, Source : String) return Boolean is
   begin
      return
      (  Prefix'Length = 0
      or else
         (  Prefix'Length <= Source'Length
         and then
            (  Prefix
            =  Source
               (  Source'First
               .. Source'First + Prefix'Length - 1
      )  )  )  );
   end Is_Prefix;

   function Is_Prefix (Prefix, Source : String; Pointer : Integer)
      return Boolean is
   begin
      return
      (  Pointer >= Source'First
      and then
         (  Pointer <= Source'Last
         or else
            Pointer = Source'Last + 1
         )
      and then
         Source'Last - Pointer + 1 >= Prefix'Length
      and then
         Prefix = Source (Pointer..Pointer + Prefix'Length - 1)
      );
   end Is_Prefix;

   function Is_Prefix
            (  Prefix, Source : String;
               Map            : Character_Mapping
            )  return Boolean is
   begin
      if Prefix'Length = 0 then
         return True;
      elsif Prefix'Length > Source'Length then
         return False;
      end if;
      declare
         J : Integer := Source'First;
      begin
         for I in Prefix'First..Prefix'Last - 1 loop
            if Value (Map, Prefix (I)) /= Value (Map, Source (J)) then
               return False;
            end if;
            J := J + 1;
         end loop;
         return
            Value (Map, Prefix (Prefix'Last)) = Value (Map, Source (J));
      end;
   end Is_Prefix;

   function Is_Prefix
            (  Prefix, Source : String;
               Pointer        : Integer;
               Map            : Character_Mapping
            )  return Boolean is
   begin
      if (  Pointer < Source'First
         or else
            (  Pointer > Source'Last
            and then
               Pointer /= Source'Last + 1
            )
         or else
            Source'Last - Pointer + 1 < Prefix'Length
         )
      then
         return False;
      end if;
      declare
         J : Integer := Pointer;
      begin
         for I in Prefix'First..Prefix'Last - 1 loop
            if Value (Map, Prefix (I)) /= Value (Map, Source (J)) then
               return False;
            end if;
            J := J + 1;
         end loop;
         return
            Value (Map, Prefix (Prefix'Last)) = Value (Map, Source (J));
      end;
   end Is_Prefix;
--
-- Text_Edit
--
-- This is an internal package containing  implementation  of  all  text
-- editing subprograms.
--
   package Text_Edit is
      function TrimCharacter
               (  Source : String;
                  Blank  : Character := ' '
               )  return String;
      function TrimSet
               (  Source : String;
                  Blanks : Character_Set
               )  return String;
      procedure GetCharacter
                (  Source  : String;
                   Pointer : in out Integer;
                   Blank   : Character := ' '
                );
      procedure GetSet
                (  Source  : String;
                   Pointer : in out Integer;
                   Blanks  : Character_Set
                );
      procedure PutString
                (  Destination : in out String;
                   Pointer     : in out Integer;
                   Value       : String;
                   Field       : Natural   := 0;
                   Justify     : Alignment := Left;
                   Fill        : Character := ' '
                );
      procedure PutCharacter
                (  Destination : in out String;
                   Pointer     : in out Integer;
                   Value       : Character;
                   Field       : Natural   := 0;
                   Justify     : Alignment := Left;
                   Fill        : Character := ' '
                );
   end Text_Edit;
   package body Text_Edit is separate;

   function Trim
            (  Source : String;
               Blank  : Character := ' '
            )  return String renames Text_Edit.TrimCharacter;
   function Trim
            (  Source : String;
               Blanks : Character_Set
            )  return String renames Text_Edit.TrimSet;
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Blank   : Character := ' '
             )  renames Text_Edit.GetCharacter;
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Blanks  : Character_Set
             )  renames Text_Edit.GetSet;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : String;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  renames Text_Edit.PutString;
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Character;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  renames Text_Edit.PutCharacter;

end Strings_Edit;
