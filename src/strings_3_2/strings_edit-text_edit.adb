--                                                                    --
--  package Strings_Edit            Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--  Strings_Edit.Text_Edit                         Spring, 2000       --
--                                                                    --
--                                Last revision :  22:44 07 Apr 2016  --
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

separate (Strings_Edit)

package body Text_Edit is
   function TrimCharacter
            (  Source : in String;
               Blank  : in Character := ' '
            )  return String is
      First : Integer := Source'First;
      Last  : Integer := Source'Last;
   begin
      while First <= Last and then Source (First) = Blank loop
         First := First + 1;
      end loop;
      while First <= Last and then Source (Last) = Blank loop
         Last := Last - 1;
      end loop;
      return Source (First..Last);
   end TrimCharacter;
   
   function TrimSet
            (  Source : in String;
               Blanks : in Ada.Strings.Maps.Character_Set
            )  return String is
      First : Integer := Source'First;
      Last  : Integer := Source'Last;
   begin
      while (  First <= Last
            and then
               Ada.Strings.Maps.Is_In (Source (First), Blanks)
            )
      loop
         First := First + 1;
      end loop;
      while (  First <= Last
            and then
               Ada.Strings.Maps.Is_In (Source (Last), Blanks)
            )
      loop
         Last := Last - 1;
      end loop;
      return Source (First..Last);
   end TrimSet;

   procedure GetCharacter
             (  Source  : in String;
                Pointer : in out Integer;
                Blank   : in Character := ' '
             )  is
   begin
      if (  Pointer < Source'First
         or else
            (  Pointer > Source'Last
            and then
               Pointer - 1 > Source'Last
         )  )
      then
         raise Layout_Error;
      end if;
      for Index in Pointer..Source'Last loop
         exit when Source (Index) /= Blank;
         Pointer := Pointer + 1;
      end loop;
   end GetCharacter;

   procedure GetSet
             (  Source  : in String;
                Pointer : in out Integer;
                Blanks  : in Ada.Strings.Maps.Character_Set
             )  is
   begin
      if (  Pointer < Source'First
         or else
            (  Pointer > Source'Last
            and then
               Pointer - 1 > Source'Last
         )  )
      then
         raise Layout_Error;
      end if;
      for Index in Pointer..Source'Last loop
         exit when not Ada.Strings.Maps.Is_In (Source (Index), Blanks);
         Pointer := Pointer + 1;
      end loop;
   end GetSet;

   procedure PutString
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : in String;
                Field       : in Natural := 0;
                Justify     : in Alignment := Left;
                Fill        : in Character := ' '
             )  is
      OutField : Natural := Field;
   begin
      if OutField = 0 then
         OutField := Value'Length;
      end if;
      if (  Pointer < Destination'First
         or else
            Pointer + OutField - 1 > Destination'Last
         or else
            OutField < Value'Length
         )
      then
         raise Layout_Error;
      end if;
      if OutField /= 0 then
         if OutField = Value'Length then
            Destination (Pointer..Pointer + OutField - 1) := Value;
         else
            declare
               First : Integer;
               Next  : Integer;
            begin
               case Justify is
                  when Left =>
                     First := Pointer;
                     Next  := First + Value'Length;
                     for Position in Next..Pointer + OutField - 1 loop
                        Destination (Position) := Fill;
                     end loop;
                  when Center =>
                     First := Pointer + (OutField - Value'Length) / 2;
                     Next  := First + Value'Length;
                     for Position in Pointer..First - 1 loop
                        Destination (Position) := Fill;
                     end loop;
                     for Position in Next..Pointer + OutField - 1 loop
                        Destination (Position) := Fill;
                     end loop;
                  when Right =>
                     Next  := Pointer + OutField;
                     First := Next - Value'Length;
                     for Position in Pointer..First - 1 loop
                        Destination (Position) := Fill;
                     end loop;
               end case;
               Destination (First..Next - 1) := Value;
            end;
         end if;
         Pointer := Pointer + OutField;
      end if;
   end PutString;
   
   procedure PutCharacter
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : in Character;
                Field       : in Natural := 0;
                Justify     : in Alignment := Left;
                Fill        : in Character := ' '
             )  is
      Text : constant String (1..1) := (1 => Value);
   begin
      Put (Destination, Pointer, Text, Field, Justify, Fill);
   end PutCharacter;
end Text_Edit;
