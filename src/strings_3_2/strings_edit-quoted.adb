--                                                                    --
--  package Strings_Edit.Quoted     Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2004       --
--                                                                    --
--                                Last revision :  21:03 21 Apr 2009  --
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

with Ada.IO_Exceptions;    use Ada.IO_Exceptions;
with Strings_Edit.Fields;  use Strings_Edit.Fields;

package body Strings_Edit.Quoted is

   function Get_Quoted_Length
            (  Source  : String;
               Pointer : access Integer;
               Mark    : Character := '"'
            )  return Natural is
      Index  : Integer := Pointer.all;
      Length : Natural := 0;
   begin
      if Index < Source'First or else Index > Source'Last + 1 then
         raise Layout_Error;
      end if;
      if Index > Source'Last or else Source (Index) /= Mark then
         raise Data_Error;
      end if;
      Index := Index + 1;
      loop
         if Index > Source'Last then
            raise Data_Error;
         end if;
         if Source (Index) = Mark then
            Index := Index + 1;
            exit when
               Index > Source'Last or else Source (Index) /= Mark;
         end if;
         Length := Length + 1;
         Index  := Index  + 1;
      end loop;
      Pointer.all := Index;
      return Length;
   end Get_Quoted_Length;

   function Get_Quoted
            (  Source  : String;
               Pointer : access Integer;
               Mark    : Character := '"'
            )  return String is
      Result : String (1..Get_Quoted_Length (Source, Pointer, Mark));
      Index  : Integer := Pointer.all - 2;
   begin
      for Item in reverse Result'Range loop
         Result (Item) := Source (Index);
         if Result (Item) = Mark then
            Index := Index - 2;
         else
            Index := Index - 1;
         end if;
      end loop;
      return Result;
   end Get_Quoted;

   procedure Put_Quoted
             (  Destination : in out String;
                Pointer     : in out Integer;
                Text        : String;
                Mark        : Character := '"';
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
     Out_Field : constant Natural :=
                    Get_Output_Field (Destination, Pointer, Field);
     subtype Output is String (Pointer..Pointer + Out_Field - 1);
     Result : Output renames
                 Destination (Pointer..Pointer + Out_Field - 1);
     Index  : Integer := Pointer;
   begin
      Result (Index) := Mark;
      Index := Index + 1;
      for Item in Text'Range loop
         if Index >= Result'Last then
            raise Layout_Error;
         end if;
         Result (Index) := Text (Item);
         Index := Index + 1;
         if Text (Item) = Mark then
            Result (Index) := Mark;
            Index := Index + 1;
         end if;
      end loop;
      if Index > Result'Last then
         raise Layout_Error;
      end if;
      Result (Index) := Mark;
      Index := Index + 1;
      Adjust_Output_Field
      (  Destination,
         Pointer,
         Index,
         Out_Field,
         Field,
         Justify,
         Fill
      );
   end Put_Quoted;

   function Quote (Text : String; Mark : Character := '"')
      return String is
      Length : Natural := Text'Length + 2;
   begin
      for Index in Text'Range loop
         if Text (Index) = Mark then
            Length := Length + 1;
         end if;
      end loop;
      declare
         Result  : String (1..Length);
         Pointer : Positive := Result'First;
      begin
         Result (Pointer) := Mark;
         Pointer := Pointer + 1;
         for Index in Text'Range loop
            if Text (Index) = Mark then
               Result (Pointer)     := Mark;
               Result (Pointer + 1) := Mark;
               Pointer := Pointer + 2;
            else
               Result (Pointer) := Text (Index);
               Pointer := Pointer + 1;
            end if;
         end loop;
         Result (Pointer) := Mark;
         return Result;
      end;
   end Quote;

end Strings_Edit.Quoted;
