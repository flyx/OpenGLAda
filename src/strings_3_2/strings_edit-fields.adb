--                                                                    --
--  package Strings_Edit.Fields     Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2002       --
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

with Ada.IO_Exceptions; use Ada.IO_Exceptions;

package body Strings_Edit.Fields is

   function Get_Output_Field
            (  Destination : String;
               Pointer     : Integer;
               Field       : Natural
            )  return Natural is
      Result : Natural := Field;
   begin
      if Result = 0 then
         Result := Destination'Last - Pointer + 1;
      end if;
      if (  Pointer < Destination'First
         or else
            Pointer + Result - 1 > Destination'Last
         )
      then
         raise Layout_Error;
      end if;
      return Result;
   end Get_Output_Field;

   procedure Adjust_Output_Field
             (  Destination : in out String;
                Pointer     : in out Integer;
                Index       : Integer;
                Out_Field   : Natural;
                Field       : Natural;
                Justify     : Alignment;
                Fill        : Character
             )  is
   begin
      if Field = 0 then
         Pointer := Index;
      else
         declare
            Last : constant Integer := Pointer + Out_Field - 1;
         begin
            if Out_Field /= Index - Pointer then
               case Justify is
                  when Left =>
                     for Position in Index..Last loop
                        Destination (Position) := Fill;
                     end loop;
                  when Center =>
                     declare
                        Length : constant Natural := Index - Pointer;
                        First  : constant Integer :=
                                    Pointer + (Out_Field - Length) / 2;
                        Next   : constant Integer := First + Length;
                     begin
                        Destination (First..Next - 1) :=
                        Destination (Pointer..Index - 1);
                        for Position in Pointer..First - 1 loop
                           Destination (Position) := Fill;
                        end loop;
                        for Position in Next..Last loop
                           Destination (Position) := Fill;
                        end loop;
                     end;
                  when Right =>
                     declare
                        First : constant Integer :=
                                   Last + 1 - Index + Pointer;
                     begin
                        Destination (First..Last) :=
                           Destination (Pointer..Index - 1);
                        for Position in Pointer..First - 1 loop
                           Destination (Position) := Fill;
                        end loop;
                     end;
               end case;
            end if;
            Pointer := Last + 1;
         end;
      end if;
   end Adjust_Output_Field;

end Strings_Edit.Fields;
