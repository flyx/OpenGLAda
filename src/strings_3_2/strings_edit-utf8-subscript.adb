--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Subscript                 Luebeck            --
--  Implementation                                 Spring, 2005       --
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

package body Strings_Edit.UTF8.Subscript is

   Page_32 : constant String :=
                Character'Val (226) & Character'Val (130);

   procedure Get_Sign
             (  Source  : String;
                Pointer : in out Integer;
                Sign_Of : out Sign
             )  is
   begin
      Sign_Of := None;
      if (  Pointer < Source'Last
         and then
            Source (Pointer) = Character'Val (226)
         and then
            Source (Pointer + 1) = Character'Val (130)
         and then
            Pointer + 1 < Source'Last
         )
      then
         case Source (Pointer + 2) is
            when Character'Val (138) =>
               Sign_Of := Plus;
               Pointer := Pointer + 3;
               Get (Source, Pointer);
            when Character'Val (139) =>
               Sign_Of := Minus;
               Pointer := Pointer + 3;
               Get (Source, Pointer);
            when others =>
               null;
         end case;
      end if;
   end Get_Sign;

   procedure Get_Digit
             (  Source  : String;
                Pointer : in out Integer;
                Digit   : out Natural
             )  is
      Index : constant Integer := Pointer;
   begin
      if (  Index >= Source'Last
         or else
            Source (Index) /= Character'Val (226)
         or else
            Source (Index + 1) /= Character'Val (130)
         or else
            Index + 1 >= Source'Last
         or else
            Character'Pos (Source (Index + 2)) not in 128..137
         )
      then
         Digit := 10;
      else
         Digit   := Character'Pos (Source (Index + 2)) - 128;
         Pointer := Index + 3;
      end if;
   end Get_Digit;

   procedure Put_Digit
             (  Destination : in out String;
                Pointer     : in out Integer;
                Digit       : Script_Digit
             )  is
   begin
      Reverse_Put
      (  Destination,
         Pointer,
         Page_32,
         128 + Natural (Digit)
      );
   end Put_Digit;

   procedure Put_Sign
             (  Destination : in out String;
                Pointer     : in out Integer;
                Sign_Of     : Sign
             )  is
   begin
      case Sign_Of is
         when Minus =>
            Reverse_Put (Destination, Pointer, Page_32, 139);
         when None =>
            null;
         when Plus =>
            Reverse_Put (Destination, Pointer, Page_32, 138);
      end case;
   end Put_Sign;

end Strings_Edit.UTF8.Subscript;
