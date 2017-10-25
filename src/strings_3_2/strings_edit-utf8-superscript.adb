--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Superscript               Luebeck            --
--  Implementation                                 Spring, 2005       --
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

package body Strings_Edit.UTF8.Superscript is

   Page_00 : constant String := (1..1 => Character'Val (194));
   Page_32 : constant String :=
                Character'Val (226) & Character'Val (129);

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
            Source (Pointer + 1) = Character'Val (129)
         and then
            Pointer + 1 < Source'Last
         )
      then
         case Source (Pointer + 2) is
            when Character'Val (186) =>
               Sign_Of := Plus;
               Pointer := Pointer + 3;
               Get (Source, Pointer );
            when Character'Val (187) =>
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
      Index : Integer := Pointer;
   begin
      if Index >= Source'Last then
         Digit := 10;
         return;
      end if;
      case Source (Index) is
         when Character'Val (194) =>
            --
            -- Latin-1 character page
            --
            case Source (Index + 1) is
               when Character'Val (185) => Digit := 1;
               when Character'Val (178) => Digit := 2;
               when Character'Val (179) => Digit := 3;
               when others =>
                  Digit := 10;
                  return;
            end case;
         when Character'Val (226) =>
            Index := Index + 1;
            if (  Index >= Source'Last
               or else
                  Source (Index) /= Character'Val (129)
               )
            then
               Digit := 10;
               return;
            end if;
            case Source (Index + 1) is
               when Character'Val (176) => Digit := 0;
               when Character'Val (180) => Digit := 4;
               when Character'Val (181) => Digit := 5;
               when Character'Val (182) => Digit := 6;
               when Character'Val (183) => Digit := 7;
               when Character'Val (184) => Digit := 8;
               when Character'Val (185) => Digit := 9;
               when others =>
                  Digit := 10;
                  return;
            end case;
         when others =>
            Digit := 10;
            return;
      end case;
      Pointer := Index + 2;
   end Get_Digit;

   procedure Put_Digit
             (  Destination : in out String;
                Pointer     : in out Integer;
                Digit       : Script_Digit
             )  is
   begin
      case Digit is
         when 0 => Reverse_Put (Destination, Pointer, Page_32, 176);
         when 1 => Reverse_Put (Destination, Pointer, Page_00, 185);
         when 2 => Reverse_Put (Destination, Pointer, Page_00, 178);
         when 3 => Reverse_Put (Destination, Pointer, Page_00, 179);
         when 4 => Reverse_Put (Destination, Pointer, Page_32, 180);
         when 5 => Reverse_Put (Destination, Pointer, Page_32, 181);
         when 6 => Reverse_Put (Destination, Pointer, Page_32, 182);
         when 7 => Reverse_Put (Destination, Pointer, Page_32, 183);
         when 8 => Reverse_Put (Destination, Pointer, Page_32, 184);
         when 9 => Reverse_Put (Destination, Pointer, Page_32, 185);
      end case;
   end Put_Digit;

   procedure Put_Sign
             (  Destination : in out String;
                Pointer     : in out Integer;
                Sign_Of     : Sign
             )  is
   begin
      case Sign_Of is
         when Minus =>
            Reverse_Put (Destination, Pointer, Page_32, 187);
         when None =>
            null;
         when Plus =>
            Reverse_Put (Destination, Pointer, Page_32, 186);
      end case;
   end Put_Sign;

end Strings_Edit.UTF8.Superscript;
