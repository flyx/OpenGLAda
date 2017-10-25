--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Lexicographical_Order          Luebeck            --
--  Implementation                                 Spring, 2012       --
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

package body Strings_Edit.Lexicographical_Order is

   procedure Get_Numeral (Text : String; Pointer : in out Integer) is
   begin
      while Pointer < Text'Last loop
         exit when Text (Pointer + 1) not in '0'..'9';
         Pointer := Pointer + 1;
      end loop;
   end Get_Numeral;

   function Compare_Numerals (Left, Right : String) return Precedence is
      I, J : Integer;
   begin
      I := Left'First;
      while I <= Left'Last and then Left (I) = '0' loop
         I := I + 1;
      end loop;
      J := Right'First;
      while J <= Right'Last and then Right (J) = '0' loop
         J := J + 1;
      end loop;
      if Left'Last - I /= Right'Last - J then
         if Left'Last - I < Right'Last - J then
            return Less;
         else
            return Greater;
         end if;
      end if;
      while I <= Left'Last loop
         if Left (I) /= Right (J) then
            if Left (I) < Right (J) then
               return Less;
            else
               return Greater;
            end if;
         end if;
         I := I + 1;
         J := J + 1;
      end loop;
      return Equal;
   end Compare_Numerals;

   function Compare_Textually (Left, Right : String) return Precedence is
      I : Integer := Left'First;
      J : Integer := Right'First;
   begin
      if Left'Length = 0 then
         if Right'Length = 0 then
            return Equal;
         else
            return Less;
         end if;
      elsif Right'Length = 0 then
         return Greater;
      end if;
      loop
         if Left (I) in '0'..'9' then
            if Right (J) not in '0'..'9' then
               return Greater;
            end if;
            Get_Numeral (Left, I);
            Get_Numeral (Right, J);
         elsif Right (J) in '0'..'9' then
            return Less;
         elsif Left (I) /= Right (J) then
            if Left (I) < Right (J) then
               return Less;
            else
               return Greater;
            end if;
         end if;
         if I = Left'Last then
            if J = Right'Last then
               return Equal;
            else
               return Less;
            end if;
         elsif J = Right'Last then
            return Greater;
         end if;
         I := I + 1;
         J := J + 1;
      end loop;
   end Compare_Textually;

   function Compare_Lexicographically (Left, Right : String)
      return Precedence is
      I : Integer := Left'First;
      J : Integer := Right'First;
   begin
      if Left'Length = 0 then
         if Right'Length = 0 then
            return Equal;
         else
            return Less;
         end if;
      elsif Right'Length = 0 then
         return Greater;
      end if;
      loop
         if Left (I) in '0'..'9' then
            if Right (J) not in '0'..'9' then
               return Greater;
            end if;
            declare
               I0 : constant Integer := I;
               J0 : constant Integer := J;
            begin
               Get_Numeral (Left, I);
               Get_Numeral (Right, J);
               case Compare_Numerals (Left (I0..I), Right (J0..J)) is
                  when Less =>
                     return Less;
                  when Greater =>
                     return Greater;
                  when Equal =>
                     null;
               end case;
            end;
         elsif Right (J) in '0'..'9' then
            return Less;
         elsif Left (I) /= Right (J) then
            if Left (I) < Right (J) then
               return Less;
            else
               return Greater;
            end if;
         end if;
         if I = Left'Last then
            if J = Right'Last then
               return Equal;
            else
               return Less;
            end if;
         elsif J = Right'Last then
            return Greater;
         end if;
         I := I + 1;
         J := J + 1;
      end loop;
   end Compare_Lexicographically;

   function Textually_Equal (Left, Right : String) return Boolean is
   begin
      return Compare_Textually (Left, Right) = Equal;
   end Textually_Equal;

   function Lexicographically_Equal (Left, Right : String)
      return Boolean is
   begin
      return Compare_Lexicographically (Left, Right) = Equal;
   end Lexicographically_Equal;

   function Textually_Less (Left, Right : String) return Boolean is
   begin
      return Compare_Textually (Left, Right) = Less;
   end Textually_Less;

   function Lexicographically_Less (Left, Right : String)
      return Boolean is
   begin
      return Compare_Lexicographically (Left, Right) = Less;
   end Lexicographically_Less;

end Strings_Edit.Lexicographical_Order;
