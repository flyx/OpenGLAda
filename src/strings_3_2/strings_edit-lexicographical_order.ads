--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Lexicographical_Order          Luebeck            --
--  Interface                                      Spring, 2012       --
--                                                                    --
--                                Last revision :  14:32 02 Apr 2012  --
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

package Strings_Edit.Lexicographical_Order is
   pragma Elaborate_Body (Strings_Edit.Lexicographical_Order);

   type Precedence is (Less, Equal, Greater);
--
-- Compare_Textually -- String categorization
--
--    Left, Right - Arguments
--
-- This function compares two  strings  as  texts.  If  strings  contain
-- chains  of  digits.  These  are  replaced by single symbol considered
-- lexicographically  greater than a non-numeric character. Thus strings
-- ab123 and ab44 are considered same. abc precedes ab1.
--
-- Returns :
--
--    Comparison result
--
   function Compare_Textually (Left, Right : String) return Precedence;
   function Textually_Equal (Left, Right : String) return Boolean;
   function Textually_Less (Left, Right : String) return Boolean;
--
-- Compare_Lexicographically -- String comparison
--
--    Left, Right - Arguments
--
-- This function compares two strings. The strings are compared so  that
-- the corresponding  chains  of  digits  within  them  are  ordered  as
-- numbers. Thus the string ab44 precedes ab0123.
--
-- Returns :
--
--    Comparison result
--
   function Compare_Lexicographically (Left, Right : String)
      return Precedence;
   function Lexicographically_Equal (Left, Right : String)
      return Boolean;
   function Lexicographically_Less (Left, Right : String)
      return Boolean;

private
   pragma Inline (Textually_Equal);
   pragma Inline (Textually_Less);
   pragma Inline (Lexicographically_Equal);
   pragma Inline (Lexicographically_Less);

end Strings_Edit.Lexicographical_Order;
