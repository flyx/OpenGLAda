--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Mapping                   Luebeck            --
--  Interface                                      Spring, 2008       --
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
--
--  This  package  provides  mappings  of  code  points  as  defined  by
--  UnicodeData file.
--
package Strings_Edit.UTF8.Mapping is
   pragma Elaborate_Body (Strings_Edit.UTF8.Mapping);
--
-- Has_Case -- Case test
--
--    Value - Code point
--
-- Returns :
--
--    True if Value has either an  upper  or  a  lower  case  equivalent
--    different from Code.
--
   function Has_Case (Value : UTF8_Code_Point) return Boolean;
--
-- Is_Lowercase -- Case test
--
--    Value - Code point
--
-- Returns :
--
--    True if Value is a lower case point
--
   function Is_Lowercase (Value : UTF8_Code_Point) return Boolean;
--
-- Is_Uppercase -- Case test
--
--    Value - Code point
--
-- Returns :
--
--    True if Value is a lower case point
--
   function Is_Uppercase (Value : UTF8_Code_Point) return Boolean;
--
-- To_Lowercase -- Convert to lower case
--
--    Value - Code point or UTF-8 encoded string
--
-- Returns :
--
--    The lower case eqivalent or else Value itself
--
   function To_Lowercase (Value : UTF8_Code_Point)
      return UTF8_Code_Point;
   function To_Lowercase (Value : String) return String;
--
-- To_Uppercase -- Convert to upper case
--
--    Value - Code point or UTF-8 encoded string
--
-- Returns :
--
--    The upper case eqivalent or else Value itself
--
   function To_Uppercase (Value : UTF8_Code_Point)
      return UTF8_Code_Point;
   function To_Uppercase (Value : String) return String;

end Strings_Edit.UTF8.Mapping;
