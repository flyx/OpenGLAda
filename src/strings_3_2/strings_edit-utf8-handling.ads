--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Handling                  Luebeck            --
--  Interface                                      Spring, 2005       --
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
--  This package provides conversions between UTF-8 strings and standard
--  Ada's strings and characters. 
--
package Strings_Edit.UTF8.Handling is
--
-- To_String -- Conversion from UTF-8 to Latin-1
--
--    Value        - The UTF-8 string to convert
--  [ Substitute ] - For non-Latin-1 code points
--
-- These functions convert a UTF-8 encoded string to  Latin-1  character
-- string (standard Ada string). The parameter Substitute specifies  the
-- character  that  substitutes  non-Latin-1  code  points  in Value. If
-- omitted Constraint_Error is propagated when non-Latin-1  code  points
-- appear in Value. 
--
-- Returns :
--
--    Latin-1 equivalent
--
-- Exceptions :
--
--    Constraint_Error - Non-Latin-1 code points in Value
--    Data_Error       - Illegal UTF-8 string
--
   function To_String (Value : String) return String;
   function To_String
            (  Value      : String;
               Substitute : Character
            )  return String;
--
-- To_UTF8 -- Conversion to UTF-8
--
--    Value - Character or a string
--
-- These  functions  convert  the  parameter  Value  to  a UTF-8 encoded
-- string.  The  parameter  can  be Character, String, Wide_Character or
-- Wide_String. The result can be from 1 to  3  bytes  long.  Note  that
-- Character  has  Latin-1  encoding  which  differs  from  UTF-8 in the
-- positions greater than 127. 
--
-- Returns :
--
--    Value encoded in UTF-8
--
   function To_UTF8 (Value : Character     ) return String;
   function To_UTF8 (Value : String        ) return String;
   function To_UTF8 (Value : Wide_Character) return String;
   function To_UTF8 (Value : Wide_String   ) return String;
--
-- To_Wide_String -- Conversion from UTF-8 to UCS-2
--
--    Value        - The UTF-8 string to convert
--  [ Substitute ] - For non-Latin-1 code points
--
-- These functions convert a UTF-8 encoded  string  to  UCS-2  character
-- string  (Ada's  Wide_String).  The parameter Substitute specifies the
-- character that substitutes non-UCS-2 code points in Value. If omitted
-- Constraint_Error  is  propagated  when non-UCS-2 characters appear in
-- Value.
--
-- Returns :
--
--    UCS-2 equivalent
--
-- Exceptions :
--
--    Constraint_Error - Non-UCS-2 code point in Value
--    Data_Error       - Illegal UTF-8 string
--
   function To_Wide_String (Value : String) return Wide_String;
   function To_Wide_String
            (  Value      : String;
               Substitute : Wide_Character
            )  return Wide_String;

end Strings_Edit.UTF8.Handling;
