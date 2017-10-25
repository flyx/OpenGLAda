--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Wildcards                 Luebeck            --
--  Interface                                      Winter, 2007       --
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

with Strings_Edit.UTF8.Maps;  use Strings_Edit.UTF8.Maps;

package Strings_Edit.UTF8.Wildcards is
--
-- Match -- An UTF-8 encoded string by a pattern
--
--    Text       - The string to match
--    Pattern    - The pattern
--  [ Map ]      - Used to convert characters before comparison
--    Wide_Space - Spaces handled as a pattern for blank sequences
--    Blanks     - The list of blank characters
--
-- This function is used to match an UTF-8 encoded text  by  a  pattern.
-- The  pattern  is  an  UTF-8  string which may contain the character *
-- treated as a wildcard matching any (possibly empty) sequence of UTF-8
-- characters. When Wide_Space is True, then spaces in Pattern match any
-- non-empty  sequence  of  characters  from  the  set Blanks. Otherwise
-- Blanks is ignored. Note that when Blanks contain non-ASCII characters
-- (with  the code points 128..255), those will match any UTF-characters
-- starting with this octet. A typical use of this function is to filter
-- file names. For example:
--
--    *.txt - matches all files with the extension txt
--    A*B*C - names starting with A ending by C with B inside
--
-- The outcome  is  undefined  when  Text  and/or  Pattern  are  illegal
-- UTF-8 strings.
--
-- Returns :
--
--    True if Pattern matches Text
--
   function Match
            (  Text       : String;
               Pattern    : String;
               Wide_Space : Boolean       := False;
               Blanks     : Character_Set := SpaceAndTab
            )  return Boolean;
   function Match
            (  Text       : String;
               Pattern    : String;
               Map        : Unicode_Mapping;
               Wide_Space : Boolean       := False;
               Blanks     : Character_Set := SpaceAndTab
            )  return Boolean;

end Strings_Edit.UTF8.Wildcards;
