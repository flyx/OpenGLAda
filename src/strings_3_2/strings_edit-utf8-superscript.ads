--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.Superscript               Luebeck            --
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

package Strings_Edit.UTF8.Superscript is

private
--
-- Get_Digit -- Get one digit
--
--    Source  - The source string
--    Pointer - The source string position
--    Digit   - The result
--    
-- This procedure takes one decimal superscript figure from  the  string
-- Source. The  process  starts  from  Source  (Pointer).  If  sucessful
-- Pointer  will  point  to  the  first  character  following the input.
-- Otherwise Pointer is untouched and the result is greater than 9. 
--
   procedure Get_Digit
             (  Source  : String;
                Pointer : in out Integer;
                Digit   : out Natural
             );
--
-- Get_Sign -- Get sign and following spaces and tabs
--
--    Source  - The source string
--    Pointer - The source string position
--    Sign    - The result
--    
-- This procedure takes sign from the string Source. The process  starts
-- from  Source  (Pointer). If sucessful Pointer will point to the first
-- character   following   the  input  and  all  consequent  spaces  and
-- tabulators. Otherwise Pointer is untouched. 
--
   procedure Get_Sign
             (  Source  : String;
                Pointer : in out Integer;
                Sign_Of : out Sign
             );
--
-- Put_Digit -- Put one decimal superscript digit in reverse
--
--    Destination - The target string
--    Pointer     - The position where to place the last character
--    Digit       - The digit
--
-- The digit is placed in the positions of Destination (..Pointer). Then
-- Pointer is  moved  back  to  the  first  position  before  the  first
-- character of the output. 
--
-- Exceptions :
--
--    Layout_Error - No room for output
--
   procedure Put_Digit
             (  Destination : in out String;
                Pointer     : in out Integer;
                Digit       : Script_Digit
             );
--
-- Put_Sign -- Put sign in reverse
--
--    Destination - The target string
--    Pointer     - The position where to place the last character
--    Sing_Of     - The digit
--
-- The digit is placed in the positions of Destination (..Pointer). Then
-- Pointer is  moved  back  to  the  first  position  before  the  first
-- character of the output. 
--
-- Exceptions :
--
--    Layout_Error - No room for output
--
   procedure Put_Sign
             (  Destination : in out String;
                Pointer     : in out Integer;
                Sign_Of     : Sign
             );

end Strings_Edit.UTF8.Superscript;
