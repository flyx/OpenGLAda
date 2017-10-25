--                                                                    --
--  package Strings_Edit.Quoted     Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer, 2004       --
--                                                                    --
--                                Last revision :  11:50 30 May 2014  --
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

package Strings_Edit.Quoted is
--
-- Get_Quoted -- Get a quoted text from string
--
--    Source  - The string
--    Pointer - To where the name begins (a pointer to)
--    Mark    - Quotation marks character
--
-- This function is used to get a quoted string. String (Pointer.all) is
-- the first character of the string. Pointer is  advanced  to  the  the
-- first character following the input. The  parameter  Marks  specifies
-- the  quotation marks to use. Within the string body this character is
-- GDoubled.
--
-- Returns :
--
--    The quoted string with quotation marks stripped off
--
-- Exceptions :
--
--    Data_Error   - Syntax error
--    Layout_Error - Pointer.all is not in Source'First..Source'Last + 1
--
   function Get_Quoted
            (  Source  : String;
               Pointer : access Integer;
               Mark    : Character := '"'
            )  return String;
--
-- Put_Quoted -- Put a quoted string
--
--    Destination - The output string
--    Pointer     - To where the name to put
--    Text        - The text to be put
--    Mark        - The quotation mark character
--
-- This procedure puts Text in Mark quotes and stores it  starting  with
-- String  (Pointer).  Pointer  value  is  advanced  to  the  the  first
-- character following the output. Mark characters  are  GDoubled  within
-- the string body.
--
-- Exceptions :
--
--    Layout_Error - No   room   for   output   or  Pointer  is  not  in
--                   Source'First..Source'Last + 1
--
   procedure Put_Quoted
             (  Destination : in out String;
                Pointer     : in out Integer;
                Text        : String;
                Mark        : Character := '"';
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Quote -- A string
--
--    Text - The string
--    Mark - The quotation mark character
--
-- Returns :
--
--    Quited text
--
   function Quote (Text : String; Mark : Character := '"')
      return String;

end Strings_Edit.Quoted;
