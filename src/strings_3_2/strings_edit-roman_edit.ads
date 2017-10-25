--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Roman_Edit                     Luebeck            --
--  Interface                                      Spring, 2002       --
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

package Strings_Edit.Roman_Edit is
   type Roman is range 1..3999;
--
-- Get -- Get a roman number from the string
--
--      Source  - The string to be processed
--      Pointer - The current position in the string
--      Value   - The result
--      First   - The lowest allowed value
--      Last    - The highest allowed value
--      ToFirst - Force value to First instead of exception
--      ToLast  - Force value to Last instead of exception
--
-- This procedure gets a  roman  number  from  the  string  Source.  The
-- process starts from Source (Pointer). 
--
-- Exceptions:
--
--      Constraint_Error - The number is not in First..Last
--      Data_Error       - Syntax error in the number
--      End_Error        - There is no any number
--      Layout_Error     - Pointer not in Source'First..Source'Last + 1 
--
   procedure Get
             (  Source  : in String;
                Pointer : in out Integer;
                Value   : out Roman;
                First   : in Roman := Roman'First;
                Last    : in Roman := Roman'Last;
                ToFirst : in Boolean := False;
                ToLast  : in Boolean := False
             );
--
-- Value -- String to roman number conversion
--
--      Source  - The string to be processed
--      First   - The lowest allowed value
--      Last    - The highest allowed value
--      ToFirst - Force value to First instead of exception
--      ToLast  - Force value to Last instead of exception
--
-- This  function  gets  the  roman  number  from the string Source. The
-- number  can be surrounded by spaces and tabs. The whole string Source
-- should be matched. Otherwise the exception Data_Error is propagated. 
--
-- Returns :
--
--      The value
--
-- Exceptions:
--
--      Constraint_Error - The number is not in First..Last
--      Data_Error       - Syntax error in the number
--      End_Error        - There is no any number
--
   function Value
            (  Source  : in String;
               First   : in Roman := Roman'First;
               Last    : in Roman := Roman'Last;
               ToFirst : in Boolean := False;
               ToLast  : in Boolean := False
            )  return Roman;
--
-- Put -- Put a roman number into a string
--
--      Destination - The string that accepts the output
--      Pointer     - The current position in the string
--      Value       - The number to be put
--      LowerCase   - Use lower case letters for the output
--      Field       - The output field
--      Justify     - Alignment within the field
--      Fill        - The fill character
--
-- This procedure places the number specified  by  the  parameter  Value
-- into  the  output  string Destination. The string is written starting
-- from  Destination  (Pointer).  The  parameter  LowerCase   determines
-- whether upper or lower case letters should be used. 
--
-- Exceptions:
--
--      Layout_Error - Pointer is not in Destination'Range or  there  is
--                     no room for the output. 
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : in Roman;
                LowerCase   : in Boolean := False;
                Field       : in Natural := 0;
                Justify     : in Alignment := Left;
                Fill        : in Character := ' '
             );
--
-- Image -- Roman number to string conversion
--
--      Value      - The value to be converted
--      LowerCase  - Use lower case letters for the output
--
-- This  function  converts  Value  to  string.  The parameter LowerCase
-- indicates whether upper or lower case letters shall be used. 
--
-- Returns :
--
--      The string
--
   function Image
            (  Value     : in Roman;
               LowerCase : in Boolean := False
            )  return String;

end Strings_Edit.Roman_Edit;
