--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.UTF8.                          Luebeck            --
--        Integer_Edit                             Spring, 2005       --
--  Interface                                                         --
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
--  This  generic  package  is  the parent for integer I/O packages (the
--  generic parameter Number). 
--
generic
   type Number is range <>;
   with procedure Get_Digit
                  (  Source  : String;
                     Pointer : in out Integer;
                     Digit   : out Natural
                  )  is <>;
   with procedure Get_Sign
                  (  Source  : String;
                     Pointer : in out Integer;
                     Sign_Of : out Sign
                  )  is <>;
   with procedure Put_Digit
                  (  Destination : in out String;
                     Pointer     : in out Integer;
                     Digit       : Script_Digit
                  )  is <>;
   with procedure Put_Sign
                  (  Destination : in out String;
                     Pointer     : in out Integer;
                     Sign_Of     : Sign
                  )  is <>;
package Strings_Edit.UTF8.Integer_Edit is
--
-- Get -- Get an integer number from UTF-8 string
--
--    Source  - The string to be processed
--    Pointer - The current position in the string
--    Value   - The result
--    Base    - The base of the expected number
--    First   - The lowest allowed value
--    Last    - The highest allowed value
--    ToFirst - Force value to First instead of exception
--    ToLast  - Force value to Last instead of exception
--
-- This procedure gets a decimal integer number from the string  Source.
-- The process starts from Source (Pointer). 
--
-- Exceptions:
--
--    Constraint_Error - The number is not in First..Last
--    Data_Error       - Syntax error in the number
--    End_Error        - There is no any number
--    Layout_Error     - Pointer not in Source'First..Source'Last + 1
--
   procedure Get
             (  Source  : in String;
                Pointer : in out Integer;
                Value   : out Number'Base;
                Base    : Script_Base := 10;
                First   : Number'Base := Number'First;
                Last    : Number'Base := Number'Last;
                ToFirst : Boolean     := False;
                ToLast  : Boolean     := False
             );
--
-- Value -- String to an integer conversion
--
--    Source  - The string to be processed
--    Base    - The base of the expected number
--    First   - The lowest allowed value
--    Last    - The highest allowed value
--    ToFirst - Force value to First instead of exception
--    ToLast  - Force value to Last instead of exception
--
-- This  function  gets a decimal integer number from the string Source.
-- The number can be surrounded by spaces and  tabs.  The  whole  string
-- Source  should  be  matched.  Otherwise  the  exception Data_Error is
-- propagated. 
--
-- Returns :
--
--    The value
--
-- Exceptions:
--
--    Constraint_Error - The number is not in First..Last
--    Data_Error       - Syntax error in the number
--    End_Error        - There is no any number
--
   function Value
            (  Source  : in String;
               Base    : Script_Base    := 10;
               First   : in Number'Base := Number'First;
               Last    : Number'Base    := Number'Last;
               ToFirst : Boolean        := False;
               ToLast  : Boolean        := False
            )  return Number'Base;
--
-- Put -- Put an integer into a string
--
--    Destination - The string that accepts the output
--    Pointer     - The current position in the string
--    Value       - The value to be put
--    Base        - The base of the number
--    PutPlus     - The plus should placed for positive numbers
--
-- This procedure places the number specified  by  the  parameter  Value
-- into the output string Destination. UTF-8 characters are used for the
-- output. The string is written starting  from  Destination  (Pointer).
-- The parameter PutPlus indicates  whether  the  plus  sign  should  be
-- placed if the number is positive. 
--
-- Exceptions:
--
--    Layout_Error - Pointer is not in Destination'Range or there is  no
--                   room for the output. 
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Number'Base;
                Base        : Script_Base := 10;
                PutPlus     : Boolean     := False
             );
--
-- Image -- Integer to string conversion
--
--    Value   - The value to be converted
--    Base    - The base of the number
--    PutPlus - The plus should placed for positive numbers
--
-- This  function  converts  Value  to  string.  The  parameter  PutPlus
-- indicates whether the plus sign should be placed  if  the  number  is
-- positive. 
--
-- Returns :
--
--    The result string
--
   function Image
            (  Value   : Number'Base;
               Base    : Script_Base := 10;
               PutPlus : Boolean     := False
            )  return String;

end Strings_Edit.UTF8.Integer_Edit ;
