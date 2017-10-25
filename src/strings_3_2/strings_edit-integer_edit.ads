--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Integer_Edit                   Luebeck            --
--  Interface                                      Spring, 2000       --
--                                                                    --
--                                Last revision :  19:10 06 Jun 2014  --
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
--  This generic package provides I/O for  integer  types  (the  generic
--  parameter   Number).   There   is   also   a   non-generic   version
--  Strings_Edit.Integers. 
--
generic
   type Number is range <>;
package Strings_Edit.Integer_Edit is
   subtype Number_Of is Number;
--
-- Get -- Get an integer number from the string
--
--      Source  - The string to be processed
--      Pointer - The current position in the string
--      Value   - The result
--      Base    - The base of the expected number
--      First   - The lowest allowed value
--      Last    - The highest allowed value
--      ToFirst - Force value to First instead of exception
--      ToLast  - Force value to Last instead of exception
--
-- This  procedure  gets  an  integer number from the string Source. The
-- process starts from Source (Pointer). The  parameter  Base  indicates
-- the base of the expected number. 
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
                Value   : out Number'Base;
                Base    : in NumberBase := 10;
                First   : in Number'Base := Number'First;
                Last    : in Number'Base := Number'Last;
                ToFirst : in Boolean := False;
                ToLast  : in Boolean := False
             );
--
-- Value -- String to an integer conversion
--
--      Source  - The string to be processed
--      Base    - The base of the expected number
--      First   - The lowest allowed value
--      Last    - The highest allowed value
--      ToFirst - Force value to First instead of exception
--      ToLast  - Force value to Last instead of exception
--
-- This function gets an integer number  from  the  string  Source.  The
-- number  can be surrounded by spaces and tabs. The whole string Source
-- should be matched. Otherwise the exception Data_Error is propagated. 
--
-- Returns :
--
--      The value
--
-- Exceptions:
--
--    Constraint_Error - The number is not in First..Last
--    Data_Error       - Syntax error in the number
--    End_Error        - There is no any number
--
   function Value
            (  Source  : in String;
               Base    : in NumberBase := 10;
               First   : in Number'Base := Number'First;
               Last    : in Number'Base := Number'Last;
               ToFirst : in Boolean := False;
               ToLast  : in Boolean := False
            )  return Number'Base;
--
-- Put -- Put an integer into a string
--
--      Destination - The string that accepts the output
--      Pointer     - The current position in the string
--      Value       - The value to be put
--      Base        - The base used for the output
--      PutPlus     - The plus should placed for positive numbers
--      Field       - The output field
--      Justify     - Alignment within the field
--      Fill        - The fill character
--
-- This procedure places the number specified  by  the  parameter  Value
-- into  the  output  string Destination. The string is written starting
-- from  Destination  (Pointer). The parameter Base indicates the number
-- base used for the output. The base itself  does  not  appear  in  the
-- output.  The parameter PutPlus indicates whether the plus sign should
-- be placed if the number is positive. 
--
-- Exceptions:
--
--      Layout_Error - Pointer is not in Destination'Range or  there  is
--                     no room for the output. 
--
-- Example :
--
--      Text    : String (1..20) := (others =>'#');
--      Pointer : Positive := Text'First;
--
--      Put (Text, Pointer, 5, 2, True, 10, Center, '@');
--
--      Now the Text is "@@@+101@@@##########" Pointer = 11
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : in Number'Base;
                Base        : in NumberBase := 10;
                PutPlus     : in Boolean := False;
                Field       : in Natural := 0;
                Justify     : in Alignment := Left;
                Fill        : in Character := ' '
             );
--
-- Image -- Integer to string conversion
--
--      Value   - The value to be converted
--      Base    - The base used for the output
--      PutPlus - The plus should placed for positive numbers
--
-- This  function converts Value to string. The parameter Base indicates
-- the number base used for the output. The base itself does not  appear
-- in the output. The parameter PutPlus indicates whether the plus  sign
-- should be placed if the number is positive. 
--
-- Returns :
--
--      The result string
--
   function Image
            (  Value   : in Number'Base;
               Base    : in NumberBase := 10;
               PutPlus : in Boolean := False
            )  return String;

end Strings_Edit.Integer_Edit ;
