--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Float_Edit                     Luebeck            --
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
--  This generic package provides  I/O  for  floating-point  types  (the
--  generic  parameter  Number).  There  is  also  a non-generic version
--  Strings_Edit.Floats. 
--
generic
   type Number is digits <>; 
package Strings_Edit.Float_Edit is
   subtype Number_Of is Number;
--
-- Get -- Get a floating-point number from the string
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
-- This procedure gets a number from  the  string  Source.  The  process
-- starts  from  Source (Pointer) position. The number in the string may
-- be  in  either floating-point or fixed-point format. The point may be
-- absent. The mantissa can have base 2..16 (defined  by  the  parameter
-- Base). The exponent part (if appears) is introduced by 'e' or 'E'. It
-- is always decimal of Base radix. Space characters are allowed between
-- the mantissa and the exponent part as well as in  the  exponent  part
-- around the exponent sign. If Base has the value 15 or 16 the exponent
-- part  shall  be  separated  by  at least one space character from the
-- mantissa. 
-- 
-- Exceptions:
--
--      Constraint_Error - The number is not in First..Last
--      Data_Error       - Syntax error in the number
--      End_Error        - There is no any number
--      Layout_Error     - Pointer not in Source'First..Source'Last + 1 
--
   procedure Get
             (  Source   : in String;
                Pointer  : in out Integer;
                Value    : out Number'Base;
                Base     : in NumberBase := 10;
                First    : in Number'Base := Number'Base'First;
                Last     : in Number'Base := Number'Base'Last;
                ToFirst  : in Boolean := False;
                ToLast   : in Boolean := False
             );
--
-- Value -- String to floating-point conversion
--
--      Source  - The string to be processed
--      Base    - The base of the expected number
--      First   - The lowest allowed value
--      Last    - The highest allowed value
--      ToFirst - Force value to First instead of exception
--      ToLast  - Force value to Last instead of exception
--
-- This  function  gets  a floating-point number from the string Source.
-- The number can be surrounded by spaces and  tabs.  The  whole  string
-- Source  should  be  matched.  Otherwise  the  exception Data_Error is
-- propagated. 
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
               Base    : in NumberBase := 10;
               First   : in Number'Base := Number'First;
               Last    : in Number'Base := Number'Last;
               ToFirst : in Boolean := False;
               ToLast  : in Boolean := False
            )  return Number'Base;
--
-- Put -- Put a floating-point number into a string
--
--      Destination - The string that accepts the output
--      Pointer     - The current position in the string
--      Value       - The value to be put
--      Base        - The base used for the output
--      PutPlus     - The plus should placed for positive numbers
--      RelSmall    - Relative precision of the output
--      AbsSmall    - Absolute one
--      Field       - The output field
--      Justify     - Alignment within the field
--      Fill        - The fill character
--
-- This procedure places the number specified  by  the  parameter  Value
-- into  the  output  string Destination. The string is written starting
-- from  Destination  (Pointer). The parameter Base indicates the number
-- base used for the output. Base itself does not appear in the  output.
-- The  exponent  part  (if  used)  is always decimal. PutPlus indicates
-- whether the plus sign should be placed if  the  number  is  positive.
-- There are two ways to specify the output precision. 
--
-- (o)  The parameter RelSmall determines the number of the  right  Base
--      places of the mantissa. For instance, with RelSmall = 3 and Base
--      = 10, the number 1.234567e+01 is represented as 12.3. 
--               
-- (o)  The parameter AbsSmall determines the rightmost correct digit of
--      the mantissa. For example, with AbsSmall = 0,  Base  =  10,  the
--      number 1.234567e+01 is represented as 12 (i.e. 12.34567  rounded
--      to 10**0). 
--
-- From two parameters RelSmall and AbsSmall, the procedure chooses one,
-- that  specifies  the  minimal  number of mantissa digits, but no more
-- than the machine representation of the number allows.  If  the  point
-- would appear in the rightmost position it is omited. The pure zero is
-- always represented as 0. If the  desired  number  of  digits  may  be
-- provided in the fixed-point format then  the  exponent  part  is  not
-- used. For example, 1.234567e-04 gives 0.0001234567 because fixed- and
-- floating-point formats have the same length. But 1.234567e-05 will be
-- shown  in the floating-point format. For bases 15 and 16 the exponent
-- part  is  separated  from  the mantissa by space (to avoid ambiguity:
-- F.Ee+2  is F.EE + 2 or F.E * 16**2?). The parameter Field defines the
-- output size. If it has the value zero, then the output field is equal
-- to the output length. 
--
-- When the parameter Field is not zero then Justify specifies alignment
-- and Fill is the character used for filling.  When  Field  is  greater
-- than Destination'Last - Pointer + 1,  the  latter  is  used  instead.
-- After  successful  completion  Pointer  is  advanced  to  the   first
-- character following the output or to Destination'Last + 1. 
--
-- Exceptions:
--
--      Layout_Error -- Pointer  is not in Destination'Range or there is
--                      no room for the output. 
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : in Number'Base;
                Base        : in NumberBase := 10;
                PutPlus     : in Boolean    := False;
                RelSmall    : in Positive   := MaxSmall;
                AbsSmall    : in Integer    := -MaxSmall;
                Field       : in Natural    := 0;
                Justify     : in Alignment  := Left;
                Fill        : in Character  := ' '
             );
--
-- Image -- Floating-point to string conversion
--
--      Value    - The value to be converted
--      Base     - The base used for the output
--      PutPlus  - The plus should placed for positive numbers
--      RelSmall - Relative precision of the output
--      AbsSmall - Absolute one
--
-- This  procedure converts the parameter Value to String. The parameter
-- Base  indicates the number base used for the output. Base itself does
-- not appear in the output. The  exponent  part  (if  used)  is  always
-- decimal. PutPlus indicates whether the plus sign should be placed  if
-- the number is positive. For precision parameters see Put. 
--
-- Returns :
--
--      The result string
--
   function Image
            (  Value    : in Number'Base;
               Base     : in NumberBase := 10;
               PutPlus  : in Boolean := False;
               RelSmall : in Positive := MaxSmall;
               AbsSmall : in Integer := -MaxSmall
            )  return String;

end Strings_Edit.Float_Edit;
