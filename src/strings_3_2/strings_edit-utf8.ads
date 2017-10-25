--                                                                    --
--  package Strings_Edit.UTF8       Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2005       --
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

package Strings_Edit.UTF8 is
   pragma Elaborate_Body (Strings_Edit.UTF8);
--
-- UTF8_Code_Point -- UFT-8 codespace
--
   type Code_Point is mod 2**32;
   subtype UTF8_Code_Point is Code_Point range 0..16#10FFFF#;
--
-- Script_Base -- Supported bases of sub- and superscript integers
--
   subtype Script_Base is NumberBase range 2..10;
--
-- Script_Digit -- Sub- and superscript digits
--
   type Script_Digit is range 0..9;
   type Sign is (Minus, None, Plus);
--
-- Get -- Get one UTF-8 code point
--
--    Source  - The source string
--    Pointer - The string position to start at
--    Value   - The result
--
-- This  procedure  decodes one UTF-8 code point from the string Source.
-- It starts at Source (Pointer). After successful completion Pointer is
-- advanced to the first character following the input.  The  result  is
-- returned through the parameter Value.
--
-- Exceptions :
--
--    Data_Error   - UTF-8 syntax error
--    End_Error    - Nothing found
--    Layout_Error - Pointer is not in Source'First..Source'Last + 1
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out UTF8_Code_Point
             );
--
-- Get_Backwards -- Get one UTF-8 code point
--
--    Source  - The source string
--    Pointer - The string position to start at
--    Value   - The result
--
-- This  procedure  decodes one UTF-8 code point from the string Source.
-- It starts at Source (Pointer -  1)  and  continues  backwards.  After
-- successful completion Pointer is moved to the first character of  the
-- result. The result is returned through the parameter Value.
--
-- Exceptions :
--
--    Data_Error   - UTF-8 syntax error
--    End_Error    - Nothing found
--    Layout_Error - Pointer is not in Source'First..Source'Last + 1
--
   procedure Get_Backwards
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out UTF8_Code_Point
             );
--
-- Image -- Of an UTF-8 code point
--
--    Value - The code point
--
-- Returns :
--
--    UTF-8 encoded equivalent
--
   function Image (Value : UTF8_Code_Point) return String;
--
-- Put -- Put one UTF-8 code point
--
--    Destination - The target string
--    Pointer     - The position where to place the character
--    Value       - The code point to put
--
-- This  procedure  puts  one  UTF-8  code  point into the string Source
-- starting from the position Source (Pointer). Pointer is then advanced
-- to the first character following the output.
--
-- Exceptions :
--
--    Layout_Error - Pointer is not in Destination'Range of there is  no
--                   room for output
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : UTF8_Code_Point
             );
--
-- Length -- The length of an UTF-8 string
--
--    Source - The string containing UTF-8 encoded code points
--
-- Returns :
--
--    The number of UTF-8 encoded code points in Source
--
-- Exceptions :
--
--    Data_Error - Invalid string
--
   function Length (Source : String) return Natural;
--
-- Skip -- Skip UTF-8 code point
--
--    Source  - UTF-8 encoded string
--    Pointer - The position of the first UTF-8 code point to skip
--    Count   - The number of code points to skip
--
-- After  successful  completion Source (Pointer) is the first character
-- following Count skipped UTF-8 encoded code points.
--
-- Exceptions :
--
--    Data_Error   - Invalid string
--    Layout_Error - Pointer is not in Source'First..Source'Last + 1
--    End_Error    - Less than Count UTF-8 points to skip
--
   procedure Skip
             (  Source  : String;
                Pointer : in out Integer;
                Count   : Natural := 1
             );
--
-- Value -- Conversion to code point
--
--    Source - One UTF-8 encoded code point
--
-- Returns :
--
--    The code point
--
-- Exceptions :
--
--    Data_Error - Illegal UTF-8 string
--
   function Value (Source : String) return UTF8_Code_Point;
--
-- Code_Points_Range -- A range of UTF-8 code points Low..High
--
   type Code_Points_Range is record
      Low  : UTF8_Code_Point;
      High : UTF8_Code_Point;
   end record;

   Full_Range : constant Code_Points_Range;
--
-- Code_Points_Ranges -- An array of ranges
--
   type Code_Points_Ranges is
      array (Positive range <>) of Code_Points_Range;

private
--
-- Reverse_Put -- Put one code point in reverse
--
--    Destination - The target string
--    Pointer     - The position where to place the encoded point
--    Prefix      - The prefix in UTF-8 encoding
--    Position    - The position of the last encoded character
--
-- This procedure  places  Prefix  &  Character'Val  (Position)  in  the
-- positions of Destination (..Pointer). Then Pointer is moved  back  to
-- the first position before the first character of the output.
--
-- Exceptions :
--
--    Layout_Error - No room for output
--
   procedure Reverse_Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Prefix      : String;
                Position    : Natural
             );

   Full_Range : constant Code_Points_Range :=
                (  Low  => UTF8_Code_Point'First,
                   High => UTF8_Code_Point'Last
                );
end Strings_Edit.UTF8;
