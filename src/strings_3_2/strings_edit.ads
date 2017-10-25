--                                                                    --
--  package Strings_Edit            Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2000       --
--                                                                    --
--                                Last revision :  22:14 08 May 2009  --
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
-- The following I/O items are supported by the package:
--
-- (o) Integer numbers (generic, package Integer_Edit)
-- (o) Floating-point numbers (generic, package Float_Edit)
-- (o) Roman numbers (the child package Roman_Edit)
-- (o) Strings
--
-- The major differences to  the  standard  Image/Value  attributes  and
-- Text_IO procedures.
--
-- 1. For  numeric  types,  the  base  is  neither written nor read. For
--    instance, output of 23 as hexadecimal gives 17, not 16#17#.
--
-- 2. Get procedures do not skip blank characters around  input  tokens,
--    except  the  cases  when  the  blank  charactes is required by the
--    syntax.
--
-- 3. Get  procedures  use  the current string position pointer, so that
--    they can be consequently called advancing the pointer as the tokes
--    are recognized.
--
-- 4. Numeric  get procedures allow to specify the expected value range.
--    When  the  actual  value  is  out  of  the range then depending on
--    procedure  parameters, either Constrain_Error is propagated or the
--    value is forced to the nearest range boundary.
--
-- 5. Put procedures also use the current string position pointer, which
--    allows to call them consequently.
--
-- 6. The format used for floating-number output is based on the  number
--    precision, instead of rather typographic approach of Text_IO.  The
--    precision can be specified either as the number of valid digits of
--    the  current  base  (i.e. relative) or as the position of the last
--    valid digit (i.e. absolute). For instance, 12.345678 with relative
--    precision 3 gives 12.3.  With  absolute  precision  -3,  it  gives
--    12.346.
--
-- G E T   P R O C E D U R E S
--
-- Get procedures are used to scan strings. The first two parameters are
-- always Source and Pointer. Source is the string to be scaned. Pointer
-- indicates  the  current  position.  After successful completion it is
-- advanced  to the first string position following the recognized item.
-- The  value  of  Pointer  shall  be  in  the  range  Source'First   ..
-- Source'Last+1. The Layout_Error exception  is  propagated  when  this
-- check fails. The third  parameter  usually  accepts  the  value.  The
-- following example shows how to use get procedures:
--
--       package Edit_Float is new Strings_Edit.Float_Edit (Float);
--       use Edit_Float;
--
--       Line        : String (1..512);   -- A line
--       Pointer     : Integer;
--       Value       : Float;
--       TabAndSpace : Ada.Strings.Maps.Character_Set :=
--                        To_Set (" " & Ada.Characters.Latin_1.HT);
--    begin
--       ...
--       Pointer := Line'First;
--       Get (Line, Pointer, TabAndSpace);  -- Skip tabs and spaces
--       Get (Line, Pointer, Value);        -- Get number
--       Get (Line, Pointer, TabAndSpace);  -- Skip tabs and spaces
--       ...
--
-- The numeric Get procedures have additional parameters controlling the
-- range  of  the  input value. The parameters First and Last define the
-- range of  the  expected  value.  The  exception  Constraint_Error  is
-- propagated  when  the value is not in the range. The exception can be
-- suppressed using the parameters ToFirst and ToLast, which  cause  the
-- input value to be substituted by the corresponding  margin  when  the
-- parameter is True.
--
-- V A L U E   F U N C T I O N S
--
-- Each  get procedure returning some value has a corresponding function
-- Value.  The  function  Value  has the same parameter profile with the
-- exception  that  the  parameter  Pointer  is  absent and the value is
-- returned via result. Unlike Get the function Value  tolerates  spaces
-- and  tabs  around  the  converted  value.  The whole string should be
-- matched, otherwise, the exception Data_Error is propagated.
--
-- P U T   P R O C E D U R E S
--
-- Put procedures place something into the  output  string  Destination.
-- The string  is  written  starting  from  Destination  (Pointer).  The
-- parameter Field defines the output size. When it has the  value  zero
-- then the output size is defined by the  output  item.  Otherwise  the
-- output  is  justified  within  the  field  and  the parameter Justify
-- specifies output alignment and  the  parameter  Fill  gives  the  pad
-- character. When Field is greater than Destination'Last - Pointer + 1,
-- the later is used instead. After  successful  completion  Pointer  is
-- advanced   to   the  first  character  following  the  output  or  to
-- Destination'Last + 1.
--
-- I M A G E   F U N C T I O N S
--
-- Image  functions convert a value into string. Unlike standard S'Image
-- they do not place an extra space character.
--
with Ada.Strings.Maps;  use Ada.Strings.Maps;

with Ada.Strings;
with Ada.Characters.Latin_1;

package Strings_Edit is
   pragma Elaborate_Body (Strings_Edit);
   MaxSmall    : constant := 250;  -- Bigger than any possible
   Figures     : constant String := "0123456789ABCDEF";
   Blanks      : constant String := ' ' & Ada.Characters.Latin_1.HT;
   SpaceAndTab : constant Character_Set := To_Set (Blanks);

   subtype Alignment is Ada.Strings.Alignment;

   Center : Alignment renames Ada.Strings.Center;
   Left   : Alignment renames Ada.Strings.Left;
   Right  : Alignment renames Ada.Strings.Right;

   subtype NumberBase is Integer range 2..16;
--
-- S T R I N G S
--
   -- Get -- Skip blank characters
   --
   --    Source  - The string to be processed
   --    Pointer - The current position in the string
   --    Blank   - The blank character
   --
   -- This procedure skips the  character  Blank  starting  from  Source
   -- (Pointer). Pointer is advanced to the first non-Blank character or
   -- to Source'Last + 1.
   --
   -- Exceptions :
   --
   --    Layout_Error - Pointer is not in Source'First..Source'Last + 1
   --
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Blank   : Character := ' '
             );
   --
   -- Get -- Skip blank characters
   --
   --    Source  - The string to be processed
   --    Pointer - The current position in the string
   --    Blanks  - The set characters to be considered as blank ones
   --
   -- This procedure skips all the characters of the set Blanks starting
   -- from  Source (Pointer). Pointer is advanced to the first non-blank
   -- character or to Source'Last + 1.
   --
   -- Exceptions :
   --
   --    Layout_Error - Pointer is not in Source'First..Source'Last + 1
   --
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Blanks  : Character_Set
             );
   --
   -- Put -- Put a character into a string
   --
   --    Destination - The string that accepts the output
   --    Pointer     - The current position in the string
   --    Value       - The character to be put
   --    Field       - The output field
   --    Justify     - Alignment within the field
   --    Fill        - The fill character
   --
   -- This  procedure  places  the specified character (Value parameter)
   -- into the output string Destination. The string is written starting
   -- from the Destination (Pointer).
   --
   -- Exceptions:
   --
   --    Layout_Error - Pointer  is not in Destination'Range or there is
   --                   no room for the output.
   --
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Character;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
   --
   -- Put -- Put a string into another string
   --
   --    Destination - The string that accepts the output
   --    Pointer     - The current position in the string
   --    Value       - The string to be put
   --    Field       - The output field
   --    Justify     - Alignment within the field
   --    Fill        - The fill character
   --
   -- This  procedure places the specified by the Value parameter string
   -- into the output string Destination. The string is written starting
   -- from the Destination (Pointer).
   --
   -- Exceptions:
   --
   --    Layout_Error - Pointer  is not in Destination'Range or there is
   --                   no room for the output.
   --
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : String;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- R O M A N   N U M B E R S
--
   -- Roman_Edit -- Child package for roman numbers
--
-- I N T E G E R   N U M B E R S
--
   -- Integer_Edit -- Generic child package for integer numbers
--
-- F L O A T I N G - P O I N T   N U M B E R S
--
   -- Float_Edit -- Generic child package for floating-point numbers
--
-- P R E F I X   T E S T
--
   --
   -- Is_Prefix -- Test if Prefix is a prefix of Text
   --
   --    Prefix    - To check
   --    Source    - The string
   --  [ Pointer ] - To start at
   --  [ Map     ] - Used to convert characters before comparison
   --
   -- Returns :
   --
   --   True if Prefix is a prefix of Source
   --
   function Is_Prefix (Prefix, Source : String) return Boolean;
   function Is_Prefix
            (  Prefix, Source : String;
               Pointer        : Integer
            )  return Boolean;
   function Is_Prefix
            (  Prefix, Source : String;
               Map            : Character_Mapping
            )  return Boolean;
   function Is_Prefix
            (  Prefix, Source : String;
               Pointer        : Integer;
               Map            : Character_Mapping
            )  return Boolean;
--
-- T R I M   F U N C T I O N S
--
   -- Trim -- Delete blank characters form string ends
   --
   --    Source - The string to be processed
   --    Blank  - The blank character
   --
   -- This function removes the Blank character from both  ends  of  the
   -- string and returns the result.
   --
   -- Returns :
   --
   --    The result string
   --
   function Trim
            (  Source : String;
               Blank  : Character := ' '
            )  return String;
   --
   -- Trim -- Delete blank characters form string ends
   --
   --    Source - The string to be processed
   --    Blanks - The set of blank characters
   --
   -- This  function  removes any characters of the Blanks set from both
   -- ends of the string and returns the result.
   --
   -- Returns :
   --
   --    The result string
   --
   function Trim
            (  Source : String;
               Blanks : Character_Set
            )  return String;

private
   pragma Inline (Is_Prefix);
   --
   -- GetDigit -- Get one digit
   --
   --	   Symbol - To be decoded
   --
   -- Returns :
   --
   --	 [0..15]  The decoded digit value
   --    [16]   The Symbol is not a digit
   --
   function GetDigit (Symbol : Character) return Natural;
   pragma Inline (GetDigit);

end Strings_Edit;
