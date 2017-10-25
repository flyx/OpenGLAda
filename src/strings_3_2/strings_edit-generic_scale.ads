--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Generic_Scale                  Luebeck            --
--  Interface                                      Spring, 2007       --
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
--  This package is used for automatic evaluation of  the  positions  of
--  the ticks on a scale. The scale looks like:
--
--    Low |                                                High |
--     -->|                    Count = 5                     -->|
--        |                                                     |
--        |  |         |         |         |         |         ||
--        || | | | | | | | | | | | | | | | | | | | | | | | | | ||
--         |\           \             \                  | |
--         | First tick  \             A minor tick    ->|-|<-- Minor
--      -->| Low_Value    A major tick
--           Low_Tick = 4                  Ticks = 4
--
generic
   type Value is digits <>;
package Strings_Edit.Generic_Scale is
--
-- Scale -- The scale description
--
   type Scale is record
      Minor     : Value'Base; -- The minor tick step
      Low_Value : Value;      -- The first tick value
      Low_Tick  : Natural;    -- The first tick number 0..Ticks
      Ticks     : Natural;    -- The number of minor ticks
      Small     : Integer;    -- The absolute precison of major ticks
   end record;
--
-- Create -- Create scale
--
--    Low   - The low value of the scale
--    High  - The high value of the scale
--    Count - The approximate number of major tick (lower bound)
--
-- This function creates a scale fitting the parameters. The first  tick
-- may  differ  from Low. The function tries to find a major tick length
-- such  that  the interval [Low, High] could be split into no less than
-- Count major ticks. Zero Count is treated as 1. The major tick  length
-- on  the value scale is chosen to be L={1|2|5}*10**n. The locations of
-- major  ticks  are  selected  at  m*L.  The  minor  ticks are selected
-- depending on L: 
--
--     L=1*10**n  | . | . | . | . | . | (1 minor tick)
--     L=2*10**n  | . | . | . | . | . | (1 minor tick)
--     L=5*10**n  | . . . . | . . . . | (4 minor ticks)
--
-- The  field  Small  can  be  used  for  resonable output of the values
-- associated with the major ticks. 
--
-- Exceptions :
--
--    Constraint_Error - Low >= High
--
   function Create (Low, High : Value; Count : Natural) return Scale;

end Strings_Edit.Generic_Scale;
