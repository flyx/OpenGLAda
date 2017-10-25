--                                                                    --
--  package Strings_Edit.Fields     Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2002       --
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
--  This package is used for developing  Put  subroutines  which  output
--  length  cannot  be  easily  estimated.  They   use   the   available
--  destination string space to first put  the  output  there  and  then
--  adjust the output within the field. They are used as follows:
--
--  procedure Put
--            (  Destination : in out String;
--               Pointer     : in out Integer;
--               Value       : in Something;
--               Field       : in Natural := 0;
--               Justify     : in Alignment := Left;
--               Fill        : in Character := ' '
--            )  is
--     Out_Field : constant Natural :=
--                    Get_Output_Field (Destination, Pointer, Field);
--     subtype Output is String (Pointer..Pointer + Out_Field - 1);
--     Text  : Output renames
--                Destination (Pointer..Pointer + Out_Field - 1);
--     Index : Integer := Pointer;
--  begin
--      -- The output is done in Text using Index as the pointer
--     Adjust_Output_Field
--     (  Destination,
--        Pointer,
--        Index,
--        Out_Field,
--        Field,
--        Justify,
--        Fill
--     );
--  end Put;
--
package Strings_Edit.Fields is
--
-- Get_Output_Field -- The space available for output
--
--      Destination - The destination string
--      Pointer     - The string pointer
--      Field       - The output field
--
-- Returns :
--
--      The number of characters
--
-- Exceptions :
--
--      Layout_Error - Illegal Pointer or no space available
--
   function Get_Output_Field
            (  Destination : String;
               Pointer     : Integer;
               Field       : Natural
            )  return Natural;
   pragma Inline (Get_Output_Field);
--
-- Get_Output_Field -- The space available for output
--
--      Destination - The destination string
--      Pointer     - The string pointer
--      Index       - The first character following the output
--      Out_Field   - The result of Get_Output_Field
--      Field       - The output field (the original parameter)
--      Justify     - Alignment within the field
--      Fill        - The fill character
--
-- The  output  Pointer..Index-1  is adjusted within the output field as
-- necessary. Pointer is set to either Pointer + Field or Index. 
--
   procedure Adjust_Output_Field
             (  Destination : in out String;
                Pointer     : in out Integer;
                Index       : Integer;
                Out_Field   : Natural;
                Field       : Natural;
                Justify     : Alignment;
                Fill        : Character
             );
   pragma Inline (Adjust_Output_Field);
end Strings_Edit.Fields;
