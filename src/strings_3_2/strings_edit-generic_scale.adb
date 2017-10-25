--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Strings_Edit.Generic_Scale                  Luebeck            --
--  Implementation                                 Spring, 2007       --
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

package body Strings_Edit.Generic_Scale is

   function Create (Low, High : Value; Count : Natural) return Scale is
      Result : Scale;
      Power  : Value'Base := 1.0;
      Major  : Value'Base := High - Low;
   begin
      if Low >= High then
         raise Constraint_Error;
      end if;
      Result.Small := 0;
      if Count > 1 then
         Major := Major / Value (Count);
      end if;
      while Major >= 10.0 loop
         Major := Major / 10.0;
         Power := Power * 10.0;
         Result.Small := Result.Small + 1;
      end loop;
      while Major < 1.0 loop
         Major := Major * 10.0;
         Power := Power / 10.0;
         Result.Small := Result.Small - 1;
      end loop;
      Major := Value'Truncation (Major);
      if Major > 1.0 then
         if Major > 2.0 then
            if Major > 5.0 then
               Result.Minor := Power * 5.0;
               Result.Ticks := 1;
            else
               Result.Minor := Power;
               Result.Ticks := 4;
            end if;
         else
            Result.Minor := Power;
            Result.Ticks := 1;
         end if;
      else
         Result.Minor := Power / 2.0;
         Result.Ticks := 1;
      end if;
      Major := Value'Ceiling (Low / Result.Minor);
      Result.Low_Value := Major * Result.Minor;
      declare
         No : constant Integer := 
                 Integer
                 (  Value'Rounding
                    (  Value'Remainder
                       (  Major,
                          Value'Base (Result.Ticks + 1)
                 )  )  );
      begin
         if No < 0 then
            Result.Low_Tick := Result.Ticks + 1 + No;
         else
            Result.Low_Tick := No;
         end if;
      end;
      Major := Result.Minor * Value'Base (Result.Ticks + 1) / Power;
      while Major > 9.9 loop
         Result.Small := Result.Small + 1;
         Major := Major / 10.0;
      end loop;
      return Result;
   end Create;

end Strings_Edit.Generic_Scale;
