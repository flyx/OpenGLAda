--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <flyx@isobeef.org>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with GL.API;
with GL.Enums;

package body GL is
   -----------------------------------------------------------------------------
   --                              Global state                               --
   -----------------------------------------------------------------------------
   
   Error_Checking_Enabled   : Boolean := True;
   Error_Checking_Suspended : Boolean := False;
   
   -----------------------------------------------------------------------------
   --                            Implementations                              --
   -----------------------------------------------------------------------------
   
   procedure Flush is
   begin
      API.Flush;
   end Flush;

   procedure Finish is
   begin
      API.Finish;
   end Finish;
   
   procedure Toggle_Error_Checking (Enabled : Boolean) is
   begin
      Error_Checking_Enabled := Enabled;
   end Toggle_Error_Checking;

   procedure Check_OpenGL_Error is
   begin
      if Error_Checking_Enabled and not Error_Checking_Suspended then
         case API.Get_Error is
            when Enums.Invalid_Operation => raise Invalid_Operation;
            when Enums.Invalid_Value => raise Invalid_Value;
            when Enums.Out_Of_Memory => raise Out_Of_Memory;
            when Enums.Invalid_Enum => raise Internal_Error;
            when Enums.No_Error => null;
         end case;
      end if;
   exception
         when Constraint_Error => raise Internal_Error;
   end Check_OpenGL_Error;
   
   procedure Suspend_Error_Checking is
   begin
      Error_Checking_Suspended := True;
   end Suspend_Error_Checking;
   
   procedure Resume_Error_Checking is
   begin
      Error_Checking_Suspended := False;
   end Resume_Error_Checking;

end GL;
