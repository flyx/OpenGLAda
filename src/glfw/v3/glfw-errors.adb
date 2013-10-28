--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <contact@flyx.org>
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

with Interfaces.C.Strings;

with Glfw.API;

package body Glfw.Errors is

   Cur_Callback : Callback := null;

   procedure Raw_Handler (Code : Kind;
                          Description : Interfaces.C.Strings.chars_ptr);
   pragma Convention (C, Raw_Handler);

   procedure Raw_Handler (Code : Kind;
                          Description : Interfaces.C.Strings.chars_ptr) is
   begin
      if Cur_Callback /= null then
         Cur_Callback.all (Code, Interfaces.C.Strings.Value (Description));
      end if;
   end Raw_Handler;

   procedure Set_Callback (Handler : Callback) is
      use type API.Error_Callback;
      Previous : API.Error_Callback;
      pragma Warnings (Off, Previous);
   begin
      Cur_Callback := Handler;
      if Handler = null then
         Previous := API.Set_Error_Callback (null);
      else
         Previous := API.Set_Error_Callback (Raw_Handler'Access);
      end if;
   end Set_Callback;

end Glfw.Errors;
