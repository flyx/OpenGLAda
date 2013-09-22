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

with Glfw.Display;

package body Glfw_Test is

   procedure Key_To_Title (Subject : Glfw.Events.Keys.Key;
                           Action : Glfw.Events.Button_State) is
      use type Glfw.Events.Button_State;
   begin
      if Action = Glfw.Events.Press then
         Glfw.Display.Set_Title ("Key " & Glfw.Events.Keys.Name (Subject)
                                 & " has been pressed.");
      else
         Glfw.Display.Set_Title ("Key " & Glfw.Events.Keys.Name (Subject)
                                 & " has been released.");
      end if;
   end Key_To_Title;

end Glfw_Test;
