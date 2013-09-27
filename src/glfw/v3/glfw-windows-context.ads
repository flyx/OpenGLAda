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

package Glfw.Windows.Context is

   type Robustness is (No_Robustness, No_Reset_Notification,
                       Lose_Context_On_Reset);

   procedure Make_Current (Window : access Glfw.Windows.Window'Class);

   function Current return access Glfw.Windows.Window'Class;

   procedure Swap_Buffers (Window : not null access Glfw.Windows.Window'Class);
private
   for Robustness use (No_Robustness => 0,
                       No_Reset_Notification => 16#31001#,
                       Lose_Context_On_Reset => 16#31002#);
   for Robustness'Size use Interfaces.C.int'Size;
end Glfw.Windows.Context;
