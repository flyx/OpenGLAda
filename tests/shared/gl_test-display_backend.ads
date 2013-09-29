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

package GL_Test.Display_Backend is
   -- abstraction layer for tests so they can run on both GLFW 2 and 3.
   -- only exposes most basic functionality, not intended for usage outside
   -- tests.

   procedure Init;

   procedure Open_Window (Width, Height : Natural; Depth_Bits : Natural := 0);

   procedure Swap_Buffers;

   procedure Poll_Events;

   procedure Set_Window_Title (Value : String);

   function Escape_Pressed return Boolean;

   function Window_Opened return Boolean;

   procedure Close_Window;

   procedure Shutdown;

   procedure Configure_Minimum_OpenGL_Version (Major, Minor : Natural);

end GL_Test.Display_Backend;
