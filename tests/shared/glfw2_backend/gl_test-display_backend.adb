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
with Glfw.Events.Keys;

package body GL_Test.Display_Backend is

   procedure Init renames Glfw.Init;

   procedure Open_Window (Width, Height : Natural; Depth_Bits : Natural := 0) is
   begin
      Glfw.Display.Open (Width, Height, Mode => Glfw.Display.Window,
                         Depth_Bits =>  Depth_Bits);
   end Open_Window;

   procedure Poll_Events renames Glfw.Events.Poll_Events;

   procedure Swap_Buffers renames Glfw.Display.Swap_Buffers;

   procedure Set_Window_Title (Value : String) renames Glfw.Display.Set_Title;

   function Escape_Pressed return Boolean is
   begin
      return Glfw.Events.Keys.Pressed (Glfw.Events.Keys.Esc);
   end Escape_Pressed;

   function Window_Opened return Boolean renames Glfw.Display.Opened;

   procedure Close_Window renames Glfw.Display.Close;

   procedure Shutdown renames Glfw.Terminate_Glfw;

   procedure Configure_Minimum_OpenGL_Version (Major, Minor : Natural) is
   begin
      Glfw.Display.Hint_Minimum_OpenGL_Version (Major, Minor);
   end Configure_Minimum_OpenGL_Version;

end GL_Test.Display_Backend;
