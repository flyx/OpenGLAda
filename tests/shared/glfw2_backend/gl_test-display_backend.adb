--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

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

   procedure Wait_For_Events renames Glfw.Events.Wait_For_Events;

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
