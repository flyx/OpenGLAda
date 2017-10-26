--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

package GL_Test.Display_Backend is
   -- abstraction layer for tests so they can run on both GLFW 2 and 3.
   -- only exposes most basic functionality, not intended for usage outside
   -- tests.

   procedure Init;

   procedure Open_Window (Width, Height : Natural; Depth_Bits : Natural := 0);

   procedure Swap_Buffers;

   procedure Poll_Events;

   procedure Wait_For_Events;

   procedure Set_Window_Title (Value : String);

   function Escape_Pressed return Boolean;

   function Window_Opened return Boolean;

   procedure Close_Window;

   procedure Shutdown;

   procedure Configure_Minimum_OpenGL_Version (Major, Minor : Natural);

end GL_Test.Display_Backend;
