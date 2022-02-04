--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Glfw.API;

package body Glfw.Input.Mouse is

   function Raw_Motion_Supported return Boolean is
   begin
      return Boolean (API.Raw_Mouse_Motion_Supported);
   end Raw_Motion_Supported;

end Glfw.Input.Mouse;
