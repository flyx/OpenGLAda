--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Glfw.API;

package body Glfw.Input is

   procedure Poll_Events renames API.Poll_Events;

   procedure Wait_For_Events renames API.Wait_Events;

end Glfw.Input;
