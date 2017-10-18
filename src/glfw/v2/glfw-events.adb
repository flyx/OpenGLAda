--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Glfw.API;

package body Glfw.Events is
   procedure Poll_Events is
   begin
      API.Poll_Events;
   end Poll_Events;

   procedure Wait_For_Events is
   begin
      API.Wait_Events;
   end Wait_For_Events;

end Glfw.Events;
