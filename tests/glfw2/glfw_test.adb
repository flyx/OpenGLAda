--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

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
