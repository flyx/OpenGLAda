--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Glfw.Events.Keys;

package Glfw_Test is

   procedure Key_To_Title (Subject : Glfw.Events.Keys.Key;
                           Action : Glfw.Events.Button_State);

end Glfw_Test;
