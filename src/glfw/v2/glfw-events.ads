--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Interfaces.C;

package Glfw.Events is

   type Button_State is (Release, Press);
   type Button_States is array (Positive range <>) of Button_State;

   procedure Poll_Events;

   procedure Wait_For_Events;

private
   for Button_State use (Release => 0, Press => 1);
   for Button_State'Size use Interfaces.C.int'Size;

   for Button_States'Component_Size use Interfaces.C.char'Size;
   pragma Convention (C, Button_States);
end Glfw.Events;
