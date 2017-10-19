--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

package Glfw.Input is
   type Button_State is (Released, Pressed);

   type Sticky_Toggle is (Sticky_Keys, Sticky_Mouse_Buttons);

   procedure Poll_Events;
   procedure Wait_For_Events;
private
   for Button_State use (Released => 0, Pressed => 1);
   for Button_State'Size use Interfaces.C.int'Size;

   for Sticky_Toggle use (Sticky_Keys          => 16#33002#,
                          Sticky_Mouse_Buttons => 16#33003#);
   for Sticky_Toggle'Size use Interfaces.C.int'Size;

   -- just so we can implement them with rename
   pragma Convention (C, Poll_Events);
   pragma Convention (C, Wait_For_Events);

end Glfw.Input;
