--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

package Glfw.Display.Modes is

   type Mode is record
      Width, Height : Natural;
      Red_Bits, Green_Bits, Blue_Bits : Natural;
   end record;

   type Mode_List is array (Positive range <>) of Mode;

   function Available_Modes return Mode_List;

   function Desktop_Mode return Mode;

end Glfw.Display.Modes;
