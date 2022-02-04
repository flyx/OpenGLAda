--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

package Glfw.Input.Mouse is

   type Button is new Interfaces.C.int range 0 .. 7;

   type Enter_Action is (Leaving, Entering);

   type Cursor_Mode is (Normal, Hidden, Disabled);

   Left_Button   : constant := 0;
   Right_Button  : constant := 1;
   Middle_Button : constant := 2;

   subtype Coordinate is Interfaces.C.double;
   subtype Scroll_Offset is Interfaces.C.double;

   function Raw_Motion_Supported return Boolean;

private
   for Button'Size use Interfaces.C.int'Size;

   for Enter_Action use (Leaving  => 0,
                         Entering => 1);
   for Enter_Action'Size use C.int'Size;

   for Cursor_Mode use (Normal   => 16#34001#,
                        Hidden   => 16#34002#,
                        Disabled => 16#34003#);
   for Cursor_Mode'Size use Interfaces.C.int'Size;
end Glfw.Input.Mouse;
