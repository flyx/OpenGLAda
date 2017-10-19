--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

package Glfw.Windows.Clipboard is
   -- strings are UTF-8 encoded

   function Get (Object : not null access Window'Class) return String;

   procedure Set (Object : not null access Window'Class; Value : String);
end Glfw.Windows.Clipboard;
