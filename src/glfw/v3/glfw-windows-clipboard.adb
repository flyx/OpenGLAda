--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Interfaces.C.Strings;

with Glfw.API;

package body Glfw.Windows.Clipboard is

   function Get (Object : not null access Window'Class) return String is
      use type Interfaces.C.Strings.chars_ptr;

      Raw : constant Interfaces.C.Strings.chars_ptr
        := API.Get_Clipboard_String (Object.Handle);
   begin
      if Raw = Interfaces.C.Strings.Null_Ptr then
         raise Operation_Exception with "Could not get clipboard string";
      end if;
      return Interfaces.C.Strings.Value (Raw);
   end Get;

   procedure Set (Object : not null access Window'Class; Value : String) is
   begin
      API.Set_Clipboard_String (Object.Handle, Interfaces.C.To_C (Value));
   end Set;

end Glfw.Windows.Clipboard;
