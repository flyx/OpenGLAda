--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <contact@flyx.org>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

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
