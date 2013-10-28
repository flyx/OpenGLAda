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

with Glfw.API;
with Glfw.Enums;

package body Glfw.Windows.Context is

   procedure Make_Current (Window : access Glfw.Windows.Window'Class) is
   begin
      if not Window.Initialized then
         -- null is accepted to detach the current context, but an uninitialized
         -- window *should* lead to an exception instead of detaching the
         -- context, so we handle this here
         raise Operation_Exception with "Window not initialized";
      end if;
      if Window = null then
         API.Make_Context_Current (System.Null_Address);
      else
         API.Make_Context_Current (Window.Handle);
      end if;
   end Make_Current;

   function Current return access Glfw.Windows.Window'Class is
      use type System.Address;

      Raw : constant System.Address := API.Get_Current_Context;
   begin
      if Raw = System.Null_Address then
         return null;
      else
         return API.Get_Window_User_Pointer (Raw);
      end if;
   end Current;

   procedure Swap_Buffers (Window : not null access Glfw.Windows.Window'Class) is
   begin
      API.Swap_Buffers (Window.Handle);
   end Swap_Buffers;

   procedure Set_Swap_Interval (Value : Swap_Interval) renames
     API.Swap_Interval;

   function Client_API (Window : not null access Glfw.Windows.Window'Class)
                        return API_Kind is
   begin
      return API.Get_Window_Attrib (Window.Handle, Enums.Client_API);
   end Client_API;

   function Profile (Window : not null access Glfw.Windows.Window'Class)
                     return OpenGL_Profile_Kind is
   begin
      return API.Get_Window_Attrib (Window.Handle, Enums.OpenGL_Profile);
   end Profile;

   procedure Get_Context_Version
     (Window : not null access Glfw.Windows.Window'Class;
      Major : out Positive;
      Minor, Revision : out Natural) is
   begin
      Major := Positive (Interfaces.C.int'(
        (API.Get_Window_Attrib (Window.Handle, Enums.Context_Version_Major))));
      Minor := Natural (Interfaces.C.int'(
        (API.Get_Window_Attrib (Window.Handle, Enums.Context_Version_Minor))));
      Revision := Natural (Interfaces.C.int'(
        (API.Get_Window_Attrib (Window.Handle, Enums.Context_Revision))));
   end Get_Context_Version;

   function Is_Forward_Compat
     (Window : not null access Glfw.Windows.Window'Class) return Boolean is
   begin
      return Boolean (Bool'(API.Get_Window_Attrib
                      (Window.Handle, Enums.OpenGL_Forward_Compat)));
   end Is_Forward_Compat;

   function Is_Debug_Context
     (Window : not null access Glfw.Windows.Window'Class) return Boolean is
   begin
      return Boolean (Bool'(API.Get_Window_Attrib
                      (Window.Handle, Enums.OpenGL_Debug_Context)));
   end Is_Debug_Context;

   function Robustness (Window : not null access Glfw.Windows.Window'Class)
                        return Robustness_Kind is
   begin
      return API.Get_Window_Attrib (Window.Handle, Enums.Context_Robustness);
   end Robustness;

end Glfw.Windows.Context;
