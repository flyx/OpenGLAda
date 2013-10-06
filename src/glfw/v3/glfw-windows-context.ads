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

package Glfw.Windows.Context is

   type OpenGL_Profile_Kind is (System_Default, Core_Profile, Compat_Profile);
   type API_Kind is (OpenGL, OpenGL_ES);
   type Robustness_Kind is (No_Robustness, No_Reset_Notification,
                            Lose_Context_On_Reset);

   subtype Swap_Interval is Interfaces.C.int;

   procedure Make_Current (Window : access Glfw.Windows.Window'Class);

   function Current return access Glfw.Windows.Window'Class;

   procedure Swap_Buffers (Window : not null access Glfw.Windows.Window'Class);

   procedure Set_Swap_Interval (Value : Swap_Interval);

   function Client_API (Window : not null access Glfw.Windows.Window'Class)
                        return API_Kind;

   function Profile (Window : not null access Glfw.Windows.Window'Class)
                     return OpenGL_Profile_Kind;

   procedure Get_Context_Version
     (Window : not null access Glfw.Windows.Window'Class;
      Major : out Positive;
      Minor, Revision : out Natural);

   function Is_Forward_Compat
     (Window : not null access Glfw.Windows.Window'Class) return Boolean;

   function Is_Debug_Context
     (Window : not null access Glfw.Windows.Window'Class) return Boolean;

   function Robustness (Window : not null access Glfw.Windows.Window'Class)
                        return Robustness_Kind;

private
   for OpenGL_Profile_Kind use (System_Default => 0,
                                Core_Profile   => 16#32001#,
                                Compat_Profile => 16#32002#);
   for OpenGL_Profile_Kind'Size use Interfaces.C.int'Size;

   for API_Kind use (OpenGL    => 16#30001#,
                     OpenGL_ES => 16#30002#);
   for API_Kind'Size use Interfaces.C.int'Size;

   for Robustness_Kind use (No_Robustness => 0,
                            No_Reset_Notification => 16#31001#,
                            Lose_Context_On_Reset => 16#31002#);
   for Robustness_Kind'Size use Interfaces.C.int'Size;

   -- implemented with renames
   pragma Convention (C, Set_Swap_Interval);
end Glfw.Windows.Context;
