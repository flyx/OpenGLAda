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

package Glfw.Display is

   type Display_Mode is (Window, Fullscreen);

   type OpenGL_Profile_Kind is (System_Default, Core_Profile, Compat_Profile);

   -- return False to cancel window closing
   type Close_Callback is access function return Boolean;

   type Size_Callback is access procedure (Width, Height : Natural);

   type Refresh_Callback is access procedure;

   Display_Exception : exception;

   -----------------------------------------------------------------------------
   --                        Display operations                               --
   -----------------------------------------------------------------------------

   procedure Open (Width        : Natural := 0; -- 0 = 4/3 Height, or 640
                   Height       : Natural := 0; -- 0 = 3/4 Width, or 480
                   Red_Bits     : Natural := 0; -- 0 = default color depth
                   Green_Bits   : Natural := 0; -- 0 = default color depth
                   Blue_Bits    : Natural := 0; -- 0 = default color depth
                   Alpha_Bits   : Natural := 0; -- 0 = no alpha channel
                   Depth_Bits   : Natural := 0; -- 0 = no depth buffer
                   Stencil_Bits : Natural := 0; -- 0 = no stencil buffer
                   Mode         : Display_Mode);

   procedure Close;

   procedure Iconify;

   procedure Restore;

   procedure Swap_Buffers;

   -----------------------------------------------------------------------------
   --                        Display Hints:                                   --
   --          These values will be applied at the next call to Open.         --
   -----------------------------------------------------------------------------

   procedure Hint_Refresh_Rate            (Value : Natural);
   procedure Hint_Accum_Buffer_Red_Bits   (Value : Natural);
   procedure Hint_Accum_Buffer_Green_Bits (Value : Natural);
   procedure Hint_Accum_Buffer_Blue_Bits  (Value : Natural);
   procedure Hint_Accum_Buffer_Alpha_Bits (Value : Natural);
   procedure Hint_Auxiliary_Buffer_Count  (Value : Natural);
   procedure Hint_Stereo_Rendering        (Value : Boolean);
   procedure Hint_Resizable               (Value : Boolean);
   procedure Hint_FSAA_Sample_Count       (Value : Natural);
   procedure Hint_Minimum_OpenGL_Version  (Major, Minor : Natural);
   procedure Hint_Forward_Compatible      (Value : Boolean);
   procedure Hint_Debug_Context           (Value : Boolean);
   procedure Hint_OpenGL_Profile          (Value : OpenGL_Profile_Kind);

   -----------------------------------------------------------------------------
   --        Display property setters, will be applied immediately.           --
   -----------------------------------------------------------------------------

   procedure Set_Title          (Title : String);
   procedure Set_Size           (Width, Height : Natural);
   procedure Set_Position       (X, Y : Natural);
   procedure Set_Swap_Interval  (Interval : Natural);

   procedure Set_Size_Callback    (Callback : Size_Callback);
   procedure Set_Close_Callback   (Callback : Close_Callback);
   procedure Set_Refresh_Callback (Callback : Refresh_Callback);

   -----------------------------------------------------------------------------
   --                      Display property getters                           --
   -----------------------------------------------------------------------------

   function Width  return Natural;
   function Height return Natural;

   function Opened      return Boolean;
   function Active      return Boolean;
   function Iconified   return Boolean;
   function Accelerated return Boolean;

   function Red_Bits     return Natural;
   function Green_Bits   return Natural;
   function Blue_Bits    return Natural;
   function Alpha_Bits   return Natural;
   function Depth_Bits   return Natural;
   function Stencil_Bits return Natural;

   function Refresh_Rate return Natural;

   function Accum_Red_Bits   return Natural;
   function Accum_Green_Bits return Natural;
   function Accum_Blue_Bits  return Natural;
   function Accum_Alpha_Bits return Natural;

   function Auxiliary_Buffer_Count return Natural;

   function Stereo_Rendering_Supported return Boolean;

   function Resizable return Boolean;

   function FSAA_Sample_Count return Natural;

   function OpenGL_Version_Major      return Natural;
   function OpenGL_Version_Minor      return Natural;
   function OpenGL_Forward_Compatible return Boolean;
   function OpenGL_Debug_Context      return Boolean;
   function OpenGL_Profile            return OpenGL_Profile_Kind;

private

   for Display_Mode use (Window => 16#10001#, Fullscreen => 16#10002#);
   for Display_Mode'Size use Interfaces.C.int'Size;

   for OpenGL_Profile_Kind use (System_Default => 0,
                                Core_Profile   => 16#50001#,
                                Compat_Profile => 16#50002#);
   for OpenGL_Profile_Kind'Size use Interfaces.C.int'Size;

end Glfw.Display;
