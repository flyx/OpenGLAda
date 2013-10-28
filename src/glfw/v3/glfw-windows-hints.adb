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

package body Glfw.Windows.Hints is

   procedure Reset_To_Defaults renames API.Default_Window_Hints;

   procedure Set_Resizable (Value : Boolean) is
   begin
      API.Window_Hint (Enums.Resizable, Bool (Value));
   end Set_Resizable;

   procedure Set_Visible (Value : Boolean) is
   begin
      API.Window_Hint (Enums.Visible, Bool (Value));
   end Set_Visible;

   procedure Set_Decorated (Value : Boolean) is
   begin
      API.Window_Hint (Enums.Decorated, Bool (Value));
   end Set_Decorated;

   procedure Set_Color_Bits (Red, Green, Blue, Alpha : Natural) is
   begin
      API.Window_Hint (Enums.Red_Bits, Interfaces.C.int (Red));
      API.Window_Hint (Enums.Green_Bits, Interfaces.C.int (Green));
      API.Window_Hint (Enums.Blue_Bits, Interfaces.C.int (Blue));
      API.Window_Hint (Enums.Alpha_Bits, Interfaces.C.int (Alpha));
   end Set_Color_Bits;

   procedure Set_Depth_Bits (Value : Natural) is
   begin
      API.Window_Hint (Enums.Depth_Bits, Interfaces.C.int (Value));
   end Set_Depth_Bits;

   procedure Set_Stencil_Bits (Value : Natural) is
   begin
      API.Window_Hint (Enums.Stencil_Bits, Interfaces.C.int (Value));
   end Set_Stencil_Bits;

   procedure Set_Accumulation_Bits (Red, Green, Blue, Alpha : Natural) is
   begin
      API.Window_Hint (Enums.Accum_Red_Bits, Interfaces.C.int (Red));
      API.Window_Hint (Enums.Accum_Green_Bits, Interfaces.C.int (Green));
      API.Window_Hint (Enums.Accum_Blue_Bits, Interfaces.C.int (Blue));
      API.Window_Hint (Enums.Accum_Alpha_Bits, Interfaces.C.int (Alpha));
   end Set_Accumulation_Bits;

   procedure Set_Aux_Buffers (Value : Natural) is
   begin
      API.Window_Hint (Enums.Aux_Buffers, Interfaces.C.int (Value));
   end Set_Aux_Buffers;

   procedure Set_Stereo (Value : Boolean) is
   begin
      API.Window_Hint (Enums.Stereo, Bool (Value));
   end Set_Stereo;

   procedure Set_Samples (Value : Natural) is
   begin
      API.Window_Hint (Enums.Samples, Interfaces.C.int (Value));
   end Set_Samples;

   procedure Set_SRGB_Capable (Value : Boolean) is
   begin
      API.Window_Hint (Enums.SRGB_Capable, Bool (Value));
   end Set_SRGB_Capable;

   procedure Set_Refresh_Rate (Value : Natural) is
   begin
      API.Window_Hint (Enums.Refresh_Rate, Interfaces.C.int (Value));
   end Set_Refresh_Rate;

   procedure Set_Client_API (Value : Context.API_Kind) is
   begin
      API.Window_Hint (Enums.Client_API, Value);
   end Set_Client_API;

   procedure Set_Minimum_OpenGL_Version (Major : Positive; Minor : Natural) is
   begin
      API.Window_Hint (Enums.Context_Version_Major, Interfaces.C.int (Major));
      API.Window_Hint (Enums.Context_Version_Minor, Interfaces.C.int (Minor));
   end Set_Minimum_OpenGL_Version;

   procedure Set_Robustness (Value : Context.Robustness_Kind) is
   begin
      API.Window_Hint (Enums.Context_Robustness, Value);
   end Set_Robustness;

   procedure Set_Forward_Compat (Value : Boolean) is
   begin
      API.Window_Hint (Enums.OpenGL_Forward_Compat, Bool (Value));
   end Set_Forward_Compat;

   procedure Set_Debug_Context (Value : Boolean) is
   begin
      API.Window_Hint (Enums.OpenGL_Debug_Context, Bool (Value));
   end Set_Debug_Context;

   procedure Set_Profile (Value : Context.OpenGL_Profile_Kind) is
   begin
      API.Window_Hint (Enums.OpenGL_Profile, Value);
   end Set_Profile;

end Glfw.Windows.Hints;
