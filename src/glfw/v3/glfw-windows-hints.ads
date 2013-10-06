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

with Glfw.Windows.Context;

package Glfw.Windows.Hints is

   procedure Reset_To_Defaults;

   -----------------------------------------------------------------------------
   -- Window related
   -----------------------------------------------------------------------------

   procedure Set_Resizable (Value : Boolean);
   procedure Set_Visible   (Value : Boolean);
   procedure Set_Decorated (Value : Boolean);

   -----------------------------------------------------------------------------
   -- Framebuffer related
   -----------------------------------------------------------------------------

   procedure Set_Color_Bits (Red, Green, Blue, Alpha : Natural);

   procedure Set_Depth_Bits   (Value : Natural);
   procedure Set_Stencil_Bits (Value : Natural);

   procedure Set_Accumulation_Bits (Red, Green, Blue, Alpha : Natural);

   procedure Set_Aux_Buffers (Value : Natural);

   procedure Set_Stereo (Value : Boolean);

   procedure Set_Samples (Value : Natural);

   procedure Set_SRGB_Capable (Value : Boolean);

   procedure Set_Refresh_Rate (Value : Natural);

   -----------------------------------------------------------------------------
   -- Context related
   -----------------------------------------------------------------------------

   procedure Set_Client_API (Value : Context.API_Kind);

   procedure Set_Minimum_OpenGL_Version (Major : Positive; Minor : Natural);

   procedure Set_Robustness (Value : Context.Robustness_Kind);

   procedure Set_Forward_Compat (Value : Boolean);

   procedure Set_Debug_Context (Value : Boolean);

   procedure Set_Profile (Value : Context.OpenGL_Profile_Kind);
private
   -- to be able to use renames
   pragma Convention (C, Reset_To_Defaults);
end Glfw.Windows.Hints;
