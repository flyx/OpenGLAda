--------------------------------------------------------------------------------
-- Copyright (c) 2016, Felix Krause <contact@flyx.org>
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

with GL.API;
with GL.Enums;

package body GL.Tessellation is

   procedure Set_Patch_Vertices (Value: Int) is
   begin
      API.Set_Patch_Parameter_Int (Enums.Vertices, Value);
   end Set_Patch_Vertices;

   procedure Set_Patch_Default_Inner_Level (Values: Single_Array) is
   begin
      API.Set_Patch_Parameter_Float_Array (Enums.Default_Inner_Level, Values);
   end Set_Patch_Default_Inner_Level;

   procedure Set_Patch_Default_Outer_Level (Values: Single_Array) is
   begin
      API.Set_Patch_Parameter_Float_Array (Enums.Default_Outer_Level, Values);
   end Set_Patch_Default_Outer_Level;
end GL.Tessellation;