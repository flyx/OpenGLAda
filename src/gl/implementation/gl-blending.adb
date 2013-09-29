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

with GL.API;

package body GL.Blending is

   procedure Set_Blend_Func (Src_Factor, Dst_Factor : Blend_Factor) is
   begin
      API.Blend_Func (Src_Factor, Dst_Factor);
      Check_OpenGL_Error;
   end Set_Blend_Func;

   procedure Set_Blend_Func (Draw_Buffer : Buffers.Draw_Buffer_Index;
                             Src_Factor, Dst_Factor : Blend_Factor) is
   begin
      API.Blend_Func_I (Draw_Buffer, Src_Factor, Dst_Factor);
      Check_OpenGL_Error;
   end Set_Blend_Func;

end GL.Blending;
