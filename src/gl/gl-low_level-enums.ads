--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <flyx@isobeef.org>
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

package GL.Low_Level.Enums is
   type Texture_Kind is (Texture_1D, Texture_2D, Texture_3D, Texture_Cube_Map);
   
   type Draw_Buffer_Index is (DB0, DB1, DB2, DB3, DB4, DB5, DB6, DB7,
      DB8, DB9, DB10, DB11, DB12, DB13, DB14, DB15);
      
   type Only_Depth_Buffer is (Depth_Buffer);
   type Only_Stencil_Buffer is (Stencil);
   type Only_Depth_Stencil_Buffer is (Depth_Stencil);
private
   for Texture_Kind use (Texture_1D       => 16#0DE0#,
                         Texture_2D       => 16#0DE1#,
                         Texture_3D       => 16#806F#,
                         Texture_Cube_Map => 16#8513#);
   for Texture_Kind'Size use Enum'Size;
   
   for Draw_Buffer_Index use (DB0  => 16#8825#,
                              DB1  => 16#8826#,
                              DB2  => 16#8827#,
                              DB3  => 16#8828#,
                              DB4  => 16#8829#,
                              DB5  => 16#882A#,
                              DB6  => 16#882B#,
                              DB7  => 16#882C#,
                              DB8  => 16#882D#,
                              DB9  => 16#882E#,
                              DB10 => 16#882F#,
                              DB11 => 16#8830#,
                              DB12 => 16#8831#,
                              DB13 => 16#8832#,
                              DB14 => 16#8833#,
                              DB15 => 16#8834#);
   for Draw_Buffer_Index'Size use Low_Level.Int'Size;
   
   for Only_Depth_Buffer use (Depth_Buffer => 16#1801#);
   for Only_Depth_Buffer'Size use Enum'Size;
   
   for Only_Stencil_Buffer use (Stencil => 16#1802#);
   for Only_Stencil_Buffer'Size use Enum'Size;
   
   for Only_Depth_Stencil_Buffer use (Depth_Stencil => 16#84F9#);
   for Only_Depth_Stencil_Buffer'Size use Enum'Size;
end GL.Low_Level.Enums;