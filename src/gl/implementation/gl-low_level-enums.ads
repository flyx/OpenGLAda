--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <contact@flyx.org>
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
   pragma Preelaborate;
   
   -- Unlike GL.Enums, this package is not private and hosts enum types that may
   -- be needed by third-party code or wrappers.
   
   type Texture_Kind is (Texture_1D, Texture_2D, Proxy_Texture_1D,
                         Proxy_Texture_2D, Texture_3D, Proxy_Texture_3D,
                         Texture_Cube_Map, Texture_Cube_Map_Positive_X,
                         Texture_Cube_Map_Negative_X,
                         Texture_Cube_Map_Positive_Y,
                         Texture_Cube_Map_Negative_Y,
                         Texture_Cube_Map_Positive_Z,
                         Texture_Cube_Map_Negative_Z,
                         Proxy_Texture_Cube_Map,
                         Texture_1D_Array,
                         Proxy_Texture_1D_Array, Texture_2D_Array,
                         Proxy_Texture_2D_Array, Texture_Buffer);
   
   type Renderbuffer_Kind is (Renderbuffer);
   
   type Framebuffer_Kind is (Read, Draw, Read_Draw);
   
   type Buffer_Kind is (Array_Buffer, Element_Array_Buffer, Pixel_Pack_Buffer,
                        Pixel_Unpack_Buffer, Uniform_Buffer, Texture_Buffer,
                        Transform_Feedback_Buffer, Copy_Read_Buffer,
                        Copy_Write_Buffer, Draw_Indirect_Buffer,
                        Atomic_Counter_Buffer);
   
   type Draw_Buffer_Index is (DB0, DB1, DB2, DB3, DB4, DB5, DB6, DB7,
      DB8, DB9, DB10, DB11, DB12, DB13, DB14, DB15);
   
   type Only_Depth_Buffer is (Depth_Buffer);
   type Only_Stencil_Buffer is (Stencil);
   type Only_Depth_Stencil_Buffer is (Depth_Stencil);
   type Only_Color_Buffer is (Color);
private
   for Texture_Kind use (Texture_1D       => 16#0DE0#,
                         Texture_2D       => 16#0DE1#,
                         Proxy_Texture_1D => 16#8063#,
                         Proxy_Texture_2D => 16#8064#,
                         Texture_3D       => 16#806F#,
                         Proxy_Texture_3D => 16#8070#,
                         Texture_Cube_Map => 16#8513#,
                         Texture_Cube_Map_Positive_X => 16#8515#,
                         Texture_Cube_Map_Negative_X => 16#8516#,
                         Texture_Cube_Map_Positive_Y => 16#8517#,
                         Texture_Cube_Map_Negative_Y => 16#8518#,
                         Texture_Cube_Map_Positive_Z => 16#8519#,
                         Texture_Cube_Map_Negative_Z => 16#851A#,
                         Proxy_Texture_Cube_Map      => 16#851B#,
                         Texture_1D_Array       => 16#8C18#,
                         Proxy_Texture_1D_Array => 16#8C19#,
                         Texture_2D_Array       => 16#8C1A#,
                         Proxy_Texture_2D_Array => 16#8C1B#,
                         Texture_Buffer         => 16#8C2A#);
   for Texture_Kind'Size use Enum'Size;
   
   for Renderbuffer_Kind use (Renderbuffer => 16#8D41#);
   for Renderbuffer_Kind'Size use Enum'Size;
   
   for Framebuffer_Kind use (Read      => 16#8CA8#,
                             Draw      => 16#8CA9#,
                             Read_Draw => 16#8D40#);
   for Framebuffer_Kind'Size use Enum'Size;
   
   for Buffer_Kind use (Array_Buffer              => 16#8892#,
                        Element_Array_Buffer      => 16#8893#,
                        Pixel_Pack_Buffer         => 16#88EB#,
                        Pixel_Unpack_Buffer       => 16#88EC#,
                        Uniform_Buffer            => 16#8A11#,
                        Texture_Buffer            => 16#8C2A#,
                        Transform_Feedback_Buffer => 16#8C8E#,
                        Copy_Read_Buffer          => 16#8F36#,
                        Copy_Write_Buffer         => 16#8F37#,
                        Draw_Indirect_Buffer      => 16#8F3F#,
                        Atomic_Counter_Buffer     => 16#92C0#);
   for Buffer_Kind'Size use Enum'Size;
   
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
   for Draw_Buffer_Index'Size use Int'Size;
   
   for Only_Depth_Buffer use (Depth_Buffer => 16#1801#);
   for Only_Depth_Buffer'Size use Enum'Size;
   
   for Only_Stencil_Buffer use (Stencil => 16#1802#);
   for Only_Stencil_Buffer'Size use Enum'Size;
   
   for Only_Depth_Stencil_Buffer use (Depth_Stencil => 16#84F9#);
   for Only_Depth_Stencil_Buffer'Size use Enum'Size;
   
   for Only_Color_Buffer use (Color => 16#1800#);
   for Only_Color_Buffer'Size use Enum'Size;
end GL.Low_Level.Enums;
