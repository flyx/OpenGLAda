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

with GL.API;

with System;

package body GL.Objects.Textures.Loader_2D is

   procedure Load_Empty_Texture (Target: Target_Kind; Level : Integer;
                                 Internal_Format : Pixel_Data.Internal_Format;
                                 Width, Height : Natural;
                                 Border : Boolean;
                                 Format : Pixel_Data.Format;
                                 Data_Type : Pixel_Data.Data_Type) is
   begin
      API.Tex_Image_2D (Target          => Target,
                        Level           => Low_Level.Int (Level),
                        Internal_Format => Internal_Format,
                        Width           => Low_Level.Int (Width),
                        Height          => Low_Level.Int (Height),
                        Border          => Low_Level.Bool (Border),
                        Format          => Format,
                        Data_Type       => Data_Type,
                        Data            => System.Null_Address);
      Check_OpenGL_Error;
   end Load_Empty_Texture;

end GL.Objects.Textures.Loader_2D;
