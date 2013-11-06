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

package body GL.Objects.Textures.With_3D_Loader is

   procedure Load_Empty_Texture (Object: Target;
                                 Level : Mipmap_Level;
                                 Internal_Format : Pixel_Data.Internal_Format;
                                 Width, Height, Depth : Types.Size) is
   begin
      API.Tex_Image_3D (Texture_Proxy (Object).Kind, Level, Internal_Format,
                        Width, Height, Depth, 0, Pixel_Data.Format'First,
                        Pixel_Data.Data_Type'First, System.Null_Address);
      Raise_Exception_On_OpenGL_Error;
   end Load_Empty_Texture;

   procedure Load_From_Data (Object : Fillable_Target;
                             Level : Mipmap_Level;
                             Internal_Format : Pixel_Data.Internal_Format;
                             Width, Height, Depth : Types.Size;
                             Source_Format : Pixel_Data.Format;
                             Source_Type   : Pixel_Data.Data_Type;
                             Source        : System.Address) is
   begin
      API.Tex_Image_3D (Texture_Proxy (Object).Kind, Level, Internal_Format,
                        Width, Height, Depth, 0, Source_Format, Source_Type,
                        Source);
      Raise_Exception_On_OpenGL_Error;
   end Load_From_Data;

end GL.Objects.Textures.With_3D_Loader;
