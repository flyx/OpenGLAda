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

with System;

with GL.API;

package body GL.Objects.Textures.With_1D_Loader is

   procedure Load_Empty_Texture (Object: Target;
                                 Level : Mipmap_Level;
                                 Internal_Format : Pixels.Internal_Format;
                                 Width : Types.Size) is
   begin
      API.Tex_Image_1D (Texture_Proxy (Object).Kind, Level, Internal_Format,
                        Width, 0, Pixels.Red,
                        Pixels.Data_Type'First,
                        Image_Source (System.Null_Address));
      Raise_Exception_On_OpenGL_Error;
   end Load_Empty_Texture;

   procedure Load_From_Data (Object : Fillable_Target;
                             Level : Mipmap_Level;
                             Internal_Format : Pixels.Internal_Format;
                             Width : Types.Size;
                             Source_Format : Pixels.Data_Format;
                             Source_Type   : Pixels.Data_Type;
                             Source        : Image_Source) is
   begin
      API.Tex_Image_1D (Texture_Proxy (Object).Kind, Level, Internal_Format,
                        Width, 0, Source_Format, Source_Type, Source);
      Raise_Exception_On_OpenGL_Error;
   end Load_From_Data;

   procedure Load_Sub_Image_From_Data
     (Object : Fillable_Target;
      Level  : Mipmap_Level;
       X_Offset, Y_Offset :Int;
       Width, Height : Size;
       Format        : Pixels.Data_Format;
       Data_Type     : Pixels.Data_Type;
       Source        : Image_Source)  is
   begin
      API.Tex_Sub_Image_1D (Texture_Proxy (Object).Kind, Level, X_Offset, Y_Offset,
                        Width, Height, Format, Data_Type, Source);
      Raise_Exception_On_OpenGL_Error;
   end Load_Sub_Image_From_Data;

   procedure Load_Compressed
     (Object            : Fillable_Target;
      Level             : Mipmap_Level;
      Internal_Format   : Pixels.Internal_Format;
      Width, Image_Size : Types.Size;
      Source            : Image_Source) is
   begin
      API.Compressed_Tex_Image_1D
        (Texture_Proxy (Object).Kind, Level, Internal_Format, Width, 0,
         Image_Size, Source);
      Raise_Exception_On_OpenGL_Error;
   end Load_Compressed;

   procedure Storage (Object : Target; Levels : Types.Size;
                      Internal_Format : Pixels.Internal_Format;
                      Width : Types.Size) is
   begin
      API.Tex_Storage_1D (Texture_Proxy (Object).Kind, Levels, Internal_Format,
                          Width);
      Raise_Exception_On_OpenGL_Error;
   end Storage;

end GL.Objects.Textures.With_1D_Loader;
