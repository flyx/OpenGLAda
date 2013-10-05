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

with Interfaces.C.Strings;
with System;

with GL.Types;

private package SOIL.API is
   pragma Preelaborate;

   package C renames Interfaces.C;

   function Load_OGL_Texture (Filename : C.char_array;
                              Force_Channels : Image_Format;
                              Reuse_Texture_Id : GL.Types.UInt;
                              Flags : Texture_Flags) return GL.Types.UInt;
   pragma Import (Convention => C, Entity => Load_OGL_Texture,
                  External_Name => "SOIL_load_OGL_texture");

   function Load_OGL_Cubemap (X_Pos_File, X_Neg_File, Y_Pos_File, Y_Neg_File,
                              Z_Pos_File, Z_Neg_File : C.char_array;
                              Force_Channels : Image_Format;
                              Reuse_Texture_Id : GL.Types.UInt;
                              Flags : Texture_Flags) return GL.Types.UInt;
   pragma Import (Convention => C, Entity => Load_OGL_Cubemap,
                  External_Name => "SOIL_load_OGL_cubemap");

   function Load_OGL_Single_Cubemap (Filename : C.char_array;
                                     Face_Order : Cubemap_Layout;
                                     Force_Channels : Image_Format;
                                     Reuse_Texture_Id : GL.Types.UInt;
                                     Flags : Texture_Flags) return GL.Types.UInt;
   pragma Import (Convention => C, Entity => Load_OGL_Single_Cubemap,
                  External_Name => "SOIL_load_OGL_single_cubemap");

   function Load_OGL_HDR_Texture
     (Filename         : C.char_array;
      Format           : Fake_HDR_Representation;
      Rescale_To_Max   : Bool;
      Reuse_Texture_Id : GL.Types.UInt;
      Flags            : Texture_Flags) return GL.Types.UInt;
   pragma Import (Convention => C, Entity => Load_OGL_HDR_Texture,
                  External_Name => "SOIL_load_OGL_HDR_texture");

   function Load_OGL_Texture_From_Memory
     (Buffer           : System.Address;
      Buffer_Length    : C.int;
      Force_Channels   : Image_Format;
      Reuse_Texture_Id : GL.Types.UInt;
      Flags            : Texture_Flags) return GL.Types.UInt;
   pragma Import (Convention => C, Entity => Load_OGL_Texture_From_Memory,
                  External_Name => "SOIL_load_OGL_texture_from_memory");

   function Load_OGL_Cubemap_From_Memory (X_Pos_Buffer : System.Address;
                                          X_Pos_Length : C.int;
                                          X_Neg_Buffer : System.Address;
                                          X_Neg_Length : C.int;
                                          Y_Pos_Buffer : System.Address;
                                          Y_Pos_Length : C.int;
                                          Y_Neg_Buffer : System.Address;
                                          Y_Neg_Length : C.int;
                                          Z_Pos_Buffer : System.Address;
                                          Z_Pos_Length : C.int;
                                          Z_Neg_Buffer : System.Address;
                                          Z_Neg_Length : C.int;
                                          Force_Channels   : Image_Format;
                                          Reuse_Texture_Id : GL.Types.UInt;
                                          Flags            : Texture_Flags)
                                          return GL.Types.UInt;
   pragma Import (Convention => C, Entity => Load_OGL_Cubemap_From_Memory,
                  External_Name => "SOIL_load_OGL_cubemap_from_memory");

   function Load_OGL_Single_Cubemap_From_Memory
     (Buffer           : System.Address;
      Buffer_Length    : C.int;
      Face_Order       : Cubemap_Layout;
      Force_Channels   : Image_Format;
      Reuse_Texture_Id : GL.Types.UInt;
      Flags            : Texture_Flags) return GL.Types.UInt;
   pragma Import (Convention => C, Entity => Load_OGL_Single_Cubemap_From_Memory,
                  External_Name => "SOIL_load_single_cubemap_from_memory");

   function Create_OGL_Texture (Data : System.Address;
                                Width, Height : GL.Types.Int;
                                Channels : Explicit_Image_Format;
                                Reuse_Texture_Id : GL.Types.UInt;
                                Flags : Texture_Flags) return GL.Types.UInt;
   pragma Import (Convention => C, Entity => Create_OGL_Texture,
                  External_Name => "SOIL_create_OGL_texture");

   function Create_OGL_Single_Cubemap (Data : System.Address;
                                       Width, Height : GL.Types.Int;
                                       Channels : Image_Format;
                                       Face_Order : Cubemap_Layout;
                                       Reuse_Texture_Id : GL.Types.UInt;
                                       Flags : Texture_Flags)
                                       return GL.Types.UInt;
   pragma Import (Convention => C, Entity => Create_OGL_Single_Cubemap,
                  External_Name => "SOIL_create_OGL_single_cubemap");

   function Save_Screenshot (Filename   : C.char_array;
                             Image_Type : Image_Save_Type;
                             X, Y, Width, Height : GL.Types.Int) return Bool;
   pragma Import (Convention => C, Entity => Save_Screenshot,
                  External_Name => "SOIL_save_screenshot");

   function Load_Image (Filename   : C.char_array;
                        Width, Height : access GL.Types.Int;
                        Channels : access Explicit_Image_Format;
                        Force_Channels : Image_Format) return System.Address;
   pragma Import (Convention => C, Entity => Load_Image,
                  External_Name => "SOIL_load_image");

   function Load_Image_From_Memory (Buffer : System.Address;
                                    Buffer_Length : C.int;
                                    Width, Height : access GL.Types.Int;
                                    Channels : access Explicit_Image_Format;
                                    Force_Channels : Image_Format)
                                    return System.Address;
   pragma Import (Convention => C, Entity => Load_Image_From_Memory,
                  External_Name => "SOIL_load_image_from_memory");

   function Save_Image (Filename      : C.char_array;
                        Image_Type    : Image_Save_Type;
                        Width, Height : GL.Types.Int;
                        Channels      : Image_Format;
                        Data          : System.Address) return Bool;
   pragma Import (Convention => C, Entity => Save_Image,
                  External_Name => "SOIL_save_image");

   procedure Free_Image_Data (Data : System.Address);
   pragma Import (Convention => C, Entity => Free_Image_Data,
                  External_Name => "SOIL_free_image_data");

   function Last_Result return C.Strings.chars_ptr;
   pragma Import (Convention => C, Entity => Last_Result,
                  External_Name => "SOIL_last_result");

end SOIL.API;
