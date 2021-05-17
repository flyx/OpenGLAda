--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with System;

with GL.API;

package body GL.Objects.Textures.With_2D_Loader is

   procedure Load_Empty_Texture (Object : Target;
                                 Level  : Mipmap_Level;
                                 Internal_Format : Pixels.Internal_Format;
                                 Width, Height   : Types.Size) is
   begin
      API.Tex_Image_2D (Texture_Proxy (Object).Kind, Level, Internal_Format,
                        Width, Height, 0,
                        Format_For_Loading_Empty_Texture (Internal_Format),
                        Pixels.Data_Type'First,
                        Image_Source (System.Null_Address));
      Raise_Exception_On_OpenGL_Error;
   end Load_Empty_Texture;

   procedure Load_From_Data (Object : Fillable_Target;
                             Level : Mipmap_Level;
                             Internal_Format : Pixels.Internal_Format;
                             Width, Height : Types.Size;
                             Source_Format : Pixels.Data_Format;
                             Source_Type   : Pixels.Data_Type;
                             Source        : Image_Source) is
   begin
      API.Tex_Image_2D (Texture_Proxy (Object).Kind, Level, Internal_Format,
                        Width, Height, 0, Source_Format, Source_Type, Source);
      Raise_Exception_On_OpenGL_Error;
   end Load_From_Data;

   procedure Load_Sub_Image_From_Data
     (Object : Fillable_Target;
      Level  : Mipmap_Level;
       X_Offset, Y_Offset : Int;
       Width, Height : Size;
       Format        : Pixels.Data_Format;
       Data_Type     : Pixels.Data_Type;
       Source        : Image_Source)  is
   begin
      API.Tex_Sub_Image_2D (Texture_Proxy (Object).Kind, Level, X_Offset, Y_Offset,
                        Width, Height, Format, Data_Type, Source);
      Raise_Exception_On_OpenGL_Error;
   end Load_Sub_Image_From_Data;

   procedure Load_Compressed
     (Object                    : Fillable_Target;
      Level                     : Mipmap_Level;
      Internal_Format           : Pixels.Internal_Format;
      Width, Height, Image_Size : Types.Size;
      Source                    : Image_Source) is
   begin
      API.Compressed_Tex_Image_2D
        (Texture_Proxy (Object).Kind, Level, Internal_Format, Width, Height, 0,
         Image_Size, Source);
      Raise_Exception_On_OpenGL_Error;
   end Load_Compressed;

   procedure Storage (Object : Target; Number_Of_Levels : Types.Size;
                      Internal_Format : Pixels.Internal_Format;
                      Width, Height : Types.Size) is
   begin
      API.Tex_Storage_2D (Texture_Proxy (Object).Kind, Number_Of_Levels, Internal_Format,
                          Width, Height);
      Raise_Exception_On_OpenGL_Error;
   end Storage;
end GL.Objects.Textures.With_2D_Loader;
