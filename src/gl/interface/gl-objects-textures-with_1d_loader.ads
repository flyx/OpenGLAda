--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

generic
   type Base (<>) is new Texture_Proxy with private;
package GL.Objects.Textures.With_1D_Loader is
   pragma Preelaborate;

   type Target is new Base with null record;

   procedure Load_Empty_Texture
     (Object : Target; Level : Mipmap_Level;
      Internal_Format : Pixels.Internal_Format;
      Width : Types.Size);

   procedure Storage (Object : Target; Levels : Types.Size;
                      Internal_Format : Pixels.Internal_Format;
                      Width : Types.Size);

   type Fillable_Target is new With_1D_Loader.Target with null record;

   procedure Load_From_Data
     (Object : Fillable_Target; Level : Mipmap_Level;
      Internal_Format : Pixels.Internal_Format;
      Width           : Types.Size;
      Source_Format   : Pixels.Data_Format;
      Source_Type     : Pixels.Data_Type;
      Source          : Image_Source);

   procedure Load_Sub_Image_From_Data
     (Object : Fillable_Target; Level : Mipmap_Level;
       X_Offset, Y_Offset : Int;
       Width, Height : Size;
       Format        : Pixels.Data_Format;
       Data_Type     : Pixels.Data_Type;
      Source         : Image_Source);

   procedure Load_Compressed
     (Object            : Fillable_Target;
      Level             : Mipmap_Level;
      Internal_Format   : Pixels.Internal_Format;
      Width, Image_Size : Types.Size;
      Source            : Image_Source);
end GL.Objects.Textures.With_1D_Loader;
