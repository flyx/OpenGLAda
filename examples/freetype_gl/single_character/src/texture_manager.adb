
with System;

with Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use  Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Types.Colors;

with FT;
with FT.API;
with Errors;
with FT.Glyphs;
with FT.Image;
with FT.Faces;
with FT.Utilities;

with Utilities;

package body Texture_Manager is
   use type Errors.Error_Code;

   theLibrary    : FT.Library_Ptr;
   Face_Ptr      : FT.API.Face_Ptr;
   Vertex_Data   : Vertex_Array;

   Image_Error : exception;

   procedure Setup_Buffer (Vertex_Buffer : in out V_Buffer;
                           X, Y, Scale   : GL.Types.Single);
   procedure Setup_Font;
   procedure Setup_Texture (aTexture : in out GL.Objects.Textures.Texture);

   --  ------------------------------------------------------------------------

   procedure Setup_Buffer (Vertex_Buffer : in out V_Buffer;
                           X, Y, Scale   : GL.Types.Single) is
      use GL.Objects.Buffers;
      use GL.Objects.Textures.Targets;
      use GL.Types;
      X_Pos       : Single := X;
      Y_Pos       : Single := Y ;
      Width       : Single;
      Height      : Single;
      Num_Triangles : Int := 2;
      Stride        : Int := 4;
   begin
      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
      Width := FT.Glyphs.Bitmap_Width (Face_Ptr) * Scale;
      Height := Single (FT.Glyphs.Bitmap_Rows (Face_Ptr)) * Scale;
      Vertex_Data := (
                      (X_Pos, Y_Pos,                  0.0, 0.0),  --  Lower left
                      (X_Pos + Width, Y_Pos,          1.0, 0.0),  --  Lower right
                      (X_Pos, Y_Pos + Height,         0.0, 1.0),  --  Upper left

                      (X_Pos, Y_Pos + Height,         0.0, 1.0),  --  Upper left
                      (X_Pos + Width, Y_Pos + Height, 1.0, 1.0),  --  Upper Right
                      (X_Pos + Width, Y_Pos,          1.0, 0.0)); --  Lower right

      Utilities.Load_Vertex_Buffer (Array_Buffer, Vertex_Data, Static_Draw);
      GL.Attributes.Set_Vertex_Attrib_Pointer (Index  => 0, Count  => Num_Triangles,
                                               Kind   => GL.Types.Single_Type,
                                               Stride => Stride, Offset => 0);
   exception
      when others =>
         Put_Line ("An exception occurred in Texture_Manager.Setup_Buffer.");
         raise;
   end Setup_Buffer;

   --  ------------------------------------------------------------------------

   procedure Setup_Font is
      use GL.Types;
      Font_File  : String := "../../fonts/NotoSerif-Regular.ttf";
   begin
      if not Ada.Directories.Exists (Font_File) then
         raise FT.FreeType_Exception with
           "Texture_Manager.Setup_Font cannot find " & Font_File;
      end if;
      if FT.Faces.New_Face (theLibrary, Font_File, 0, Face_Ptr) /=
        Errors.Ok then
         raise FT.FreeType_Exception with
             "Texture_Manager.Setup_Font - A face failed to load.";
      end if;
      --  Set pixel size to 48 x 48
      if FT.Faces.Set_Pixel_Sizes (Face_Ptr, 0, 48) /= Errors.Ok then
         raise FT.FreeType_Exception with "Unable to set pixel sizes.";
      end if;

      GL.Pixels.Set_Unpack_Alignment (GL.Pixels.Bytes);  --  Disable byte-alignment restriction
   exception
      when others =>
         Put_Line ("An exception occurred in Texture_Manager.Setup_Font.");
         raise;
   end Setup_Font;

   --  ------------------------------------------------------------------------

   procedure Setup_Graphic (Vertex_Buffer : in out V_Buffer;
                            aTexture      : in out GL.Objects.Textures.Texture;
                            X, Y: GL.Types.Single; Scale : GL.Types.Single;
                            Char          : Character := 'g') is
      use GL.Types;
      Error_Code   : Errors.Error_Code;
   begin
      if FT.Initialize (theLibrary) /= Errors.Ok then
         raise FT.FreeType_Exception with
             "Texture_Manager.Setup_Graphic - The Freetype Library failed to load.";
      end if;

      Setup_Font;
      Error_Code := FT.Faces.Load_Character
          (Face_Ptr, Character'Pos (Char), FT.Faces.Load_Render);
      if Error_Code /= Errors.Ok then
         raise FT.FreeType_Exception with
             "Texture_Manager.Setup_Graphic - A character failed to load."
             & Errors.Description (Error_Code);
      end if;

      --  Ensure that the glyph image is an anti-aliased bitmap
      if FT.Glyphs.Render_Glyph (Face_Ptr, FT.API.Render_Mode_Mono) /=
        Errors.Ok then
         raise FT.FreeType_Exception with
             "Texture_Manager.Setup_Graphic - A character failed to render.";
      end if;
      FT.Utilities.Print_Character_Metadata (Face_Ptr, Char);

      Setup_Buffer (Vertex_Buffer, X, Y, Scale);
      Setup_Texture (aTexture);

      FT.Faces.Done_Face (Face_Ptr);
      FT.Faces.Done_Library (theLibrary);
   end Setup_Graphic;

   --  ------------------------------------------------------------------------

   procedure Setup_Texture (aTexture : in out GL.Objects.Textures.Texture) is
      use GL.Objects.Textures.Targets;
      use GL.Pixels;
      use GL.Types;
      Width        : Size;
      Height       : Size;
      Bitmap_Image : GL.Objects.Textures.Image_Source;
   begin
      Width := Size (FT.Glyphs.Bitmap_Width (Face_Ptr));
      Height := Size (FT.Glyphs.Bitmap_Rows (Face_Ptr));

      aTexture.Initialize_Id;
      Texture_2D.Bind (aTexture);
      Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_S
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_T
      FT.Glyphs.Bitmap_Image (Face_Ptr, Bitmap_Image);
      Texture_2D.Load_From_Data  (0, Red, Width, Height, Red, Unsigned_Byte,
                                  Bitmap_Image);
   exception
      when others =>
         Put_Line ("An exception occurred in Texture_Manager.Setup_Texture.");
         raise;
   end Setup_Texture;

   --  ------------------------------------------------------------------------

end Texture_Manager;
