
with System;
with System.Address_Image;
with Interfaces.C;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use  Ada.Text_IO;

with GL.Attributes;
with GL.Low_Level.Enums;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Types.Colors;

with FT_Glyphs;
with FT_Image;
with FT_Types;
with FT_Utilities;

with Utilities;

package body Texture_Manager is

   theLibrary    : FT_Types.FT_Library;
   Face_Ptr      : FT_Interface.FT_Face;
   Vertex_Data   : Vertex_Array;

   Image_Error : exception;

   procedure Setup_Buffer (Vertex_Buffer : in out V_Buffer;
                           X, Y, Scale   : GL.Types.Single);
   procedure Setup_Font;
   procedure Setup_Texture (aTexture : in out GL.Objects.Textures.Texture);

   --  ------------------------------------------------------------------------

   function Get_Face_Ptr return FT_Interface.FT_Face is
   begin
      return Face_Ptr;
   end Get_Face_Ptr;

   --  ------------------------------------------------------------------------

   procedure Setup_Graphic (Vertex_Buffer : in out V_Buffer;
                            aTexture      : in out GL.Objects.Textures.Texture;
                            X, Y: GL.Types.Single; Scale : GL.Types.Single;
                            Char          : Character := 'g') is
      use Interfaces.C;
   begin
      if FT_Interface.Init_FreeType (theLibrary) /= 0 then
         Put_Line ("The Freetype Library failed to load.");
         raise FT_Types.FT_Exception;
      end if;

      Setup_Font;
      if FT_Interface.Load_Character (Face_Ptr, Character'Pos (Char),
                                      FT_Types.Load_Render) /= 0 then
         Put_Line ("A character failed to load.");
         raise FT_Types.FT_Exception;
      end if;

      --  Ensure that the glyph image is an anti-aliased bitmap
      if FT_Interface.Render_Glyph (Face_Ptr) /= 0 then
         Put_Line ("A character failed to render.");
         raise FT_Types.FT_Exception;
      end if;
      FT_Utilities.Print_Character_Data (Face_Ptr, Char);

      Setup_Buffer (Vertex_Buffer, X, Y, Scale);
      Setup_Texture (aTexture);

      FT_Interface.Done_Face (Face_Ptr);
      FT_Interface.Done_Library (theLibrary);
   end Setup_Graphic;

   --  ------------------------------------------------------------------------

   procedure Setup_Buffer (Vertex_Buffer : in out V_Buffer;
                           X, Y, Scale   : GL.Types.Single) is
      use GL.Objects.Buffers;
      use GL.Objects.Textures.Targets;
      use GL.Types;
      aFace       : FT_Interface.FT_Face_Record := FT_Interface.Face (Face_Ptr);
      X_Pos       : Single := X;
      Y_Pos       : Single := Y ;
      Width       : Single := FT_Glyphs.Get_Bitmap_Width (aFace.Glyph_Slot) * Scale;
      Height      : Single := Single (FT_Glyphs.Get_Bitmap_Rows (aFace.Glyph_Slot)) * Scale;
      Num_Triangles : Int := 2;
      Stride        : Int := 4;
   begin
      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
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
      GL.Attributes.Enable_Vertex_Attrib_Array (0);

   exception
      when others =>
         Put_Line ("An exceptiom occurred in Setup_Buffer.");
         raise;
   end Setup_Buffer;

   --  ------------------------------------------------------------------------

   procedure Setup_Font is
      use Interfaces.C;
--        Font_File       : String := "/Library/Fonts/Arial.ttf";
      Font_File       : String := "/System/Library/Fonts/Helvetica.dfont";
   begin
      if FT_Interface.New_Face (theLibrary, Font_File, 0, Face_Ptr) /= 0 then
         Put_Line ("A face failed to load.");
         raise FT_Types.FT_Exception;
      end if;
      --  Set pixel size to 48 x 48
      if FT_Interface.Set_Pixel_Sizes (Face_Ptr, 0, 48) /= 0 then
         Put_Line ("Unable to set pixel sizes.");
         raise FT_Types.FT_Exception;
      end if;

      GL.Pixels.Set_Unpack_Alignment (GL.Pixels.Bytes);  --  Disable byte-alignment restriction
   exception
      when others =>
         Put_Line ("An exception occurred in Setup_Font.");
         raise;
   end Setup_Font;

   --  ------------------------------------------------------------------------

   procedure Setup_Texture (aTexture : in out GL.Objects.Textures.Texture) is
      use Interfaces.C;
      use GL.Objects.Textures.Targets;
      use GL.Pixels;
      use GL.Types;
      aFace        : FT_Interface.FT_Face_Record;
--        Border_Color : GL.Types.Colors.Color := (1.0, 0.0, 0.0, 1.0);
      Priority     : GL.Objects.Textures.Priority := 0.9;
      Width        : Size;
      Height       : Size;
      Bitmap_Image : GL.Objects.Textures.Image_Source;
   begin
      aFace := FT_Interface.Face (Face_Ptr);
      Width := Size (FT_Glyphs.Get_Bitmap_Width (aFace.Glyph_Slot));
      Height := Size (FT_Glyphs.Get_Bitmap_Rows (aFace.Glyph_Slot));
      aTexture.Initialize_Id;
      Texture_2D.Bind (aTexture);
--        Texture_2D.Set_Texture_Priority (Priority);
--        Texture_2D.Set_Border_Color (Border_Color);
      Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_S
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_T

      Bitmap_Image := FT_Glyphs.Get_Bitmap_Image (aFace.Glyph_Slot);
      Texture_2D.Load_From_Data  (0, Red, Width, Height, Red, Unsigned_Byte,
                                  Bitmap_Image);
   exception
      when others =>
         Put_Line ("An exceptiom occurred in Setup_Texture.");
         raise;
   end Setup_Texture;

   --  ------------------------------------------------------------------------

end Texture_Manager;
