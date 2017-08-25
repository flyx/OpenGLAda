
with System;
with System.Address_Image;
with Interfaces.C;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use  Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Textures.Targets;
with GL.Pixels;

with FT_Glyphs;
with FT_Image;
with FT_Interface;
with FT_Types;

with Utilities;

package body Texture_Manager is

   theLibrary    : FT_Types.FT_Library;
   Face_Ptr      : FT_Interface.FT_Face;
   --  2D quad as two triangles requires 2 * 3 vertices of 4 floats
   Vertex_Data   : GL.Types.Singles.Vector4_Array (1 .. 6);

   Image_Error : exception;

   procedure Setup_Buffer (Vertex_Buffer : in out V_Buffer;
                           X, Y, Scale   : GL.Types.Single);
   procedure Setup_Font;
   procedure Setup_Texture (aTexture : in out GL.Objects.Textures.Texture);

   --  ------------------------------------------------------------------------

   procedure Print_Character_Data (Char : Character) is
      use GL.Types;
      use FT_Glyphs;
      aFace     : FT_Interface.FT_Face_Record := FT_Interface.Face (Face_Ptr);
      Advance_X : GL.Types.Int := FT_Image.Vector_X (Get_Glyph_Advance (aFace.Glyph));
   begin
      Put_Line ("Character" & Char & " Data");
      Put_Line ("Width: " & Single'Image (Get_Bitmap_Width (aFace.Glyph)));
      Put_Line ("Rows: " & Int'Image (Get_Bitmap_Rows (aFace.Glyph)));
      Put_Line ("Left: " & Int'Image (Get_Bitmap_Left (aFace.Glyph)));
      Put_Line ("Top: " & Int'Image (Get_Bitmap_Top (aFace.Glyph)));
      Put_Line ("Advance X: " & Int'Image (Advance_X) & " bits");
      Put_Line ("Bitmap address: " & System.Address_Image (System.Address (Get_Bitmap_Image (aFace.Glyph))));
      New_Line;
   end Print_Character_Data;

   --  ------------------------------------------------------------------------

    procedure Setup_Graphic (Vertex_Buffer : in out V_Buffer;
                             aTexture      : in out GL.Objects.Textures.Texture;
                             X, Y: GL.Types.Single; Scale : GL.Types.Single := 1.0) is
      use Interfaces.C;
    begin
      if FT_Interface.Init_FreeType (theLibrary) /= 0 then
         Put_Line ("The Freetype Library failed to load.");
         raise FT_Types.FT_Exception;
      end if;

      Setup_Font;
      Setup_Buffer (Vertex_Buffer, X, Y, Scale);
      Setup_Texture (aTexture);
      FT_Interface.Done_Face (Face_Ptr);
      FT_Interface.Done_Library (theLibrary);
    end Setup_Graphic;

   --  ------------------------------------------------------------------------

   procedure Setup_Texture (aTexture : in out GL.Objects.Textures.Texture) is
      use Interfaces.C;
      use GL.Objects.Textures.Targets;
      use GL.Pixels;
      use GL.Types;
      aFace       : FT_Interface.FT_Face_Record := FT_Interface.Face (Face_Ptr);
      Width       : Size := Size (FT_Glyphs.Get_Bitmap_Width (aFace.Glyph));
      Height      : Size := Size (FT_Glyphs.Get_Bitmap_Rows (aFace.Glyph));
      Char        : Character := 'G';
   begin
      if FT_Interface.Load_Character (Face_Ptr, Character'Pos (Char), FT_Types.Load_Render) /= 0 then
            Put_Line ("A character failed to load.");
            raise FT_Types.FT_Exception;
      end if;
      Print_Character_Data (Char);

      aTexture.Initialize_Id;
      Texture_2D.Bind (aTexture);
      Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_S
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_T

      Texture_2D.Load_From_Data  (0, RGB, Width, Height, Red, Unsigned_Byte,
                                  FT_Glyphs.Get_Bitmap_Image (aFace.Glyph));
   exception
      when others =>
         Put_Line ("An exceptiom occurred in Setup_Texture.");
         raise;
   end Setup_Texture;

   --  ------------------------------------------------------------------------

   procedure Setup_Buffer (Vertex_Buffer : in out V_Buffer;
                           X, Y, Scale   : GL.Types.Single) is
      use GL.Objects.Buffers;
      use GL.Objects.Textures.Targets;
      use GL.Types;
      aFace       : FT_Interface.FT_Face_Record := FT_Interface.Face (Face_Ptr);
      Width       : Single := FT_Glyphs.Get_Bitmap_Width (aFace.Glyph);
      Height      : Single := Single (FT_Glyphs.Get_Bitmap_Rows (aFace.Glyph));
   begin
      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
      Vertex_Data := ((X, Y + Height,         0.0, 0.0),
                      (X, Y,                  0.0, 1.0),
                      (X + Width, Y,          1.0, 1.0),
                      (X, Y + Height,         0.0, 0.0),
                      (X + Width, Y,          1.0, 1.0),
                      (X + Width, Y + Height, 1.0, 0.0));

      Utilities.Load_Vertex_Buffer (Array_Buffer, Vertex_Data, Static_Draw);

      GL.Attributes.Set_Vertex_Attrib_Pointer (Index  => 0, Count  => 4,
                                               Kind   => GL.Types.Single_Type,
                                               Stride => 0, Offset => 0);
      GL.Attributes.Enable_Vertex_Attrib_Array (0);

   exception
      when others =>
         Put_Line ("An exceptiom occurred in Setup_Buffer.");
         raise;
   end Setup_Buffer;

   --  ------------------------------------------------------------------------

   procedure Setup_Font is
      use Interfaces.C;
      Font_File       : String := "/System/Library/Fonts/Helvetica.dfont";
   begin
      if FT_Interface.New_Face (theLibrary, Font_File, 0, Face_Ptr) /= 0 then
         Put_Line ("A face failed to load.");
         raise FT_Types.FT_Exception;
      end if;
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

end Texture_Manager;
