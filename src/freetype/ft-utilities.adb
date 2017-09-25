
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Types;

with FT.Errors;
with FT.Glyphs;

package body FT.Utilities is

   procedure Load_Texture (Face_Ptr  : FT.API.Face_Ptr;
                           Char_Data : in out FT.Interfac.Character_Record;
                           Width, Height : GL.Types.Size;
                           X_Offset, Y_Offset : GL.Types.Int) is
      use GL.Objects.Textures.Targets;
      use GL.Pixels;
      use GL.Types;
      aTexture          : GL.Objects.Textures.Texture;
      Bitmap_Image_Ptr  : GL.Objects.Textures.Image_Source;
      Num_Levels        : constant GL.Types.Size := 1;
      Mip_Level_0       : constant GL.Objects.Textures.Mipmap_Level := 0;
      Error_Code        : FT.FT_Error;
   begin
      aTexture.Initialize_Id;
      Texture_2D.Bind (aTexture);
      Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_S
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_T

      if Width > 0 and then Height > 0 then
         Texture_2D.Storage (Num_Levels, RGBA8, Width, Height);
      else
         Texture_2D.Storage (Num_Levels, RGBA8, 1, 1);
      end if;

      Error_Code := FT.Glyphs.Bitmap_Image (Face_Ptr, Bitmap_Image_Ptr);
      if Error_Code /= 0 then
         Put_Line ("Setup_Texture: " & FT.Errors.Error (Error_Code));
         raise FT.FT_Exception;
      end if;

      Texture_2D.Load_Sub_Image_From_Data
        (Mip_Level_0, X_Offset, Y_Offset, Width, Height, Red, Unsigned_Byte,
         Bitmap_Image_Ptr);
      FT.Interfac.Set_Texture (Char_Data, aTexture);
   end Load_Texture;

   -- --------------------------------------------------------------------------

   procedure Print_Bitmap_Metadata (Bitmap : FT.Image.Bitmap_Record) is
      use GL.Types;
      use FT.Image;
   begin
      New_Line;
      Put_Line ("Bitmap data:");
      Put_Line ("Rows: " & GL.Types.int'Image (Rows (Bitmap)));
      Put_Line ("Width: " & uint'Image (Width (Bitmap)));
      Put_Line ("Pitch: " & GL.Types.int'Image (Pitch (Bitmap)));
      Put_Line ("Num_Grays: " & GL.Types.short'Image (Num_Grays (Bitmap)));
      Put_Line ("Pixel_mode: " & unsigned_char'Image (Pixel_Mode (Bitmap)));
      Put_Line ("Palette_mode: " & unsigned_char'Image (Palette_Mode (Bitmap)));
      New_Line;
   end Print_Bitmap_Metadata;

   --  -------------------------------------------------------------------------

   procedure Print_Character_Metadata (aFace : FT.API.Face_Ptr; aChar : Character) is
      use GL.Types;
      use FT.Glyphs;
      Advance_X : constant GL.Types.Int := FT.Image.Vector_X (Glyph_Advance (aFace));
   begin
      Put_Line ("Character " & aChar & " Data");
      Put_Line ("Width: " & Single'Image (Bitmap_Width (aFace)));
      Put_Line ("Rows: " & GL.Types.Int'Image (Bitmap_Rows (aFace)));
      Put_Line ("Left: " & GL.Types.Int'Image (Bitmap_Left (aFace)));
      Put_Line ("Top: " & GL.Types.Int'Image (Bitmap_Top (aFace)));
      Put_Line ("Advance X: " & GL.Types.Int'Image (Advance_X) & " bits");
      Put_Line ("Glyph format: " & FT.Image.Glyph_Format'Image (Glyph_Format (aFace)));
      New_Line;
   end Print_Character_Metadata;

   --  ------------------------------------------------------------------------

   procedure Print_Character_Metadata (Data : FT.Interfac.Character_Record) is
      use GL.Types;
      use FT.Interfac;
   begin
      Put_Line ("Width: " & GL.Types.Int'Image (Width (Data)));
      Put_Line ("Rows: " & GL.Types.Int'Image (Rows (Data)));
      Put_Line ("Left: " & GL.Types.Int'Image (Left (Data)));
      Put_Line ("Top: " & GL.Types.Int'Image (Top (Data)));
      Put_Line ("Advance X: " & GL.Types.Int'Image (Advance_X (Data)) & " bits");
      New_Line;
   end Print_Character_Metadata;

   --  ------------------------------------------------------------------------

   procedure Setup_Character_Textures
     (Face_Ptr  : FT.API.Face_Ptr;
      Character_Data : in out FT.Interfac.Character_Data_Vector) is
      use GL.Types;
      Width          : GL.Types.Size;
      Height         : GL.Types.Size;
      X_Offset       : constant GL.Types.Int := 0;
      Y_Offset       : constant GL.Types.Int := 0;
      Char_Data      : FT.Interfac.Character_Record;
   begin
      for index in Character_Data'First .. Character_Data'Last loop
         --  Load_Render asks FreeType to create an 8-bit grayscale bitmap image
         --  that can be accessed via face->glyph->bitmap.
         if FT.Interfac.Load_Character (Face_Ptr, GL.Types.long (index),
                                        FT.Interfac.Load_Render) /= 0 then
            Put_Line ("Setup_Textures, a character failed to load.");
            raise FT.FT_Exception;
         end if;
         --  Ensure that the glyph image is an anti-aliased bitmap
         if FT.Glyphs.Render_Glyph (Face_Ptr, FT.API.Render_Mode_Mono) /= 0 then
            Put_Line ("A character failed to render.");
            raise FT.FT_Exception;
         end if;

         Width := Size (FT.Glyphs.Bitmap_Width (Face_Ptr));
         Height := Size (FT.Glyphs.Bitmap_Rows (Face_Ptr));
         FT.Interfac.Set_Char_Data (Char_Data, Width, Height,
                                    FT.Glyphs.Bitmap_Left (Face_Ptr),
                                    FT.Glyphs.Bitmap_Top (Face_Ptr),
                                    FT.Image.Vector_X (FT.Glyphs.Glyph_Advance (Face_Ptr)));

         Load_Texture (Face_Ptr, Char_Data, Width, Height, X_Offset, Y_Offset);
         Character_Data (index) := Char_Data;
      end loop;
   exception
      when others =>
         Put_Line ("An exceptiom occurred in FT.Utilities.Setup_Character_Textures.");
         raise;
   end Setup_Character_Textures;

   --  ------------------------------------------------------------------------

end FT.Utilities;
