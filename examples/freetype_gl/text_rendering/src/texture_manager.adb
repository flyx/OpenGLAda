
with System;

with Interfaces.C;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use  Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Types.Colors;

with FT;
with FT.API;
with FT.Errors;
with FT.Glyphs;
with FT.Image;
with FT.Utilities;

with Utilities;

package body Texture_Manager is
   theLibrary     : FT.API.Library_Ptr;
   Face_Ptr       : FT.API.Face_Ptr;

   --  ------------------------------------------------------------------------

   procedure Setup_Font;
   procedure Setup_Textures
     (Character_Data : in out FT.Interfac.Character_Data_Vector;
      Vertex_Buffer : in out V_Buffer; X, Y, Scale : GL.Types.Single);

   --  ------------------------------------------------------------------------

   procedure Setup_Font is
      use GL.Types;
      --        Font_File       : String := "/Library/Fonts/Arial.ttf";
      Font_File       : String := "/System/Library/Fonts/Helvetica.dfont";
   begin
      if FT.Interfac.New_Face (theLibrary, Font_File, 0, Face_Ptr) /= 0 then
         Put_Line ("A face failed to load.");
         raise FT.FT_Exception;
      end if;
      --  Set pixel size to 48 x 48
      if FT.Interfac.Set_Pixel_Sizes (Face_Ptr, 0, 48) /= 0 then
         Put_Line ("Unable to set pixel sizes.");
         raise FT.FT_Exception;
      end if;

      GL.Pixels.Set_Unpack_Alignment (GL.Pixels.Bytes);  --  Disable byte-alignment restriction
   exception
      when others =>
         Put_Line ("An exception occurred in Setup_Font.");
         raise;
   end Setup_Font;

   --  ------------------------------------------------------------------------

   procedure Setup_Graphic (Vertex_Buffer : in out V_Buffer;
              Character_Data : in out FT.Interfac.Character_Data_Vector;
              X, Y: GL.Types.Single; Scale : GL.Types.Single := 1.0) is
      use GL.Types;
      aTexture      : GL.Objects.Textures.Texture;
   begin
      if FT.Interfac.Init_FreeType (theLibrary) /= 0 then
         Put_Line ("The Freetype Library failed to load.");
         raise FT.FT_Exception;
      end if;

      Setup_Font;
      Setup_Textures (Character_Data, Vertex_Buffer, X, Y, Scale);

      FT.Interfac.Done_Face (Face_Ptr);
      FT.Interfac.Done_Library (theLibrary);

   end Setup_Graphic;

   --  ------------------------------------------------------------------------

   procedure Setup_Textures
     (Character_Data : in out FT.Interfac.Character_Data_Vector;
      Vertex_Buffer : in out V_Buffer; X, Y, Scale : GL.Types.Single) is
      use System;
      use GL.Objects.Textures.Targets;
      use GL.Pixels;
      use GL.Types;
      Width          : GL.Types.Size;
      Height         : GL.Types.Size;
      X_Offset       : constant GL.Types.Int := 0;
      Y_Offset       : constant GL.Types.Int := 0;
      Char_Data      : FT.Interfac.Character_Record;
      Num_Levels     : constant GL.Types.Size := 1;
      Mip_Level_0    : constant GL.Objects.Textures.Mipmap_Level := 0;
      Error_Code     : FT.FT_Error;
   begin
--        for index in 0 .. 127 loop
      for index in 95 .. 127 loop
         Put_Line ("Setup_Textures, index: " & Integer'Image (index));
         --  Load_Render asks FreeType to create an 8-bit grayscale bitmap image
         --  that can be accessed via face->glyph->bitmap.
         if FT.Interfac.Load_Character (Face_Ptr, long (index),
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
         if Width > 0 and then Height > 0 then
            FT.Interfac.Set_Char_Data (Char_Data, Width, Height,
                                       FT.Glyphs.Bitmap_Left (Face_Ptr),
                                       FT.Glyphs.Bitmap_Top (Face_Ptr),
                                       FT.Image.Vector_X (FT.Glyphs.Glyph_Advance (Face_Ptr)));
            Put_Line ("Setup_Textures, Width: " & GL.Types.Size'Image (Width));
            Put_Line ("Setup_Textures, Height: " & GL.Types.Size'Image (Height));

            declare
               aTexture          : GL.Objects.Textures.Texture;
               Bitmap_Image_Ptr  : GL.Objects.Textures.Image_Source;
            begin
               aTexture.Initialize_Id;
               Texture_2D.Bind (aTexture);
               Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
               Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
               Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_S
               Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_T

               Texture_2D.Storage (Num_Levels, RGBA8, Width, Height);

               Error_Code := FT.Glyphs.Bitmap_Image (Face_Ptr, Bitmap_Image_Ptr);
               if Error_Code /= 0 then
                  Put_Line ("Setup_Texture: " & FT.Errors.Error (Error_Code));
                  raise FT.FT_Exception;
               end if;

               Texture_2D.Load_Sub_Image_From_Data
                   (Mip_Level_0, X_Offset, Y_Offset, Width, Height, Red, Unsigned_Byte,
                    Bitmap_Image_Ptr);
               FT.Interfac.Set_Texture (Char_Data, aTexture);
               FT.Interfac.Append_Data (Character_Data, Char_Data);
            end;  -- declare block
         end if;
      end loop;
   exception
      when others =>
         Put_Line ("An exceptiom occurred in Setup_Texture.");
         raise;
   end Setup_Textures;

   --  ------------------------------------------------------------------------

end Texture_Manager;
