
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
with FT.Interfac;
with FT.Utilities;

with Utilities;

package body Texture_Manager is

   package Data_Vector_Package is new
     Ada.Containers.Vectors (Natural, Character_Record);
   type Character_Data_Vector is new Data_Vector_Package.Vector with null record;

   theLibrary     : FT.API.Library_Ptr;
   Face_Ptr       : FT.API.Face_Ptr;
   Character_Data : Character_Data_Vector;

   Image_Error : exception;

   --  ------------------------------------------------------------------------

   procedure Setup_Font;
   procedure Setup_Textures;

   --  ------------------------------------------------------------------------

   function Advance_X (Data : Character_Record) return GL.Types.Int is
   begin
      return Data.Advance_X;
   end Advance_X;

   --  ------------------------------------------------------------------------

   function Data (Index : GL.Types.Int) return Character_Record is

   begin
      if Character_Data.Is_Empty then
         raise Image_Error;
      end if;
      return Character_Data.Element (Natural (Index));
   end Data;

   --  ------------------------------------------------------------------------

   function Left (Data : Character_Record) return GL.Types.Int is
   begin
      return Data.Bearing.Left;
   end Left;

   --  ------------------------------------------------------------------------

   procedure Print_Character_Data (Char : Character;
                                   Data : Character_Record) is
      use GL.Types;
   begin
      Put_Line ("Character" & Char & " Data");
      Put_Line ("Width: " & Int'Image (Data.Size.Width));
      Put_Line ("Rows: " & Int'Image (Data.Size.Rows));
      Put_Line ("Left: " & Int'Image (Data.Bearing.Left));
      Put_Line ("Top: " & Int'Image (Data.Bearing.Top));
      Put_Line ("Advance X: " & Int'Image (Data.Advance_X) & " bits");
      New_Line;
   end Print_Character_Data;

   --  ------------------------------------------------------------------------

   function Rows (Data : Character_Record) return GL.Types.Int is
   begin
      return Data.Size.Rows;
   end Rows;

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

   procedure Setup_Graphic (Vertex_Buffer : in out V_Buffer) is
      use GL.Types;
      aTexture      : GL.Objects.Textures.Texture;
   begin
      if FT.Interfac.Init_FreeType (theLibrary) /= 0 then
         Put_Line ("The Freetype Library failed to load.");
         raise FT.FT_Exception;
      end if;

      Setup_Font;
      Setup_Textures;

      FT.Interfac.Done_Face (Face_Ptr);
      FT.Interfac.Done_Library (theLibrary);

   end Setup_Graphic;

   --  ------------------------------------------------------------------------

   procedure Setup_Textures is
      use System;
      use GL.Objects.Textures.Targets;
      use GL.Pixels;
      use GL.Types;
      aTexture       : GL.Objects.Textures.Texture;
      Width          : GL.Types.Size;
      Height         : GL.Types.Size;
      X_Offset       : constant GL.Types.Int := 0;
      Y_Offset       : constant GL.Types.Int := 0;
      Bitmap_Image   : GL.Objects.Textures.Image_Source;
      Char_Data      : Character_Record;
      Error_Code     : FT.FT_Error;
   begin
      for index in 0 .. 127 loop
         Put_Line ("Setup_Textures, index: " & Integer'Image (index));
         if FT.Interfac.Load_Character (Face_Ptr, long (index),
                                        FT.Interfac.Load_Render) /= 0 then
            Put_Line ("Setup_Textures, a character failed to load.");
            raise FT.FT_Exception;
         end if;
         --  Ensure that the glyph image is an anti-aliased bitmap
--           if FT.Glyphs.Render_Glyph (Face_Ptr, FT.API.Render_Mode_Mono) /= 0 then
--              Put_Line ("A character failed to render.");
--              raise FT.FT_Exception;
--           end if;

         Width := Size (FT.Glyphs.Bitmap_Width (Face_Ptr));
         Height := Size (FT.Glyphs.Bitmap_Rows (Face_Ptr));
         if Width > 0 and then Height > 0 then
            Char_Data.Size.Width := Width;
            Char_Data.Size.Rows := Height;
            Char_Data.Bearing.Left := FT.Glyphs.Bitmap_Left (Face_Ptr);
            Char_Data.Bearing.Top := FT.Glyphs.Bitmap_Top (Face_Ptr);
            Char_Data.Advance_X := FT.Image.Vector_X (FT.Glyphs.Glyph_Advance (Face_Ptr));
            Put_Line ("Setup_Textures, Width: " & GL.Types.Size'Image (Width));
            Put_Line ("Setup_Textures, Height: " & GL.Types.Size'Image (Height));

            aTexture.Initialize_Id;
            Texture_2D.Bind (aTexture);
            Put_Line ("Setup_Textures, texture bound.");
            Texture_2D.Storage (1, RGBA32I, Width, Height);
            Put_Line ("Setup_Textures, storage set.");
            Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
            Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
            Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_S
            Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_T

            Error_Code := FT.Glyphs.Bitmap_Image (Face_Ptr, Bitmap_Image);
            if Error_Code /= 0 then
               Put_Line ("Setup_Texture: " & FT.Errors.Error (Error_Code));
               raise FT.FT_Exception;
            end if;
            Put_Line ("Setup_Texture: " & FT.Errors.Error (Error_Code));

            Put_Line ("Setup_Textures, X_Offset: " & GL.Types.Size'Image (X_Offset));
            Put_Line ("Setup_Textures, Y_Offset: " & GL.Types.Size'Image (Y_Offset));
            Texture_2D.Load_Sub_Image_From_Data
              (1, X_Offset, Y_Offset, Width, Height, Red, Unsigned_Byte,
               Bitmap_Image);
            Char_Data.Texture := aTexture;
            Character_Data.Append (Char_Data);
         end if;
      end loop;
   exception
      when others =>
         Put_Line ("An exceptiom occurred in Setup_Texture.");
         raise;
   end Setup_Textures;

   --  ------------------------------------------------------------------------

   function Char_Texture (Data : Character_Record)
                          return GL.Objects.Textures.Texture is
   begin
      return Data.Texture;
   end Char_Texture;

   --  -----------------------------------------------------------------------

   function Top (Data : Character_Record) return GL.Types.Int is
   begin
      return Data.Bearing.Top;
   end Top;

   --  ------------------------------------------------------------------------

   function Width (Data : Character_Record) return GL.Types.Int is
   begin
      return Data.Size.Width;
   end Width;

   --  ------------------------------------------------------------------------

end Texture_Manager;
