--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <flyx@isobeef.org>
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

with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Blending;
with GL.Objects.Buffers;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Pixels;
with GL.Toggles;

with FT.Glyphs;

package body FT.Utilities is
   use Interfaces.C;
    procedure Load_Vertex_Buffer is new
     GL.Objects.Buffers.Load_To_Buffer (GL.Types.Singles.Vector4_Pointers);

   theLibrary           : FT.Library_Ptr;
   Face_Ptr             : FT.Faces.Face_Ptr;
   Vertex_Array         : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer        : GL.Objects.Buffers.Buffer;
   Extended_Ascii_Data  : FT.Faces.Character_Data_Vector (0 .. 255);

   procedure Setup_Character_Textures (Face_Ptr  : FT.Faces.Face_Ptr);
   procedure Setup_Font (Font_File : String);

   --  ------------------------------------------------------------------------

   procedure Initialize_Font_Data (Font_File : String) is
      use GL.Types;
   begin
      FT.Initialize (theLibrary);
      Setup_Font (Font_File);
      Setup_Character_Textures (Face_Ptr);

      FT.Faces.Done_Face (Face_Ptr);
      FT.Done_Library (theLibrary);
   end Initialize_Font_Data;

   --  ------------------------------------------------------------------------

   procedure Load_Texture (Face_Ptr  : FT.Faces.Face_Ptr;
                           Char_Data : in out FT.Faces.Character_Record;
                           Width, Height : GL.Types.Size;
                           X_Offset, Y_Offset : GL.Types.Int) is
      use GL.Objects.Textures.Targets;
      use GL.Pixels;
      use GL.Types;
      aTexture          : GL.Objects.Textures.Texture;
      Bitmap_Image_Ptr  : GL.Objects.Textures.Image_Source;
      Num_Levels        : constant GL.Types.Size := 1;
      Mip_Level_0       : constant GL.Objects.Textures.Mipmap_Level := 0;
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

      FT.Glyphs.Bitmap_Image (Face_Ptr, Bitmap_Image_Ptr);

      Texture_2D.Load_Sub_Image_From_Data
        (Mip_Level_0, X_Offset, Y_Offset, Width, Height, Red, Unsigned_Byte,
         Bitmap_Image_Ptr);
      FT.Faces.Set_Texture (Char_Data, aTexture);
   exception
      when others =>
         Put_Line ("An exception occurred in FT.Utilities.Load_Texture.");
         raise;
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

   procedure Print_Character_Metadata (aFace : FT.Faces.Face_Ptr; aChar : Character) is
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

   procedure Print_Character_Metadata (Data : FT.Faces.Character_Record) is
      use GL.Types;
      use FT.Faces;
   begin
      Put_Line ("Width: " & GL.Types.Int'Image (Width (Data)));
      Put_Line ("Rows: " & GL.Types.Int'Image (Rows (Data)));
      Put_Line ("Left: " & GL.Types.Int'Image (Left (Data)));
      Put_Line ("Top: " & GL.Types.Int'Image (Top (Data)));
      Put_Line ("Advance X: " & GL.Types.Int'Image (Advance_X (Data)) & " bits");
      New_Line;
   end Print_Character_Metadata;

   --  ------------------------------------------------------------------------

   procedure Render_Text (Render_Program : GL.Objects.Programs.Program;
                          Text   : String; X, Y, Scale : GL.Types.Single;
                          Colour : GL.Types.Colors.Basic_Color;
                          Texture_ID, Projection_Matrix_ID, Colour_ID : GL.Uniforms.Uniform;
                          Projection_Matrix : GL.Types.Singles.Matrix4) is
      use GL.Objects.Buffers;
      use GL.Objects.Textures.Targets;
      use GL.Types.Colors;
      use GL.Types;
      use FT.Faces;

      Num_Triangles  : constant GL.Types.Int := 2;
      Num_Vertices   : constant GL.Types.Int := Num_Triangles * 3; -- Two triangles
      Num_Components : constant GL.Types.Int := 4;                 -- Coords vector size;
      Stride         : constant GL.Types.Int := 0;

      Char           : Character;
      Char_Data      : FT.Faces.Character_Record;
      Char_Texture   : GL.Objects.Textures.Texture;
      X_Orig         : Single := X;
      Y_Orig         : constant Single := Y;
      X_Pos          : Single;
      Y_Pos          : Single;
      Char_Width     : Single;
      Height         : Single;
      --  2D quad as two triangles requires 2 * 3 vertices of 4 floats
      Vertex_Data    : Singles.Vector4_Array (1 .. Num_Vertices);
   begin
      --  Blending allows a fragment colour's alpha value to control the resulting
      --  colour which will be transparent for all the glyph's background colours and
      --  non-transparent for the actual character pixels.
      GL.Toggles.Enable (GL.Toggles.Blend);
      GL.Blending.Set_Blend_Func (GL.Blending.Src_Alpha,
                                  GL.Blending.One_Minus_Src_Alpha);
      GL.Objects.Programs.Use_Program (Render_Program);

      for index in Text'Range loop
         Char := Text (index);
         Char_Data := Extended_Ascii_Data (Character'Pos (Char));
         X_Pos := X_Orig + Single (Left (Char_Data)) * Scale;
         Y_Pos := Y_Orig - Single (Rows (Char_Data) - Top (Char_Data)) * Scale;
         Char_Width := Single (Width (Char_Data)) * Scale;
         Height := Single (Rows (Char_Data)) * Scale;

         Vertex_Data := ((X_Pos, Y_Pos + Height,             0.0, 0.0),
                         (X_Pos, Y_Pos,                      0.0, 1.0),
                         (X_Pos + Char_Width, Y_Pos,         1.0, 1.0),

                         (X_Pos, Y_Pos + Height,              0.0, 0.0),
                         (X_Pos + Char_Width, Y_Pos,          1.0, 1.0),
                         (X_Pos + Char_Width, Y_Pos + Height, 1.0, 0.0));

         Vertex_Array.Bind;
         Array_Buffer.Bind (Vertex_Buffer);
         Load_Vertex_Buffer (Array_Buffer, Vertex_Data, Dynamic_Draw);

         Char_Texture := Character_Texture (Char_Data);
         if not GL.Objects.Textures.Is_Texture  (Char_Texture.Raw_Id) then
            Put_Line ("FT.Utilities.Render_Text, aTexture is invalid.");
         end if;

         GL.Objects.Textures.Set_Active_Unit (0);
         Texture_2D.Bind (Char_Texture);
         GL.Uniforms.Set_Int (Texture_ID, 0);
         GL.Uniforms.Set_Single (Colour_ID, Colour (R), Colour (G), Colour (B));
         GL.Uniforms.Set_Single (Projection_Matrix_ID, Projection_Matrix);

         GL.Attributes.Enable_Vertex_Attrib_Array (0);
         Array_Buffer.Bind (Vertex_Buffer);
         GL.Attributes.Set_Vertex_Attrib_Pointer (Index  => 0, Count  => Num_Components,
                                                  Kind   => GL.Types.Single_Type,
                                                  Stride => Stride, Offset => 0);

         GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, Num_Vertices);
         GL.Attributes.Disable_Vertex_Attrib_Array (0);

         --  Bitshift by 6 to get value in pixels (2^6 = 64
         --  (divide amount of 1/64th pixels by 64 to get amount of pixels))
         X_Orig := X_Orig + Single (Advance_X (Char_Data)) / 64.0 * Scale;
      end loop;
      GL.Toggles.Disable (GL.Toggles.Blend);

   exception
      when  others =>
         Put_Line ("An exception occurred in FT.Utilities.Render_Text.");
         raise;
   end Render_Text;

   --  ------------------------------------------------------------------------

   procedure Setup_Character_Textures
     (Face_Ptr  : FT.Faces.Face_Ptr) is
      use GL.Objects.Buffers;
      use GL.Types;
      Width          : GL.Types.Size;
      Height         : GL.Types.Size;
      X_Offset       : constant GL.Types.Int := 0;
      Y_Offset       : constant GL.Types.Int := 0;
      Char_Data      : FT.Faces.Character_Record;
   begin
      --  Blending allows a fragment colour's alpha value to control the resulting
      --  colour which will be transparent for all the glyph's background colours and
      --  non-transparent for the actual character pixels.
      GL.Toggles.Enable (GL.Toggles.Blend);
      GL.Blending.Set_Blend_Func (GL.Blending.Src_Alpha,
                                  GL.Blending.One_Minus_Src_Alpha);

      Vertex_Array.Initialize_Id;
      Vertex_Array.Bind;

      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);

      for index in Extended_Ascii_Data'Range loop
         --  Load_Render asks FreeType to create an 8-bit grayscale bitmap image
         --  that can be accessed via face->glyph->bitmap.
         FT.Faces.Load_Character (Face_Ptr, GL.Types.long (index),
                                  FT.Faces.Load_Render);
         --  Ensure that the glyph image is an anti-aliased bitmap
         FT.Glyphs.Render_Glyph (Face_Ptr, FT.Faces.Render_Mode_Mono);

         Width := GL.Types.Size (FT.Glyphs.Bitmap_Width (Face_Ptr));
         Height := GL.Types.Size (FT.Glyphs.Bitmap_Rows (Face_Ptr));
         FT.Faces.Set_Char_Data (Char_Data, Width, Height,
                     FT.Glyphs.Bitmap_Left (Face_Ptr),
                     FT.Glyphs.Bitmap_Top (Face_Ptr),
                     FT.Image.Vector_X (FT.Glyphs.Glyph_Advance (Face_Ptr)));

         Load_Texture (Face_Ptr, Char_Data, Width, Height, X_Offset, Y_Offset);
         Extended_Ascii_Data (index) := Char_Data;
      end loop;

   exception
      when others =>
         Put_Line ("An exception occurred in FT.Utilities.Setup_Character_Textures.");
         raise;
   end Setup_Character_Textures;

   --  ------------------------------------------------------------------------

   procedure Setup_Font (Font_File : String) is
      use GL.Types;
   begin
      FT.Faces.New_Face (theLibrary, Font_File, 0, Face_Ptr);
      --  Set pixel size to 48 x 48
      FT.Faces.Set_Pixel_Sizes (Face_Ptr, 0, 48);
      --  Disable byte-alignment restriction
      GL.Pixels.Set_Unpack_Alignment (GL.Pixels.Bytes);
   exception
      when others =>
         Put_Line ("An exception occurred in FT.Utilities.Setup_Font.");
         raise;
   end Setup_Font;

   --  ------------------------------------------------------------------------

end FT.Utilities;
