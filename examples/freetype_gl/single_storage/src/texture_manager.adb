
with System;

with Ada.Containers.Vectors;
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

   type Character_Size is record
      Width     : GL.Types.Int;
      Rows      : GL.Types.Int;
   end record;

   type Character_Bearing is record
      Left      : GL.Types.Int;
      Top       : GL.Types.Int;
   end record;

   type Character_Record is record
      Texture   : GL.Objects.Textures.Texture;
      Size      : Character_Size;
      Bearing   : Character_Bearing;
      Advance_X : GL.Types.Int;
   end record;

   package Data_Vector_Package is new
     Ada.Containers.Vectors (Natural, Character_Record);
   type Character_Data_Vector is new Data_Vector_Package.Vector with null record;

   theLibrary     : FT.API.Library_Ptr;
   Face_Ptr       : FT.API.Face_Ptr;
   Character_Data : Character_Data_Vector;
   Vertex_Data    : Vertex_Array;

   Image_Error : exception;

   procedure Setup_Buffer (Vertex_Buffer : in out V_Buffer;
                           Char          : Character;
                           X, Y, Scale   : GL.Types.Single);
   procedure Setup_Font;
   procedure Setup_Texture (aTexture : in out GL.Objects.Textures.Texture);

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

   procedure Setup_Buffer (Vertex_Buffer : in out V_Buffer;
                           Char          : Character;
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
     if FT.Interfac.Load_Character (Face_Ptr, Character'Pos (Char),
                                      FT.Interfac.Load_Render) /= 0 then
         Put_Line ("A character failed to load.");
         raise FT.FT_Exception;
     end if;

      --  Ensure that the glyph image is an anti-aliased bitmap
      if FT.Glyphs.Render_Glyph (Face_Ptr, FT.API.Render_Mode_Mono) /= 0 then
         Put_Line ("A character failed to render.");
         raise FT.FT_Exception;
      end if;
      FT.Utilities.Print_Character_Metadata (Face_Ptr, Char);
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
         Put_Line ("An exceptiom occurred in Setup_Buffer.");
         raise;
   end Setup_Buffer;

   --  ------------------------------------------------------------------------

   procedure Setup_Font is
      use GL.Types;
      Font_File  : String := "/System/Library/Fonts/Helvetica.dfont";
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
                            aTexture      : in out GL.Objects.Textures.Texture;
                            X, Y: GL.Types.Single; Scale : GL.Types.Single;
                            Char          : Character := 'g') is
      use GL.Types;
   begin
      if FT.Interfac.Init_FreeType (theLibrary) /= 0 then
         Put_Line ("The Freetype Library failed to load.");
         raise FT.FT_Exception;
      end if;

      Setup_Font;
      Setup_Buffer (Vertex_Buffer, Char, X, Y, Scale);
      Setup_Texture (aTexture);

      FT.Interfac.Done_Face (Face_Ptr);
      FT.Interfac.Done_Library (theLibrary);
   end Setup_Graphic;

   --  ------------------------------------------------------------------------

   procedure Setup_Texture (aTexture : in out GL.Objects.Textures.Texture) is
      use GL.Objects.Textures.Targets;
      use GL.Pixels;
      use GL.Types;
      Width        : Size;
      Height       : Size;
      X_Offset     : constant GL.Types.Int := 0;
      Y_Offset     : constant GL.Types.Int := 0;
      Char_Data    : Character_Record;
      Bitmap_Image : GL.Objects.Textures.Image_Source;
      Error_Code   : FT.FT_Error;
   begin
      Width := Size (FT.Glyphs.Bitmap_Width (Face_Ptr));
      Height := Size (FT.Glyphs.Bitmap_Rows (Face_Ptr));

      Char_Data.Size.Width := Width;
      Char_Data.Size.Rows := Height;
      Char_Data.Bearing.Left := FT.Glyphs.Bitmap_Left (Face_Ptr);
      Char_Data.Bearing.Top := FT.Glyphs.Bitmap_Top (Face_Ptr);
      Char_Data.Advance_X := FT.Image.Vector_X (FT.Glyphs.Glyph_Advance (Face_Ptr));
      Put_Line ("Setup_Textures, Width: " & GL.Types.Size'Image (Width));
      Put_Line ("Setup_Textures, Height: " & GL.Types.Size'Image (Height));
      aTexture.Initialize_Id;
      Texture_2D.Bind (aTexture);
      Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_S
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_T

      --  Mipmap level has to be set to 1 for Storage but
      --  0 for Load_Sub_Image_From_Data!
      Texture_2D.Storage (1, RGBA8, Width, Height);

      Error_Code := FT.Glyphs.Bitmap_Image (Face_Ptr, Bitmap_Image);
      Put_Line ("Setup_Texture Bitmap_Image set, error code: " &
                       FT.Errors.Error (Error_Code));
      if Error_Code = 0 then
         Texture_2D.Load_Sub_Image_From_Data     --  glTexSubImage2D
              (0, X_Offset, Y_Offset, Width, Height, Red, Unsigned_Byte,
               Bitmap_Image);
      else
         Put_Line ("Setup_Texture Bitmap_Image error: " &
                       FT.Errors.Error (Error_Code));
         raise FT.FT_Exception;
      end if;
   exception
      when others =>
         Put_Line ("An exception occurred in Setup_Texture.");
         raise;
   end Setup_Texture;

   --  ------------------------------------------------------------------------

end Texture_Manager;
