
with System;

with Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Objects.Textures.Targets;
with GL.Pixels;
with GL.Types.Colors;

with FT;
with FT.Errors;
with FT.Glyphs;
with FT.Image;
with FT.Faces;
with FT.Utilities;

with Utilities;

package body Texture_Manager is
   use type FT.Errors.Error_Code;

   Face_Ptr      : FT.Faces.Face_Reference;
   Vertex_Data   : Vertex_Array;

   Image_Error : exception;

   procedure Setup_Buffer (Vertex_Buffer : in out V_Buffer;
                           X, Y, Scale   : GL.Types.Single);
   procedure Setup_Font (theLibrary : FT.Library_Reference);

   procedure Setup_Texture (aTexture : in out GL.Objects.Textures.Texture);

   --  ------------------------------------------------------------------------

   procedure Setup_Buffer (Vertex_Buffer : in out V_Buffer;
                           X, Y, Scale   : GL.Types.Single) is
      use GL.Objects.Buffers;
      use GL.Objects.Textures.Targets;
      use GL.Types;
      X_Pos         : Single := X;
      Y_Pos         : Single := Y ;
      Width         : Single;
      Height        : Single;
      Num_Triangles : Int := 2;
      Stride        : Int := 4;
      Bitmap        : constant FT.Bitmap_Record :=
        FT.Glyphs.Bitmap (Face_Ptr.Glyph_Slot);

   begin
      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
      Width := Single (Bitmap.Width) * Scale;
      Height := Single (Bitmap.Rows) * Scale;
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

   procedure Setup_Font (theLibrary : FT.Library_Reference) is
      use GL.Types;
      Font_File  : String := "../../fonts/NotoSerif-Regular.ttf";
   begin
      FT.Faces.New_Face (theLibrary, Font_File, 0, Face_Ptr);
      --  Set pixel size to 48 x 48
      FT.Faces.Set_Pixel_Sizes (Face_Ptr, 0, 48);
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
      theLibrary : FT.Library_Reference;
   begin
      theLibrary.Init;
      Setup_Font (theLibrary);
      FT.Faces.Load_Character
          (Face_Ptr, Character'Pos (Char), FT.Faces.Load_Render);

      --  Ensure that the glyph image is an anti-aliased bitmap
      FT.Glyphs.Render_Glyph (Face_Ptr.Glyph_Slot, FT.Faces.Render_Mode_Mono);
      FT.Utilities.Print_Character_Metadata (Face_Ptr, Char);

      Setup_Buffer (Vertex_Buffer, X, Y, Scale);
      Setup_Texture (aTexture);
   end Setup_Graphic;

   --  ------------------------------------------------------------------------

   procedure Setup_Texture (aTexture : in out GL.Objects.Textures.Texture) is
      use GL.Objects.Textures.Targets;
      use GL.Pixels;
      use GL.Types;

      Slot         : FT.Glyph_Slot_Reference := Face_Ptr.Glyph_Slot;
      Bitmap       : constant FT.Bitmap_Record :=
                     FT.Glyphs.Bitmap (Slot);
      Width        : constant Size := Size (Bitmap.Width);
      Height       : constant Size := Size (Bitmap.Rows);
      X_Offset     : constant GL.Types.Int := 0;
      Y_Offset     : constant GL.Types.Int := 0;
      Num_Levels   : constant GL.Types.Size := 1;
      Char_Data    : Character_Record;
      Bitmap_Image : GL.Objects.Textures.Image_Source;
   begin
      FT.Faces.Set_Char_Data
                     (Char_Data, Width, Height, FT.Glyphs.Bitmap_Left (Face_Ptr),
                     FT.Glyphs.Bitmap_Top (Face_Ptr),
                     FT.Image.Vector_X (FT.Glyphs.Glyph_Advance (Face_Ptr)));

      aTexture.Initialize_Id;
      Texture_2D.Bind (aTexture);
      Texture_2D.Set_Minifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_Magnifying_Filter (GL.Objects.Textures.Linear);
      Texture_2D.Set_X_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_S
      Texture_2D.Set_Y_Wrapping (GL.Objects.Textures.Clamp_To_Edge); --  Wrap_T
      If Width < 1 and then Height < 1 then
         Texture_2D.Storage (Num_Levels, RGBA8, 1, 1);
      else
         Texture_2D.Storage (Num_Levels, RGBA8, Width, Height);
      end if;
      Bitmap_Image := Bitmap.Buffer;
      Texture_2D.Load_Sub_Image_From_Data (0, X_Offset, Y_Offset, Width, Height,
                                           Red, Unsigned_Byte, Bitmap_Image);

   exception
      when others =>
         Put_Line ("An exception occurred in Texture_Manager.Setup_Texture.");
         raise;
   end Setup_Texture;

   --  ------------------------------------------------------------------------

end Texture_Manager;
