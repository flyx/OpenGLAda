
with System.Address_To_Access_Conversions;

with Ada.Text_IO; use  Ada.Text_IO;
with Ada.Unchecked_Conversion;

with FT_Types;

package body FT_Image is

   package Glyph_Bitmap_Access is new System.Address_To_Access_Conversions (FT_Image.FT_Bitmap);

   --  -------------------------------------------------------------------------

   --  FT_Glyph_Slot (SA) => FT_Glyph_Slot_Record => FT_Bitmap (record)
   --  FT_Bitmap => Buffer (access unsigned_char)

   function Get_Buffer (Bitmap : FT_Bitmap) return GL.Objects.Textures.Image_Source is
     Addr   : System.Address := FT_Types.Unsigned_Char_To_Address.To_Address (Bitmap.Buffer);
   begin
      return GL.Objects.Textures.Image_Source (Addr);
   end Get_Buffer;

   --  -------------------------------------------------------------------------

   function Get_Rows (Bitmap : FT_Bitmap) return GL.Types.Int is
   begin
      return GL.Types.Int (Bitmap.Rows);
   end Get_Rows;

   --  -------------------------------------------------------------------------

   function Get_Width (Bitmap : FT_Bitmap) return GL.Types.UInt is
   begin
      return GL.Types.UInt (Bitmap.Width);
   end Get_Width;

   --  -------------------------------------------------------------------------

   procedure Print_Bitmap (Bitmap : FT_Bitmap) is
      use GL.Types;
   begin
      New_Line;
      Put_Line ("Bitmap data:");
      Put_Line ("Rows: " & uint'Image (Bitmap.Rows));
      Put_Line ("Width: " & uint'Image (Bitmap.Width));
      Put_Line ("Pitch: " & GL.Types.int'Image (Bitmap.Pitch));
      Put_Line ("Num_Grays: " & GL.Types.short'Image (Bitmap.Num_Grays));
      Put_Line ("Pixel_mode: " & unsigned_char'Image (Bitmap.Pixel_mode));
      Put_Line ("Palette_mode: " & unsigned_char'Image (Bitmap.Palette_mode));
      New_Line;
   end Print_Bitmap;

   --  -------------------------------------------------------------------------

   function Vector_X (theVector : FT_Vector) return GL.Types.Int is
   begin
      return GL.Types.Int (theVector.x);
   end Vector_X;

   --  -------------------------------------------------------------------------

   function Vector_Y (theVector : FT_Vector) return GL.Types.Int is
   begin
      return GL.Types.Int (theVector.y);
   end Vector_Y;

   --  -------------------------------------------------------------------------

end FT_Image;
