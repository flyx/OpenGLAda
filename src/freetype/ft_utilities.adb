
with System;
with System.Address_Image;

with Interfaces.C; use Interfaces.C;

with Ada.Text_IO; use Ada.Text_IO;

with GL.Types;

with FT_Glyphs;
with FT_Types;

package body FT_Utilities is

   procedure Print_Bitmap (Bitmap : FT_Image.FT_Bitmap) is
      use GL.Types;
      use FT_Image;
   begin
      New_Line;
      Put_Line ("Bitmap data:");
      Put_Line ("Rows: " & GL.Types.int'Image (Get_Rows (Bitmap)));
      Put_Line ("Width: " & uint'Image (Get_Width (Bitmap)));
      Put_Line ("Pitch: " & GL.Types.int'Image (Get_Pitch (Bitmap)));
      Put_Line ("Num_Grays: " & GL.Types.short'Image (Get_Num_Grays (Bitmap)));
      Put_Line ("Pixel_mode: " & unsigned_char'Image (Get_Pixel_Mode (Bitmap)));
      Put_Line ("Palette_mode: " & unsigned_char'Image (Get_Palette_Mode (Bitmap)));
      New_Line;
   end Print_Bitmap;

   --  -------------------------------------------------------------------------

   procedure Print_Character_Data (Face_Ptr : FT_Interface.FT_Face; aChar : Character) is
      use GL.Types;
      use FT_Glyphs;
      Slot_Ptr  : constant FT_Types.FT_Glyph_Slot_Ptr := FT_Interface.Get_Glyph_Slot (Face_Ptr);
      Advance_X : constant GL.Types.Int := FT_Image.Vector_X (Get_Glyph_Advance (Slot_Ptr));
   begin
      Put_Line ("Character " & aChar & " Data");
      Put_Line ("Width: " & Single'Image (Get_Bitmap_Width (Slot_Ptr)));
      Put_Line ("Rows: " & GL.Types.Int'Image (Get_Bitmap_Rows (Slot_Ptr)));
      Put_Line ("Left: " & GL.Types.Int'Image (Get_Bitmap_Left (Slot_Ptr)));
      Put_Line ("Top: " & GL.Types.Int'Image (Get_Bitmap_Top (Slot_Ptr)));
      Put_Line ("Advance X: " & GL.Types.Int'Image (Advance_X) & " bits");
      Put_Line ("Glyph format: " & FT_Image.FT_Glyph_Format'Image (Get_Glyph_Format (Slot_Ptr)));
      Put_Line ("Bitmap address: " & System.Address_Image
                (System.Address (Get_Bitmap_Image (Slot_Ptr))));
      New_Line;
   end Print_Character_Data;

   --  ------------------------------------------------------------------------

end FT_Utilities;
